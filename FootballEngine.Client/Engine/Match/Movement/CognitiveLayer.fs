namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain


type MentalState =
    { ComposureLevel: float
      ConfidenceLevel: float
      FocusLevel: float
      AggressionLevel: float
      RiskTolerance: float }

module MentalState =
    let initial (player: Player) =
        { ComposureLevel = float player.Mental.Composure / 100.0
          ConfidenceLevel = float player.Morale / 100.0
          FocusLevel = float player.Mental.Concentration / 100.0
          AggressionLevel = float player.Mental.Aggression / 100.0
          RiskTolerance = 0.5 }

    let updateAfterSuccess ms =
        { ms with
            ConfidenceLevel = min 1.0 (ms.ConfidenceLevel + 0.05)
            ComposureLevel = min 1.0 (ms.ComposureLevel + 0.02) }

    let updateAfterFailure ms =
        { ms with
            ConfidenceLevel = max 0.0 (ms.ConfidenceLevel - 0.08)
            ComposureLevel = max 0.0 (ms.ComposureLevel - 0.03) }

    let updateFocus isInvolved dt ms =
        let focusDelta = if isInvolved then 0.02 * dt else -0.01 * dt

        { ms with
            FocusLevel = System.Math.Clamp(ms.FocusLevel + focusDelta, 0.0, 1.0) }

    let effectiveVision (ms: MentalState) baseVision =
        float baseVision / 100.0 * ms.FocusLevel

    let effectivePositioning (ms: MentalState) basePositioning =
        float basePositioning / 100.0 * ms.ComposureLevel

type ChemistryGraph =
    { Familiarity: float[,]
      Leadership: float[]
      PlayerCount: int }

module ChemistryGraph =
    let init playerCount =
        { Familiarity = Array2D.create playerCount playerCount 0.5
          Leadership = Array.zeroCreate playerCount
          PlayerCount = playerCount }

    let familiarity (cg: ChemistryGraph) i j =
        if i >= 0 && i < cg.PlayerCount && j >= 0 && j < cg.PlayerCount then
            cg.Familiarity.[i, j]
        else
            0.5

    let leadershipCorrections (cg: ChemistryGraph) leaderIdx (positions: Spatial[]) =
        let leaderPos = positions[leaderIdx]

        positions
        |> Array.mapi (fun i pos ->
            let dx = pos.X - leaderPos.X
            let dy = pos.Y - leaderPos.Y
            let dist = sqrt (dx * dx + dy * dy)
            let pullStrength = cg.Leadership[leaderIdx] * 0.1
            let pullX = if dist > 0.0 then dx / dist * pullStrength else 0.0
            let pullY = if dist > 0.0 then dy / dist * pullStrength else 0.0
            (i, pullX, pullY))
        |> Array.toList

type GameRead =
    { NearestOpponentDist: float
      NearestTeammateDist: float
      SpaceBehindDefense: float
      PressureCount: int
      SupportCount: int
      DefensiveLineX: float
      TeamCentroidX: float
      TeamCentroidY: float
      MomentumDirection: float }

module GameRead =
    let compute meIdx mySide oppSide ballX ballY dir momentum : GameRead =

        let myPos = mySide.Positions[meIdx]

        let nearestOppDist =
            oppSide.Positions
            |> Array.map (fun op ->
                let dx = op.X - myPos.X
                let dy = op.Y - myPos.Y
                sqrt (dx * dx + dy * dy))
            |> Array.min

        let nearestTeammateDist =
            mySide.Positions
            |> Array.mapi (fun i pos ->
                if i = meIdx then
                    System.Double.MaxValue
                else
                    let dx = pos.X - myPos.X
                    let dy = pos.Y - myPos.Y
                    sqrt (dx * dx + dy * dy))
            |> Array.min

        let defensiveLineX =
            match dir with
            | LeftToRight -> oppSide.Positions |> Array.maxBy (fun p -> p.X) |> (fun p -> p.X)
            | RightToLeft -> oppSide.Positions |> Array.minBy (fun p -> p.X) |> (fun p -> p.X)

        let spaceBehindDefense =
            match dir with
            | LeftToRight -> 100.0 - defensiveLineX
            | RightToLeft -> defensiveLineX

        let pressureCount =
            oppSide.Positions
            |> Array.filter (fun op ->
                let dx = op.X - myPos.X
                let dy = op.Y - myPos.Y
                sqrt (dx * dx + dy * dy) < 8.0)
            |> Array.length

        let supportCount =
            mySide.Positions
            |> Array.mapi (fun i pos -> i, pos)
            |> Array.filter (fun (i, pos) ->
                if i = meIdx then
                    false
                else
                    let dx = pos.X - myPos.X
                    let dy = pos.Y - myPos.Y
                    sqrt (dx * dx + dy * dy) < 15.0)
            |> Array.length

        let centroidX = mySide.Positions |> Array.averageBy (fun p -> p.X)
        let centroidY = mySide.Positions |> Array.averageBy (fun p -> p.Y)

        { NearestOpponentDist = nearestOppDist
          NearestTeammateDist = nearestTeammateDist
          SpaceBehindDefense = spaceBehindDefense
          PressureCount = pressureCount
          SupportCount = supportCount
          DefensiveLineX = defensiveLineX
          TeamCentroidX = centroidX
          TeamCentroidY = centroidY
          MomentumDirection = momentum }


module CognitiveLayer =
    let CognitiveInterval = 6

    type CognitiveUpdate =
        { NewDirectives: Directive list
          RemovedKinds: DirectiveKind list
          MentalStateDelta: MentalState option
          RunAssignments: RunAssignment list }

    let evaluate
        currentSecond
        (player: Player)
        meIdx
        (mySide: TeamSide)
        (oppSide: TeamSide)
        ballX
        ballY
        dir
        momentum
        mentalState
        chemistry
        existingDirectives
        : CognitiveUpdate =

        let gameRead = GameRead.compute meIdx mySide oppSide ballX ballY dir momentum

        let newDirectives = ResizeArray<Directive>()
        let forwardDir = AttackDir.forwardX dir
        let myPos = mySide.Positions[meIdx]

        let ballRelativeShapeX =
            match player.Position with
            | GK ->
                match dir with
                | LeftToRight -> 5.0
                | RightToLeft -> 95.0
            | DR
            | DC
            | DL
            | WBR
            | WBL -> ballX - 8.0 * forwardDir
            | DM -> ballX - 3.0 * forwardDir
            | MC
            | MR
            | ML -> ballX + 2.0 * forwardDir
            | AMR
            | AML
            | AMC -> ballX + 8.0 * forwardDir
            | ST -> ballX + 12.0 * forwardDir

        let ballRelativeShapeY =
            match player.Position with
            | GK -> 50.0
            | DR -> myPos.Y * 0.7 + 15.0 * 0.3
            | DL -> myPos.Y * 0.7 + (100.0 - myPos.Y) * 0.3
            | _ -> ballY * 0.3 + myPos.Y * 0.7

        newDirectives.Add(
            Directive.create
                Shape
                (System.Math.Clamp(ballRelativeShapeX, 2.0, 98.0))
                (System.Math.Clamp(ballRelativeShapeY, 2.0, 98.0))
                0.5
                0.4
                (currentSecond + 4)
                "cognitive"
        )

        let findNearestOpponent () =
            oppSide.Positions
            |> Array.mapi (fun i pos -> i, pos)
            |> Array.minBy (fun (_, op) ->
                let dx = op.X - myPos.X
                let dy = op.Y - myPos.Y
                dx * dx + dy * dy)

        match player.Position with
        | GK ->
            if gameRead.PressureCount >= 2 then
                let coverX = gameRead.TeamCentroidX - 3.0 * forwardDir
                let coverY = gameRead.TeamCentroidY
                newDirectives.Add(Directive.create Cover coverX coverY 0.7 0.8 (currentSecond + 10) "cognitive")
        | DR
        | DC
        | DL
        | WBR
        | WBL ->
            if gameRead.PressureCount >= 1 then
                let oppIdx, oppPos = findNearestOpponent ()
                newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.7 0.7 (currentSecond + 10) "cognitive")
            else
                let markZoneX = (ballX + myPos.X) / 2.0
                let markZoneY = (ballY + myPos.Y) / 2.0

                newDirectives.Add(Directive.create MarkZone markZoneX markZoneY 0.7 0.5 (currentSecond + 4) "cognitive")
        | DM
        | MC
        | MR
        | ML ->
            let supportTargetX = ballX + 5.0 * forwardDir
            let supportTargetY = ballY

            newDirectives.Add(
                Directive.create Support supportTargetX supportTargetY 0.6 0.5 (currentSecond + 4) "cognitive"
            )

            if gameRead.SpaceBehindDefense > 20.0 then
                let runTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 98.0)
                newDirectives.Add(Directive.create Run runTargetX ballY 0.6 0.6 (currentSecond + 4) "cognitive")
        | AMR
        | AML ->
            let supportTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 98.0)

            let supportTargetY =
                match player.Position with
                | AML -> gameRead.TeamCentroidY - 15.0
                | AMR -> gameRead.TeamCentroidY + 15.0
                | _ -> ballY

            newDirectives.Add(
                Directive.create Support supportTargetX supportTargetY 0.6 0.5 (currentSecond + 4) "cognitive"
            )

            newDirectives.Add(
                Directive.create Flank supportTargetX supportTargetY 0.6 0.5 (currentSecond + 4) "cognitive"
            )

            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.6
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 98.0)

            newDirectives.Add(
                Directive.create Run runTargetX supportTargetY 0.7 runUrgency (currentSecond + 4) "cognitive"
            )
        | AMC ->
            let supportTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 98.0)
            let supportTargetY = ballY

            newDirectives.Add(
                Directive.create Support supportTargetX supportTargetY 0.6 0.5 (currentSecond + 4) "cognitive"
            )

            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.6
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 98.0)

            newDirectives.Add(
                Directive.create Run runTargetX supportTargetY 0.7 runUrgency (currentSecond + 4) "cognitive"
            )
        | ST ->
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 98.0)
            let runTargetY = ballY
            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.7

            newDirectives.Add(Directive.create Run runTargetX runTargetY 0.7 runUrgency (currentSecond + 4) "cognitive")

            newDirectives.Add(Directive.create Support runTargetX runTargetY 0.5 0.5 (currentSecond + 4) "cognitive")


        let updatedMental =
            let newFocus =
                if gameRead.PressureCount > 0 then
                    { mentalState with
                        FocusLevel = min 1.0 (mentalState.FocusLevel + 0.02) }
                else
                    mentalState

            Some newFocus

        { NewDirectives = newDirectives |> Seq.toList
          RemovedKinds = []
          MentalStateDelta = updatedMental
          RunAssignments = [] }
