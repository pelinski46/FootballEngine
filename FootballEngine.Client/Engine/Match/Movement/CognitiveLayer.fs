namespace FootballEngine.Movement

// CognitiveLayer.fs — evaluación cognitiva por jugador.
// Usa MentalState, ChemistryGraph, Directive, DirectiveKind, RunAssignment
// canónicos de FootballEngine (MatchStateTypes.fs).
// Todos los offsets espaciales están en metros (campo 105×68).
// Todos los tiempos están en SubTicks (1 SubTick = 0.025s).

open FootballEngine
open FootballEngine.Domain

module ChemistryGraph =

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
    let compute meIdx (mySide: TeamSide) (oppSide: TeamSide) ballX ballY (dir: AttackDir) momentum : GameRead =
        let myPos = mySide.Positions[meIdx]
        let myX = myPos.X
        let myY = myPos.Y

        // Pass 1: oppSide — nearestOppDist, pressureCount, defensiveLineX
        let mutable nearestOppDistSq = System.Double.MaxValue
        let mutable pressureCount = 0
        let mutable defLineX = if dir = LeftToRight then -1.0 else 106.0

        for i = 0 to oppSide.Positions.Length - 1 do
            let op = oppSide.Positions[i]
            let dx = op.X - myX
            let dy = op.Y - myY
            let dSq = dx * dx + dy * dy
            if dSq < nearestOppDistSq then
                nearestOppDistSq <- dSq
            if dSq < 64.0 then
                pressureCount <- pressureCount + 1
            if dir = LeftToRight then
                if op.X > defLineX then defLineX <- op.X
            else
                if op.X < defLineX then defLineX <- op.X

        // Pass 2: mySide — nearestTeammateDist, supportCount, centroid sum
        let mutable nearestTeamDistSq = System.Double.MaxValue
        let mutable supportCount = 0
        let mutable sumX = 0.0
        let mutable sumY = 0.0
        let n = mySide.Positions.Length

        for i = 0 to n - 1 do
            let sp = mySide.Positions[i]
            sumX <- sumX + sp.X
            sumY <- sumY + sp.Y
            if i <> meIdx then
                let dx = sp.X - myX
                let dy = sp.Y - myY
                let dSq = dx * dx + dy * dy
                if dSq < nearestTeamDistSq then
                    nearestTeamDistSq <- dSq
                if dSq < 225.0 then
                    supportCount <- supportCount + 1

        let defensiveLineX = defLineX
        let spaceBehindDefense =
            if dir = LeftToRight then PhysicsContract.PitchLength - defensiveLineX
            else defensiveLineX

        { NearestOpponentDist = sqrt nearestOppDistSq
          NearestTeammateDist = sqrt nearestTeamDistSq
          SpaceBehindDefense = spaceBehindDefense
          PressureCount = pressureCount
          SupportCount = supportCount
          DefensiveLineX = defensiveLineX
          TeamCentroidX = sumX / float n
          TeamCentroidY = sumY / float n
          MomentumDirection = momentum }


module CognitiveLayer =

    type CognitiveUpdate =
        { NewDirectives: Directive[]
          RemovedKinds: DirectiveKind list
          MentalStateDelta: MentalState option
          RunAssignments: RunAssignment list }

    let evaluate
        (currentSubTick: int)
        (player: Player)
        (meIdx: int)
        (mySide: TeamSide)
        (oppSide: TeamSide)
        (ballX: float)
        (ballY: float)
        (dir: AttackDir)
        (momentum: float)
        (mentalState: MentalState)
        (chemistry: ChemistryGraph)
        (existingDirectives: Directive[])
        : CognitiveUpdate =

        let gameRead = GameRead.compute meIdx mySide oppSide ballX ballY dir momentum

        let newDirectives = System.Collections.Generic.List<Directive>()
        let forwardDir = AttackDir.forwardX dir
        let myPos = mySide.Positions[meIdx]

        // Expiry: 4 real seconds = 160 SubTicks
        let shortExpiry = currentSubTick + 160
        // Expiry: 10 real seconds = 400 SubTicks
        let longExpiry = currentSubTick + 400

        // Ball-relative shape target — offsets in metres
        let ballRelativeShapeX =
            match player.Position with
            | GK ->
                match dir with
                | LeftToRight -> 5.0
                | RightToLeft -> 100.0
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
            | GK -> PhysicsContract.PitchWidth / 2.0
            | DR -> myPos.Y * 0.7 + 15.0 * 0.3
            | DL -> myPos.Y * 0.7 + (PhysicsContract.PitchWidth - myPos.Y) * 0.3
            | _ -> ballY * 0.3 + myPos.Y * 0.7

        newDirectives.Add(
            Directive.create
                Shape
                (System.Math.Clamp(ballRelativeShapeX, 2.0, 103.0))
                (System.Math.Clamp(ballRelativeShapeY, 2.0, 66.0))
                0.5
                0.4
                shortExpiry
                "cognitive"
        )

        let findNearestOpponent () =
            let mutable bestIdx = 0
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to oppSide.Positions.Length - 1 do
                let op = oppSide.Positions[i]
                let dx = op.X - myPos.X
                let dy = op.Y - myPos.Y
                let dSq = dx * dx + dy * dy
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestIdx <- i

            oppSide.Positions[bestIdx]

        match player.Position with
        | GK ->
            if gameRead.PressureCount >= 2 then
                let coverX = gameRead.TeamCentroidX - 3.0 * forwardDir
                let coverY = gameRead.TeamCentroidY
                newDirectives.Add(Directive.create Cover coverX coverY 0.7 0.8 longExpiry "cognitive")

        | DR
        | DC
        | DL
        | WBR
        | WBL ->
            if gameRead.PressureCount >= 1 then
                let oppPos = findNearestOpponent ()
                newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.7 0.7 longExpiry "cognitive")
            else
                let markZoneX = (ballX + myPos.X) / 2.0
                let markZoneY = (ballY + myPos.Y) / 2.0
                newDirectives.Add(Directive.create MarkZone markZoneX markZoneY 0.7 0.5 shortExpiry "cognitive")

        | DM
        | MC
        | MR
        | ML ->
            let supportTargetX = ballX + 5.0 * forwardDir
            newDirectives.Add(Directive.create Support supportTargetX ballY 0.6 0.5 shortExpiry "cognitive")
            // SpaceBehindDefense threshold: 20 metres
            if gameRead.SpaceBehindDefense > 20.0 then
                let runTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create Run runTargetX ballY 0.6 0.6 shortExpiry "cognitive")

        | AMR
        | AML ->
            let supportTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 103.0)

            let supportTargetY =
                match player.Position with
                | AML -> gameRead.TeamCentroidY - 15.0
                | AMR -> gameRead.TeamCentroidY + 15.0
                | _ -> ballY

            newDirectives.Add(Directive.create Support supportTargetX supportTargetY 0.6 0.5 shortExpiry "cognitive")
            newDirectives.Add(Directive.create Flank supportTargetX supportTargetY 0.6 0.5 shortExpiry "cognitive")
            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.6
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Run runTargetX supportTargetY 0.7 runUrgency shortExpiry "cognitive")

        | AMC ->
            let supportTargetX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Support supportTargetX ballY 0.6 0.5 shortExpiry "cognitive")
            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.6
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Run runTargetX ballY 0.7 runUrgency shortExpiry "cognitive")

        | ST ->
            let runTargetX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 103.0)
            let runUrgency = if gameRead.SpaceBehindDefense > 15.0 then 0.9 else 0.7
            newDirectives.Add(Directive.create Run runTargetX ballY 0.7 runUrgency shortExpiry "cognitive")
            newDirectives.Add(Directive.create Support runTargetX ballY 0.5 0.5 shortExpiry "cognitive")

        let updatedMental =
            if gameRead.PressureCount > 0 then
                Some
                    { mentalState with
                        FocusLevel = min 1.0 (mentalState.FocusLevel + 0.02) }
            else
                Some mentalState

        { NewDirectives = newDirectives.ToArray()
          RemovedKinds = []
          MentalStateDelta = updatedMental
          RunAssignments = [] }
