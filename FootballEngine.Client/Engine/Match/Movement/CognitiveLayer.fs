namespace FootballEngine.Movement

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
      MomentumDirection: float
      TeamHasPossession: bool
      NearestOpponentIdx: int
      BallDist: float }

module GameRead =
    let compute
        meIdx
        (myPositions: Spatial[])
        (oppPositions: Spatial[])
        ballX
        ballY
        (dir: AttackDir)
        momentum
        : GameRead =
        let myPos = myPositions[meIdx]
        let myX = myPos.X
        let myY = myPos.Y

        let mutable nearestOppDistSq = System.Double.MaxValue
        let mutable nearestOppIdx = 0
        let mutable pressureCount = 0
        let mutable defLineX = if dir = LeftToRight then -1.0 else 106.0

        for i = 0 to oppPositions.Length - 1 do
            let op = oppPositions[i]
            let dx = op.X - myX
            let dy = op.Y - myY
            let dSq = dx * dx + dy * dy

            if dSq < nearestOppDistSq then
                nearestOppDistSq <- dSq
                nearestOppIdx <- i

            if dSq < 64.0 then
                pressureCount <- pressureCount + 1

            if dir = LeftToRight then
                if op.X > defLineX then
                    defLineX <- op.X
            else if op.X < defLineX then
                defLineX <- op.X

        let mutable nearestTeamDistSq = System.Double.MaxValue
        let mutable supportCount = 0
        let mutable sumX = 0.0
        let mutable sumY = 0.0
        let n = myPositions.Length

        for i = 0 to n - 1 do
            let sp = myPositions[i]
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

        let mutable myTeamNearestBallDistSq = System.Double.MaxValue

        for i = 0 to myPositions.Length - 1 do
            let sp = myPositions[i]
            let dx = sp.X - ballX
            let dy = sp.Y - ballY
            let dSq = dx * dx + dy * dy

            if dSq < myTeamNearestBallDistSq then
                myTeamNearestBallDistSq <- dSq

        let mutable oppNearestBallDistSq = System.Double.MaxValue

        for i = 0 to oppPositions.Length - 1 do
            let op = oppPositions[i]
            let dx = op.X - ballX
            let dy = op.Y - ballY
            let dSq = dx * dx + dy * dy

            if dSq < oppNearestBallDistSq then
                oppNearestBallDistSq <- dSq

        let teamHasPossession = myTeamNearestBallDistSq <= oppNearestBallDistSq

        let ballDx = ballX - myX
        let ballDy = ballY - myY
        let ballDist = sqrt (ballDx * ballDx + ballDy * ballDy)

        let spaceBehindDefense =
            if dir = LeftToRight then
                PhysicsContract.PitchLength - defLineX
            else
                defLineX

        { NearestOpponentDist = sqrt nearestOppDistSq
          NearestTeammateDist = sqrt nearestTeamDistSq
          SpaceBehindDefense = spaceBehindDefense
          PressureCount = pressureCount
          SupportCount = supportCount
          DefensiveLineX = defLineX
          TeamCentroidX = sumX / float n
          TeamCentroidY = sumY / float n
          MomentumDirection = momentum
          TeamHasPossession = teamHasPossession
          NearestOpponentIdx = nearestOppIdx
          BallDist = ballDist }


module CognitiveLayer =

    type CognitiveUpdate =
        { NewDirectives: Directive[]
          RemovedKinds: DirectiveKind list
          MentalStateDelta: MentalState option
          RunAssignments: RunAssignment list }

    let private pitchLen = PhysicsContract.PitchLength
    let private half = PhysicsContract.PitchWidth / 2.0

    let private baseShapeX (pos: Position) (dir: AttackDir) =
        let fwd = AttackDir.forwardX dir
        let mid = pitchLen / 2.0

        match pos with
        | GK -> if dir = LeftToRight then 5.0 else 100.0
        | DC -> mid - 20.0 * fwd
        | DR -> mid - 20.0 * fwd
        | DL -> mid - 20.0 * fwd
        | WBR -> mid - 15.0 * fwd
        | WBL -> mid - 15.0 * fwd
        | DM -> mid - 8.0 * fwd
        | MC -> mid
        | MR -> mid + 2.0 * fwd
        | ML -> mid + 2.0 * fwd
        | AMR -> mid + 12.0 * fwd
        | AML -> mid + 12.0 * fwd
        | AMC -> mid + 12.0 * fwd
        | ST -> mid + 22.0 * fwd

    let private baseShapeY (pos: Position) =
        match pos with
        | GK -> half
        | DC -> half
        | DR -> half - 22.0
        | DL -> half + 22.0
        | WBR -> half - 26.0
        | WBL -> half + 26.0
        | DM -> half
        | MC -> half
        | MR -> half - 20.0
        | ML -> half + 20.0
        | AMR -> half - 18.0
        | AML -> half + 18.0
        | AMC -> half
        | ST -> half

    let evaluate
        (currentSubTick: int)
        (player: Player)
        (meIdx: int)
        (myPlayers: Player[])
        (myPositions: Spatial[])
        (myConditions: int[])
        (oppPlayers: Player[])
        (oppPositions: Spatial[])
        (ballX: float)
        (ballY: float)
        (dir: AttackDir)
        (momentum: float)
        (mentalState: MentalState)
        (chemistry: ChemistryGraph)
        (existingDirectives: Directive[])
        (basePos: Spatial)
        : CognitiveUpdate =

        let gr = GameRead.compute meIdx myPositions oppPositions ballX ballY dir momentum

        let newDirectives = System.Collections.Generic.List<Directive>()
        let forwardDir = AttackDir.forwardX dir
        let myPos = myPositions[meIdx]

        let shortExpiry = currentSubTick + 120
        let longExpiry = currentSubTick + 320


        let phasePullScale =
            match player.Position with
            | GK -> 0.0
            | DC
            | DR
            | DL -> 0.05
            | WBR
            | WBL -> 0.08
            | DM -> 0.10
            | MC
            | MR
            | ML -> 0.15
            | AMR
            | AML
            | AMC -> 0.20
            | ST -> 0.25

        let phasePullX =
            let raw = ballX - pitchLen / 2.0
            raw * phasePullScale * forwardDir

        let anchorX = System.Math.Clamp(basePos.X + phasePullX, 2.0, 103.0)

        let anchorY = System.Math.Clamp(basePos.Y + (ballY - half) * 0.12, 2.0, 66.0)

        let distFromShape =
            let dx = myPos.X - anchorX
            let dy = myPos.Y - anchorY
            sqrt (dx * dx + dy * dy)

        let shapeWeight = System.Math.Clamp(0.7 + distFromShape * 0.03, 0.7, 1.5)

        newDirectives.Add(Directive.create Shape anchorX anchorY shapeWeight 0.6 shortExpiry "cognitive")

        match player.Position with

        | GK ->
            let penaltyX =
                if dir = LeftToRight then
                    PhysicsContract.PenaltyAreaDepth
                else
                    pitchLen - PhysicsContract.PenaltyAreaDepth

            if gr.TeamHasPossession then
                let sweepBase = if dir = LeftToRight then 5.0 else 100.0
                let advance = (gr.TeamCentroidX - pitchLen / 2.0) * 0.08

                let sweepX =
                    if dir = LeftToRight then
                        System.Math.Clamp(sweepBase + advance, 3.0, penaltyX - 4.0)
                    else
                        System.Math.Clamp(sweepBase + advance, penaltyX + 4.0, 102.0)

                let sweepY = System.Math.Clamp(ballY * 0.25 + half * 0.75, half - 8.0, half + 8.0)
                newDirectives.Add(Directive.create Cover sweepX sweepY 0.7 0.5 longExpiry "cognitive")
            else
                let dangerBase = if dir = LeftToRight then 4.0 else 101.0
                let pull = (gr.TeamCentroidX - pitchLen / 2.0) * 0.05

                let dangerX =
                    if dir = LeftToRight then
                        System.Math.Clamp(dangerBase + pull, 2.0, penaltyX - 2.0)
                    else
                        System.Math.Clamp(dangerBase + pull, penaltyX + 2.0, 103.0)

                let dangerY = System.Math.Clamp(ballY * 0.5 + half * 0.5, half - 12.0, half + 12.0)
                newDirectives.Add(Directive.create Cover dangerX dangerY 0.9 0.8 shortExpiry "cognitive")

        | DC
        | DR
        | DL
        | WBR
        | WBL ->
            if gr.TeamHasPossession then
                let supportX = System.Math.Clamp(anchorX + 4.0 * forwardDir, 2.0, 103.0)
                let supportY = anchorY
                newDirectives.Add(Directive.create Support supportX supportY 0.4 0.4 longExpiry "cognitive")

                match player.Position with
                | WBR ->
                    let overlapX = System.Math.Clamp(ballX + 5.0 * forwardDir, 2.0, 103.0)
                    let overlapY = System.Math.Clamp(half - 28.0, 2.0, 66.0)
                    newDirectives.Add(Directive.create Flank overlapX overlapY 0.5 0.5 shortExpiry "cognitive")
                | WBL ->
                    let overlapX = System.Math.Clamp(ballX + 5.0 * forwardDir, 2.0, 103.0)
                    let overlapY = System.Math.Clamp(half + 28.0, 2.0, 66.0)
                    newDirectives.Add(Directive.create Flank overlapX overlapY 0.5 0.5 shortExpiry "cognitive")
                | _ -> ()
            else
                let oppPos = oppPositions[gr.NearestOpponentIdx]
                let threatDist = gr.NearestOpponentDist

                if threatDist < 15.0 then
                    newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.8 0.8 shortExpiry "cognitive")
                else
                    let blockX = System.Math.Clamp(anchorX - 3.0 * forwardDir, 2.0, 103.0)
                    newDirectives.Add(Directive.create MarkZone blockX anchorY 0.6 0.5 longExpiry "cognitive")

                if gr.PressureCount >= 1 then
                    let pressX = System.Math.Clamp(ballX - 3.0 * forwardDir, 2.0, 103.0)
                    newDirectives.Add(Directive.create Press pressX ballY 0.5 0.6 shortExpiry "cognitive")

        | DM ->
            if gr.TeamHasPossession then
                let receiveX = System.Math.Clamp(ballX - 6.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create Support receiveX ballY 0.6 0.5 shortExpiry "cognitive")
            else
                let screenX = System.Math.Clamp(anchorX, 2.0, 103.0)
                newDirectives.Add(Directive.create MarkZone screenX half 0.7 0.6 shortExpiry "cognitive")

                if gr.NearestOpponentDist < 12.0 then
                    let oppPos = oppPositions[gr.NearestOpponentIdx]
                    newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.7 0.7 shortExpiry "cognitive")

        | MC
        | MR
        | ML ->
            if gr.TeamHasPossession then
                let supportX = System.Math.Clamp(ballX + 5.0 * forwardDir, 2.0, 103.0)

                let supportY =
                    match player.Position with
                    | MR -> System.Math.Clamp(ballY - 12.0, 2.0, 66.0)
                    | ML -> System.Math.Clamp(ballY + 12.0, 2.0, 66.0)
                    | _ -> ballY

                newDirectives.Add(Directive.create Support supportX supportY 0.6 0.5 shortExpiry "cognitive")

                if gr.SpaceBehindDefense > 18.0 then
                    let runX = System.Math.Clamp(ballX + 14.0 * forwardDir, 2.0, 103.0)
                    newDirectives.Add(Directive.create Run runX supportY 0.5 0.6 shortExpiry "cognitive")
            else
                let compactX = System.Math.Clamp(anchorX - 4.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create MarkZone compactX anchorY 0.6 0.5 longExpiry "cognitive")

                if gr.NearestOpponentDist < 10.0 then
                    let oppPos = oppPositions[gr.NearestOpponentIdx]
                    newDirectives.Add(Directive.create Press oppPos.X oppPos.Y 0.6 0.7 shortExpiry "cognitive")

        | AMR
        | AML
        | AMC ->
            if gr.TeamHasPossession then
                let advanceX = System.Math.Clamp(ballX + 10.0 * forwardDir, 2.0, 103.0)

                let advanceY =
                    match player.Position with
                    | AMR -> System.Math.Clamp(half - 16.0, 2.0, 66.0)
                    | AML -> System.Math.Clamp(half + 16.0, 2.0, 66.0)
                    | _ -> ballY

                newDirectives.Add(Directive.create Support advanceX advanceY 0.6 0.5 shortExpiry "cognitive")

                match player.Position with
                | AMR
                | AML -> newDirectives.Add(Directive.create Flank advanceX advanceY 0.5 0.5 shortExpiry "cognitive")
                | _ -> ()

                if gr.SpaceBehindDefense > 12.0 then
                    let runX = System.Math.Clamp(ballX + 18.0 * forwardDir, 2.0, 103.0)
                    let runUrgency = if gr.SpaceBehindDefense > 22.0 then 0.8 else 0.6
                    newDirectives.Add(Directive.create Run runX advanceY 0.6 runUrgency shortExpiry "cognitive")
            else
                let dropX = System.Math.Clamp(anchorX - 6.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create MarkZone dropX anchorY 0.5 0.4 longExpiry "cognitive")

                if gr.BallDist < 20.0 then
                    let pressX = System.Math.Clamp(ballX + 2.0 * forwardDir, 2.0, 103.0)
                    newDirectives.Add(Directive.create Press pressX ballY 0.6 0.7 shortExpiry "cognitive")

        | ST ->
            if gr.TeamHasPossession then
                let strikeX = System.Math.Clamp(ballX + 15.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create Support strikeX ballY 0.5 0.5 shortExpiry "cognitive")

                let runUrgency = if gr.SpaceBehindDefense > 15.0 then 0.8 else 0.5
                let runX = System.Math.Clamp(gr.DefensiveLineX + 3.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create Run runX ballY 0.7 runUrgency shortExpiry "cognitive")
            else
                let pressX = System.Math.Clamp(ballX + 3.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create Press pressX ballY 0.6 0.7 shortExpiry "cognitive")

                let dropX = System.Math.Clamp(anchorX - 8.0 * forwardDir, 2.0, 103.0)
                newDirectives.Add(Directive.create MarkZone dropX half 0.4 0.4 longExpiry "cognitive")

        let updatedMental =
            if gr.PressureCount > 0 then
                Some
                    { mentalState with
                        FocusLevel = min 1.0 (mentalState.FocusLevel + 0.02) }
            else
                Some mentalState

        { NewDirectives = newDirectives.ToArray()
          RemovedKinds = []
          MentalStateDelta = updatedMental
          RunAssignments = [] }
