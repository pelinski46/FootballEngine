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

type TeamGameRead =
    { SpaceBehindDefense: float
      DefensiveLineX: float
      MyCentroidX: float
      MyCentroidY: float
      TeamHasPossession: bool }

type PerPlayerGameRead =
    { NearestOpponentDist: float
      NearestTeammateDist: float
      PressureCount: int
      SupportCount: int
      DefensiveLineX: float
      NearestOpponentIdx: int
      BallDist: float }

module GameRead =
    let computeTeam
        (myPositions: Spatial[])
        (oppPositions: Spatial[])
        ballX
        ballY
        (dir: AttackDir)
        : TeamGameRead =
        let mutable mySumX = 0.0
        let mutable mySumY = 0.0
        let mutable myNearestBallDistSq = System.Double.MaxValue
        let mutable oppNearestBallDistSq = System.Double.MaxValue
        let mutable defLineX = if dir = LeftToRight then -1.0 else 106.0
        let n = myPositions.Length

        for i = 0 to n - 1 do
            let sp = myPositions[i]
            mySumX <- mySumX + sp.X
            mySumY <- mySumY + sp.Y
            let dx = sp.X - ballX
            let dy = sp.Y - ballY
            let dSq = dx * dx + dy * dy
            if dSq < myNearestBallDistSq then
                myNearestBallDistSq <- dSq

        for i = 0 to oppPositions.Length - 1 do
            let op = oppPositions[i]
            let dx = op.X - ballX
            let dy = op.Y - ballY
            let dSq = dx * dx + dy * dy
            if dSq < oppNearestBallDistSq then
                oppNearestBallDistSq <- dSq
            if dir = LeftToRight then
                if op.X > defLineX then defLineX <- op.X
            else if op.X < defLineX then
                defLineX <- op.X

        let spaceBehindDefense =
            if dir = LeftToRight then
                PhysicsContract.PitchLength - defLineX
            else
                defLineX

        { SpaceBehindDefense = spaceBehindDefense
          DefensiveLineX = defLineX
          MyCentroidX = mySumX / float n
          MyCentroidY = mySumY / float n
          TeamHasPossession = myNearestBallDistSq <= oppNearestBallDistSq }

    let computePerPlayer
        meIdx
        (myPositions: Spatial[])
        (oppPositions: Spatial[])
        ballX
        ballY
        (dir: AttackDir)
        (teamRead: TeamGameRead)
        : PerPlayerGameRead =
        let myPos = myPositions[meIdx]
        let myX = myPos.X
        let myY = myPos.Y

        let mutable nearestOppDistSq = System.Double.MaxValue
        let mutable nearestOppIdx = 0
        let mutable pressureCount = 0

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

        let mutable nearestTeamDistSq = System.Double.MaxValue
        let mutable supportCount = 0
        let n = myPositions.Length

        for i = 0 to n - 1 do
            if i <> meIdx then
                let sp = myPositions[i]
                let dx = sp.X - myX
                let dy = sp.Y - myY
                let dSq = dx * dx + dy * dy
                if dSq < nearestTeamDistSq then
                    nearestTeamDistSq <- dSq
                if dSq < 225.0 then
                    supportCount <- supportCount + 1

        let ballDx = ballX - myX
        let ballDy = ballY - myY
        let ballDist = sqrt (ballDx * ballDx + ballDy * ballDy)

        { NearestOpponentDist = sqrt nearestOppDistSq
          NearestTeammateDist = sqrt nearestTeamDistSq
          PressureCount = pressureCount
          SupportCount = supportCount
          DefensiveLineX = teamRead.DefensiveLineX
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
        (profile: BehavioralProfile)
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
        (teamRead: TeamGameRead)
        : CognitiveUpdate =

        let perPlayer = GameRead.computePerPlayer meIdx myPositions oppPositions ballX ballY dir teamRead

        let newDirectives = System.Collections.Generic.List<Directive>()
        let forwardDir = AttackDir.forwardX dir
        let myPos = myPositions[meIdx]

        let shortExpiry = currentSubTick + 120
        let longExpiry = currentSubTick + 320


        let phasePullScale = profile.PositionalFreedom * 0.30

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

        let lateralShift = profile.LateralTendency * 12.0

        let directiveY = System.Math.Clamp(anchorY + lateralShift, 2.0, 66.0)

        if profile.PressingIntensity > 0.6 && not teamRead.TeamHasPossession then
            let pressX = System.Math.Clamp(ballX - 3.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Press pressX ballY (profile.PressingIntensity * 0.8) 0.7 shortExpiry "cognitive")

        if profile.AttackingDepth > 0.6 && teamRead.TeamHasPossession then
            let runX = System.Math.Clamp(ballX + 14.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Run runX directiveY 0.6 0.7 shortExpiry "cognitive")

        if profile.HoldUpPlay > 0.5 && teamRead.TeamHasPossession then
            let holdX = System.Math.Clamp(ballX + 5.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Support holdX directiveY 0.5 0.5 shortExpiry "cognitive")

        if abs profile.LateralTendency > 0.3 then
            let flankX = System.Math.Clamp(ballX + 8.0 * forwardDir, 2.0, 103.0)
            let flankY = System.Math.Clamp(half + profile.LateralTendency * 28.0, 2.0, 66.0)
            newDirectives.Add(Directive.create Flank flankX flankY 0.5 0.5 shortExpiry "cognitive")

        if profile.CreativityWeight > 0.6 && profile.RiskAppetite > 0.5 && teamRead.TeamHasPossession then
            let thirdX = System.Math.Clamp(ballX + 18.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create Run thirdX directiveY 0.7 0.6 shortExpiry "cognitive")

        if profile.DefensiveHeight < 0.4 && not teamRead.TeamHasPossession then
            let dropX = System.Math.Clamp(anchorX - 8.0 * forwardDir, 2.0, 103.0)
            newDirectives.Add(Directive.create MarkZone dropX anchorY 0.5 0.4 longExpiry "cognitive")

        if profile.DefensiveHeight > 0.5 && not teamRead.TeamHasPossession then
            let oppPos = oppPositions[perPlayer.NearestOpponentIdx]
            if perPlayer.NearestOpponentDist < 15.0 then
                newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.7 0.7 shortExpiry "cognitive")

        match player.Position with
        | GK ->
            let penaltyX =
                if dir = LeftToRight then
                    PhysicsContract.PenaltyAreaDepth
                else
                    pitchLen - PhysicsContract.PenaltyAreaDepth

            if teamRead.TeamHasPossession then
                let sweepBase = if dir = LeftToRight then 5.0 else 100.0
                let advance = (teamRead.MyCentroidX - pitchLen / 2.0) * profile.RiskAppetite * 0.15

                let sweepX =
                    if dir = LeftToRight then
                        System.Math.Clamp(sweepBase + advance, 3.0, penaltyX - 4.0)
                    else
                        System.Math.Clamp(sweepBase + advance, penaltyX + 4.0, 102.0)

                let sweepY = System.Math.Clamp(ballY * 0.25 + half * 0.75, half - 8.0, half + 8.0)
                newDirectives.Add(Directive.create Cover sweepX sweepY 0.7 0.5 longExpiry "cognitive")
            else
                let dangerBase = if dir = LeftToRight then 4.0 else 101.0
                let pull = (teamRead.MyCentroidX - pitchLen / 2.0) * profile.DefensiveHeight * 0.1

                let dangerX =
                    if dir = LeftToRight then
                        System.Math.Clamp(dangerBase + pull, 2.0, penaltyX - 2.0)
                    else
                        System.Math.Clamp(dangerBase + pull, penaltyX + 2.0, 103.0)

                let dangerY = System.Math.Clamp(ballY * 0.5 + half * 0.5, half - 12.0, half + 12.0)
                newDirectives.Add(Directive.create Cover dangerX dangerY 0.9 0.8 shortExpiry "cognitive")
        | _ -> ()

        let updatedMental =
            if perPlayer.PressureCount > 0 then
                Some
                    { mentalState with
                        FocusLevel = min 1.0 (mentalState.FocusLevel + 0.02) }
            else
                Some mentalState

        { NewDirectives = newDirectives.ToArray()
          RemovedKinds = []
          MentalStateDelta = updatedMental
          RunAssignments = [] }
