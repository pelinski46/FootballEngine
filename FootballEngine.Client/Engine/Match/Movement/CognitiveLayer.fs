namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.SimStateOps

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
            let dist = pos.DistTo2D leaderPos
            let pullStrength = cg.Leadership[leaderIdx] * 0.1
            let pullX = if dist > 0.0<meter> then (pos.X - leaderPos.X) / dist * pullStrength else 0.0
            let pullY = if dist > 0.0<meter> then (pos.Y - leaderPos.Y) / dist * pullStrength else 0.0
            (i, pullX, pullY))
        |> Array.toList

type TeamGameRead =
    { SpaceBehindDefense: float<meter>
      DefensiveLineX: float<meter>
      MyCentroidX: float<meter>
      MyCentroidY: float<meter>
      TeamHasPossession: bool }

type PerPlayerGameRead =
    { NearestOpponentDist: float<meter>
      NearestTeammateDist: float<meter>
      PressureCount: int
      SupportCount: int
      DefensiveLineX: float<meter>
      NearestOpponentIdx: int
      BallDist: float<meter> }

module GameRead =
    let computeTeam (myPositions: Spatial[]) (oppPositions: Spatial[]) ballX ballY (dir: AttackDir) : TeamGameRead =
        let ballPos = { X = ballX; Y = ballY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let mutable mySumX = 0.0<meter>
        let mutable mySumY = 0.0<meter>
        let mutable myNearestBallDistSq = PhysicsContract.MaxDistanceSq
        let mutable oppNearestBallDistSq = PhysicsContract.MaxDistanceSq
        let mutable defLineX = if dir = LeftToRight then -1.0<meter> else 106.0<meter>
        let n = myPositions.Length

        for i = 0 to n - 1 do
            let sp = myPositions[i]
            mySumX <- mySumX + sp.X
            mySumY <- mySumY + sp.Y
            let dSq = sp.DistSqTo2D ballPos

            if dSq < myNearestBallDistSq then
                myNearestBallDistSq <- dSq

        for i = 0 to oppPositions.Length - 1 do
            let op = oppPositions[i]
            let dSq = op.DistSqTo2D ballPos

            if dSq < oppNearestBallDistSq then
                oppNearestBallDistSq <- dSq

            if dir = LeftToRight then
                if op.X > defLineX then
                    defLineX <- op.X
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
        (teamRead: TeamGameRead)
        : PerPlayerGameRead =
        let ballPos = { X = ballX; Y = ballY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let myPos = myPositions[meIdx]

        let mutable nearestOppDistSq = PhysicsContract.MaxDistanceSq
        let mutable nearestOppIdx = 0
        let mutable pressureCount = 0

        for i = 0 to oppPositions.Length - 1 do
            let op = oppPositions[i]
            let dSq = op.DistSqTo2D myPos

            if dSq < nearestOppDistSq then
                nearestOppDistSq <- dSq
                nearestOppIdx <- i

            if dSq < 64.0<meter^2> then
                pressureCount <- pressureCount + 1

        let mutable nearestTeamDistSq = PhysicsContract.MaxDistanceSq
        let mutable supportCount = 0
        let n = myPositions.Length

        for i = 0 to n - 1 do
            if i <> meIdx then
                let sp = myPositions[i]
                let dSq = sp.DistSqTo2D myPos

                if dSq < nearestTeamDistSq then
                    nearestTeamDistSq <- dSq

                if dSq < 225.0<meter^2> then
                    supportCount <- supportCount + 1

        { NearestOpponentDist = sqrt nearestOppDistSq
          NearestTeammateDist = sqrt nearestTeamDistSq
          PressureCount = pressureCount
          SupportCount = supportCount
          DefensiveLineX = teamRead.DefensiveLineX
          NearestOpponentIdx = nearestOppIdx
          BallDist = myPos.DistTo2D ballPos }


module CognitiveLayer =

    type CognitiveUpdate =
        { NewDirectives: Directive[]
          RemovedKinds: DirectiveKind list
          MentalStateDelta: MentalState option
          RunAssignments: RunAssignment list }

    let private pitchLen = PhysicsContract.PitchLength
    let private half = PhysicsContract.PitchWidth / 2.0

    let private baseShapeX (pos: Position) (dir: AttackDir) =
        let fwd = forwardX dir
        let mid = pitchLen / 2.0

        match pos with
        | GK -> if dir = LeftToRight then 5.0<meter> else 100.0<meter>
        | DC -> mid - 20.0<meter> * fwd
        | DR -> mid - 20.0<meter> * fwd
        | DL -> mid - 20.0<meter> * fwd
        | WBR -> mid - 15.0<meter> * fwd
        | WBL -> mid - 15.0<meter> * fwd
        | DM -> mid - 8.0<meter> * fwd
        | MC -> mid
        | MR -> mid + 2.0<meter> * fwd
        | ML -> mid + 2.0<meter> * fwd
        | AMR -> mid + 12.0<meter> * fwd
        | AML -> mid + 12.0<meter> * fwd
        | AMC -> mid + 12.0<meter> * fwd
        | ST -> mid + 22.0<meter> * fwd

    let private baseShapeY (pos: Position) =
        match pos with
        | GK -> half
        | DC -> half
        | DR -> half - 22.0<meter>
        | DL -> half + 22.0<meter>
        | WBR -> half - 26.0<meter>
        | WBL -> half + 26.0<meter>
        | DM -> half
        | MC -> half
        | MR -> half - 20.0<meter>
        | ML -> half + 20.0<meter>
        | AMR -> half - 18.0<meter>
        | AML -> half + 18.0<meter>
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
        (ballX: float<meter>)
        (ballY: float<meter>)
        (dir: AttackDir)
        (momentum: float)
        (mentalState: MentalState)
        (chemistry: ChemistryGraph)
        (existingDirectives: Directive[])
        (basePos: Spatial)
        (teamRead: TeamGameRead)
        : CognitiveUpdate =

        let perPlayer =
            GameRead.computePerPlayer meIdx myPositions oppPositions ballX ballY teamRead

        let newDirectives = System.Collections.Generic.List<Directive>()
        let forwardDir = forwardX dir
        let myPos = myPositions[meIdx]

        let shortExpiry = currentSubTick + 120
        let longExpiry = currentSubTick + 320


        let phasePullScale = profile.PositionalFreedom * 0.30

        let phasePullX =
            let raw = ballX - pitchLen / 2.0
            raw * phasePullScale * forwardDir

        let anchorX = PhysicsContract.clamp (basePos.X + phasePullX) 2.0<meter> 103.0<meter>

        let anchorY = PhysicsContract.clamp (basePos.Y + (ballY - half) * 0.12) 2.0<meter> 66.0<meter>

        let anchor = { X = anchorX; Y = anchorY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let distFromShape = myPos.DistTo2D anchor

        // convert distFromShape (float<meter>) into dimensionless metres for weight scaling
        let shapeWeight = System.Math.Clamp(0.7 + (distFromShape / 1.0<meter>) * 0.03, 0.7, 1.5)

        newDirectives.Add(Directive.create Shape anchorX anchorY shapeWeight 0.6 shortExpiry "cognitive")

        let lateralShift = profile.LateralTendency * 12.0<meter>

        let directiveY = PhysicsContract.clamp (anchorY + lateralShift) 2.0<meter> 66.0<meter>

        if profile.PressingIntensity > 0.6 && not teamRead.TeamHasPossession then
            let pressX = PhysicsContract.clamp (ballX - 3.0<meter> * forwardDir) 2.0<meter> 103.0<meter>

            newDirectives.Add(
                Directive.create Press pressX ballY (profile.PressingIntensity * 0.8) 0.7 shortExpiry "cognitive"
            )

        if profile.AttackingDepth > 0.6 && teamRead.TeamHasPossession then
            let runX = PhysicsContract.clamp (ballX + 14.0<meter> * forwardDir) 2.0<meter> 103.0<meter>
            newDirectives.Add(Directive.create Run runX directiveY 0.6 0.7 shortExpiry "cognitive")

        if profile.HoldUpPlay > 0.5 && teamRead.TeamHasPossession then
            let holdX = PhysicsContract.clamp (ballX + 5.0<meter> * forwardDir) 2.0<meter> 103.0<meter>
            newDirectives.Add(Directive.create Support holdX directiveY 0.5 0.5 shortExpiry "cognitive")

        if abs profile.LateralTendency > 0.3 then
            let flankX = PhysicsContract.clamp (ballX + 8.0<meter> * forwardDir) 2.0<meter> 103.0<meter>
            let flankY = PhysicsContract.clamp (half + profile.LateralTendency * 28.0<meter>) 2.0<meter> 66.0<meter>
            newDirectives.Add(Directive.create Flank flankX flankY 0.5 0.5 shortExpiry "cognitive")

        if
            profile.CreativityWeight > 0.6
            && profile.RiskAppetite > 0.5
            && teamRead.TeamHasPossession
        then
            let thirdX = PhysicsContract.clamp (ballX + 18.0<meter> * forwardDir) 2.0<meter> 103.0<meter>
            newDirectives.Add(Directive.create Run thirdX directiveY 0.7 0.6 shortExpiry "cognitive")

        if profile.DefensiveHeight < 0.4 && not teamRead.TeamHasPossession then
            let dropX = PhysicsContract.clamp (anchorX - 8.0<meter> * forwardDir) 2.0<meter> 103.0<meter>
            newDirectives.Add(Directive.create MarkZone dropX anchorY 0.5 0.4 longExpiry "cognitive")

        if profile.DefensiveHeight > 0.5 && not teamRead.TeamHasPossession then
            let oppPos = oppPositions[perPlayer.NearestOpponentIdx]

            if perPlayer.NearestOpponentDist < 15.0<meter> then
                newDirectives.Add(Directive.create MarkMan oppPos.X oppPos.Y 0.7 0.7 shortExpiry "cognitive")

        match player.Position with
        | GK ->
            let penaltyX =
                if dir = LeftToRight then
                    PhysicsContract.PenaltyAreaDepth
                else
                    pitchLen - PhysicsContract.PenaltyAreaDepth

            if teamRead.TeamHasPossession then
                let sweepBase = if dir = LeftToRight then 5.0<meter> else 100.0<meter>
                let advance = (teamRead.MyCentroidX - pitchLen / 2.0) * profile.RiskAppetite * 0.15

                let sweepX =
                    if dir = LeftToRight then
                        PhysicsContract.clamp (sweepBase + advance) 3.0<meter> (penaltyX - 4.0<meter>)
                    else
                        PhysicsContract.clamp (sweepBase + advance) (penaltyX + 4.0<meter>) 102.0<meter>

                let sweepY = PhysicsContract.clamp (ballY * 0.25 + half * 0.75) (half - 8.0<meter>) (half + 8.0<meter>)
                newDirectives.Add(Directive.create Cover sweepX sweepY 0.7 0.5 longExpiry "cognitive")
            else
                let dangerBase = if dir = LeftToRight then 4.0<meter> else 101.0<meter>
                let pull = (teamRead.MyCentroidX - pitchLen / 2.0) * profile.DefensiveHeight * 0.1

                let dangerX =
                    if dir = LeftToRight then
                        PhysicsContract.clamp (dangerBase + pull) 2.0<meter> (penaltyX - 2.0<meter>)
                    else
                        PhysicsContract.clamp (dangerBase + pull) (penaltyX + 2.0<meter>) 103.0<meter>

                let dangerY = PhysicsContract.clamp (ballY * 0.5 + half * 0.5) (half - 12.0<meter>) (half + 12.0<meter>)
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
