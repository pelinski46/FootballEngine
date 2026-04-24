namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps
open MatchSpatial

module TeamIntentModule =

    let empty clubSide = {
        BuildUpSide = BuildUpSide.Balanced
        PressTrigger = false
        PressTriggerZone = None
        TargetRunner = None
        RunType = None
        RunTarget = None
        SupportPositions = Array.init 11 (fun _ -> defaultSpatial 52.5<meter> 34.0<meter>)
        DesiredWidth = 0.5
        Tempo = 0.5
    }

    let private computeBuildUpSide (team: TeamPerspective) (ballX: float<meter>) (ballY: float<meter>) : BuildUpSide =
        let frame = team.OwnFrame
        let roster = team.OwnRoster
        let dir = team.AttackDir
        let forwardX = if dir = LeftToRight then 1.0 else -1.0

        let mutable leftWeight = 0.0
        let mutable rightWeight = 0.0
        let mutable centralWeight = 0.0

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let py = float frame.PosY[i] * 1.0<meter>
                let px = float frame.PosX[i] * 1.0<meter>
                let isAdvanced = (px - 52.5<meter>) * forwardX > 0.0<meter>
                let weight = if isAdvanced then 1.5 else 1.0

                if py < 20.0<meter> then
                    leftWeight <- leftWeight + weight
                elif py > 48.0<meter> then
                    rightWeight <- rightWeight + weight
                else
                    centralWeight <- centralWeight + weight
            | _ -> ()

        let ballSideWeight =
            if ballY < 20.0<meter> then 2.0
            elif ballY > 48.0<meter> then 2.0
            else 1.0

        let ballXSide =
            if ballX < 40.0<meter> then BuildUpSide.LeftFlank
            elif ballX > 65.0<meter> then BuildUpSide.RightFlank
            else BuildUpSide.Central

        let maxSide =
            if leftWeight > rightWeight && leftWeight > centralWeight then BuildUpSide.LeftFlank
            elif rightWeight > leftWeight && rightWeight > centralWeight then BuildUpSide.RightFlank
            else BuildUpSide.Central

        if abs (leftWeight - rightWeight) < 3.0 then BuildUpSide.Balanced
        elif ballSideWeight > 1.5 then ballXSide
        else maxSide

    let private computeDesiredWidth (tactics: TacticsConfig) (emergent: EmergentState) : float =
        let baseWidth = 0.5 + tactics.PressingIntensity * 0.2
        baseWidth * emergent.WingPlayPreference + (1.0 - emergent.WingPlayPreference) * baseWidth * 0.7

    let private computeTempo (emergent: EmergentState) (tactics: TacticsConfig) : float =
        emergent.TempoLevel * 0.6 + tactics.ForwardPush * 0.4

    let private computePressTrigger
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballPossession: Possession)
        (emergent: EmergentState)
        : bool * PitchZone option =
        let teamHasBall =
            match ballPossession with
            | Owned(side, _) -> side = team.ClubSide
            | InFlight(side, _) -> side = team.ClubSide
            | SetPiece(side, _) -> side = team.ClubSide
            | Contest(side) -> side = team.ClubSide
            | Transition(side) -> side = team.ClubSide
            | Loose -> false

        if teamHasBall then false, None
        elif emergent.PressingIntensity < 0.55 then false, None
        else
            let dir = team.AttackDir
            let zone = ofBallX ballPos.X dir
            match zone with
            | DefensiveZone -> true, Some zone
            | MidfieldZone -> emergent.PressingIntensity > 0.65, Some zone
            | AttackingZone -> false, None

    let private computeSupportPositions
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballPossession: Possession)
        (phase: MatchPhase)
        (tactics: TacticsConfig)
        (desiredWidth: float)
        : Spatial[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let result = Array.zeroCreate<Spatial> n

        let ballCarrierId =
            match ballPossession with
            | Owned(_, pid) -> Some pid
            | InFlight(_, pid) -> Some pid
            | SetPiece(_, _) -> None
            | Contest _ -> None
            | Transition _ -> None
            | Loose -> None

        for i = 0 to n - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let px = float frame.PosX[i] * 1.0<meter>
                let py = float frame.PosY[i] * 1.0<meter>

                let isCarrier =
                    match ballCarrierId with
                    | Some pid ->
                        let roster = team.OwnRoster
                        roster.Players[i].Id = pid
                    | None -> false

                if isCarrier then
                    result[i] <- defaultSpatial px py
                else
                    let dir = team.AttackDir
                    let forwardX = if dir = LeftToRight then 1.0 else -1.0

                    let baseX = px + 8.0<meter> * forwardX
                    let centerY = 34.0<meter>
                    let widthOffset = (py - centerY) * desiredWidth

                    let targetX = PhysicsContract.clamp baseX 2.0<meter> 98.0<meter>
                    let targetY = PhysicsContract.clamp (centerY + widthOffset) 2.0<meter> 98.0<meter>

                    result[i] <- defaultSpatial targetX targetY
            | _ ->
                result[i] <- defaultSpatial 52.5<meter> 34.0<meter>

        result

    let private pickRunner
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballPossession: Possession)
        (emergent: EmergentState)
        : PlayerId option * RunType option * Spatial option =
        let frame = team.OwnFrame
        let roster = team.OwnRoster

        let ballCarrierId =
            match ballPossession with
            | Owned(_, pid) -> Some pid
            | InFlight(_, pid) -> Some pid
            | _ -> None

        let dir = team.AttackDir
        let forwardX = if dir = LeftToRight then 1.0 else -1.0

        let mutable bestPlayerId: PlayerId option = None
        let mutable bestRunType: RunType option = None
        let mutable bestTarget: Spatial option = None
        let mutable bestScore = -1.0

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]
                if ballCarrierId <> Some player.Id then
                    let px = float frame.PosX[i] * 1.0<meter>
                    let py = float frame.PosY[i] * 1.0<meter>

                    let isAdvanced = (px - 52.5<meter>) * forwardX > 5.0<meter>
                    let workRate = float player.Mental.WorkRate / 20.0
                    let positioning = float player.Mental.Positioning / 20.0
                    let score = workRate * 0.3 + positioning * 0.4 + (if isAdvanced then 0.5 else 0.0)

                    if score > bestScore then
                        bestScore <- score
                        bestPlayerId <- Some player.Id

                        let runType, target =
                            match player.Position with
                            | ST | AMC ->
                                let tx = PhysicsContract.clamp (px + 12.0<meter> * forwardX) 2.0<meter> 98.0<meter>
                                DeepRun, defaultSpatial tx py
                            | AML | ML | WBL ->
                                let ty = PhysicsContract.clamp (py - 8.0<meter>) 2.0<meter> 98.0<meter>
                                DriftWide, defaultSpatial px ty
                            | AMR | MR | WBR ->
                                let ty = PhysicsContract.clamp (py + 8.0<meter>) 2.0<meter> 98.0<meter>
                                DriftWide, defaultSpatial px ty
                            | MC ->
                                let tx = PhysicsContract.clamp (px + 8.0<meter> * forwardX) 2.0<meter> 98.0<meter>
                                CheckToBall, defaultSpatial tx py
                            | DL | DR ->
                                let tx = PhysicsContract.clamp (px + 10.0<meter> * forwardX) 2.0<meter> 98.0<meter>
                                OverlapRun, defaultSpatial tx py
                            | _ ->
                                let tx = PhysicsContract.clamp (px + 6.0<meter> * forwardX) 2.0<meter> 98.0<meter>
                                DeepRun, defaultSpatial tx py

                        bestRunType <- Some runType
                        bestTarget <- Some target
            | _ -> ()

        bestPlayerId, bestRunType, bestTarget

    let build
        (clubSide: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (cFrame: CognitiveFrame)
        (emergent: EmergentState)
        : TeamIntent =
        let team = SimStateOps.buildTeamPerspective clubSide ctx state
        let ballPos = state.Ball.Position
        let dir = team.AttackDir
        let phase = phaseFromBallZone dir ballPos.X
        let tactics = tacticsConfig (getTactics state clubSide) (getInstructions state clubSide)

        let buildUpSide = computeBuildUpSide team ballPos.X ballPos.Y
        let desiredWidth = computeDesiredWidth tactics emergent
        let tempo = computeTempo emergent tactics
        let pressTrigger, pressZone = computePressTrigger team ballPos state.Ball.Possession emergent
        let supportPositions = computeSupportPositions team ballPos state.Ball.Possession phase tactics desiredWidth
        let runnerId, runType, runTarget = pickRunner team ballPos state.Ball.Possession emergent

        { BuildUpSide = buildUpSide
          PressTrigger = pressTrigger
          PressTriggerZone = pressZone
          TargetRunner = runnerId
          RunType = runType
          RunTarget = runTarget
          SupportPositions = supportPositions
          DesiredWidth = desiredWidth
          Tempo = tempo }
