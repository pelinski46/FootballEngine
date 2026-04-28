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
        DefensiveAssignments = Array.empty
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
        (tactics: TacticsConfig)
        : bool * PitchZone option =
        let teamHasBall =
            match ballPossession with
            | Owned(side, _) -> side = team.ClubSide
            | InFlight -> false
            | SetPiece(side, _) -> side = team.ClubSide
            | Contest(side) -> side = team.ClubSide
            | Transition(side) -> side = team.ClubSide
            | Loose -> false

        if teamHasBall then false, None
        elif emergent.PressingIntensity < 0.45 then false, None
        else
            let dir = team.AttackDir
            let zone = ofBallX ballPos.X dir
            let shouldPress =
                match tactics.PressTriggerZone, zone with
                | AttackingZone, AttackingZone -> true
                | MidfieldZone, (AttackingZone | MidfieldZone) -> true
                | DefensiveZone, _ -> true
                | _ -> false
            shouldPress, Some zone

    let private computeSupportPositions
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballPossession: Possession)
        (phase: MatchPhase)
        (tactics: TacticsConfig)
        (desiredWidth: float)
        (basePositions: Spatial[])
        : Spatial[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let result = Array.zeroCreate<Spatial> n

        let ballCarrierId =
            match ballPossession with
            | Owned(_, pid) -> Some pid
            | InFlight -> None
            | SetPiece(_, _) -> None
            | Contest _ -> None
            | Transition _ -> None
            | Loose -> None

        for i = 0 to n - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let px = basePositions[i].X
                let py = basePositions[i].Y

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
        (influence: InfluenceTypes.InfluenceFrame)
        : PlayerId option * RunType option * Spatial option =
        let frame = team.OwnFrame
        let roster = team.OwnRoster

        let ballCarrierId =
            match ballPossession with
            | Owned(_, pid) -> Some pid
            | InFlight -> None
            | _ -> None

        let dir = team.AttackDir
        let forwardX = if dir = LeftToRight then 1.0 else -1.0

        let mutable bestCellIdx = 0
        let mutable bestCellScore = -1.0
        for cell = 0 to InfluenceTypes.GridSize - 1 do
            let s = InfluenceTypes.scoreCellRaw cell influence (float32 ballPos.X) (float32 ballPos.Y) dir
            if s > bestCellScore then
                bestCellScore <- s
                bestCellIdx <- cell

        let bestCellTarget =
            let cx, cy = InfluenceTypes.cellToCenter bestCellIdx
            defaultSpatial (PhysicsContract.clamp (float cx * 1.0<meter>) 2.0<meter> 98.0<meter>)
                           (PhysicsContract.clamp (float cy * 1.0<meter>) 2.0<meter> 98.0<meter>)

        let mutable bestPlayerId: PlayerId option = None
        let mutable bestRunType: RunType option = None
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

                        let runType =
                            match player.Position with
                            | ST | AMC -> DeepRun
                            | AML | ML | WBL -> DriftWide
                            | AMR | MR | WBR -> DriftWide
                            | MC -> CheckToBall
                            | DL | DR -> OverlapRun
                            | _ -> DeepRun

                        bestRunType <- Some runType
            | _ -> ()

        bestPlayerId, bestRunType, Some bestCellTarget

    let private computeDefensiveShape
        (team: TeamPerspective)
        (cFrame: CognitiveFrame)
        (currentSubTick: int)
        (transitionExpiry: int)
        : DefensiveRole[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let assignments = Array.create n DefensiveRole.Marker

        if cFrame.BallCarrierOppIdx < 0s then assignments
        else
            let bcIdx = int cFrame.BallCarrierOppIdx
            let bcX32 = team.OppFrame.PosX[bcIdx]
            let bcY32 = team.OppFrame.PosY[bcIdx]

            let mutable nearestIdx = -1
            let mutable nearestDistSq = System.Single.MaxValue
            for i = 0 to n - 1 do
                let occ = frame.Occupancy[i]
                match occ with
                | OccupancyKind.Active _ ->
                    let dx = frame.PosX[i] - bcX32
                    let dy = frame.PosY[i] - bcY32
                    let dSq = dx * dx + dy * dy
                    if dSq < nearestDistSq then
                        nearestDistSq <- dSq
                        nearestIdx <- i
                | _ -> ()

            if nearestIdx >= 0 then
                assignments[nearestIdx] <- FirstDefender

                let inTransition = currentSubTick < transitionExpiry
                let maxCover = if inTransition then 2 else 3

                let mutable coverCount = 0
                for i = 0 to n - 1 do
                    if i <> nearestIdx && coverCount < maxCover then
                        let occ = frame.Occupancy[i]
                        match occ with
                        | OccupancyKind.Active _ ->
                            let roster = team.OwnRoster
                            if roster.Players[i].Position <> GK then
                                assignments[i] <- Cover
                                coverCount <- coverCount + 1
                        | _ -> ()

            assignments

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
        let pressTrigger, pressZone = computePressTrigger team ballPos state.Ball.Possession emergent tactics
        let basePositions = SimStateOps.getBasePositions state clubSide
        let supportPositions = computeSupportPositions team ballPos state.Ball.Possession phase tactics desiredWidth basePositions
        let influence =
            if clubSide = HomeClub then state.HomeInfluenceFrame
            else state.AwayInfluenceFrame

        let runnerId, runType, runTarget = pickRunner team ballPos state.Ball.Possession emergent influence
        let defAssignments = computeDefensiveShape team cFrame state.SubTick (getTeam state clubSide).TransitionPressExpiry

        { BuildUpSide = buildUpSide
          PressTrigger = pressTrigger
          PressTriggerZone = pressZone
          TargetRunner = runnerId
          RunType = runType
          RunTarget = runTarget
          SupportPositions = supportPositions
          DesiredWidth = desiredWidth
          Tempo = tempo
          DefensiveAssignments = defAssignments }
