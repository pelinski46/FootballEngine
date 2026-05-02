namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps
open FootballEngine.Movement

module BatchDecision =

    let private computeSupportPositions
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballControl: BallControl)
        (phase: MatchPhase)
        (tactics: TacticsConfig)
        (desiredWidth: float)
        (basePositions: Spatial[])
        : Spatial[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let result = Array.zeroCreate<Spatial> n

        let ballCarrierId =
            match ballControl with
            | Controlled(_, pid) | Receiving(_, pid, _) -> Some pid
            | _ -> None

        for i = 0 to n - 1 do
            match frame.Physics.Occupancy[i] with
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

                    let targetX = clamp baseX 2.0<meter> 98.0<meter>
                    let targetY = clamp (centerY + widthOffset) 2.0<meter> 98.0<meter>

                    result[i] <- defaultSpatial targetX targetY
            | _ -> result[i] <- defaultSpatial 52.5<meter> 34.0<meter>

        result

    let private pickRunner
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballControl: BallControl)
        (emergent: EmergentState)
        (influence: InfluenceTypes.InfluenceFrame)
        : PlayerId option * RunType option * Spatial option =
        let frame = team.OwnFrame
        let roster = team.OwnRoster

        let ballCarrierId =
            match ballControl with
            | Controlled(_, pid) | Receiving(_, pid, _) -> Some pid
            | _ -> None

        let dir = team.AttackDir
        let forwardX = if dir = LeftToRight then 1.0 else -1.0

        let mutable bestCellIdx = 0
        let mutable bestCellScore = -1.0

        for cell = 0 to InfluenceTypes.GridSize - 1 do
            let s =
                InfluenceTypes.scoreCellRaw cell influence (float32 ballPos.X) (float32 ballPos.Y) dir

            if s > bestCellScore then
                bestCellScore <- s
                bestCellIdx <- cell

        let bestCellTarget =
            let cx, cy = InfluenceTypes.cellToCenter bestCellIdx

            defaultSpatial
                (clamp (float cx * 1.0<meter>) 2.0<meter> 98.0<meter>)
                (clamp (float cy * 1.0<meter>) 2.0<meter> 98.0<meter>)

        let mutable bestPlayerId: PlayerId option = None
        let mutable bestRunType: RunType option = None
        let mutable bestScore = -1.0

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]

                if ballCarrierId <> Some player.Id then
                    let px = float frame.Physics.PosX[i] * 1.0<meter>
                    let py = float frame.Physics.PosY[i] * 1.0<meter>

                    let isAdvanced = (px - 52.5<meter>) * forwardX > 5.0<meter>
                    let workRate = float player.Mental.WorkRate / 20.0
                    let positioning = float player.Mental.Positioning / 20.0
                    let score = workRate * 0.3 + positioning * 0.4 + (if isAdvanced then 0.5 else 0.0)

                    if score > bestScore then
                        bestScore <- score
                        bestPlayerId <- Some player.Id

                        let runType =
                            match player.Position with
                            | ST
                            | AMC -> DeepRun
                            | AML
                            | ML
                            | WBL -> DriftWide
                            | AMR
                            | MR
                            | WBR -> DriftWide
                            | MC -> CheckToBall
                            | DL
                            | DR -> OverlapRun
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

        if cFrame.BallCarrierOppIdx < 0s then
            assignments
        else
            let bcIdx = int cFrame.BallCarrierOppIdx
            let bcX32 = team.OppFrame.Physics.PosX[bcIdx]
            let bcY32 = team.OppFrame.Physics.PosY[bcIdx]

            let mutable nearestIdx = -1
            let mutable nearestDistSq = System.Single.MaxValue

            for i = 0 to n - 1 do
                let occ = frame.Physics.Occupancy[i]

                match occ with
                | OccupancyKind.Active _ ->
                    let dx = frame.Physics.PosX[i] - bcX32
                    let dy = frame.Physics.PosY[i] - bcY32
                    let dSq = dx * dx + dy * dy

                    if dSq < nearestDistSq then
                        nearestDistSq <- dSq
                        nearestIdx <- i
                | _ -> ()

            if nearestIdx >= 0 then
                assignments[nearestIdx] <- DefensiveRole.FirstDefender

                let inTransition = currentSubTick < transitionExpiry
                let maxCover = if inTransition then 2 else 3

                let mutable coverCount = 0

                for i = 0 to n - 1 do
                    if i <> nearestIdx && coverCount < maxCover then
                        let occ = frame.Physics.Occupancy[i]

                        match occ with
                        | OccupancyKind.Active _ ->
                            let roster = team.OwnRoster

                            if roster.Players[i].Position <> GK then
                                assignments[i] <- DefensiveRole.Cover
                                coverCount <- coverCount + 1
                        | _ -> ()

            assignments

    let processTeam
        (currentSubTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (clubSide: ClubSide)
        (cFrame: CognitiveFrame)
        : unit =
        let team = SimStateOps.buildTeamPerspective clubSide ctx state
        let frame = team.OwnFrame
        let roster = team.OwnRoster
        let emergent = getEmergentState state clubSide
        let basePositions = getBasePositions state clubSide
        let dir = team.AttackDir
        let ballXSmooth = state.BallXSmooth
        let phase = phaseFromBallZone dir ballXSmooth

        let tacticsCfg =
            tacticsConfig (getTactics state clubSide) (getInstructions state clubSide)

        let shapeTargetX = Array.zeroCreate<float32> frame.SlotCount
        let shapeTargetY = Array.zeroCreate<float32> frame.SlotCount

        ShapeEngine.computeShapeTargets basePositions dir phase ballXSmooth tacticsCfg shapeTargetX shapeTargetY

        let ballPos = state.Ball.Position

        let supportPositions =
            computeSupportPositions team ballPos state.Ball.Control phase tacticsCfg tacticsCfg.Width basePositions

        let influence =
            if clubSide = HomeClub then
                state.HomeInfluenceFrame
            else
                state.AwayInfluenceFrame

        let runnerId, runType, runTarget =
            pickRunner team ballPos state.Ball.Control emergent influence

        let defAssignments =
            computeDefensiveShape team cFrame currentSubTick (getTeam state clubSide).TransitionPressExpiry

        // Write support positions and defensive roles directly to TeamFrame arrays
        for i = 0 to frame.SlotCount - 1 do
            if i < supportPositions.Length then
                frame.SupportPositionX[i] <- float32 supportPositions[i].X
                frame.SupportPositionY[i] <- float32 supportPositions[i].Y

            if i < defAssignments.Length then
                frame.DefensiveRole[i] <- byte defAssignments[i]

        // Derive and set TeamDirective on the team state
        let kind = TeamDirectiveOps.kindFromTactics (getTactics state clubSide)
        let directiveParams = SimStateOps.defaultParams tacticsCfg emergent

        let newDirective: TeamDirective =
            { Kind = kind
              Params = directiveParams
              TargetRunner = runnerId
              RunType = runType
              RunTarget = runTarget
              ActiveSince = state.SubTick }

        setDirective state clubSide (TeamDirectiveState.Active newDirective)

        let directiveState = SimStateOps.getDirective state clubSide

        let directive =
            TeamDirectiveOps.currentDirective directiveState
            |> Option.defaultValue (TeamDirectiveOps.empty currentSubTick)

        SimStateOps.setCognitiveFrame state clubSide cFrame

        let pressTrigger = directive.Params.Press.Intensity > 0.4

        let ballDistances =
            if pressTrigger then
                let bx = float cFrame.BallX
                let by = float cFrame.BallY

                Array.init frame.SlotCount (fun i ->
                    match frame.Physics.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let dx = float frame.Physics.PosX[i] - bx
                        let dy = float frame.Physics.PosY[i] - by
                        dx * dx + dy * dy
                    | _ -> System.Double.MaxValue)
            else
                Array.empty

        let pressCount =
            if pressTrigger then
                int (3.0 + tacticsCfg.PressingIntensity * 3.0)
            else
                0

        let pressersArray =
            if pressCount > 0 && pressTrigger then
                let n = frame.SlotCount
                let indices = Array.create n -1
                let mutable count = 0

                for i = 0 to n - 1 do
                    match frame.Physics.Occupancy[i] with
                    | OccupancyKind.Active _ when ballDistances[i] < System.Double.MaxValue ->
                        indices[count] <- i
                        count <- count + 1
                    | _ -> ()

                if count > 1 && count > pressCount then
                    let rec quickSelect lo hi k =
                        if lo >= hi then
                            ()
                        else
                            let pivotIdx = indices[lo + (hi - lo) / 2]
                            let pivotDist = ballDistances[pivotIdx]
                            let mutable m = lo
                            let mutable n = hi

                            while m <= n do
                                while m <= hi && ballDistances[indices[m]] < pivotDist do
                                    m <- m + 1

                                while n >= lo && ballDistances[indices[n]] > pivotDist do
                                    n <- n - 1

                                if m <= n then
                                    let tmp = indices[m]
                                    indices[m] <- indices[n]
                                    indices[n] <- tmp
                                    m <- m + 1
                                    n <- n - 1

                            if n >= k then
                                quickSelect lo n k

                            if m <= k then
                                quickSelect m hi k

                    quickSelect 0 (count - 1) (pressCount - 1)

                indices
            else
                Array.empty

        let isPresserIdx (i: int) =
            if pressCount > 0 && pressTrigger then
                let mutable result = false
                let mutable j = 0

                while j < pressCount && pressersArray[j] >= 0 && not result do
                    if pressersArray[j] = i then result <- true else j <- j + 1

                result
            else
                false

        let isSetPiece =
            match state.Flow with
            | RestartDelay _ -> true
            | _ -> false

        let setPiecePositions =
            if isSetPiece then
                SetPiecePositioning.computePositions ctx state clubSide
            else
                Array.empty

        if isSetPiece then
            SetPiecePositioning.assignPositions ctx setPiecePositions clubSide

        let possessionHistory = state.PossessionHistory

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Sidelined _ -> ()
            | OccupancyKind.Active rosterIdx ->
                if not (IntentPhase.shouldRecalculate frame.Intent i currentSubTick possessionHistory) then
                    () // committed and trigger has not fired — skip entirely
                else

                    let player = roster.Players[rosterIdx]
                    let profile = roster.Profiles[rosterIdx]

                    let previousIntent =
                        match frame.Intent.Kind[i] with
                        | IntentKind.Idle -> ValueNone
                        | kind ->
                            let tx = float frame.Intent.TargetX[i] * 1.0<meter>
                            let ty = float frame.Intent.TargetY[i] * 1.0<meter>
                            let fallback = SimStateOps.defaultSpatial tx ty

                            ValueSome(
                                IntentFrame.toMovementIntent
                                    kind
                                    frame.Intent.TargetX[i]
                                    frame.Intent.TargetY[i]
                                    frame.Intent.TargetPid[i]
                                    fallback
                            )

                    let myX = float frame.Physics.PosX[i] * 1.0<meter>
                    let myY = float frame.Physics.PosY[i] * 1.0<meter>
                    let myVx = float frame.Physics.VelX[i] * 1.0<meter / second>
                    let myVy = float frame.Physics.VelY[i] * 1.0<meter / second>

                    let visibilityMask =
                        if isSetPiece then
                            ValueNone
                        else
                            ValueSome(
                                Perception.computeVisibilityMask
                                    i
                                    { X = myX
                                      Y = myY
                                      Z = 0.0<meter>
                                      Vx = myVx
                                      Vy = myVy
                                      Vz = 0.0<meter / second> }
                                    myVx
                                    myVy
                                    frame.Intent.Kind[i]
                                    frame.Intent.TargetX[i]
                                    frame.Intent.TargetY[i]
                                    player.Mental.Vision
                                    player.Mental.Positioning
                                    (player.Position = GK)
                                    state.Ball.Position
                                    frame
                                    team.OppFrame
                                    ctx.Config.Perception
                            )

                    let influence =
                        if clubSide = HomeClub then
                            state.HomeInfluenceFrame
                        else
                            state.AwayInfluenceFrame

                    let actx =
                        AgentContext.build
                            player
                            profile
                            i
                            team
                            previousIntent
                            state
                            clock
                            ctx
                            state.Config.Decision
                            state.Config.BuildUp
                            (Some cFrame)
                            visibilityMask
                            influence

                    let movementScores = MovementScorer.computeAll actx emergent

                    let adjustedScores =
                        if pressTrigger then
                            let isPresser = isPresserIdx i

                            if isPresser then
                                { movementScores with
                                    PressBall = movementScores.PressBall * 1.5 }
                            else
                                { movementScores with
                                    CoverSpace = movementScores.CoverSpace * 1.3 }
                        else
                            movementScores

                    let intent = MovementScorer.pickIntent currentSubTick adjustedScores actx

                    let fallbackPos = SimStateOps.defaultSpatial myX myY

                    let mutable finalIntent =

                        match intent with

                        | MaintainShape _ ->
                            let tx = float shapeTargetX[i] * 1.0<meter>
                            let ty = float shapeTargetY[i] * 1.0<meter>
                            MaintainShape(SimStateOps.defaultSpatial tx ty)
                        | other -> other

                    if isSetPiece then
                        let pos =
                            if i < setPiecePositions.Length then
                                setPiecePositions[i]
                            else
                                fallbackPos

                        finalIntent <- MoveToSetPiecePos pos

                    let kind, tx, ty, tpid = IntentFrame.fromMovementIntent finalIntent
                    FrameMutate.setIntent frame.Intent i kind tx ty tpid

                    let dur, trigger = IntentPhase.duration clock finalIntent
                    FrameMutate.commitIntent frame.Intent i currentSubTick dur trigger

                    match finalIntent with
                    | ExecuteRun run ->
                        let teamState = SimStateOps.getTeam state clubSide

                        teamState.ActiveRuns <-
                            run
                            :: (teamState.ActiveRuns |> List.filter (fun r -> r.PlayerId <> run.PlayerId))
                    | _ -> ()
