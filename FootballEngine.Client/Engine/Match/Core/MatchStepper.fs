namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open FootballEngine.Player.Actions
open FootballEngine.Player.Perception
open FootballEngine.Player.Steering
open FootballEngine.TeamOrchestrator
open FootballEngine.Types
open FootballEngine.Types.TacticsConfig
open SimStateOps
open SchedulingTypes

type StepResult =
    { State: SimState
      Events: MatchEvent list }

module CognitiveFrameModule =
    let build (ctx: MatchContext) (state: SimState) (clubSide: ClubSide) : CognitiveFrame =
        let ownFrame = getFrame state clubSide
        let oppFrame = getFrame state (ClubSide.flip clubSide)
        let ownRoster = getRoster ctx clubSide
        let n = ownFrame.SlotCount
        let m = oppFrame.SlotCount

        let buffers =
            let existing =
                if clubSide = HomeClub then
                    state.HomeCFrameBuffers
                else
                    state.AwayCFrameBuffers

            match existing with
            | Some buf when buf.NearestTeammateIdx.Length >= n ->
                Array.fill buf.NearestTeammateIdx 0 buf.NearestTeammateIdx.Length 0s
                Array.fill buf.NearestTeammateDistSq 0 buf.NearestTeammateDistSq.Length System.Single.MaxValue
                Array.fill buf.NearestOpponentIdx 0 buf.NearestOpponentIdx.Length 0s
                Array.fill buf.NearestOpponentDistSq 0 buf.NearestOpponentDistSq.Length System.Single.MaxValue
                Array.fill buf.BestPassTargetIdx 0 buf.BestPassTargetIdx.Length -1s
                Array.fill buf.BestPassTargetPos 0 buf.BestPassTargetPos.Length ValueNone
                Array.fill buf.PressureOnPlayer 0 buf.PressureOnPlayer.Length System.Single.MaxValue
                buf
            | _ ->
                let newBuf = CognitiveFrameBuffers.create n

                if clubSide = HomeClub then
                    state.HomeCFrameBuffers <- Some newBuf
                else
                    state.AwayCFrameBuffers <- Some newBuf

                newBuf

        let nearestTMIdx = buffers.NearestTeammateIdx
        let nearestTMDistSq = buffers.NearestTeammateDistSq
        let nearestOppIdx = buffers.NearestOpponentIdx
        let nearestOppDistSq = buffers.NearestOpponentDistSq
        let bestPassIdx = buffers.BestPassTargetIdx
        let bestPassPos = buffers.BestPassTargetPos
        let pressure = buffers.PressureOnPlayer

        let bx = float32 state.Ball.Position.X
        let by = float32 state.Ball.Position.Y
        let dir = attackDirFor clubSide state
        let ballZone = ofBallX state.Ball.Position.X dir
        let phase = phaseFromBallZone dir state.Ball.Position.X

        let ballCarrierOppIdx =
            let ballControlledByOpp =
                match state.Ball.Control with
                | Controlled(side, pid) -> if side <> clubSide then Some pid else None
                | Receiving(side, pid, _) -> if side <> clubSide then Some pid else None
                | _ -> None

            match ballControlledByOpp with
            | Some pid ->
                let mutable found = -1s
                let oppFrame = getFrame state (ClubSide.flip clubSide)
                let oppRoster = getRoster ctx (ClubSide.flip clubSide)

                for i = 0 to oppFrame.SlotCount - 1 do
                    match oppFrame.Physics.Occupancy[i] with
                    | OccupancyKind.Active rosterIdx when oppRoster.Players[rosterIdx].Id = pid -> found <- int16 i
                    | _ -> ()

                found
            | _ -> -1s

        for i = 0 to n - 1 do
            match ownFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let ox = ownFrame.Physics.PosX[i]
                let oy = ownFrame.Physics.PosY[i]

                let mutable minTMDistSq = System.Single.MaxValue
                let mutable minTMIdx = int16 -1s

                for j = 0 to n - 1 do
                    if i <> j then
                        match ownFrame.Physics.Occupancy[j] with
                        | OccupancyKind.Active _ ->
                            let dx = ownFrame.Physics.PosX[j] - ox
                            let dy = ownFrame.Physics.PosY[j] - oy
                            let d = dx * dx + dy * dy

                            if d < minTMDistSq then
                                minTMDistSq <- d
                                minTMIdx <- int16 j
                        | _ -> ()

                nearestTMIdx[i] <- minTMIdx
                nearestTMDistSq[i] <- minTMDistSq

                let mutable minOppDistSq = System.Single.MaxValue
                let mutable minOppIdx = int16 -1s

                for j = 0 to m - 1 do
                    match oppFrame.Physics.Occupancy[j] with
                    | OccupancyKind.Active _ ->
                        let dx = oppFrame.Physics.PosX[j] - ox
                        let dy = oppFrame.Physics.PosY[j] - oy
                        let d = dx * dx + dy * dy

                        if d < minOppDistSq then
                            minOppDistSq <- d
                            minOppIdx <- int16 j
                    | _ -> ()

                nearestOppIdx[i] <- minOppIdx
                nearestOppDistSq[i] <- minOppDistSq
                pressure[i] <- minOppDistSq

                let bestPass = findBestPassTargetFrame i ownFrame ownRoster oppFrame dir

                match bestPass with
                | ValueSome(idx, sp) ->
                    bestPassIdx[i] <- int16 idx
                    bestPassPos[i] <- ValueSome sp
                | ValueNone -> ()

            | _ -> ()

        { NearestTeammateIdx = nearestTMIdx
          NearestTeammateDistSq = nearestTMDistSq
          NearestOpponentIdx = nearestOppIdx
          NearestOpponentDistSq = nearestOppDistSq
          BestPassTargetIdx = bestPassIdx
          BestPassTargetPos = bestPassPos
          BallX = bx
          BallY = by
          BallZone = ballZone
          Phase = phase
          PressureOnPlayer = pressure
          SlotCount = n
          BallCarrierOppIdx = ballCarrierOppIdx }

module MatchStepper =

    let private bothSides = [| HomeClub; AwayClub |]

    let private appendEvent (state: SimState) (events: ResizeArray<MatchEvent>) (e: MatchEvent) =
        events.Add e
        state.MatchEvents.Add e

        if state.MatchEvents.Count > 1024 then
            state.MatchEvents.RemoveRange(0, 512)

    let private setFlow (state: SimState) (flow: MatchFlow) = state.Flow <- flow

    let private setPieceDelay (clock: SimulationClock) (config: BalanceConfig) (kind: SetPieceKind) =
        match kind with
        | SetPieceKind.KickOff -> TickDelay.delayFrom clock config.Timing.KickOffDelay
        | SetPieceKind.ThrowIn -> TickDelay.delayFrom clock config.Timing.ThrowInDelay
        | SetPieceKind.Corner -> TickDelay.delayFrom clock config.Timing.CornerDelay
        | SetPieceKind.GoalKick -> TickDelay.delayFrom clock config.Timing.GoalKickDelay
        | SetPieceKind.FreeKick -> TickDelay.delayFrom clock config.Timing.FreeKickDelay
        | SetPieceKind.Penalty -> TickDelay.delayFrom clock config.Timing.FreeKickDelay

    let private restartFromSetPiece
        (state: SimState)
        (clock: SimulationClock)
        (kind: SetPieceKind)
        (team: ClubSide)
        (cause: RestartCause)
        =
        RestartDelay
            { Kind = kind
              Team = team
              Cause = cause
              RemainingTicks = setPieceDelay clock state.Config kind }


    let private applyVARDecision
        (subTick: int)
        (ctx: MatchContext)
        (clock: SimulationClock)
        (state: SimState)
        (events: ResizeArray<MatchEvent>)
        (review: VARFlowState)
        =
        let decision = VARReview.evaluate state review.Incident

        let varEvents =
            match decision with
            | Overturn -> VARApplicator.applyOverturn subTick review.Incident ctx state
            | CheckComplete -> VARApplicator.applyCheckComplete subTick review.Incident ctx state
            | _ -> []

        varEvents |> List.iter (appendEvent state events)

        match review.Incident with
        | GoalCheck(scoringClub, _, _, _) ->
            let receiving = ClubSide.flip scoringClub
            setFlow state (restartFromSetPiece state clock SetPieceKind.KickOff receiving AfterVAR)
        | PenaltyCheck(team, _, _) ->
            let kind =
                if decision = Overturn then
                    SetPieceKind.FreeKick
                else
                    SetPieceKind.Penalty

            setFlow state (restartFromSetPiece state clock kind team AfterVAR)
        | RedCardCheck _
        | OffsideCheck _ ->
            setFlow state (restartFromSetPiece state clock SetPieceKind.FreeKick state.AttackingSide AfterVAR)

    let private startGoalFlow
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (events: ResizeArray<MatchEvent>)
        =
        let scoringClub = state.AttackingSide
        let scorerId, isOwnGoal = GoalDetector.scorer scoringClub state.Ball ctx state

        RefereeApplicator.apply subTick (ConfirmGoal(scoringClub, scorerId, isOwnGoal)) ctx state
        |> List.iter (appendEvent state events)

        match VARDetector.detectGoalCheck scoringClub scorerId isOwnGoal subTick with
        | Some incident ->
            state.StoppageTime.Add(subTick, StoppageReason.VARReviewDelay) |> ignore
            let duration = VARReview.reviewDuration subTick

            setFlow
                state
                (VARReview
                    { Incident = incident
                      Phase = CheckingIncident
                      RemainingTicks = duration
                      TotalTicks = duration })
        | None ->
            setFlow
                state
                (GoalPause
                    { ScoringTeam = scoringClub
                      ScorerId = scorerId
                      IsOwnGoal = isOwnGoal
                      RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.KickOffDelay
                      VARRequested = false })

    let private sideByClubId (ctx: MatchContext) (clubId: ClubId) =
        if clubId = ctx.Home.Id then Some HomeClub
        elif clubId = ctx.Away.Id then Some AwayClub
        else None

    let private pendingSubs (state: SimState) (side: ClubSide) : SubstitutionRequest list =
        if side = HomeClub then
            state.HomePendingSubstitutions
        else
            state.AwayPendingSubstitutions

    let private setPendingSubs (state: SimState) (side: ClubSide) (requests: SubstitutionRequest list) =
        if side = HomeClub then
            state.HomePendingSubstitutions <- requests
        else
            state.AwayPendingSubstitutions <- requests

    let private tryApplySubstitution
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (request: SubstitutionRequest)
        =
        match sideByClubId ctx request.ClubId with
        | None -> []
        | Some side ->
            let team = getTeam state side
            let frame = team.Frame
            let roster = getRoster ctx side

            match findIdxByPid request.OutPlayerId frame roster with
            | ValueNone -> []
            | ValueSome outIdx ->
                let squad = if side = HomeClub then ctx.HomePlayers else ctx.AwayPlayers

                match squad |> Array.tryFind (fun p -> p.Id = request.InPlayerId) with
                | None -> []
                | Some incoming ->
                    ManagerAgent.resolve subTick (MakeSubstitution(request.ClubId, outIdx, incoming)) ctx state

    let private flushPendingSubstitutions
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (events: ResizeArray<MatchEvent>)
        =
        match state.Flow with
        | Live
        | MatchEnded -> ()
        | _ ->
            for side in bothSides do
                let requests = pendingSubs state side
                let applied = ResizeArray<SubstitutionRequest>()

                for request in requests do
                    let evs = tryApplySubstitution subTick ctx state request

                    if not (List.isEmpty evs) then
                        applied.Add request
                        evs |> List.iter (appendEvent state events)

                if applied.Count > 0 then
                    let appliedIds =
                        applied
                        |> Seq.map (fun r -> r.CommandId, r.OutPlayerId, r.InPlayerId)
                        |> Set.ofSeq

                    requests
                    |> List.filter (fun r -> not (Set.contains (r.CommandId, r.OutPlayerId, r.InPlayerId) appliedIds))
                    |> setPendingSubs state side

    let private applyCommand
        (ctx: MatchContext)
        (state: SimState)
        (events: ResizeArray<MatchEvent>)
        (command: MatchCommandEnvelope)
        =
        match command.Command with
        | PauseSimulation ->
            if state.Flow = Live then
                setFlow
                    state
                    (RestartDelay
                        { Kind = SetPieceKind.KickOff
                          Team = state.AttackingSide
                          Cause = AfterBallOut
                          RemainingTicks = 1 })

        | ResumeSimulation ->
            match state.Flow with
            | RestartDelay r when r.RemainingTicks <= 1 -> setFlow state Live
            | _ -> ()

        | ChangeTactics(clubId, tactics) -> setTacticsByClubId clubId ctx state tactics

        | ChangeInstructions(clubId, instructions) ->
            match sideByClubId ctx clubId with
            | Some HomeClub -> state.Home.Instructions <- Some instructions
            | Some AwayClub -> state.Away.Instructions <- Some instructions
            | None -> ()

        | RequestSubstitution(clubId, outPlayerId, inPlayerId) ->
            match sideByClubId ctx clubId with
            | None -> ()
            | Some side ->
                let request =
                    { ClubId = clubId
                      OutPlayerId = outPlayerId
                      InPlayerId = inPlayerId
                      RequestedSubTick = state.SubTick
                      CommandId = Some command.CommandId }

                match state.Flow with
                | Live -> request :: pendingSubs state side |> setPendingSubs state side
                | _ ->
                    let evs = tryApplySubstitution state.SubTick ctx state request

                    if List.isEmpty evs then
                        request :: pendingSubs state side |> setPendingSubs state side
                    else
                        evs |> List.iter (appendEvent state events)

    let private applyCommands
        (ctx: MatchContext)
        (state: SimState)
        (events: ResizeArray<MatchEvent>)
        (commands: MatchCommandEnvelope[])
        =
        MatchCommands.orderForTick state.SubTick commands
        |> Array.iter (applyCommand ctx state events)

    let private processTransition (state: SimState) (transition: MatchFlow option) =
        match transition with
        | None -> ()
        | Some f -> setFlow state f

    let private runSetPiece
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (events: ResizeArray<MatchEvent>)
        (restart: RestartPlan)
        =
        let result = SetPieceAgent.run restart.Kind restart.Team ctx state clock
        result.Events |> List.iter (appendEvent state events)
        processTransition state result.Transition

    let private updatePossessionHistory (result: BallResult) (subTick: int) (state: SimState) =
        let h = state.PossessionHistory

        state.PossessionHistory <-
            { h with
                LastChangeTick =
                    if result.PossessionChanged then
                        subTick
                    else
                        h.LastChangeTick
                LastBallInFlightTick =
                    if result.BallInFlight then
                        subTick
                    else
                        h.LastBallInFlightTick
                LastSetPieceTick =
                    if result.SetPieceAwarded then
                        subTick
                    else
                        h.LastSetPieceTick
                LastBallReceivedTick =
                    match result.ReceivedByPlayer with
                    | Some _ -> subTick
                    | None -> h.LastBallReceivedTick
                ChangedToSide =
                    if result.PossessionChanged then
                        match state.Ball.Control with
                        | Controlled(side, _)
                        | Receiving(side, _, _) -> Some side
                        | _ -> h.ChangedToSide
                    else
                        h.ChangedToSide }

    let private updateFlow
        (ctx: MatchContext)
        (clock: SimulationClock)
        (state: SimState)
        (events: ResizeArray<MatchEvent>)
        =
        let wasLive = state.Flow = Live

        match state.Flow with
        | GoalPause goal when goal.RemainingTicks > 0 ->
            setFlow
                state
                (GoalPause
                    { goal with
                        RemainingTicks = goal.RemainingTicks - 1 })

        | GoalPause goal ->
            setFlow
                state
                (restartFromSetPiece state clock SetPieceKind.KickOff (ClubSide.flip goal.ScoringTeam) AfterGoal)

        | VARReview review when review.RemainingTicks > 0 ->
            setFlow
                state
                (VARReview
                    { review with
                        RemainingTicks = review.RemainingTicks - 1 })

        | VARReview review -> applyVARDecision state.SubTick ctx clock state events review

        | InjuryPause injury when injury.RemainingTicks > 0 ->
            setFlow
                state
                (InjuryPause
                    { injury with
                        RemainingTicks = injury.RemainingTicks - 1 })

        | InjuryPause _ ->
            setFlow state (restartFromSetPiece state clock SetPieceKind.FreeKick state.AttackingSide AfterInjury)

        | RestartDelay restart when restart.RemainingTicks > 0 ->
            setFlow
                state
                (RestartDelay
                    { restart with
                        RemainingTicks = restart.RemainingTicks - 1 })

        | RestartDelay restart ->
            flushPendingSubstitutions state.SubTick ctx state events
            runSetPiece ctx state clock events restart

        | HalfTimePause remaining when remaining > 0 -> setFlow state (HalfTimePause(remaining - 1))

        | HalfTimePause _ ->
            setFlow state (restartFromSetPiece state clock SetPieceKind.KickOff AwayClub InitialKickOff)

        | FullTimeReview -> setFlow state MatchEnded

        | Live
        | MatchEnded -> ()

        // Phase 4: suspend/resume directives on Live<->pause transitions
        let isNowLive = state.Flow = Live

        if wasLive && not isNowLive then
            suspendDirective state HomeClub
            suspendDirective state AwayClub
        elif not wasLive && isNowLive then
            resumeDirective state HomeClub
            resumeDirective state AwayClub

    let private runCognition (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) =
        let homeInfluence = InfluenceFrame.compute state.Home.Frame state.Away.Frame
        let awayInfluence = InfluenceFrame.compute state.Away.Frame state.Home.Frame
        state.HomeInfluenceFrame <- homeInfluence
        state.AwayInfluenceFrame <- awayInfluence

        for clubSide in bothSides do
            let cFrame = CognitiveFrameModule.build ctx state clubSide
            BatchDecision.processTeam subTick ctx state clock clubSide cFrame

    let private runAdaptive (state: SimState) =
        for clubSide in bothSides do
            let stats = getMatchStats state clubSide
            let emergent = getEmergentState state clubSide

            let shortPassRate =
                if stats.PassAttempts > 0 then
                    float stats.PassSuccesses / float stats.PassAttempts
                else
                    0.5

            let pressRate =
                if stats.PressAttempts > 0 then
                    float stats.PressSuccesses / float stats.PressAttempts
                else
                    0.5

            let flankRate =
                if stats.FlankAttempts > 0 then
                    float stats.FlankSuccesses / float stats.FlankAttempts
                else
                    0.5

            let frame = getFrame state clubSide
            let mutable totalCondition = 0
            let mutable activeCount = 0

            for i = 0 to frame.SlotCount - 1 do
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    totalCondition <- totalCondition + int frame.Condition[i]
                    activeCount <- activeCount + 1
                | _ -> ()

            let avgCondition =
                if activeCount > 0 then
                    float totalCondition / float activeCount
                else
                    50.0

            let updated =
                emergent
                |> EmergentLoops.updateCompactness shortPassRate
                |> EmergentLoops.updatePressing pressRate
                |> EmergentLoops.updateWingPlay flankRate
                |> EmergentLoops.updateFatigueSpiral avgCondition 0

            setEmergentState state clubSide updated

            // Phase 3: update directive params based on new emergent state (without changing kind)
            let directive = SimStateOps.getDirective state clubSide

            match directive with
            | TeamDirectiveState.Active d ->
                let tactics =
                    SimStateOps.tacticsConfig
                        (SimStateOps.getTactics state clubSide)
                        (SimStateOps.getInstructions state clubSide)

                let newParams = defaultParams tactics updated
                SimStateOps.setDirective state clubSide (TeamDirectiveState.Active { d with Params = newParams })
            | _ -> ()

            let recent = EventWindow.recentEvents 1200 state.MatchEvents
            let adaptiveState = getAdaptiveState state clubSide

            let updatedRecords =
                adaptiveState.Records
                |> Array.map (fun r -> EventWindow.patternResults r.Pattern recent)

            let updatedAdaptive =
                { AdaptiveTactics.initial with
                    Records = updatedRecords }

            setAdaptiveState state clubSide updatedAdaptive

            let rate (pattern: AttackPattern) =
                updatedAdaptive.Records
                |> Array.tryFind (fun r -> r.Pattern = pattern)
                |> Option.map (fun r ->
                    if r.Attempts > 3 then
                        float r.Successes / float r.Attempts
                    else
                        0.5)
                |> Option.defaultValue 0.5

            let wingBias =
                System.Math.Clamp((rate AttackPattern.LeftFlank - rate AttackPattern.RightFlank) * 0.3, -0.3, 0.3)

            let directnBias =
                System.Math.Clamp((rate AttackPattern.LongBall - 0.5) * 0.3, -0.3, 0.3)

            match SimStateOps.getDirective state clubSide with
            | TeamDirectiveState.Active d ->
                let newTransition =
                    { d.Params.Transition with
                        WingBias = wingBias
                        DirectnessBias = directnBias }

                let newParams =
                    { d.Params with
                        Transition = newTransition }

                SimStateOps.setDirective state clubSide (TeamDirectiveState.Active { d with Params = newParams })
            | _ -> ()

            resetAdaptiveStats state clubSide

    let private maybeRunManagerWindow
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (events: ResizeArray<MatchEvent>)
        =
        let elapsedSec = int (SimulationClock.subTicksToSeconds clock subTick)

        let shouldRun =
            subTick % clock.SubTicksPerSecond = 0
            && (ctx.Config.Manager.SubWindowMinutes
                |> Array.exists (fun m -> elapsedSec = m * 60))

        if shouldRun then
            let result = ManagerAgent.agent ctx state clock
            result.Events |> List.iter (appendEvent state events)
            processTransition state result.Transition

    let private runLiveSystems
        (ctx    : MatchContext)
        (clock  : SimulationClock)
        (state  : SimState)
        (events : ResizeArray<MatchEvent>)
        =
        match state.Flow with
        | Live ->
            let subTick = state.SubTick

            // Física: siempre corre
            SimStateOps.expireReceiving subTick ctx.Config.Physics.ReceivingGraceSubTicks state
            let dtPlayer = SimulationClock.dtPlayer clock
            MovementEngine.updateTeamSide subTick ctx state HomeClub dtPlayer clock
            MovementEngine.updateTeamSide subTick ctx state AwayClub dtPlayer clock

            let ballResult = BallAgent.agent ctx state clock
            updatePossessionHistory ballResult subTick state
            ballResult.Events |> List.iter (appendEvent state events)

            match ballResult.GoalScored with
            | Some _ -> startGoalFlow subTick ctx state clock events
            | None   -> processTransition state ballResult.Transition

            // Drenar semánticos acumulados por BallAgent y RefereeApplicator
            let semanticEvents = SimStateOps.drainSemanticEvents state

            // Router decide qué agentes corren
            let activations = EventRouter.route semanticEvents subTick clock

            for activation in activations do
                match activation with
                | ActivatePhysics -> ()

                | ActivateCognition ->
                    runCognition subTick ctx state clock

                | ActivateAction _ ->
                    let frameHome = getFrame state HomeClub
                    let frameAway = getFrame state AwayClub
                    if frameHome.SlotCount > 0 && frameAway.SlotCount > 0 then
                        let actionResult, pendingRefActions = ActionResolver.run subTick ctx state clock
                        actionResult.Events |> List.iter (appendEvent state events)
                        pendingRefActions
                        |> List.collect (fun a -> RefereeApplicator.apply subTick a ctx state)
                        |> List.iter (appendEvent state events)

                | ActivateReferee _ ->
                    let refResult = RefereeAgent.agent ctx state clock
                    refResult.Actions
                    |> List.collect (fun a -> RefereeApplicator.apply subTick a ctx state)
                    |> List.iter (appendEvent state events)
                    processTransition state refResult.Transition

                | ActivateTeam _ ->
                    TeamOrchestrator.tick subTick ctx state clock

                | ActivateManager _ ->
                    maybeRunManagerWindow subTick ctx state clock events

            runAdaptive state
        | _ -> () // Si es VAR, Pausa, Gol, etc., los sistemas de juego no corren


    let private updateMatchClock (clock: SimulationClock) (state: SimState) =
        if state.EffectiveFullTimeSubTick = 0 then
            state.EffectiveFullTimeSubTick <- SimulationClock.fullTime clock

        let ht = SimulationClock.halfTime clock

        if not state.HalfTimeHandled && state.SubTick >= ht && state.Flow = Live then
            state.HalfTimeHandled <- true
            let halfAdded = state.StoppageTime.DecideHalfTime()

            state.EffectiveFullTimeSubTick <-
                max state.EffectiveFullTimeSubTick (ht + halfAdded * clock.SubTicksPerSecond)

            setFlow state (HalfTimePause(TickDelay.delayFrom clock state.Config.Timing.KickOffDelay))

        if not state.FullTimeHandled && state.SubTick >= state.EffectiveFullTimeSubTick then
            state.FullTimeHandled <- true
            let fullAdded = state.StoppageTime.DecideFullTime()

            state.EffectiveFullTimeSubTick <-
                max
                    state.EffectiveFullTimeSubTick
                    (SimulationClock.fullTime clock + fullAdded * clock.SubTicksPerSecond)

            if state.SubTick >= state.EffectiveFullTimeSubTick then
                setFlow state MatchEnded

    let updateOne (ctx: MatchContext) (clock: SimulationClock) (commands: MatchCommandEnvelope[]) (state: SimState) =
        let events = ResizeArray<MatchEvent>()

        applyCommands ctx state events commands
        updateFlow ctx clock state events

        if state.Flow = Live then
            runLiveSystems ctx clock state events

        if
            state.SubTick - state.LastMemoryDecaySubTick
            >= MatchMemory.DecayIntervalSubTicks
        then
            MatchMemory.decay state.MatchMemory
            state.LastMemoryDecaySubTick <- state.SubTick

        flushPendingSubstitutions state.SubTick ctx state events
        updateMatchClock clock state

        if state.Flow <> MatchEnded then
            state.SubTick <- state.SubTick + 1

        { State = state
          Events = events |> Seq.toList }
