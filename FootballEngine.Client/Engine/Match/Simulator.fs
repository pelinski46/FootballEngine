namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SetPlayAction
open SimStateOps
open FootballEngine.Movement
open FootballEngine.PhysicsContract
open FootballEngine.Stats
open SchedulingTypes
open SimulationClock

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private generateInitialTicks (ctx: MatchContext) (clock: SimulationClock) : ScheduledTick list =
        let sub s = secondsToSubTicks clock s

        let subs =
            [ for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub (float min * 60.0)
                    Priority = TickPriority.Manager
                    SequenceId = int64 idx
                    Kind = SubstitutionTick ctx.Home.Id }
              for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub (float min * 60.0)
                    Priority = TickPriority.Manager
                    SequenceId = int64 (idx + 3)
                    Kind = SubstitutionTick ctx.Away.Id } ]

        [ yield
              { SubTick = clock.PhysicsRate
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick }
          yield
              { SubTick = clock.PhysicsRate
                Priority = TickPriority.MatchControl
                SequenceId = 1L
                Kind = RefereeTick }
          yield! subs
          yield
              { SubTick = halfTime clock
                Priority = TickPriority.MatchControl
                SequenceId = 10L
                Kind = HalfTimeTick }
          yield
              { SubTick = fullTime clock
                Priority = TickPriority.MatchControl
                SequenceId = 11L
                Kind = FullTimeTick }
          yield
              { SubTick = 0
                Priority = TickPriority.SetPiece
                SequenceId = -1L
                Kind = KickOffTick } ]

    let private runTick
        (tick: ScheduledTick)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : AgentOutput =
        let dispatchAgent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) =
            match tick.Kind with
            | PhysicsTick -> BallAgent.agent tick ctx state clock
            | DuelTick _
            | DecisionTick _
            | PlayerActionTick _ -> PlayerAgent.agent tick ctx state clock
            | RefereeTick -> RefereeAgent.agent tick ctx state clock
            | FreeKickTick _
            | CornerTick _
            | ThrowInTick _
            | PenaltyTick _
            | GoalKickTick
            | KickOffTick -> SetPieceAgent.agent tick ctx state clock
            | InjuryTick _
            | ResumePlayTick
            | HalfTimeTick
            | FullTimeTick
            | MatchEndTick
            | ExtraTimeTick _ -> RefereeAgent.agent tick ctx state clock
            | SubstitutionTick _
            | ManagerReactionTick _ -> ManagerAgent.agent tick ctx state clock


        dispatchAgent tick ctx state clock

    let private resolveContinuation
        (tick: ScheduledTick)
        (cont: Continuation)
        (transition: PlayState option)
        (counter: int64)
        (clock: SimulationClock)
        (playState: PlayState)
        (totalChainDepth: int)
        =
        if totalChainDepth >= BalanceConfig.MaxChainLength * 2 then
            [], counter
        else
            match cont with
            | SelfReschedule offset ->
                [ { tick with
                      SubTick = tick.SubTick + offset
                      SequenceId = counter } ],
                counter + 1L

            | ChainTo(head, tail, delay) ->
                let kinds = head :: tail

                let ticks =
                    kinds
                    |> List.mapi (fun i k ->
                        { SubTick = tick.SubTick + delay
                          Priority = tick.Priority
                          SequenceId = counter + int64 i
                          Kind = k })

                ticks, counter + int64 kinds.Length

            | Defer(delay, kind) ->
                let offset = BalanceConfig.TickDelay.delayFrom delay

                [ { SubTick = tick.SubTick + offset
                    Priority = TickPriority.Duel
                    SequenceId = counter
                    Kind = kind } ],
                counter + 1L

            | EndChain ->
                let offset = BalanceConfig.TickDelay.delayFrom BalanceConfig.duelNextDelay

                [ { SubTick = tick.SubTick + offset
                    Priority = TickPriority.Duel
                    SequenceId = counter
                    Kind = DuelTick 0 } ],
                counter + 1L

            | StopPlay _ -> [], counter

    let private isChainReset =
        function
        | DuelTick 0
        | FreeKickTick _
        | CornerTick _
        | ThrowInTick _
        | PenaltyTick _
        | GoalKickTick
        | KickOffTick
        | ResumePlayTick
        | HalfTimeTick
        | InjuryTick _
        | SubstitutionTick _
        | ManagerReactionTick _ -> true
        | _ -> false

    let private isChainLink =
        function
        | DuelTick _
        | DecisionTick _
        | PlayerActionTick _ -> true
        | _ -> false

    let runLoopFast (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : SimState * MatchEvent list =
        let scheduler = TickScheduler(fullTime clock)
        generateInitialTicks ctx clock |> List.iter scheduler.Insert

        let mutable initialState =
            { Context = ctx
              State = state
              Events = ResizeArray()
              PlayState = LivePlay
              MatchEndScheduled = false
              SequenceCounter = scheduler.Count |> int64 }

        let firstDuelInterval =
            normalInt
                (float BalanceConfig.duelNextDelay.MeanST)
                (float BalanceConfig.duelNextDelay.StdST)
                BalanceConfig.duelNextDelay.MinST
                BalanceConfig.duelNextDelay.MaxST

        scheduler.Insert
            { SubTick = firstDuelInterval
              Priority = TickPriority.Duel
              SequenceId = -1L
              Kind = DuelTick 0 }

        let mutable running = true
        let mutable totalChainDepth = 0

        while running do
            match scheduler.Dequeue() with
            | ValueNone -> running <- false
            | ValueSome tick ->
                initialState.State.SubTick <- tick.SubTick

                match tick.Kind with
                | FullTimeTick ->
                    initialState.State.SubTick <- tick.SubTick
                    running <- false
                | _ ->
                    let shouldProcess =
                        match initialState.PlayState, tick.Kind with
                        | (Stopped _ | PlayState.SetPiece _), DuelTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), DecisionTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), PlayerActionTick _ -> false
                        | _ -> true

                    if shouldProcess then

                        if isChainReset tick.Kind then
                            totalChainDepth <- 0
                        elif isChainLink tick.Kind then
                            totalChainDepth <- totalChainDepth + 1

                        let result = runTick tick initialState.Context initialState.State clock

                        match tick.Kind with
                        | PhysicsTick ->
                            if tick.SubTick % clock.SteeringRate = 0 then
                                let dtPlayer = SimulationClock.dtPlayer clock

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    initialState.Context
                                    initialState.State
                                    HomeClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    initialState.Context
                                    initialState.State
                                    AwayClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate
                        | _ -> ()

                        let continuationTicks, counterAfterCont =
                            resolveContinuation
                                tick
                                result.Continuation
                                result.Transition
                                initialState.SequenceCounter
                                clock
                                initialState.PlayState
                                totalChainDepth

                        let sideEffectTicks, newCounter =
                            result.SideEffects
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) counterAfterCont

                        continuationTicks |> List.iter scheduler.Insert
                        sideEffectTicks |> List.iter scheduler.Insert

                        match result.Transition with
                        | Some(Stopped _ | PlayState.SetPiece _) ->
                            scheduler.CancelTicks (function
                                | DuelTick _
                                | DecisionTick _
                                | PlayerActionTick _ -> true
                                | _ -> false)
                        | _ -> ()

                        initialState <-
                            { initialState with
                                PlayState = result.Transition |> Option.defaultValue initialState.PlayState
                                SequenceCounter = newCounter }

                        result.Events |> List.iter initialState.Events.Add

        initialState.State, initialState.Events |> Seq.toList

    let runLoopFull (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchReplay =
        let scheduler = TickScheduler(fullTime clock)
        generateInitialTicks ctx clock |> List.iter scheduler.Insert
        let snapshots = System.Collections.Generic.List<SimSnapshot>()
        let snapshotInterval = clock.SubTicksPerSecond

        let mutable initialState =
            { Context = ctx
              State = state
              Events = ResizeArray()
              PlayState = LivePlay
              MatchEndScheduled = false
              SequenceCounter = scheduler.Count |> int64 }

        let firstDuelInterval =
            normalInt
                (float BalanceConfig.duelNextDelay.MeanST)
                (float BalanceConfig.duelNextDelay.StdST)
                BalanceConfig.duelNextDelay.MinST
                BalanceConfig.duelNextDelay.MaxST

        scheduler.Insert
            { SubTick = firstDuelInterval
              Priority = TickPriority.Duel
              SequenceId = -1L
              Kind = DuelTick 0 }

        let mutable lastSnapshotAt = 0
        let mutable running = true
        let mutable totalChainDepth = 0

        while running do
            match scheduler.Dequeue() with
            | ValueNone -> running <- false
            | ValueSome tick ->
                initialState.State.SubTick <- tick.SubTick

                // Snapshot check ALWAYS runs
                if tick.SubTick >= lastSnapshotAt + snapshotInterval then
                    snapshots.Add(SnapshotBuilder.take initialState.State)
                    lastSnapshotAt <- tick.SubTick + snapshotInterval

                match tick.Kind with
                | FullTimeTick ->
                    initialState.State.SubTick <- tick.SubTick
                    running <- false
                | _ ->
                    let shouldProcess =
                        match initialState.PlayState, tick.Kind with
                        | (Stopped _ | PlayState.SetPiece _), DuelTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), DecisionTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), PlayerActionTick _ -> false
                        | _ -> true

                    if shouldProcess then
                        // Bug 12: Track total chain depth per subtick to prevent SideEffects accumulation
                        if isChainReset tick.Kind then
                            totalChainDepth <- 0
                        elif isChainLink tick.Kind then
                            totalChainDepth <- totalChainDepth + 1

                        let result = runTick tick initialState.Context initialState.State clock

                        match tick.Kind with
                        | PhysicsTick ->
                            if tick.SubTick % clock.SteeringRate = 0 then
                                let dtPlayer = SimulationClock.dtPlayer clock

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    initialState.Context
                                    initialState.State
                                    HomeClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    initialState.Context
                                    initialState.State
                                    AwayClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate
                        | _ -> ()

                        let continuationTicks, counterAfterCont =
                            resolveContinuation
                                tick
                                result.Continuation
                                result.Transition
                                initialState.SequenceCounter
                                clock
                                initialState.PlayState
                                totalChainDepth

                        let sideEffectTicks, newCounter =
                            result.SideEffects
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) counterAfterCont

                        continuationTicks |> List.iter scheduler.Insert
                        sideEffectTicks |> List.iter scheduler.Insert

                        // Bug 4: Cancel ticks on transition to stopped or set piece
                        match result.Transition with
                        | Some(Stopped _ | PlayState.SetPiece _) ->
                            scheduler.CancelTicks (function
                                | DuelTick _
                                | DecisionTick _
                                | PlayerActionTick _ -> true
                                | _ -> false)
                        | _ -> ()

                        initialState <-
                            { initialState with
                                PlayState = result.Transition |> Option.defaultValue initialState.PlayState
                                SequenceCounter = newCounter }

                        result.Events |> List.iter initialState.Events.Add

        { Context = ctx
          Final = initialState.State
          Events = initialState.Events |> Seq.toList
          Snapshots = snapshots.ToArray() }

    open FsToolkit.ErrorHandling

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    let private toCoords slotX slotY (clubSide: ClubSide) : float<meter> * float<meter> =
        match clubSide with
        | HomeClub -> (1.0 - slotY) * PitchLength, slotX * PitchWidth
        | AwayClub -> slotY * PitchLength, slotX * PitchWidth

    let private kickOffPosition (x: float) (y: float) (clubSide: ClubSide) : float<meter> * float<meter> =
        let tacticalX, tacticalY = toCoords x y clubSide

        let clampedX =
            match clubSide with
            | HomeClub -> PhysicsContract.clamp tacticalX 2.0<meter> (HalfwayLineX - 1.0<meter>)
            | AwayClub -> PhysicsContract.clamp tacticalX (HalfwayLineX + 1.0<meter>) (PitchLength - 2.0<meter>)

        clampedX, tacticalY

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (clubSide: ClubSide)
        : Result<(Player * float<meter> * float<meter>)[] * TeamTactics * TacticalInstructions option, SimulationError> =
        match headCoach.Attributes.Coaching.Lineup with
        | None -> Error(MissingLineup club.Name)
        | Some lu ->
            let slots =
                lu.Slots
                |> List.choose (fun s ->
                    s.PlayerId
                    |> Option.bind (fun pid ->
                        players
                        |> Map.tryFind pid
                        |> Option.map (fun p ->
                            let x, y = toCoords s.X s.Y clubSide
                            p, x, y)))
                |> Array.ofList

            Ok(slots, lu.Tactics, lu.Instructions)

    let private validateLineups (home: Club) homeData (away: Club) awayData : Result<unit, SimulationError> =
        if home.Id = away.Id then
            Error(SameClub home.Name)
        elif homeData |> Array.length <> 11 then
            Error(IncompleteLineup(home.Name, homeData |> Array.length))
        elif awayData |> Array.length <> 11 then
            Error(IncompleteLineup(away.Name, awayData |> Array.length))
        else
            Ok()

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float<meter> * float<meter>>) : Spatial[] =
        players
        |> Array.map (fun p ->
            match Map.tryFind p.Id posMap with
            | Some(x, y) -> defaultSpatial x y
            | None -> failwithf "Position missing for player %s (%A)" p.Name p.Id)

    let private initSimState
        home
        homeCoach
        hp
        (hPosMap: Map<PlayerId, float<meter> * float<meter>>)
        homeTactics
        homeInstructions
        away
        awayCoach
        ap
        (aPosMap: Map<PlayerId, float<meter> * float<meter>>)
        awayTactics
        awayInstructions
        isKnockout
        profileMap
        =

        let hPos =
            hp
            |> Array.mapi (fun i (p: Player) ->
                let tx, ty =
                    hPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let prof = profileMap |> Map.tryFind p.Id |> Option.defaultValue (Player.profile p)

                let kx =
                    match p.Position with
                    | ST
                    | AMC ->
                        let allowedDepth = (float prof.AttackingDepth) * 5.0<meter>
                        let threshold = HalfwayLineX - 5.0<meter> + allowedDepth

                        if tx > threshold then
                            HalfwayLineX - 1.0<meter> + allowedDepth * 0.5
                        else
                            PhysicsContract.clamp tx 2.0<meter> (HalfwayLineX - 2.0<meter>)
                    | _ -> PhysicsContract.clamp tx 2.0<meter> (HalfwayLineX - 2.0<meter>)

                defaultSpatial kx ty)

        let aPos =
            ap
            |> Array.map (fun (p: Player) ->
                let tx, ty =
                    aPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let kx =
                    PhysicsContract.clamp tx (HalfwayLineX + 4.0<meter>) (PitchLength - 2.0<meter>)

                defaultSpatial kx ty)

        let defaultInstr = TacticalInstructions.defaultInstructions
        let hCount = hp |> Array.length
        let aCount = ap |> Array.length

        let ctx =
            { Home = home
              Away = away
              HomeCoach = homeCoach
              AwayCoach = awayCoach
              HomePlayers = hp
              AwayPlayers = ap
              HomeBasePositions = hPos
              AwayBasePositions = aPos
              HomeChemistry = ChemistryGraph.init hCount
              AwayChemistry = ChemistryGraph.init aCount
              IsKnockoutMatch = isKnockout }

        let state = SimState()
        state.SubTick <- 0
        state.HomeScore <- 0
        state.AwayScore <- 0

        state.Ball <-
            { defaultBall with
                Possession = Possession.SetPiece(HomeClub, SetPieceKind.KickOff) }

        state.Momentum <- 0.0
        state.HomeBasePositions <- ctx.HomeBasePositions
        state.AwayBasePositions <- ctx.AwayBasePositions
        state.HomeAttackDir <- LeftToRight

        state.Home <-
            { TeamSimState.empty with
                Slots =
                    Array.init hCount (fun i ->
                        let p = hp[i]
                        let prof = profileMap |> Map.tryFind p.Id |> Option.defaultValue (Player.profile p)

                        PlayerSlot.Active
                            { Player = p
                              Pos = hPos[i]
                              Condition = p.Condition
                              Mental = MentalState.initial p
                              Directives = Array.empty
                              Profile = prof
                              CachedTarget = (hPos[i].X, hPos[i].Y)
                              CachedExecution = 1.0 })
                Tactics = homeTactics
                Instructions = homeInstructions |> Option.orElse (Some defaultInstr) }

        state.Away <-
            { TeamSimState.empty with
                Slots =
                    Array.init aCount (fun i ->
                        let p = ap[i]
                        let prof = profileMap |> Map.tryFind p.Id |> Option.defaultValue (Player.profile p)

                        PlayerSlot.Active
                            { Player = p
                              Pos = aPos[i]
                              Condition = p.Condition
                              Mental = MentalState.initial p
                              Directives = Array.empty
                              Profile = prof
                              CachedTarget = (aPos[i].X, aPos[i].Y)
                              CachedExecution = 1.0 })
                Tactics = awayTactics
                Instructions = awayInstructions |> Option.orElse (Some defaultInstr) }

        ctx, state

    let private simulatePenaltyShootout
        (ctx: MatchContext)
        (state: SimState)
        (players: Map<PlayerId, Player>)
        (clock: SimulationClock)
        =
        result {
            let homePlayers =
                ctx.Home.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let awayPlayers =
                ctx.Away.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let rec simulateKicks homeKicks awayKicks kickNum =
                let homeKicker = homePlayers |> List.item ((kickNum - 1) % homePlayers.Length)
                let awayKicker = awayPlayers |> List.item ((kickNum - 1) % awayPlayers.Length)

                let prevHomeScore = state.HomeScore
                resolvePenalty ctx state homeKicker HomeClub kickNum clock |> ignore
                let homeScored = state.HomeScore > prevHomeScore

                let prevAwayScore = state.AwayScore
                resolvePenalty ctx state awayKicker AwayClub kickNum clock |> ignore
                let awayScored = state.AwayScore > prevAwayScore

                let newHomeKicks = (homeKicker.Id, homeScored, kickNum) :: homeKicks
                let newAwayKicks = (awayKicker.Id, awayScored, kickNum) :: awayKicks
                let homeGoals = newHomeKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)
                let awayGoals = newAwayKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)

                if kickNum > 5 && homeGoals <> awayGoals then
                    { HomeKicks = List.rev newHomeKicks
                      AwayKicks = List.rev newAwayKicks
                      CurrentKick = kickNum
                      IsComplete = true }
                elif kickNum > 5 then
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                elif kickNum <= 5 then
                    let remaining = 5 - kickNum

                    if homeGoals > awayGoals + remaining || awayGoals > homeGoals + remaining then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    elif kickNum = 5 then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    else
                        simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                else
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)

            let shootout = simulateKicks [] [] 1

            let baseSubTick = fullTime clock

            let events =
                [ for pid, scored, k in shootout.HomeKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = ctx.Home.Id
                        Type = PenaltyAwarded scored }
                  for pid, scored, k in shootout.AwayKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = ctx.Away.Id
                        Type = PenaltyAwarded scored } ]

            return
                { Context = ctx
                  Final = state
                  Events = events
                  Snapshots = [||] }
        }

    let setup
        home
        away
        players
        staff
        isKnockout
        profileMap
        : Result<MatchContext * SimState * Player list * Player list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players HomeClub
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players AwayClub
            do! validateLineups home homeData away awayData

            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)

            let hPosMap = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
            let aPosMap = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray

            let ctx, state =
                initSimState
                    home
                    homeCoach
                    hp
                    hPosMap
                    homeTactics
                    homeInstructions
                    away
                    awayCoach
                    ap
                    aPosMap
                    awayTactics
                    awayInstructions
                    isKnockout
                    profileMap

            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            return ctx, state, homeSquad, awaySquad
        }

    let trySimulateMatch
        home
        away
        players
        staff
        profileMap
        : Result<int * int * MatchEvent list * SimState, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff false profileMap
            let final, events = runLoopFast ctx state defaultClock
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff profileMap : Result<MatchReplay, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff false profileMap
            return runLoopFull ctx state defaultClock
        }

    let trySimulateMatchKnockout home away players staff profileMap : Result<MatchReplay * bool, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff true profileMap
            let replay = runLoopFull ctx state defaultClock

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootout = simulatePenaltyShootout ctx replay.Final players defaultClock
                return shootout, true
            else
                return replay, false
        }

    let buildInitState home away players staff profileMap =
        match setup home away players staff false profileMap with
        | Ok(ctx, state, homeSquad, awaySquad) -> Some(ctx, state, homeSquad, awaySquad)
        | Error _ -> None
