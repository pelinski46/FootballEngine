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
          yield
              { SubTick = sub 12.0
                Priority = TickPriority.Manager
                SequenceId = 1L
                Kind = CognitiveTick }
          yield
              { SubTick = sub 60.0
                Priority = TickPriority.Manager
                SequenceId = 2L
                Kind = AdaptiveTick }
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

    let private spawnCognitiveTick (tick: ScheduledTick) (clock: SimulationClock) =
        { SubTick = tick.SubTick + clock.CognitiveRate
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = CognitiveTick }

    let private spawnAdaptiveTick (tick: ScheduledTick) (clock: SimulationClock) =
        { SubTick = tick.SubTick + clock.AdaptiveRate
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = AdaptiveTick }

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
            | PlayerActionTick _
            | CognitiveTick
            | AdaptiveTick
            | PossessionChangeTick _ -> PlayerAgent.agent tick ctx state clock
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

    let runLoopDes (ctx: MatchContext) (state: SimState) (saveSnapshots: bool) (clock: SimulationClock) =
        let scheduler = TickScheduler(fullTime clock)
        generateInitialTicks ctx clock |> List.iter scheduler.Insert

        let snapshots =
            if saveSnapshots then
                Some(System.Collections.Generic.List<SimSnapshot>())
            else
                None

        let initialState =
            { Context = ctx
              State = state
              Events = ResizeArray()
              PlayState = SetPiece KickOff
              Snapshots = snapshots
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

        let snapshotInterval = clock.SubTicksPerSecond


        let rec loop (ls: LoopState) (scheduler: TickScheduler) lastSnapshotAt clock =
            match scheduler.Dequeue() with
            | ValueNone -> ls.State, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

            | ValueSome tick ->
                ls.State.SubTick <- tick.SubTick

                match tick.Kind with
                | FullTimeTick ->
                    ls.State.SubTick <- tick.SubTick
                    ls.State, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

                | _ ->
                    let shouldProcess =
                        match ls.PlayState, tick.Kind with
                        | (Stopped _ | PlayState.SetPiece _), DuelTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), DecisionTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), PlayerActionTick _ -> false
                        | _ -> true

                    if not shouldProcess then
                        loop ls scheduler lastSnapshotAt clock
                    else
                        let result = runTick tick ls.Context ls.State clock

                        let autoSpawned =
                            match tick.Kind with
                            | CognitiveTick -> [ spawnCognitiveTick tick clock ]
                            | AdaptiveTick -> [ spawnAdaptiveTick tick clock ]
                            | RefereeTick ->
                                [ { tick with
                                      Kind = RefereeTick
                                      SubTick = tick.SubTick + clock.PhysicsRate } ]
                            | _ -> []

                        match tick.Kind with
                        | PhysicsTick ->
                            if tick.SubTick % clock.SteeringRate = 0 then
                                let dtPlayer = SimulationClock.dtPlayer clock

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    ls.Context
                                    ls.State
                                    HomeClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate

                                MovementEngine.updateTeamSide
                                    tick.SubTick
                                    ls.Context
                                    ls.State
                                    AwayClub
                                    dtPlayer
                                    clock.SteeringRate
                                    clock.CognitiveRate

                        | CognitiveTick ->
                            MovementEngine.updateCognitive
                                tick.SubTick
                                ls.Context
                                ls.State
                                HomeClub
                                ls.Events
                                clock.CognitiveRate
                                clock

                            MovementEngine.updateCognitive
                                tick.SubTick
                                ls.Context
                                ls.State
                                AwayClub
                                ls.Events
                                clock.CognitiveRate
                                clock

                        | AdaptiveTick ->
                            MovementEngine.updateAdaptive
                                tick.SubTick
                                ls.Context
                                ls.State
                                HomeClub
                                ls.Events
                                clock.AdaptiveRate
                                clock

                            MovementEngine.updateAdaptive
                                tick.SubTick
                                ls.Context
                                ls.State
                                AwayClub
                                ls.Events
                                clock.AdaptiveRate
                                clock

                        | RefereeTick -> ()

                        | _ -> ()

                        let newLastSnapshotAt =
                            match ls.Snapshots with
                            | Some snaps when tick.SubTick >= lastSnapshotAt + snapshotInterval ->
                                snaps.Add
                                    { SubTick = ls.State.SubTick
                                      HomePositions =
                                        ls.State.Home.Slots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Pos
                                            | Sidelined _ -> kickOffSpatial)
                                      AwayPositions =
                                        ls.State.Away.Slots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Pos
                                            | Sidelined _ -> kickOffSpatial)
                                      BallX = ls.State.Ball.Position.X
                                      BallY = ls.State.Ball.Position.Y
                                      BallVx = ls.State.Ball.Position.Vx
                                      BallVy = ls.State.Ball.Position.Vy
                                      BallZ = ls.State.Ball.Position.Z
                                      BallVz = ls.State.Ball.Position.Vz
                                      BallSpinTop = ls.State.Ball.Spin.Top
                                      BallSpinSide = ls.State.Ball.Spin.Side
                                      Possession = ls.State.Ball.Possession
                                      HomeScore = ls.State.HomeScore
                                      AwayScore = ls.State.AwayScore
                                      HomeConditions =
                                        ls.State.Home.Slots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Condition
                                            | Sidelined _ -> 0)
                                      AwayConditions =
                                        ls.State.Away.Slots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Condition
                                            | Sidelined _ -> 0)
                                      HomeSidelined = ls.State.Home.Sidelined
                                      AwaySidelined = ls.State.Away.Sidelined
                                      Momentum = ls.State.Momentum }

                                tick.SubTick + snapshotInterval
                            | _ -> lastSnapshotAt

                        let allSpawned =
                            match autoSpawned with
                            | [] -> result.Spawned
                            | _ -> autoSpawned @ result.Spawned

                        let stampedTicks, newCounter =
                            allSpawned
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) ls.SequenceCounter

                        stampedTicks |> List.iter scheduler.Insert

                        let newPlayState = result.Transition |> Option.defaultValue ls.PlayState

                        result.Events |> List.iter ls.Events.Add

                        loop
                            { ls with
                                PlayState = newPlayState
                                SequenceCounter = newCounter }
                            scheduler
                            newLastSnapshotAt
                            clock

        loop initialState scheduler 0 clock

    let runLoopFastDes ctx state =
        let s, evs, _ = runLoopDes ctx state false defaultClock
        s, evs

    let private runLoopFullDes ctx state =
        match runLoopDes ctx state true defaultClock with
        | s, evs, Some snaps -> s, evs, snaps
        | s, evs, None -> s, evs, [||]

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
            let x, y =
                posMap
                |> Map.tryFind p.Id
                |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

            defaultSpatial x y)

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
                              Profile = prof })
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
                              Profile = prof })
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
            let final, events = runLoopFastDes ctx state
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff profileMap : Result<MatchReplay, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff false profileMap
            let final, events, snapshots = runLoopFullDes ctx state

            return
                { Context = ctx
                  Final = final
                  Events = events
                  Snapshots = snapshots }
        }

    let trySimulateMatchKnockout home away players staff profileMap : Result<MatchReplay * bool, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff true profileMap
            let final, events, snapshots = runLoopFullDes ctx state

            let replay =
                { Context = ctx
                  Final = final
                  Events = events
                  Snapshots = snapshots }

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
