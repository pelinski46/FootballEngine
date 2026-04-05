namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchStateOps
open FootballEngine.Movement
open FootballEngine.PhysicsContract
open FootballEngine.Stats
open SchedulingTypes

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    // Phase 0: all times in SubTicks
    let private generateInitialTicks (init: MatchState) : ScheduledTick list =
        let sub min = secondsToSubTicks (float min * 60.0)

        let subs =
            [ for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub min
                    Priority = TickPriority.Manager
                    SequenceId = int64 idx
                    Kind = SubstitutionTick init.Home.Id }
              for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub min
                    Priority = TickPriority.Manager
                    SequenceId = int64 (idx + 3)
                    Kind = SubstitutionTick init.Away.Id } ]

        [ yield
              { SubTick = secondsToSubTicks 30.0
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick }
          yield
              { SubTick = secondsToSubTicks 12.0
                Priority = TickPriority.Manager
                SequenceId = 1L
                Kind = CognitiveTick }
          yield
              { SubTick = secondsToSubTicks 60.0
                Priority = TickPriority.Manager
                SequenceId = 2L
                Kind = AdaptiveTick }
          yield! subs
          yield
              { SubTick = HalfTimeSubTick
                Priority = TickPriority.MatchControl
                SequenceId = 10L
                Kind = HalfTimeTick }
          yield
              { SubTick = FullTimeSubTick
                Priority = TickPriority.MatchControl
                SequenceId = 11L
                Kind = FullTimeTick } ]

    let private spawnCognitiveTick (tick: ScheduledTick) =
        { SubTick = tick.SubTick + PhysicsContract.CognitiveIntervalSubTicks
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = CognitiveTick }

    let private spawnAdaptiveTick (tick: ScheduledTick) =
        { SubTick = tick.SubTick + PhysicsContract.AdaptiveIntervalSubTicks
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = AdaptiveTick }

    let private runTick homeId homeSquad awaySquad (tick: ScheduledTick) (s: MatchState) : AgentOutput =
        let dispatchAgent (tick: ScheduledTick) : Agent =
            match tick.Kind with
            | PhysicsTick -> BallAgent.agent
            | DuelTick _
            | PlayerActionTick _
            | CognitiveTick
            | AdaptiveTick -> PlayerAgent.agent
            | FreeKickTick _
            | CornerTick _
            | ThrowInTick _
            | PenaltyTick _
            | GoalKickTick
            | KickOffTick -> SetPieceAgent.agent
            | InjuryTick _
            | ResumePlayTick
            | HalfTimeTick
            | FullTimeTick
            | MatchEndTick
            | ExtraTimeTick _ -> RefereeAgent.agent
            | SubstitutionTick _
            | ManagerReactionTick _ -> ManagerAgent.agent

        (dispatchAgent tick) homeId homeSquad awaySquad tick s

    // -------------------------------------------------------------------------
    // Phase 1: main loop — ball @ 40Hz, players @ 10Hz, cognitive @ 1Hz
    // Phase 4: fatigue applied on CognitiveTick via FatiguePipeline
    // Phase 5: JIT — all action resolutions use current positions at dispatch
    // -------------------------------------------------------------------------

    let runLoopDes homeId homeSquad awaySquad (init: MatchState) (saveSnapshots: bool) =
        let scheduler = TickScheduler(PhysicsContract.MaxMatchSubTicks)
        generateInitialTicks init |> List.iter scheduler.Insert

        let snapshots =
            if saveSnapshots then
                Some(System.Collections.Generic.List<MatchState>())
            else
                None

        let initialState =
            { MatchState = init
              Events = ResizeArray()
              PlayState = LivePlay
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

        // Phase 1: snapshot every 40 SubTicks = 1 real second
        let snapshotInterval = PhysicsContract.SubTicksPerSecond
        // Phase 1: player dt = 4 SubTicks × 0.025s = 0.1s
        let dtPlayer = PhysicsContract.Dt * 4.0

        let rec loop (ls: LoopState) (scheduler: TickScheduler) lastSnapshotAt =
            match scheduler.Dequeue() with
            | ValueNone -> ls.MatchState, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

            | ValueSome tick ->
                match tick.Kind with
                | FullTimeTick ->
                    let finalState =
                        { ls.MatchState with
                            SubTick = tick.SubTick }

                    finalState, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

                | _ ->
                    let shouldProcess =
                        match ls.PlayState, tick.Kind with
                        | (Stopped _ | PlayState.SetPiece _), DuelTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), PlayerActionTick _ -> false
                        | _ -> true // PhysicsTick always runs — ball moves during stoppages

                    if not shouldProcess then
                        loop ls scheduler lastSnapshotAt
                    else
                        let result = runTick homeId homeSquad awaySquad tick ls.MatchState

                        let autoSpawned =
                            match tick.Kind with
                            | CognitiveTick -> [ spawnCognitiveTick tick ]
                            | AdaptiveTick -> [ spawnAdaptiveTick tick ]
                            | _ -> []

                        let finalState =
                            match tick.Kind with
                            | PhysicsTick ->
                                // Phase 1: player movement gated to SteeringIntervalSubTicks
                                if tick.SubTick % PhysicsContract.SteeringIntervalSubTicks = 0 then
                                    result.State
                                    |> fun s -> MovementEngine.updateTeamSide tick.SubTick s HomeClub dtPlayer
                                    |> fun s -> MovementEngine.updateTeamSide tick.SubTick s AwayClub dtPlayer
                                else
                                    result.State

                            | CognitiveTick ->
                                // Phase 4: cognitive update — mental states + fatigue-weighted directives
                                result.State
                                |> fun s -> MovementEngine.updateCognitive tick.SubTick s HomeClub ls.Events
                                |> fun s -> MovementEngine.updateCognitive tick.SubTick s AwayClub ls.Events

                            | AdaptiveTick ->
                                result.State
                                |> fun s -> MovementEngine.updateAdaptive tick.SubTick s HomeClub ls.Events
                                |> fun s -> MovementEngine.updateAdaptive tick.SubTick s AwayClub ls.Events

                            | _ -> result.State

                        let newLastSnapshotAt =
                            match ls.Snapshots with
                            | Some snaps when tick.SubTick >= lastSnapshotAt + snapshotInterval ->
                                snaps.Add(finalState)
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
                                MatchState = finalState
                                PlayState = newPlayState
                                SequenceCounter = newCounter }
                            scheduler
                            newLastSnapshotAt

        loop initialState scheduler 0

    let runLoopFastDes homeId homeSquad awaySquad init =
        let s, evs, _ = runLoopDes homeId homeSquad awaySquad init false
        s, evs

    let private runLoopFullDes homeId homeSquad awaySquad init =
        match runLoopDes homeId homeSquad awaySquad init true with
        | s, evs, Some snaps -> s, evs, snaps
        | s, evs, None -> s, evs, [||]

    open FsToolkit.ErrorHandling

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    // Phase 2: toCoords maps formation slots → metric pitch coordinates
    let private toCoords slotX slotY (clubSide: ClubSide) =
        match clubSide with
        | HomeClub -> slotY * PhysicsContract.PitchLength, slotX * PhysicsContract.PitchWidth
        | AwayClub -> (1.0 - slotY) * PhysicsContract.PitchLength, slotX * PhysicsContract.PitchWidth

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (clubSide: ClubSide)
        : Result<(Player * float * float)[] * TeamTactics * TacticalInstructions option, SimulationError> =
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

    let private buildContext (homeData: (Player * float * float)[]) (awayData: (Player * float * float)[]) =
        { HomePositions = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
          AwayPositions = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray }

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float * float>) : Spatial[] =
        players
        |> Array.map (fun p ->
            let x, y =
                posMap
                |> Map.tryFind p.Id
                |> Option.defaultValue (PhysicsContract.HalfwayLineX, PhysicsContract.PitchWidth / 2.0)

            defaultSpatial x y)

    let private initState
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
        =

        let hPos = positionArrayOf hp hPosMap
        let aPos = positionArrayOf ap aPosMap
        let defaultInstr = TacticalInstructions.defaultInstructions
        let hCount = hp |> Array.length
        let aCount = ap |> Array.length

        { Home = home
          Away = away
          HomeCoach = homeCoach
          AwayCoach = awayCoach
          SubTick = 0 // Phase 0: was Second = 0
          HomeScore = 0
          AwayScore = 0
          Ball = defaultBall
          AttackingClub = HomeClub
          Momentum = 0.0
          HomeSide =
            { Players = hp
              Conditions = hp |> Array.map _.Condition
              Positions = hPos
              BasePositions = hPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = homeTactics
              Instructions = homeInstructions |> Option.orElse (Some defaultInstr) }
          AwaySide =
            { Players = ap
              Conditions = ap |> Array.map _.Condition
              Positions = aPos
              BasePositions = aPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = awayTactics
              Instructions = awayInstructions |> Option.orElse (Some defaultInstr) }
          PenaltyShootout = None
          IsExtraTime = false
          IsKnockoutMatch = isKnockout
          PendingOffsideSnapshot = None

          // Phase 0/4 — SSOT cognitive/movement state embedded in MatchState
          HomeMentalStates = Array.zeroCreate hCount
          HomeDirectives = Array.zeroCreate hCount
          HomeActiveRuns = []
          HomeChemistry = ChemistryGraph.init hCount
          HomeEmergentState = EmergentState.initial
          HomeAdaptiveState = AdaptiveTactics.initial
          HomeLastCognitiveSubTick = 0
          HomeLastShapeSubTick = 0
          HomeLastMarkingSubTick = 0
          HomeLastAdaptiveSubTick = 0

          AwayMentalStates = Array.zeroCreate aCount
          AwayDirectives = Array.zeroCreate aCount
          AwayActiveRuns = []
          AwayChemistry = ChemistryGraph.init aCount
          AwayEmergentState = EmergentState.initial
          AwayAdaptiveState = AdaptiveTactics.initial
          AwayLastCognitiveSubTick = 0
          AwayLastShapeSubTick = 0
          AwayLastMarkingSubTick = 0
          AwayLastAdaptiveSubTick = 0 }

    // Phase 0: shootout events use SubTick field
    let private simulatePenaltyShootout (s: MatchState) (home: Club) (away: Club) (players: Map<PlayerId, Player>) =
        result {
            let homePlayers =
                home.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let awayPlayers =
                away.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let rec simulateKicks homeKicks awayKicks kickNum =
                let homeKicker = homePlayers |> List.item ((kickNum - 1) % homePlayers.Length)
                let awayKicker = awayPlayers |> List.item ((kickNum - 1) % awayPlayers.Length)

                let homeAction = PlayerAction.Penalty(homeKicker, HomeClub, kickNum)

                let homeS, _ =
                    PlayerAgent.resolve home.Id (PhysicsContract.FullTimeSubTick + kickNum) homeAction s

                let awayAction = PlayerAction.Penalty(awayKicker, AwayClub, kickNum)

                let awayS, _ =
                    PlayerAgent.resolve home.Id (PhysicsContract.FullTimeSubTick + kickNum) awayAction homeS

                let homeScored = homeS.HomeScore > s.HomeScore
                let awayScored = awayS.AwayScore > homeS.AwayScore

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

            // Phase 0: SubTick field (was Second)
            let baseSubTick = PhysicsContract.secondsToSubTicks (95.0 * 60.0)

            let events =
                [ for pid, scored, k in shootout.HomeKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = home.Id
                        Type = PenaltyAwarded scored }
                  for pid, scored, k in shootout.AwayKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = away.Id
                        Type = PenaltyAwarded scored } ]

            return
                { Final =
                    { s with
                        PenaltyShootout = Some shootout }
                  Events = events
                  Snapshots = [||] }
        }

    let setup home away players staff isKnockout : Result<MatchState * Player list * Player list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players HomeClub
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players AwayClub
            do! validateLineups home homeData away awayData

            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)

            let init =
                initState
                    home
                    homeCoach
                    hp
                    ctx.HomePositions
                    homeTactics
                    homeInstructions
                    away
                    awayCoach
                    ap
                    ctx.AwayPositions
                    awayTactics
                    awayInstructions
                    isKnockout

            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            return init, homeSquad, awaySquad
        }

    let trySimulateMatch home away players staff : Result<int * int * MatchEvent list * MatchState, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events = runLoopFastDes home.Id homeSquad awaySquad init
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff : Result<MatchReplay, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events, snapshots = runLoopFullDes home.Id homeSquad awaySquad init

            return
                { Final = final
                  Events = events
                  Snapshots = snapshots }
        }

    let trySimulateMatchKnockout home away players staff : Result<MatchReplay * bool, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff true
            let final, events, snapshots = runLoopFullDes home.Id homeSquad awaySquad init

            let replay =
                { Final = final
                  Events = events
                  Snapshots = snapshots }

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootout = simulatePenaltyShootout replay.Final home away players
                return shootout, true
            else
                return replay, false
        }

    let buildInitState home away players staff =
        match setup home away players staff false with
        | Ok(init, homeSquad, awaySquad) -> Some(init, homeSquad, awaySquad)
        | Error _ -> None
