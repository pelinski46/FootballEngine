namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchStateOps
open FootballEngine.Stats
open SchedulingTypes

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private generateInitialTicks (init: MatchState) : ScheduledTick list =
        let subs =
            [ for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { Second = min * 60
                    Priority = TickPriority.Manager
                    SequenceId = int64 idx
                    Kind = SubstitutionTick init.Home.Id }
              for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { Second = min * 60
                    Priority = TickPriority.Manager
                    SequenceId = int64 (idx + 3)
                    Kind = SubstitutionTick init.Away.Id } ]

        [ yield
              { Second = 30
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick }
          yield! subs
          yield
              { Second = 45 * 60
                Priority = TickPriority.MatchControl
                SequenceId = 10L
                Kind = HalfTimeTick }
          yield
              { Second = 95 * 60
                Priority = TickPriority.MatchControl
                SequenceId = 11L
                Kind = FullTimeTick } ]

    let private runTick
        (homeId: ClubId)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (tick: ScheduledTick)
        (s: MatchState)
        : AgentOutput =

        let dispatchAgent (tick: ScheduledTick) : Agent =
            match tick.Kind with
            | PhysicsTick -> BallAgent.agent
            | DuelTick _
            | PlayerActionTick _ -> PlayerAgent.agent
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

    let private runLoopDes
        (homeId: ClubId)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (init: MatchState)
        (saveSnapshots: bool)
        =

        let scheduler = TickScheduler(95 * 60)
        generateInitialTicks init |> List.iter scheduler.Insert

        let snapshots =
            if saveSnapshots then
                Some(System.Collections.Generic.List<MatchState>())
            else
                None

        let initialState =
            { MatchState = init
              ReversedEvents = []
              PlayState = LivePlay
              Snapshots = snapshots
              MatchEndScheduled = false
              SequenceCounter = scheduler.Count |> int64 }

        let firstDuelInterval = normalInt 25.0 5.0 15 35

        scheduler.Insert
            { Second = firstDuelInterval
              Priority = TickPriority.Duel
              SequenceId = -1L
              Kind = DuelTick 0 }

        let snapshotInterval = 5

        let rec loop ls (scheduler: TickScheduler) lastSnapshotAt =
            match scheduler.Dequeue() with
            | ValueNone -> ls.MatchState, List.rev ls.ReversedEvents, ls.Snapshots |> Option.map _.ToArray()

            | ValueSome tick ->

                match tick.Kind with
                | FullTimeTick ->
                    let finalState =
                        { ls.MatchState with
                            Second = tick.Second }

                    finalState, List.rev ls.ReversedEvents, ls.Snapshots |> Option.map _.ToArray()

                | _ ->
                    let shouldProcess =
                        match ls.PlayState, tick.Kind with
                        | (Stopped _ | SetPiece _), DuelTick _ -> false
                        | (Stopped _ | SetPiece _), PlayerActionTick _ -> false
                        | (Stopped _ | SetPiece _), PhysicsTick -> false
                        | _ -> true

                    if not shouldProcess then
                        loop ls scheduler lastSnapshotAt
                    else
                        let result = runTick homeId homeSquad awaySquad tick ls.MatchState

                        let newLastSnapshotAt =
                            match ls.Snapshots with
                            | Some snaps when tick.Second >= lastSnapshotAt + snapshotInterval ->
                                snaps.Add(result.State)
                                tick.Second
                            | _ -> lastSnapshotAt

                        let stampedTicks, newCounter =
                            result.Spawned
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) ls.SequenceCounter

                        stampedTicks |> List.iter scheduler.Insert

                        let newPlayState = result.Transition |> Option.defaultValue ls.PlayState

                        loop
                            { ls with
                                MatchState = result.State
                                ReversedEvents = result.Events @ ls.ReversedEvents
                                PlayState = newPlayState
                                SequenceCounter = newCounter }
                            scheduler
                            newLastSnapshotAt

        loop initialState scheduler 0

    let private runLoopFastDes homeId homeSquad awaySquad init =
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

    let private toCoords slotX slotY (clubSide: ClubSide) =
        match clubSide with
        | HomeClub -> slotY * 100.0, slotX * 100.0
        | AwayClub -> (1.0 - slotY) * 100.0, slotX * 100.0

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
            let x, y = posMap |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)
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
        let defaultInstructions = TacticalInstructions.defaultInstructions

        { Home = home
          Away = away
          HomeCoach = homeCoach
          AwayCoach = awayCoach
          Second = 0
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
              Instructions = homeInstructions |> Option.orElse (Some defaultInstructions) }
          AwaySide =
            { Players = ap
              Conditions = ap |> Array.map _.Condition
              Positions = aPos
              BasePositions = aPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = awayTactics
              Instructions = awayInstructions |> Option.orElse (Some defaultInstructions) }
          PenaltyShootout = None
          IsExtraTime = false
          IsKnockoutMatch = isKnockout
          PendingOffsideSnapshot = None }

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
                let homeS, _ = PlayerAgent.resolve home.Id (95 * 60 + kickNum) homeAction s

                let awayAction = PlayerAction.Penalty(awayKicker, AwayClub, kickNum)
                let awayS, _ = PlayerAgent.resolve home.Id (95 * 60 + kickNum) awayAction homeS

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

            let events =
                [ for pid, scored, k in shootout.HomeKicks ->
                      { Second = 95 * 60 + k
                        PlayerId = pid
                        ClubId = home.Id
                        Type = PenaltyAwarded scored }
                  for pid, scored, k in shootout.AwayKicks ->
                      { Second = 95 * 60 + k
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

    let private setup
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (isKnockout: bool)
        : Result<MatchState * Player list * Player list, SimulationError> =
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

    let trySimulateMatch
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<int * int * MatchEvent list * MatchState, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events = runLoopFastDes home.Id homeSquad awaySquad init
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events, snapshots = runLoopFullDes home.Id homeSquad awaySquad init

            return
                { Final = final
                  Events = events
                  Snapshots = snapshots }
        }

    let trySimulateMatchKnockout
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay * bool, SimulationError> =
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

   
