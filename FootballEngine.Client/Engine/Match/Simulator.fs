namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.MatchPlayerMovement
open FootballEngine.MatchPlayerDecision
open FootballEngine.MatchPlayerAction
open FootballEngine.Stats
open MatchState

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private fatigue
        (p: Player)
        (pressing: bool)
        (tactics: TeamTactics)
        (instructions: TacticalInstructions option)
        =
        let config = tacticsConfig tactics instructions
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15

        int (
            float (base' + workRate)
            * (if pressing then 1.5 else 1.0)
            * config.PressingIntensity
        )

    let private applyFatigue (s: MatchState) : MatchState =
        let ballX = s.Ball.Position.X

        let drain (ts: TeamSide) (pressing: bool) (fatigueMultiplier: float) =
            { ts with
                Conditions =
                    Array.map2
                        (fun c p ->
                            Math.Max(
                                0,
                                c
                                - int (float (fatigue p pressing ts.Tactics ts.Instructions) * fatigueMultiplier)
                            ))
                        ts.Conditions
                        ts.Players }

        s
        |> withSide true (drain (homeSide s) (ballX > 70.0) (1.0 - BalanceConfig.HomeFatigueReduction))
        |> withSide false (drain (awaySide s) (ballX < 30.0) 1.0)

    open SchedulingTypes

    let private physicsDt = 0.5

    let private generateInitialTicks (init: MatchState) : ScheduledTick list =
        let rec duelTicks acc t seqId =
            if t >= 95 * 60 then
                List.rev acc, seqId
            else
                let interval = normalInt 25.0 5.0 15 35
                let t' = t + interval

                if t' < 95 * 60 then
                    let variation = normalInt 0.0 5.0 -8 8

                    let tick =
                        { Second = t' + variation
                          Priority = TickPriority.Duel
                          SequenceId = seqId
                          Kind = DuelTick 0 }

                    duelTicks (tick :: acc) t' (seqId + 1L)
                else
                    List.rev acc, seqId

        let duel, seqAfterDuel = duelTicks [] 0 0L

        let physics =
            [ for sec in 30..30 .. (95 * 60) ->
                  { Second = sec
                    Priority = TickPriority.Physics
                    SequenceId = seqAfterDuel + int64 (sec / 30)
                    Kind = PhysicsTick } ]

        let seqAfterPhysics = seqAfterDuel + int64 (95 * 60 / 30 + 1)

        let subs =
            [ for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { Second = min * 60
                    Priority = TickPriority.Manager
                    SequenceId = seqAfterPhysics + int64 idx
                    Kind = SubstitutionTick init.Home.Id } ]

        let seqAfterSubs = seqAfterPhysics + 3L

        [ { Second = 45 * 60
            Priority = TickPriority.MatchControl
            SequenceId = seqAfterSubs
            Kind = HalfTimeTick }
          { Second = 95 * 60
            Priority = TickPriority.MatchControl
            SequenceId = seqAfterSubs + 1L
            Kind = FullTimeTick } ]
        |> List.append subs
        |> List.append physics
        |> List.append duel

    let private runPlayerChain
        (homeId: ClubId)
        (second: int)
        (s: MatchState)
        : MatchState * MatchEvent list * Player option * Player option =

        let rec loop state events intent depth lastAtt lastDef =
            if depth >= BalanceConfig.AvgChainLength || intent = PlayerIdle then
                state, List.rev events, lastAtt, lastDef
            else
                let lastAtt' =
                    match intent with
                    | ResolveDuel(att, _) -> Some att
                    | ExecuteShot att -> Some att
                    | ExecutePass att -> Some att
                    | ExecuteDribble att -> Some att
                    | ExecuteCross att -> Some att
                    | ExecuteLongBall att -> Some att
                    | ExecuteFreeKick att -> Some att
                    | _ -> lastAtt

                let lastDef' =
                    match intent with
                    | ResolveDuel(_, def) -> Some def
                    | ExecuteTackle def -> Some def
                    | _ -> lastDef

                let newState, newEvents, nextIntent = resolve homeId second intent state
                loop newState (newEvents @ events) nextIntent (depth + 1) lastAtt' lastDef'

        loop s [] (decide s) 0 None None

    let private runRefereeStep second att def s =
        let intents = MatchReferee.decide second att def s

        let s', evs =
            intents
            |> List.fold
                (fun (accState, accEvents) intent ->
                    let s', evs = MatchReferee.resolve second intent accState
                    s', evs @ accEvents)
                (s, [])
            |> fun (s, evs) -> s, List.rev evs

        s', evs, intents

    let private runManagerStep second homeSquad awaySquad s =
        MatchManager.decide second homeSquad awaySquad s
        |> List.fold
            (fun (accState, accEvents) intent ->
                let s', evs = MatchManager.resolve second intent accState
                s', evs @ accEvents)
            (s, [])
        |> fun (s, evs) -> s, List.rev evs

    let private runTick
        (homeId: ClubId)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (allPlayers: Player list)
        (tick: ScheduledTick)
        (s: MatchState)
        : TickResult =

        let s = { s with Second = tick.Second }

        match tick.Kind with
        | MatchEndTick
        | FullTimeTick ->
            { State = s
              Events = []
              SpawnedTicks = []
              PlayStateTransition = None }

        | HalfTimeTick ->
            { State = s
              Events = []
              SpawnedTicks = []
              PlayStateTransition = None }

        | PhysicsTick ->
            let newState =
                s
                |> updatePlayerVelocities physicsDt
                |> Pitch.updatePositions physicsDt
                |> MatchBall.updatePhysics physicsDt
                |> applyFatigue

            { State = newState
              Events = []
              SpawnedTicks = []
              PlayStateTransition = None }

        | SubstitutionTick clubId ->
            let newState, newEvents = runManagerStep tick.Second homeSquad awaySquad s

            { State = newState
              Events = newEvents
              SpawnedTicks = []
              PlayStateTransition = None }

        | DuelTick chainDepth ->
            let s1 = s |> updatePlayerVelocities physicsDt
            let s2, playerEvents, att, def = runPlayerChain homeId tick.Second s1
            let s3 = s2 |> MatchBall.updatePhysics physicsDt |> Pitch.updatePositions physicsDt
            let s4, refereeEvents, refereeIntents = runRefereeStep tick.Second att def s3
            let s5, managerEvents = runManagerStep tick.Second homeSquad awaySquad s4
            let finalState = s5 |> applyFatigue
            let allEvents = playerEvents @ refereeEvents @ managerEvents

            let throwInIntent =
                refereeIntents
                |> List.tryPick (function
                    | MatchReferee.AwardThrowIn team -> Some team
                    | _ -> None)

            let transition =
                allEvents
                |> List.tryFind (fun e ->
                    match e.Type with
                    | FoulCommitted
                    | MatchEventType.Goal
                    | MatchEventType.Corner -> true
                    | _ -> false)
                |> Option.map (fun e ->
                    match e.Type with
                    | FoulCommitted -> Stopped Foul
                    | MatchEventType.Goal -> Stopped Goal
                    | MatchEventType.Corner -> SetPiece Corner
                    | _ -> LivePlay)

            let injuryIntent =
                refereeIntents
                |> List.tryPick (function
                    | MatchReferee.IssueInjury(player, _) -> Some(player.Id, 1)
                    | _ -> None)

            let stoppageSeconds =
                match injuryIntent with
                | Some _ -> 30
                | None -> 0



            let spawned =
                match throwInIntent with
                | Some team when chainDepth < 6 ->
                    [ { Second = tick.Second + 1
                        Priority = TickPriority.SetPiece
                        SequenceId = 0L
                        Kind = ThrowInTick(team, chainDepth + 1) } ]
                | Some _ -> []
                | None ->
                    match injuryIntent with
                    | Some(playerId, severity) when chainDepth < 6 ->
                        [ { Second = tick.Second + 1
                            Priority = TickPriority.MatchControl
                            SequenceId = 0L
                            Kind = InjuryTick(playerId, severity) }
                          { Second = tick.Second + stoppageSeconds
                            Priority = TickPriority.MatchControl
                            SequenceId = 1L
                            Kind = ResumePlayTick } ]
                    | Some _ -> []
                    | None ->
                        match transition with
                        | Some(Stopped Foul) when chainDepth < 6 ->
                            match att with
                            | Some kicker ->
                                [ { Second = tick.Second + 1
                                    Priority = TickPriority.SetPiece
                                    SequenceId = 0L
                                    Kind = FreeKickTick(kicker.Id, finalState.Ball.Position, chainDepth + 1) } ]
                            | None ->
                                [ { Second = tick.Second + 1
                                    Priority = TickPriority.MatchControl
                                    SequenceId = 0L
                                    Kind = ResumePlayTick } ]
                        | Some(Stopped Foul) -> []
                        | Some(SetPiece Corner) when chainDepth < 6 ->
                            [ { Second = tick.Second + 1
                                Priority = TickPriority.SetPiece
                                SequenceId = 0L
                                Kind = CornerTick(finalState.Possession, chainDepth + 1) } ]
                        | Some(SetPiece Corner) -> []
                        | _ -> []

            let managerReactions =
                if chainDepth < 6 then
                    let goalEvents = allEvents |> List.filter (fun e -> e.Type = MatchEventType.Goal)

                    let cardEvents =
                        allEvents
                        |> List.filter (fun e -> e.Type = MatchEventType.RedCard || e.Type = MatchEventType.YellowCard)

                    [ yield!
                          goalEvents
                          |> List.map (fun e ->
                              { Second = tick.Second + 5
                                Priority = TickPriority.Manager
                                SequenceId = 0L
                                Kind = ManagerReactionTick(GoalScored) })
                      yield!
                          cardEvents
                          |> List.map (fun e ->
                              { Second = tick.Second + 5
                                Priority = TickPriority.Manager
                                SequenceId = 0L
                                Kind = ManagerReactionTick(RedCardTrigger e.PlayerId) }) ]
                else
                    []

            let finalSpawned = spawned @ managerReactions

            let finalTransition =
                match injuryIntent with
                | Some _ when chainDepth < 6 -> Some(Stopped Injury)
                | Some _ -> Some LivePlay
                | None ->
                    match throwInIntent with
                    | Some _ -> Some(SetPiece ThrowIn)
                    | None -> transition

            { State = finalState
              Events = allEvents
              SpawnedTicks = finalSpawned
              PlayStateTransition = finalTransition }

        | FreeKickTick(kickerId, _position, _chainDepth) ->
            let kicker = allPlayers |> List.find (fun p -> p.Id = kickerId)
            let intent = ExecuteFreeKick kicker
            let newState, events, _ = resolve homeId tick.Second intent s

            { State = newState |> applyFatigue
              Events = events
              SpawnedTicks = []
              PlayStateTransition = Some LivePlay }

        | CornerTick(_team, _chainDepth) ->
            let intent = ExecuteCorner
            let newState, events, _ = resolve homeId tick.Second intent s

            { State = newState |> applyFatigue
              Events = events
              SpawnedTicks = []
              PlayStateTransition = Some LivePlay }

        | ThrowInTick(team, _chainDepth) ->
            let intent = ExecuteThrowIn team
            let newState, events, _ = resolve homeId tick.Second intent s

            { State = newState |> applyFatigue
              Events = events
              SpawnedTicks = []
              PlayStateTransition = Some LivePlay }

        | InjuryTick(_playerId, _severity) ->
            { State = s
              Events = []
              SpawnedTicks = []
              PlayStateTransition = Some(Stopped Injury) }

        | ResumePlayTick ->
            { State = s
              Events = []
              SpawnedTicks = []
              PlayStateTransition = Some LivePlay }

        | ManagerReactionTick _trigger ->
            let newState, events = runManagerStep tick.Second homeSquad awaySquad s
            { State = newState
              Events = events
              SpawnedTicks = []
              PlayStateTransition = Some LivePlay }
        | GoalKickTick
        | KickOffTick
        | PenaltyTick _
        | ExtraTimeTick _ ->
            { State = s
              Events = []
              SpawnedTicks = []
              PlayStateTransition = None }


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

        let allPlayers = homeSquad @ awaySquad

        let rec loop ls (scheduler: TickScheduler) =
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
                        | (Stopped _ | SetPiece _), PhysicsTick -> false
                        | _ -> true

                    if not shouldProcess then
                        loop ls scheduler
                    else
                        let result = runTick homeId homeSquad awaySquad allPlayers tick ls.MatchState

                        match ls.Snapshots, tick.Kind with
                        | Some snaps, DuelTick _ -> snaps.Add(result.State)
                        | _ -> ()

                        let stampedTicks, newCounter =
                            result.SpawnedTicks
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) ls.SequenceCounter

                        stampedTicks |> List.iter scheduler.Insert

                        let newPlayState = result.PlayStateTransition |> Option.defaultValue ls.PlayState

                        loop
                            { ls with
                                MatchState = result.State
                                ReversedEvents = result.Events @ ls.ReversedEvents
                                PlayState = newPlayState
                                SequenceCounter = newCounter }
                            scheduler

        loop initialState scheduler

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

    let private toCoords slotX slotY isHome =
        if isHome then
            (1.0 - slotY) * 100.0, slotX * 100.0
        else
            slotY * 100.0, slotX * 100.0

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (isHome: bool)
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
                            let x, y = toCoords s.X s.Y isHome
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
          Possession = Home
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
          IsKnockoutMatch = isKnockout }

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

                let homeS, _, _ =
                    resolve home.Id (95 * 60 + kickNum) (ExecutePenalty(homeKicker, true, kickNum)) s

                let awayS, _, _ =
                    resolve home.Id (95 * 60 + kickNum) (ExecutePenalty(awayKicker, false, kickNum)) homeS

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
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players true
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players false
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
