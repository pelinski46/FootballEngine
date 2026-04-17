namespace FootballEngine

open BalanceConfig
open FootballEngine.Domain
open SchedulingTypes

module MatchEventProcessor =

    let private hasTerminatingEvent (events: MatchEvent list) =
        events
        |> List.exists (fun e ->
            match e.Type with
            | MatchEventType.Goal
            | MatchEventType.FoulCommitted
            | MatchEventType.ShotOffTarget
            | MatchEventType.ShotBlocked
            | MatchEventType.Save -> true
            | _ -> false)

    let private transitionFromEvents (events: MatchEvent list) =
        let hasGoal =
            events
            |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

        if hasGoal then
            Some(Stopped Goal)
        else
            events
            |> List.tryPick (fun e ->
                match e.Type with
                | MatchEventType.FoulCommitted -> Some(Stopped Foul)
                | MatchEventType.Corner -> Some(SetPiece SetPieceKind.Corner)
                | _ -> None)

    let private spawnDecisionTick subTick depth controllerId =
        { SubTick = subTick + TickDelay.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DecisionTick(depth, controllerId) }

    let private spawnNextDuel subTick interval =
        { SubTick = subTick + interval
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DuelTick 0 }

    let spawnPlayerActionTick subTick depth action attackerId =
        { SubTick = subTick + TickDelay.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = PlayerActionTick(depth, action, Some attackerId) }

    let processEventsAndSpawnTicks
        (subTick: int)
        (depth: int)
        (playerEvents: MatchEvent list)
        (playerHasBall: bool)
        (attId: PlayerId option)
        (prevAttackingClub: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : AgentOutput =

        let bx, by = state.Ball.Position.X, state.Ball.Position.Y

        let att =
            attId
            |> Option.bind (fun pid ->
                SimStateOps.clubSideOf state pid
                |> Option.bind (fun side ->
                    let slots = SimStateOps.getSlots state side

                    slots
                    |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                        | _ -> None)))

        let defSide = ClubSide.flip state.AttackingClub
        let defSlots = SimStateOps.getSlots state defSide

        let def =
            match MatchSpatial.nearestActiveSlot defSlots bx by with
            | ValueSome s -> Some s.Player
            | ValueNone -> None

        let refEvents, _refActions = RefereeAgent.runRefereeStep subTick att def ctx state

        let allEvents = playerEvents @ refEvents

        let foulEvents =
            allEvents |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted)

        let cardEvents =
            foulEvents
            |> List.collect (fun fe ->
                let foulerOpt =
                    state.Home.Slots
                    |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Id = fe.PlayerId -> Some s.Player
                        | _ -> None)
                    |> Option.orElseWith (fun () ->
                        state.Away.Slots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = fe.PlayerId -> Some s.Player
                            | _ -> None))

                match foulerOpt with
                | Some fouler ->
                    let actions = RefereeAgent.decideCard fouler ctx state
                    actions |> List.collect (fun a -> RefereeAgent.resolve subTick a ctx state)
                | None -> [])

        let allEventsWithCards = allEvents @ cardEvents

        let possessionChanged = state.AttackingClub <> prevAttackingClub

        let possTick =
            if possessionChanged then
                [ { SubTick = subTick + 1
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = PossessionChangeTick prevAttackingClub } ]
            else
                []

        let addPossessionTick baseSpawned = baseSpawned @ possTick

        let chainBreaks =
            allEventsWithCards
            |> List.exists (fun e ->
                match e.Type with
                | MatchEventType.PassDeflected _
                | MatchEventType.PassMisplaced _ -> true
                | _ -> false)
            || state.AttackingClub <> prevAttackingClub
            || not playerHasBall

        if hasTerminatingEvent allEventsWithCards || chainBreaks then
            let transition = transitionFromEvents allEventsWithCards

            let hasGoal =
                allEventsWithCards
                |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

            let hasFoul =
                allEventsWithCards
                |> List.exists (fun e -> e.Type = MatchEventType.FoulCommitted)

            let foulTick =
                if hasFoul then
                    let attSlots2 = SimStateOps.getSlots state state.AttackingClub
                    let bx2, by2 = state.Ball.Position.X, state.Ball.Position.Y

                    match MatchSpatial.nearestActiveSlot attSlots2 bx2 by2 with
                    | ValueSome s ->
                        let foulPos = SimStateOps.defaultSpatial bx2 by2

                        [ { SubTick = subTick + TickDelay.delayFrom BalanceConfig.foulDelay
                            Priority = TickPriority.SetPiece
                            SequenceId = 0L
                            Kind = FreeKickTick(s.Player.Id, foulPos, 0) } ]
                    | ValueNone -> []
                else
                    []

            let spawned =
                if hasGoal then
                    let kt =
                        { SubTick = subTick + TickDelay.delayFrom BalanceConfig.goalDelay
                          Priority = TickPriority.MatchControl
                          SequenceId = 0L
                          Kind = KickOffTick }

                    addPossessionTick [ kt ]
                else
                    let dt =
                        match state.Ball.Possession with
                        | Owned(_, pid) -> Some pid
                        | _ -> None
                        |> fun pidOpt ->
                            match pidOpt with
                            | Some pid -> spawnDecisionTick subTick 0 (Some pid)
                            | None -> spawnNextDuel subTick (TickDelay.delayFrom BalanceConfig.duelNextDelay)

                    addPossessionTick (foulTick @ [ dt ])

            { Events = allEventsWithCards
              Spawned = spawned
              Transition = transition }
        elif depth < BalanceConfig.MaxChainLength - 1 then
            let attSlots2 = SimStateOps.getSlots state state.AttackingClub

            if attSlots2.Length = 0 then
                let dt = spawnNextDuel subTick (TickDelay.delayFrom BalanceConfig.duelNextDelay)

                { Events = allEventsWithCards
                  Spawned = addPossessionTick [ dt ]
                  Transition = None }
            else
                let bx', by' = state.Ball.Position.X, state.Ball.Position.Y

                match MatchSpatial.nearestActiveSlot attSlots2 bx' by' with
                | ValueSome s ->
                    let actx = AgentContext.build s.Player s.Profile 0 state clock
                    let scores = PlayerScorer.computeAll actx

                    let spawned' =
                        match PlayerDecision.decide actx scores with
                        | BallContested ->
                            [ { SubTick = subTick + TickDelay.delayFrom BalanceConfig.duelChainDelay
                                Priority = TickPriority.Duel
                                SequenceId = 0L
                                Kind = DuelTick 0 } ]
                        | PlayerActing act -> [ spawnPlayerActionTick subTick (depth + 1) act s.Player.Id ]

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick spawned'
                      Transition = None }
                | ValueNone ->
                    { Events = allEventsWithCards
                      Spawned = []
                      Transition = None }

        else
            let dt = spawnNextDuel subTick (TickDelay.delayFrom BalanceConfig.duelNextDelay)

            { Events = allEventsWithCards
              Spawned = addPossessionTick [ dt ]
              Transition = None }
