namespace FootballEngine

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


    let processEventsAndSpawnTicks
        (subTick: int)
        (depth: int)
        (allEvents: MatchEvent list)
        (playerHasBall: bool)
        (attId: PlayerId option)
        (prevAttackingClub: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : AgentOutput =

        let possessionChanged = state.AttackingSide <> prevAttackingClub

        let chainBreaks =
            allEvents
            |> List.exists (fun e ->
                match e.Type with
                | MatchEventType.PassDeflected _
                | MatchEventType.PassMisplaced _ -> true
                | _ -> false)
            || possessionChanged
            || not playerHasBall

        if hasTerminatingEvent allEvents || chainBreaks then
            let transition = transitionFromEvents allEvents

            let hasGoal =
                allEvents
                |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

            let hasFoul =
                allEvents |> List.exists (fun e -> e.Type = MatchEventType.FoulCommitted)

            let continuation =
                if hasGoal then
                    Defer(BalanceConfig.goalDelay, KickOffTick)
                elif hasFoul then

                    let attSlots2 = SimStateOps.getSlots state prevAttackingClub
                    let bx2, by2 = state.Ball.Position.X, state.Ball.Position.Y

                    match MatchSpatial.nearestActiveSlot attSlots2 bx2 by2 with
                    | ValueSome s ->
                        let foulPos = SimStateOps.defaultSpatial bx2 by2
                        Defer(BalanceConfig.foulDelay, FreeKickTick(s.Player.Id, foulPos, 0))
                    | ValueNone -> Defer(BalanceConfig.duelNextDelay, DuelTick 0)
                else
                    match state.Ball.Possession with
                    | Owned(_, pid) -> Defer(BalanceConfig.duelChainDelay, DecisionTick(0, Some pid))
                    | _ -> Defer(BalanceConfig.duelNextDelay, DuelTick 0)

            { Events = allEvents
              Continuation = continuation
              Transition = transition
              SideEffects = [] }
        elif depth < BalanceConfig.MaxChainLength - 1 then
            let currentAttSide = state.AttackingSide
            let attSlots2 = SimStateOps.getSlots state currentAttSide

            if attSlots2.Length = 0 then
                { Events = allEvents
                  Continuation = Defer(BalanceConfig.duelNextDelay, DuelTick 0)
                  Transition = None
                  SideEffects = [] }
            else
                let bx', by' = state.Ball.Position.X, state.Ball.Position.Y

                match MatchSpatial.nearestActiveSlot attSlots2 bx' by' with
                | ValueSome s ->
                    let actx = AgentContext.build s.Player s.Profile 0 state clock
                    let scores = PlayerScorer.computeAll actx

                    let continuation' =
                        match PlayerDecision.decide actx scores with
                        | BallContested -> Defer(BalanceConfig.duelChainDelay, DuelTick 0)
                        | PlayerActing act ->
                            Defer(BalanceConfig.duelChainDelay, PlayerActionTick(depth + 1, act, Some s.Player.Id))

                    { Events = allEvents
                      Continuation = continuation'
                      Transition = None
                      SideEffects = [] }
                | ValueNone ->
                    { Events = allEvents
                      Continuation = EndChain
                      Transition = None
                      SideEffects = [] }

        else
            { Events = allEvents
              Continuation = Defer(BalanceConfig.duelNextDelay, DuelTick 0)
              Transition = None
              SideEffects = [] }
