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

    let private intentFromEvents
        (events: MatchEvent list)
        (possessionChanged: bool)
        (playerHasBall: bool)
        (tc: TimingConfig)
        (state: SimState)
        : TickIntent =

        let hasGoal =
            events |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

        let hasFoul =
            events |> List.exists (fun e -> e.Type = MatchEventType.FoulCommitted)

        let chainBreaks =
            events
            |> List.exists (fun e ->
                match e.Type with
                | MatchEventType.PassDeflected _
                | MatchEventType.PassMisplaced _ -> true
                | _ -> false)
            || possessionChanged
            || not playerHasBall

        if hasGoal then
            StopPlay Goal
        elif hasFoul then
            StopPlay Foul
        elif chainBreaks then
            match state.Ball.Possession with
            | Owned(_, pid) -> GiveDecisionTo pid
            | _ -> FindNextDuel
        else
            FindNextDuel

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

        let tc = ctx.Config.Timing
        let mv = ctx.Config.MatchVolume

        let possessionChanged = state.AttackingSide <> prevAttackingClub

        let hasTerminating = hasTerminatingEvent allEvents
        let chainBreaks =
            allEvents
            |> List.exists (fun e ->
                match e.Type with
                | MatchEventType.PassDeflected _
                | MatchEventType.PassMisplaced _ -> true
                | _ -> false)
            || possessionChanged
            || not playerHasBall

        if hasTerminating || chainBreaks then
            let transition = transitionFromEvents allEvents
            let intent = intentFromEvents allEvents possessionChanged playerHasBall tc state

            { Events = allEvents
              Transition = transition
              Intent = intent }
        elif depth < mv.MaxChainLength - 1 then
            let currentAttSide = state.AttackingSide
            let attSlots2 = SimStateOps.getSlots state currentAttSide

            if attSlots2.Length = 0 then
                { Events = allEvents
                  Transition = None
                  Intent = FindNextDuel }
            else
                let bx', by' = state.Ball.Position.X, state.Ball.Position.Y

                match MatchSpatial.nearestActiveSlot attSlots2 bx' by' with
                | ValueSome s ->
                    let team = SimStateOps.buildTeamPerspective state.AttackingSide ctx state

                    let actx =
                        AgentContext.build
                            s.Player
                            s.Profile
                            0
                            team
                            s.MovementIntent
                            s.IntentLockExpiry
                            state
                            clock
                            state.Config.Decision
                            state.Config.BuildUp

                    let scores = PlayerScorer.computeAll actx

                    let intent =
                        match PlayerDecision.decide actx scores with
                        | BallContested -> FindNextDuel
                        | PlayerActing act ->
                            let _ = act
                            FindNextDuel

                    { Events = allEvents
                      Transition = None
                      Intent = intent }
                | ValueNone ->
                    { Events = allEvents
                      Transition = None
                      Intent = FindNextDuel }

        else
            { Events = allEvents
              Transition = None
              Intent = FindNextDuel }
