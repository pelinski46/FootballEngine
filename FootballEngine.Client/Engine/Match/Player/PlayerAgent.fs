namespace FootballEngine

open BalanceConfig.TickDelay
open FootballEngine.Domain
open FootballEngine.SimStateOps
open SchedulingTypes

module PlayerAgent =

    let decide
        (me: Player)
        (profile: BehavioralProfile)
        (meIdx: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : DecisionOutcome =
        let actx = AgentContext.build me profile meIdx state clock
        let scores = PlayerScorer.computeAll actx
        PlayerDecision.decide actx scores

    let resolve (second: int) (action: PlayerAction) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) =
        match action with
        | PlayerAction.Shoot -> ShotAction.resolve second ctx state clock
        | PlayerAction.Pass target -> PassAction.resolve second ctx state target
        | PlayerAction.Dribble -> DuelAction.resolve second ctx state clock
        | PlayerAction.Cross -> CrossAction.resolve second ctx state
        | PlayerAction.LongBall -> PassAction.resolveLong second ctx state
        | PlayerAction.Tackle opponent -> DuelAction.resolveTackle second ctx state opponent
        | PlayerAction.FreeKick -> SetPlayAction.resolveFreeKick second ctx state
        | PlayerAction.Corner -> SetPlayAction.resolveCorner second ctx state
        | PlayerAction.ThrowIn side -> SetPlayAction.resolveThrowIn second ctx state side
        | PlayerAction.Penalty(kicker, side, kickNum) ->
            SetPlayAction.resolvePenalty ctx state kicker side kickNum clock

    let private fatigue p pressing tactics instructions =
        let config = SimStateOps.tacticsConfig tactics instructions
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15

        int (
            float (base' + workRate)
            * (if pressing then 1.5 else 1.0)
            * config.PressingIntensity
        )

    let private spawnNextDuel subTick interval =
        { SubTick = subTick + interval
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DuelTick 0 }

    let private spawnDecisionTick subTick depth controllerId =
        { SubTick = subTick + delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DecisionTick(depth, controllerId) }

    let agent tick (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentOutput =
        match tick.Kind with
        | DuelTick _ ->
            match state.Ball.Possession with
            | Owned(_, pid) ->
                { Events = []
                  Continuation = Defer(BalanceConfig.duelChainDelay, DecisionTick(0, Some pid))
                  Transition = None
                  SideEffects = [] }
            | _ ->
                { Events = []
                  Continuation = Defer(BalanceConfig.duelNextDelay, DuelTick 0)
                  Transition = None
                  SideEffects = [] }

        | DecisionTick(depth, controllerId) ->
            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let prevAttackingClub = state.AttackingSide

            let attSlots =
                match controllerId with
                | Some pid ->
                    SimStateOps.clubSideOf state pid
                    |> Option.map (fun side -> SimStateOps.getSlots state side)
                    |> Option.defaultValue [||]
                | None -> SimStateOps.getSlots state prevAttackingClub

            let controller, controllerIdx, action =
                match controllerId with
                | Some pid ->
                    let mutable idx = 0
                    let mutable found = false

                    for i = 0 to attSlots.Length - 1 do
                        match attSlots[i] with
                        | PlayerSlot.Active s when s.Player.Id = pid ->
                            idx <- i
                            found <- true
                        | _ -> ()

                    if found then
                        match attSlots[idx] with
                        | PlayerSlot.Active s -> s.Player, idx, decide s.Player s.Profile idx ctx state clock
                        | _ -> Unchecked.defaultof<Player>, 0, BallContested
                    else
                        match MatchSpatial.nearestActiveSlot attSlots bx by with
                        | ValueSome s -> s.Player, 0, decide s.Player s.Profile 0 ctx state clock
                        | ValueNone -> Unchecked.defaultof<Player>, 0, BallContested
                | None ->
                    if attSlots.Length = 0 then
                        Unchecked.defaultof<Player>, 0, BallContested
                    else
                        match MatchSpatial.nearestActiveSlot attSlots bx by with
                        | ValueSome s -> s.Player, 0, decide s.Player s.Profile 0 ctx state clock
                        | ValueNone -> Unchecked.defaultof<Player>, 0, BallContested

            // Bug 6 Fix: Capture defOpt BEFORE action resolve
            let defSide = ClubSide.flip prevAttackingClub
            let defSlots = SimStateOps.getSlots state defSide

            let defOpt =
                match MatchSpatial.nearestActiveSlot defSlots bx by with
                | ValueSome s -> Some s.Player
                | ValueNone -> None

            let controllerSlot =
                attSlots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Id = controller.Id -> Some s
                    | _ -> None)

            let playerEvents =
                match action with
                | BallContested -> []
                | PlayerActing act -> resolve tick.SubTick act ctx state clock

            let playerHasBall =
                match state.Ball.Possession with
                | Owned(_, pid) -> pid = controller.Id
                | _ -> false

            let attOpt = if playerHasBall then Some controller else None

            let refEvents, _ = RefereeAgent.runRefereeStep tick.SubTick attOpt defOpt ctx state
            let allEvents = EventPipeline.run (playerEvents @ refEvents) ctx state tick.SubTick

            { Events = allEvents
              Continuation = Defer(BalanceConfig.duelNextDelay, DuelTick 0)
              Transition = None
              SideEffects = [] }

        | PlayerActionTick(depth, action, attId) ->
            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let prevAttackingClub = state.AttackingSide

            // Bug 6 Fix: Capture defOpt BEFORE action resolve
            let defSide = ClubSide.flip prevAttackingClub
            let defSlots = SimStateOps.getSlots state defSide

            let defOpt =
                match MatchSpatial.nearestActiveSlot defSlots bx by with
                | ValueSome s -> Some s.Player
                | ValueNone -> None

            let playerEvents = resolve tick.SubTick action ctx state clock
            let playerHasBall = true

            let attOpt = attId |> Option.bind (SimStateOps.findActivePlayer state)

            let refEvents, _ = RefereeAgent.runRefereeStep tick.SubTick attOpt defOpt ctx state
            let allEvents = EventPipeline.run (playerEvents @ refEvents) ctx state tick.SubTick

            MatchEventProcessor.processEventsAndSpawnTicks
                tick.SubTick
                depth
                allEvents
                playerHasBall
                attId
                prevAttackingClub
                ctx
                state
                clock

        | _ ->
            { Events = []
              Continuation = EndChain
              Transition = None
              SideEffects = [] }
