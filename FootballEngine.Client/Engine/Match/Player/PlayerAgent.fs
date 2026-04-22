namespace FootballEngine

open FootballEngine.Domain
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
        let clubSide =
            SimStateOps.clubSideOf state me.Id |> Option.defaultValue state.AttackingSide

        let team = SimStateOps.buildTeamPerspective clubSide ctx state

        let actx =
            AgentContext.build me profile meIdx team None 0 state clock state.Config.Decision state.Config.BuildUp

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

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentOutput =
        match tick.Kind with
        | DuelTick _ ->
            match state.Ball.Possession with
            | Owned(_, pid) ->
                { Events = []
                  Transition = None
                  Intent = GiveDecisionTo pid }
            | _ ->
                { Events = []
                  Transition = None
                  Intent = FindNextDuel }

        | DecisionTick(_depth, controllerId) ->
            let controllerHasBall =
                match state.Ball.Possession, controllerId with
                | Possession.Owned(_, pid), Some ctrlPid -> pid = ctrlPid
                | _ -> false

            if not controllerHasBall then
                { Events = []
                  Transition = None
                  Intent = FindNextDuel }
            else

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
              Transition = None
              Intent = FindNextDuel }

        | PlayerActionTick(depth, action, attId) ->
            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let prevAttackingClub = state.AttackingSide

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
              Transition = None
              Intent = NoOp }
