namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module PlayerAgent =

    let decide (me: Player) (meIdx: int) (ctx: MatchContext) (state: SimState) : PlayerAction =
        let actx = AgentContext.build me meIdx ctx state
        let scores = PlayerScorer.computeAll actx
        PlayerDecision.decide actx scores

    let resolve (homeId: ClubId) (second: int) (action: PlayerAction) (ctx: MatchContext) (state: SimState) =
        match action with
        | PlayerAction.Shoot -> ShotAction.resolve second ctx state
        | PlayerAction.Pass target -> PassAction.resolve second ctx state target
        | PlayerAction.Dribble -> DuelAction.resolve second ctx state
        | PlayerAction.Cross -> CrossAction.resolve second ctx state
        | PlayerAction.LongBall -> PassAction.resolveLong second ctx state
        | PlayerAction.Tackle opponent -> DuelAction.resolveTackle second ctx state opponent
        | PlayerAction.FreeKick -> SetPlayAction.resolveFreeKick second ctx state
        | PlayerAction.Corner -> SetPlayAction.resolveCorner second ctx state
        | PlayerAction.ThrowIn side -> SetPlayAction.resolveThrowIn second ctx state side
        | PlayerAction.Penalty(kicker, side, kickNum) ->
            SetPlayAction.resolvePenalty second ctx state kicker side kickNum
        | PlayerAction.Idle -> []

    let private fatigue p pressing tactics instructions =
        let config = SimStateOps.tacticsConfig tactics instructions
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15

        int (
            float (base' + workRate)
            * (if pressing then 1.5 else 1.0)
            * config.PressingIntensity
        )

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
            Some(SetPiece KickOff)
        else
            events
            |> List.tryPick (fun e ->
                match e.Type with
                | MatchEventType.FoulCommitted -> Some(Stopped Foul)
                | MatchEventType.Corner -> Some(SetPiece Corner)
                | _ -> None)

    let private spawnNextDuel subTick interval =
        { SubTick = subTick + interval
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DuelTick 0 }

    let private spawnPlayerActionTick subTick depth action attackerId =
        { SubTick = subTick + Stats.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = PlayerActionTick(depth, action, Some attackerId) }

    let agent homeId homeSquad awaySquad tick (ctx: MatchContext) (state: SimState) : AgentOutput =
        match tick.Kind with
        | DuelTick _ ->
            if state.HomeSlots.Length = 0 && state.AwaySlots.Length = 0 then
                { Events = []
                  Spawned = [ spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                  Transition = None }
            else
                let bx, by = state.Ball.Position.X, state.Ball.Position.Y
                let attSlots = SimStateOps.getSlots state state.AttackingClub
                let attIdx = MatchSpatial.nearestIdxToBall attSlots bx by

                let att =
                    match attSlots[attIdx] with
                    | PlayerSlot.Active s -> s.Player
                    | _ -> Unchecked.defaultof<Player>

                let action = decide att attIdx ctx state

                { Events = []
                  Spawned = [ spawnPlayerActionTick tick.SubTick 0 action att.Id ]
                  Transition = None }

        | PlayerActionTick(depth, action, attId) ->
            let prevAttackingClub = state.AttackingClub
            let playerEvents = resolve homeId tick.SubTick action ctx state
            let possessionChanged = state.AttackingClub <> prevAttackingClub

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

            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let defSide = ClubSide.flip state.AttackingClub
            let defSlots = SimStateOps.getSlots state defSide

            let def =
                if defSlots.Length = 0 then
                    None
                else
                    let idx = MatchSpatial.nearestIdxToBall defSlots bx by

                    match defSlots[idx] with
                    | PlayerSlot.Active s -> Some s.Player
                    | _ -> None

            let refEvents, _refActions =
                RefereeAgent.runRefereeStep tick.SubTick att def ctx state

            let allEvents = playerEvents @ refEvents

            let foulEvents =
                allEvents |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted)

            let cardEvents =
                foulEvents
                |> List.collect (fun fe ->
                    let foulerOpt =
                        state.HomeSlots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = fe.PlayerId -> Some s.Player
                            | _ -> None)
                        |> Option.orElseWith (fun () ->
                            state.AwaySlots
                            |> Array.tryPick (function
                                | PlayerSlot.Active s when s.Player.Id = fe.PlayerId -> Some s.Player
                                | _ -> None))

                    match foulerOpt with
                    | Some fouler ->
                        let actions = RefereeAgent.decideCard tick.SubTick fouler ctx state
                        actions |> List.collect (fun a -> RefereeAgent.resolve tick.SubTick a ctx state)
                    | None -> [])

            let allEventsWithCards = allEvents @ cardEvents

            let cornerSpawned =
                allEventsWithCards
                |> List.exists (fun e ->
                    match e.Type with
                    | MatchEventType.Corner -> true
                    | _ -> false)

            let possTick =
                if possessionChanged then
                    [ { SubTick = tick.SubTick + 1
                        Priority = TickPriority.Duel
                        SequenceId = 0L
                        Kind = PossessionChangeTick prevAttackingClub } ]
                else
                    []

            let cornerTick =
                if cornerSpawned then
                    [ { SubTick = tick.SubTick + 1
                        Priority = TickPriority.SetPiece
                        SequenceId = 0L
                        Kind = CornerTick(state.AttackingClub, 0) } ]
                else
                    []

            let addPossessionTick baseSpawned = baseSpawned @ possTick @ cornerTick

            let chainBreaks =
                allEventsWithCards
                |> List.exists (fun e ->
                    match e.Type with
                    | MatchEventType.PassDeflected _
                    | MatchEventType.PassMisplaced _ -> true
                    | _ -> false)
                || state.AttackingClub <> prevAttackingClub

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
                        let attSlots = SimStateOps.getSlots state state.AttackingClub
                        let bx, by = state.Ball.Position.X, state.Ball.Position.Y

                        let kickerOpt =
                            if attSlots.Length = 0 then
                                None
                            else
                                let idx = MatchSpatial.nearestIdxToBall attSlots bx by

                                match attSlots[idx] with
                                | PlayerSlot.Active s -> Some s.Player.Id
                                | _ -> None

                        match kickerOpt with
                        | None -> []
                        | Some kickerId ->
                            let foulPos = SimStateOps.defaultSpatial bx by

                            [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.foulDelay
                                Priority = TickPriority.SetPiece
                                SequenceId = 0L
                                Kind = FreeKickTick(kickerId, foulPos, 0) } ]
                    else
                        []

                let spawned =
                    if hasGoal then
                        let kt =
                            { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.goalDelay
                              Priority = TickPriority.MatchControl
                              SequenceId = 0L
                              Kind = KickOffTick }

                        addPossessionTick [ kt ]
                    else
                        let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)
                        addPossessionTick (foulTick @ [ dt ])

                { Events = allEventsWithCards
                  Spawned = spawned
                  Transition = transition }
            elif depth < BalanceConfig.MaxChainLength - 1 then
                let attSlots = SimStateOps.getSlots state state.AttackingClub

                if attSlots.Length = 0 then
                    let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick [ dt ]
                      Transition = None }
                else
                    let bx', by' = state.Ball.Position.X, state.Ball.Position.Y
                    let newAttIdx = MatchSpatial.nearestIdxToBall attSlots bx' by'

                    let newAtt =
                        match attSlots[newAttIdx] with
                        | PlayerSlot.Active s -> s.Player
                        | _ -> Unchecked.defaultof<Player>

                    let newAction = decide newAtt newAttIdx ctx state

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick [ spawnPlayerActionTick tick.SubTick (depth + 1) newAction newAtt.Id ]
                      Transition = None }
            else
                let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)

                { Events = allEventsWithCards
                  Spawned = addPossessionTick [ dt ]
                  Transition = None }

        | _ ->
            { Events = []
              Spawned = []
              Transition = None }
