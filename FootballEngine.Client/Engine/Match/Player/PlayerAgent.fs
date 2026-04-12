namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module PlayerAgent =

    let decide (me: Player) (profile: BehavioralProfile) (meIdx: int) (ctx: MatchContext) (state: SimState) : PlayerAction =
        let actx = AgentContext.build me profile meIdx ctx state
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

    let private spawnDecisionTick subTick depth controllerId =
        { SubTick = subTick + Stats.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DecisionTick(depth, controllerId) }

    let private spawnPlayerActionTick subTick depth action attackerId =
        { SubTick = subTick + Stats.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = PlayerActionTick(depth, action, Some attackerId) }

    let agent homeId homeSquad awaySquad tick (ctx: MatchContext) (state: SimState) : AgentOutput =
        match tick.Kind with
        | DuelTick _ ->
            { Events = []
              Spawned = [ spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay) ]
              Transition = None }

        | DecisionTick(depth, controllerId) ->
            let attSlots =
                match controllerId with
                | Some pid ->
                    SimStateOps.clubSideOf state pid
                    |> Option.map (fun side -> SimStateOps.getSlots state side)
                    |> Option.defaultValue [||]
                | None -> SimStateOps.getSlots state state.AttackingClub

            let bx, by = state.Ball.Position.X, state.Ball.Position.Y

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
                        | PlayerSlot.Active s -> s.Player, idx, decide s.Player s.Profile idx ctx state
                        | _ -> Unchecked.defaultof<Player>, 0, PlayerAction.Idle
                    else
                        let newIdx = MatchSpatial.nearestIdxToBall attSlots bx by
                        match attSlots[newIdx] with
                        | PlayerSlot.Active s -> s.Player, newIdx, decide s.Player s.Profile newIdx ctx state
                        | _ -> Unchecked.defaultof<Player>, 0, PlayerAction.Idle
                | None ->
                    if attSlots.Length = 0 then
                        Unchecked.defaultof<Player>, 0, PlayerAction.Idle
                    else
                        let newIdx = MatchSpatial.nearestIdxToBall attSlots bx by
                        match attSlots[newIdx] with
                        | PlayerSlot.Active s -> s.Player, newIdx, decide s.Player s.Profile newIdx ctx state
                        | _ -> Unchecked.defaultof<Player>, 0, PlayerAction.Idle

            let prevAttackingClub = state.AttackingClub
            let playerEvents = resolve homeId tick.SubTick action ctx state
            let possessionChanged = state.AttackingClub <> prevAttackingClub

            let att =
                controllerId
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
                        let actions = RefereeAgent.decideCard tick.SubTick fouler ctx state
                        actions |> List.collect (fun a -> RefereeAgent.resolve tick.SubTick a ctx state)
                    | None -> [])

            let allEventsWithCards = allEvents @ cardEvents

            let possTick =
                if possessionChanged then
                    [ { SubTick = tick.SubTick + 1
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

                        let kickerOpt =
                            if attSlots2.Length = 0 then
                                None
                            else
                                let idx = MatchSpatial.nearestIdxToBall attSlots2 bx2 by2

                                match attSlots2[idx] with
                                | PlayerSlot.Active s -> Some s.Player.Id
                                | _ -> None

                        match kickerOpt with
                        | None -> []
                        | Some kickerId ->
                            let foulPos = SimStateOps.defaultSpatial bx2 by2

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
                let attSlots2 = SimStateOps.getSlots state state.AttackingClub

                if attSlots2.Length = 0 then
                    let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick [ dt ]
                      Transition = None }
                else
                    let bx', by' = state.Ball.Position.X, state.Ball.Position.Y
                    let newAttIdx = MatchSpatial.nearestIdxToBall attSlots2 bx' by'

                    let newAtt, newAttProfile =
                        match attSlots2[newAttIdx] with
                        | PlayerSlot.Active s -> s.Player, s.Profile
                        | _ -> Unchecked.defaultof<Player>, BehavioralProfile.neutral

                    let newAction = decide newAtt newAttProfile newAttIdx ctx state

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick [ spawnPlayerActionTick tick.SubTick (depth + 1) newAction newAtt.Id ]
                      Transition = None }
            else
                let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)

                { Events = allEventsWithCards
                  Spawned = addPossessionTick [ dt ]
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
                        let actions = RefereeAgent.decideCard tick.SubTick fouler ctx state
                        actions |> List.collect (fun a -> RefereeAgent.resolve tick.SubTick a ctx state)
                    | None -> [])

            let allEventsWithCards = allEvents @ cardEvents

            let possTick =
                if possessionChanged then
                    [ { SubTick = tick.SubTick + 1
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

                        let kickerOpt =
                            if attSlots2.Length = 0 then
                                None
                            else
                                let idx = MatchSpatial.nearestIdxToBall attSlots2 bx2 by2

                                match attSlots2[idx] with
                                | PlayerSlot.Active s -> Some s.Player.Id
                                | _ -> None

                        match kickerOpt with
                        | None -> []
                        | Some kickerId ->
                            let foulPos = SimStateOps.defaultSpatial bx2 by2

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
                let attSlots2 = SimStateOps.getSlots state state.AttackingClub

                if attSlots2.Length = 0 then
                    let dt = spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay)

                    { Events = allEventsWithCards
                      Spawned = addPossessionTick [ dt ]
                      Transition = None }
                else
                    let bx', by' = state.Ball.Position.X, state.Ball.Position.Y
                    let newAttIdx = MatchSpatial.nearestIdxToBall attSlots2 bx' by'

                    let newAtt, newAttProfile =
                        match attSlots2[newAttIdx] with
                        | PlayerSlot.Active s -> s.Player, s.Profile
                        | _ -> Unchecked.defaultof<Player>, BehavioralProfile.neutral

                    let newAction = decide newAtt newAttProfile newAttIdx ctx state

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
