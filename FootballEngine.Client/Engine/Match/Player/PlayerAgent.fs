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
        |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.FoulCommitted)

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

    let private nearestAttackerIdx ballX ballY (state: SimState) (attSide: ClubSide) =
        let slots =
            if attSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let mutable bestIdx = 0
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - ballX
                let dy = s.Pos.Y - ballY
                let dSq = dx * dx + dy * dy

                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestIdx <- i
            | _ -> ()

        bestIdx

    let private nearestDefender ballX ballY (state: SimState) (defSide: ClubSide) : Player option =
        let slots =
            if defSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        if slots.Length = 0 then
            None
        else
            let mutable bestIdx = 0
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to slots.Length - 1 do
                match slots[i] with
                | PlayerSlot.Active s ->
                    let dx = s.Pos.X - ballX
                    let dy = s.Pos.Y - ballY
                    let dSq = dx * dx + dy * dy

                    if dSq < bestDistSq then
                        bestDistSq <- dSq
                        bestIdx <- i
                | _ -> ()

            match slots[bestIdx] with
            | PlayerSlot.Active s -> Some s.Player
            | _ -> None

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
                let attIdx = nearestAttackerIdx bx by state state.AttackingClub

                let attSlots =
                    if state.AttackingClub = HomeClub then
                        state.HomeSlots
                    else
                        state.AwaySlots

                let att =
                    match attSlots[attIdx] with
                    | PlayerSlot.Active s -> s.Player
                    | _ -> Unchecked.defaultof<Player>

                let action = decide att attIdx ctx state

                { Events = []
                  Spawned = [ spawnPlayerActionTick tick.SubTick 0 action att.Id ]
                  Transition = None }

        | PlayerActionTick(depth, action, attId) ->
            let playerEvents = resolve homeId tick.SubTick action ctx state

            let att =
                attId
                |> Option.bind (fun pid ->
                    state.HomeSlots
                    |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                        | _ -> None)
                    |> Option.orElseWith (fun () ->
                        state.AwaySlots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                            | _ -> None)))

            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let defSide = ClubSide.flip state.AttackingClub
            let def = nearestDefender bx by state defSide

            let refEvents, _refActions =
                RefereeAgent.runRefereeStep tick.SubTick att def ctx state

            let allEvents = playerEvents @ refEvents

            if hasTerminatingEvent allEvents then
                let transition = transitionFromEvents allEvents

                let hasGoal =
                    allEvents
                    |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

                let spawned =
                    if hasGoal then
                        [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.goalDelay
                            Priority = TickPriority.MatchControl
                            SequenceId = 0L
                            Kind = KickOffTick } ]
                    else
                        [ spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay) ]

                { Events = allEvents
                  Spawned = spawned
                  Transition = transition }
            elif depth < BalanceConfig.AvgChainLength - 1 then
                let attSlots =
                    if state.AttackingClub = HomeClub then
                        state.HomeSlots
                    else
                        state.AwaySlots

                if attSlots.Length = 0 then
                    { Events = allEvents
                      Spawned = [ spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                      Transition = None }
                else
                    let bx', by' = state.Ball.Position.X, state.Ball.Position.Y
                    let newAttIdx = nearestAttackerIdx bx' by' state state.AttackingClub

                    let newAtt =
                        match attSlots[newAttIdx] with
                        | PlayerSlot.Active s -> s.Player
                        | _ -> Unchecked.defaultof<Player>

                    let newAction = decide newAtt newAttIdx ctx state

                    { Events = allEvents
                      Spawned = [ spawnPlayerActionTick tick.SubTick (depth + 1) newAction newAtt.Id ]
                      Transition = None }
            else
                { Events = allEvents
                  Spawned = [ spawnNextDuel tick.SubTick (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                  Transition = None }

        | _ ->
            { Events = []
              Spawned = []
              Transition = None }
