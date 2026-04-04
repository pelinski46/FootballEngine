namespace FootballEngine

open System
open FootballEngine.Domain
open SchedulingTypes

module PlayerAgent =

    let decide (me: Player) (meIdx: int) (s: MatchState) : PlayerAction =
        let ctx = AgentContext.build me meIdx s
        let scores = PlayerScorer.computeAll ctx
        PlayerDecision.decide ctx scores

    let resolve (homeId: ClubId) (second: int) (action: PlayerAction) (s: MatchState) =
        match action with
        | PlayerAction.Shoot -> ShotAction.resolve second s
        | PlayerAction.Pass target -> PassAction.resolve second s target
        | PlayerAction.Dribble -> DuelAction.resolve second s
        | PlayerAction.Cross -> CrossAction.resolve second s
        | PlayerAction.LongBall -> PassAction.resolveLong second s
        | PlayerAction.Tackle opponent -> DuelAction.resolveTackle second s opponent
        | PlayerAction.FreeKick -> SetPlayAction.resolveFreeKick second s
        | PlayerAction.Corner -> SetPlayAction.resolveCorner second s
        | PlayerAction.ThrowIn side -> SetPlayAction.resolveThrowIn second s side
        | PlayerAction.Penalty(kicker, side, kickNum) -> SetPlayAction.resolvePenalty second s kicker side kickNum
        | PlayerAction.Idle -> s, []



    let private fatigue p pressing tactics instructions =
        let config = MatchStateOps.tacticsConfig tactics instructions
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
        events
        |> List.tryFind (fun e ->
            e.Type = MatchEventType.FoulCommitted
            || e.Type = MatchEventType.Goal
            || e.Type = MatchEventType.Corner)
        |> Option.map (fun e ->
            match e.Type with
            | MatchEventType.FoulCommitted -> Stopped Foul
            | MatchEventType.Goal -> Stopped Goal
            | MatchEventType.Corner -> SetPiece Corner
            | _ -> LivePlay)

    let private nearestAttackerIdx ballX ballY (attSide: TeamSide) =
        attSide.Positions
        |> Array.mapi (fun i _ -> i)
        |> Array.minBy (fun i ->
            let dx = attSide.Positions[i].X - ballX
            let dy = attSide.Positions[i].Y - ballY
            dx * dx + dy * dy)

    let private nearestDefender ballX ballY (defSide: TeamSide) : Player option =
        if defSide.Players.Length = 0 then
            None
        else
            let idx =
                defSide.Positions
                |> Array.mapi (fun i _ -> i)
                |> Array.minBy (fun i ->
                    let dx = defSide.Positions[i].X - ballX
                    let dy = defSide.Positions[i].Y - ballY
                    dx * dx + dy * dy)

            Some defSide.Players[idx]

    let private spawnNextDuel second interval =
        { Second = second + interval
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = DuelTick 0 }

    let private spawnPlayerActionTick second depth action attackerId =
        { Second = second + Stats.delayFrom BalanceConfig.duelChainDelay
          Priority = TickPriority.Duel
          SequenceId = 0L
          Kind = PlayerActionTick(depth, action, Some attackerId) }

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | DuelTick _ ->
            let attSide = ClubSide.teamSide state.AttackingClub state

            if attSide.Players.Length = 0 then
                { State = state
                  Events = []
                  Spawned = [ spawnNextDuel tick.Second (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                  Transition = None }
            else
                let bx, by = state.Ball.Position.X, state.Ball.Position.Y
                let attIdx = nearestAttackerIdx bx by attSide
                let att = attSide.Players[attIdx]
                let action = decide att attIdx state

                { State = state
                  Events = []
                  Spawned = [ spawnPlayerActionTick tick.Second 0 action att.Id ]
                  Transition = None }

        | PlayerActionTick(depth, action, attId) ->
            let newState, playerEvents = resolve homeId tick.Second action state

            let att =
                attId
                |> Option.bind (fun pid ->
                    state.HomeSide.Players
                    |> Array.tryFind (fun p -> p.Id = pid)
                    |> Option.orElseWith (fun () -> state.AwaySide.Players |> Array.tryFind (fun p -> p.Id = pid)))

            let bx, by = state.Ball.Position.X, state.Ball.Position.Y
            let defSide = ClubSide.teamSide (ClubSide.flip state.AttackingClub) state
            let def = nearestDefender bx by defSide

            let refState, refEvents, _refActions =
                RefereeAgent.runRefereeStep tick.Second att def newState

            let allEvents = playerEvents @ refEvents

            if hasTerminatingEvent allEvents then
                let transition = transitionFromEvents allEvents

                { State = refState
                  Events = allEvents
                  Spawned = [ spawnNextDuel tick.Second (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                  Transition = transition }
            elif depth < BalanceConfig.AvgChainLength - 1 then
                let attSide = ClubSide.teamSide refState.AttackingClub refState

                if attSide.Players.Length = 0 then
                    { State = refState
                      Events = allEvents
                      Spawned = [ spawnNextDuel tick.Second (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                      Transition = None }
                else
                    let bx', by' = refState.Ball.Position.X, refState.Ball.Position.Y
                    let newAttIdx = nearestAttackerIdx bx' by' attSide
                    let newAtt = attSide.Players[newAttIdx]
                    let newAction = decide newAtt newAttIdx refState

                    { State = refState
                      Events = allEvents
                      Spawned = [ spawnPlayerActionTick tick.Second (depth + 1) newAction newAtt.Id ]
                      Transition = None }
            else
                { State = refState
                  Events = allEvents
                  Spawned = [ spawnNextDuel tick.Second (Stats.delayFrom BalanceConfig.duelNextDelay) ]
                  Transition = None }

        | _ ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }
