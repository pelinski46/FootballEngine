namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module SetPieceAgent =

    // Phase 0: all tick times use SubTick via BalanceConfig delays
    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | FreeKickTick(_kickerId, _position, _chainDepth) ->
            let newState, events = SetPlayAction.resolveFreeKick tick.SubTick state
            { State = newState
              Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.freeKickDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | CornerTick(_club, _chainDepth) ->
            let newState, events = SetPlayAction.resolveCorner tick.SubTick state
            { State = newState
              Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.cornerDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | ThrowInTick(team, _chainDepth) ->
            let newState, events = SetPlayAction.resolveThrowIn tick.SubTick state team
            { State = newState
              Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.throwInDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | PenaltyTick(kicker, isHome) ->
            let kickerPlayer =
                (if isHome then state.HomeSide else state.AwaySide).Players
                |> Array.tryFind (fun p -> p.Id = kicker)
                |> Option.defaultWith (fun () -> state.HomeSide.Players.[0])
            let kickClub = if isHome then HomeClub else AwayClub
            let newState, events = SetPlayAction.resolvePenalty tick.SubTick state kickerPlayer kickClub 1
            { State = newState
              Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.foulDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | GoalKickTick | KickOffTick | _ ->
            { State = state; Events = []; Spawned = []; Transition = None }
