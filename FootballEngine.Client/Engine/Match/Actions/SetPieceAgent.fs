namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module SetPieceAgent =

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | FreeKickTick(_kickerId, _position, _chainDepth) ->
            let newState, events = SetPlayAction.resolveFreeKick tick.Second state
            { State = newState
              Events = events
              Spawned =
                  [ { Second = tick.Second + Stats.delayFrom BalanceConfig.freeKickDelay
                      Priority = TickPriority.Duel
                      SequenceId = 0L
                      Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | CornerTick(_club, _chainDepth) ->
            let newState, events = SetPlayAction.resolveCorner tick.Second state
            { State = newState
              Events = events
              Spawned =
                  [ { Second = tick.Second + Stats.delayFrom BalanceConfig.cornerDelay
                      Priority = TickPriority.Duel
                      SequenceId = 0L
                      Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | ThrowInTick(team, _chainDepth) ->
            let newState, events = SetPlayAction.resolveThrowIn tick.Second state team
            { State = newState
              Events = events
              Spawned =
                  [ { Second = tick.Second + Stats.delayFrom BalanceConfig.throwInDelay
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
            let newState, events = SetPlayAction.resolvePenalty tick.Second state kickerPlayer kickClub 1
            { State = newState
              Events = events
              Spawned =
                  [ { Second = tick.Second + Stats.delayFrom BalanceConfig.foulDelay
                      Priority = TickPriority.Duel
                      SequenceId = 0L
                      Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | GoalKickTick ->
            { State = state; Events = []; Spawned = []; Transition = None }
        | KickOffTick ->
            { State = state; Events = []; Spawned = []; Transition = None }
        | _ ->
            { State = state; Events = []; Spawned = []; Transition = None }
