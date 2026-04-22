namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SimStateOps
open FootballEngine.MatchSpatial
open SchedulingTypes
open SimulationClock

module TickOrchestrator =

    type PendingRequests =
        { HasPendingDuel: bool
          HasPendingDecision: bool
          PendingDecisionPid: PlayerId option
          LastDuelScheduledAt: int
          LastDecisionScheduledAt: int }

    let empty =
        { HasPendingDuel = false
          HasPendingDecision = false
          PendingDecisionPid = None
          LastDuelScheduledAt = -9999
          LastDecisionScheduledAt = -9999 }

    let private scheduleDuel
        (currentSubTick: int)
        (counter: int64)
        (clock: SimulationClock)
        (config: BalanceConfig)
        (pending: PendingRequests)
        : ScheduledTick list * int64 * PendingRequests =
        let delay = TickDelay.delayFrom clock config.Timing.DuelNextDelay
        let ticks =
            [ { SubTick = currentSubTick + delay
                Priority = TickPriority.Duel
                SequenceId = counter
                Kind = DuelTick 0 } ]
        ticks, counter + 1L,
            { pending with
                HasPendingDuel = true
                LastDuelScheduledAt = currentSubTick + delay }

    let private scheduleDecision
        (pid: PlayerId)
        (currentSubTick: int)
        (counter: int64)
        (clock: SimulationClock)
        (config: BalanceConfig)
        (pending: PendingRequests)
        : ScheduledTick list * int64 * PendingRequests =
        if pending.HasPendingDecision && pending.PendingDecisionPid = Some pid then
            [], counter, pending
        else
            let delay = TickDelay.delayFrom clock config.Timing.DuelChainDelay
            let ticks =
                [ { SubTick = currentSubTick + delay
                    Priority = TickPriority.Duel
                    SequenceId = counter
                    Kind = DecisionTick(0, Some pid) } ]
            ticks, counter + 1L,
                { pending with
                    HasPendingDecision = true
                    PendingDecisionPid = Some pid
                    LastDecisionScheduledAt = currentSubTick + delay }

    let private scheduleSetPieceTick
        (kind: SetPieceKind)
        (club: ClubSide)
        (currentSubTick: int)
        (counter: int64)
        : ScheduledTick list * int64 =
        let tickKind =
            match kind with
            | SetPieceKind.Corner -> CornerTick(club, 0)
            | SetPieceKind.ThrowIn -> ThrowInTick(club, 0)
            | SetPieceKind.GoalKick -> GoalKickTick
            | SetPieceKind.KickOff -> KickOffTick
            | SetPieceKind.FreeKick -> failwith "Use ScheduleFreeKick for free kicks"
            | SetPieceKind.Penalty -> failwith "Penalties are handled separately"
        let ticks =
            [ { SubTick = currentSubTick + 1
                Priority = TickPriority.SetPiece
                SequenceId = counter
                Kind = tickKind } ]
        ticks, counter + 1L

    let resolve
        (tick: ScheduledTick)
        (intent: TickIntent)
        (transition: PlayState option)
        (counter: int64)
        (clock: SimulationClock)
        (config: BalanceConfig)
        (state: SimState)
        (pending: PendingRequests)
        : ScheduledTick list * int64 * PendingRequests =

        let resetPending newPending =
            { newPending with
                HasPendingDuel = false
                HasPendingDecision = false
                PendingDecisionPid = None }

        match intent with
        | NoOp ->
            [], counter, pending

        | FindNextDuel ->
            scheduleDuel tick.SubTick counter clock config (resetPending pending)

        | GiveDecisionTo pid ->
            scheduleDecision pid tick.SubTick counter clock config (resetPending pending)

        | ScheduleSetPiece(kind, club) ->
            let ticks, newCounter = scheduleSetPieceTick kind club tick.SubTick counter
            ticks, newCounter, resetPending pending

        | ScheduleFreeKick(kicker, position) ->
            let ticks =
                [ { SubTick = tick.SubTick + 1
                    Priority = TickPriority.SetPiece
                    SequenceId = counter
                    Kind = FreeKickTick(kicker, position, 0) } ]
            ticks, counter + 1L, resetPending pending

        | ScheduleInjury(playerId, severity) ->
            let ticks =
                [ { SubTick = tick.SubTick + 1
                    Priority = TickPriority.Referee
                    SequenceId = counter
                    Kind = InjuryTick(playerId, severity) } ]
            ticks, counter + 1L, pending

        | ScheduleSubstitution(clubId) ->
            let ticks =
                [ { SubTick = tick.SubTick + 1
                    Priority = TickPriority.Manager
                    SequenceId = counter
                    Kind = SubstitutionTick clubId } ]
            ticks, counter + 1L, pending

        | StopPlay reason ->
            match reason with
            | Goal ->
                let ticks =
                    [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.GoalDelay
                        Priority = TickPriority.MatchControl
                        SequenceId = counter
                        Kind = KickOffTick } ]
                ticks, counter + 1L, resetPending pending
            | Foul ->
                let bx, by = state.Ball.Position.X, state.Ball.Position.Y
                let attSlots = getSlots state state.AttackingSide
                match nearestActiveSlot attSlots bx by with
                | ValueSome s ->
                    let foulPos = defaultSpatial bx by
                    let ticks =
                        [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.FoulDelay
                            Priority = TickPriority.SetPiece
                            SequenceId = counter
                            Kind = FreeKickTick(s.Player.Id, foulPos, 0) } ]
                    ticks, counter + 1L, resetPending pending
                | ValueNone ->
                    scheduleDuel tick.SubTick counter clock config (resetPending pending)
            | BallOut ->
                scheduleDuel tick.SubTick counter clock config (resetPending pending)
            | Injury ->
                let ticks =
                    [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.InjuryDelay
                        Priority = TickPriority.MatchControl
                        SequenceId = counter
                        Kind = ResumePlayTick } ]
                ticks, counter + 1L, resetPending pending

        | ResumeAfter(delay, kind) ->
            let ticks =
                [ { SubTick = tick.SubTick + TickDelay.delayFrom clock delay
                    Priority =
                        match kind with
                        | KickOffTick -> TickPriority.MatchControl
                        | ResumePlayTick -> TickPriority.MatchControl
                        | _ -> TickPriority.SetPiece
                    SequenceId = counter
                    Kind = kind } ]
            ticks, counter + 1L, resetPending pending
