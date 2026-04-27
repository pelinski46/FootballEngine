namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SimStateOps
open FootballEngine.MatchSpatial
open SchedulingTypes
open SimulationClock

module TickOrchestrator =

    type PendingRequests =
        { LastScheduledSubTick: int
          LastScheduledKind: TickKind option }

    let empty =
        { LastScheduledSubTick = -9999
          LastScheduledKind = None }

    let private isDuplicate (subTick: int) (kind: TickKind) (pending: PendingRequests) : bool =
        pending.LastScheduledSubTick = subTick
        && pending.LastScheduledKind = Some kind

    let trySchedule
        (subTick: int)
        (kind: TickKind)
        (priority: TickPriority)
        (counter: int64)
        (pending: PendingRequests)
        : (int64 * PendingRequests) voption =

        if isDuplicate subTick kind pending then
            ValueNone
        else
            ValueSome(counter, { LastScheduledSubTick = subTick; LastScheduledKind = Some kind })

    let resolve
        (tick: ScheduledTick)
        (intent: TickIntent)
        (playerNextTick: ScheduledTick option)
        (transition: PlayState option)
        (counter: int64)
        (clock: SimulationClock)
        (config: BalanceConfig)
        (ctx: MatchContext)
        (state: SimState)
        (pending: PendingRequests)
        : ScheduledTick list * int64 * PendingRequests =

        let resetPending () =
            { LastScheduledSubTick = -9999; LastScheduledKind = None }

        let matchLevelTicks, newPending =
            match intent with
            | NoOp -> [], pending

            | ScheduleInjury(playerId, severity) ->
                [ { SubTick = tick.SubTick + 1
                    Priority = TickPriority.Referee
                    SequenceId = counter
                    Kind = InjuryTick(playerId, severity) } ],
                { pending with LastScheduledSubTick = tick.SubTick + 1; LastScheduledKind = Some(InjuryTick(playerId, severity)) }

            | ScheduleSubstitution(clubId) ->
                [ { SubTick = tick.SubTick + 1
                    Priority = TickPriority.Manager
                    SequenceId = counter
                    Kind = SubstitutionTick clubId } ],
                { pending with LastScheduledSubTick = tick.SubTick + 1; LastScheduledKind = Some(SubstitutionTick clubId) }

            | StopPlay reason ->
                match reason with
                | Goal ->
                    [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.GoalDelay
                        Priority = TickPriority.MatchControl
                        SequenceId = counter
                        Kind = KickOffTick } ],
                    resetPending ()

                | Foul ->
                    let bx, by = state.Ball.Position.X, state.Ball.Position.Y
                    let attFrame = getFrame state state.AttackingSide
                    match nearestActiveSlotInFrame attFrame bx by with
                    | ValueSome idx ->
                        let attRoster = getRoster ctx state.AttackingSide
                        match tryGetPlayerFromFrame attFrame attRoster idx with
                        | Some player ->
                            let foulPos = defaultSpatial bx by
                            [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.FoulDelay
                                Priority = TickPriority.SetPiece
                                SequenceId = counter
                                Kind = SetPieceTick(SetPieceKind.FreeKick, state.AttackingSide) } ],
                            resetPending ()
                        | None -> [], resetPending ()
                    | ValueNone -> [], resetPending ()

                | BallOut ->
                    [], resetPending ()

                | Injury ->
                    [ { SubTick = tick.SubTick + TickDelay.delayFrom clock config.Timing.InjuryDelay
                        Priority = TickPriority.MatchControl
                        SequenceId = counter
                        Kind = ResumePlayTick } ],
                    resetPending ()

        let allTicks =
            match playerNextTick with
            | Some nt when not (isDuplicate nt.SubTick nt.Kind newPending) -> nt :: matchLevelTicks
            | _ -> matchLevelTicks

        let newCounter = counter + int64 allTicks.Length

        allTicks, newCounter, newPending
