namespace FootballEngine

open FootballEngine.Domain

module EventPipeline =

    /// A reactor takes the current accumulated events, match context, state, and subTick,
    /// and returns additional events to append. It never removes or modifies existing events.
    type EventReactor = MatchEvent list -> MatchContext -> SimState -> int -> MatchEvent list

    /// Referee reactor: detects fouls in the event list and appends card events.
    let refereeReactor: EventReactor =
        fun events ctx state subTick ->
            let foulEvents =
                events |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted)

            foulEvents
            |> List.collect (fun fe ->
                match SimStateOps.findActivePlayer state fe.PlayerId with
                | Some fouler ->
                    RefereeAgent.decideCard fouler ctx state
                    |> List.collect (fun a -> RefereeAgent.resolve subTick a ctx state)
                | None -> [])

    /// The ordered list of reactors. Add new reactors here — order matters.
    /// Each reactor receives the full accumulated event list so it can react to
    /// events added by previous reactors.
    let reactors: EventReactor list = [ refereeReactor ]

    /// Run all reactors in order, accumulating events.
    /// Each reactor sees the events produced by all previous reactors.
    let run (events: MatchEvent list) (ctx: MatchContext) (state: SimState) (subTick: int) : MatchEvent list =
        let acc = System.Collections.Generic.List<MatchEvent>(events)

        for reactor in reactors do
            let current = List.ofSeq acc
            let newEvents = reactor current ctx state subTick
            acc.AddRange(newEvents)

        let result = List.ofSeq acc
        result
