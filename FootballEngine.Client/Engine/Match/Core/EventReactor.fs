namespace FootballEngine

open FootballEngine.Domain

module EventPipeline =

    type EventReactor = ResizeArray<MatchEvent> -> MatchContext -> SimState -> int -> unit

    let refereeReactor: EventReactor =
        fun acc ctx state subTick ->
            for i = 0 to acc.Count - 1 do
                if acc[i].Type = MatchEventType.FoulCommitted then
                    match SimStateOps.findActivePlayer ctx state acc[i].PlayerId with
                    | Some fouler ->
                        let actions = RefereeAgent.decideCard fouler ctx state
                        for a in actions do
                            acc.AddRange(RefereeApplicator.apply subTick a ctx state)
                    | None -> ()

    let reactors: EventReactor list = [ refereeReactor ]

    let run (events: MatchEvent list) (ctx: MatchContext) (state: SimState) (subTick: int) : MatchEvent list =
        let acc = System.Collections.Generic.List<MatchEvent>(events)

        for reactor in reactors do
            reactor acc ctx state subTick

        let mutable result = []
        for i = acc.Count - 1 downto 0 do
            result <- acc[i] :: result
        result
