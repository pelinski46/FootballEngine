namespace FootballEngine

open SchedulingTypes

module PlayerAgent =

    let agent (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlayerResult =
        let actionResult = ActionResolver.run state.SubTick ctx state clock

        { Events = actionResult.Events
          Transition = None }
