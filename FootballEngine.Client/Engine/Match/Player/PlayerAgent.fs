namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module PlayerAgent =

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlayerResult =
        let actionResult = ActionResolver.run tick.SubTick ctx state clock
        { NextTick = None
          Events = actionResult.Events
          Transition = None }
