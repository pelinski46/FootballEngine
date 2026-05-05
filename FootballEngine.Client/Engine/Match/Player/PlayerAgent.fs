namespace FootballEngine

open FootballEngine.Types
open FootballEngine.Types.SchedulingTypes

module PlayerAgent =

    let agent (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlayerResult =
        let actionResult, pendingRefereeActions = ActionResolver.run state.SubTick ctx state clock

        { Events = actionResult.Events
          Transition = None
          PendingRefereeActions = pendingRefereeActions }
