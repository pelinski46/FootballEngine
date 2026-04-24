namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module PlayerAgent =

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentResult =
        let actionEvents = ActionResolver.run tick.SubTick ctx state clock
        { Intent =
            { Movement = MovementIntent.MaintainShape state.Ball.Position
              Action = None
              Context = NormalPlay
              Urgency = 0.0
              Confidence = 0.5 }
          NextTick = None
          Events = actionEvents
          Transition = None }
