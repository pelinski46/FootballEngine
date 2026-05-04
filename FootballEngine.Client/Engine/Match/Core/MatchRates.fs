namespace FootballEngine

open FootballEngine.Types

module MatchRates =

    let inline shouldRun (rate: int) (subTick: int) = rate > 0 && subTick % rate = 0

    let physics (clock: SimulationClock) subTick = shouldRun clock.PhysicsRate subTick

    let steering (clock: SimulationClock) subTick = shouldRun clock.SteeringRate subTick

    let cognition (clock: SimulationClock) subTick = shouldRun clock.CognitiveRate subTick

    let action (clock: SimulationClock) subTick = shouldRun clock.ActionRate subTick

    let reactive (clock: SimulationClock) subTick = shouldRun clock.ReactiveRate subTick

    let strategic (clock: SimulationClock) subTick = shouldRun clock.StrategicRate subTick

    let referee (clock: SimulationClock) subTick = shouldRun clock.PhysicsRate subTick
