namespace FootballEngine

module MatchRates =

    let inline shouldRun (rate: int) (subTick: int) =
        rate > 0 && subTick % rate = 0

    let physics (clock: SimulationClock) subTick =
        shouldRun clock.PhysicsRate subTick

    let steering (clock: SimulationClock) subTick =
        shouldRun clock.SteeringRate subTick

    let cognition (clock: SimulationClock) subTick =
        shouldRun clock.CognitiveRate subTick

    let action (clock: SimulationClock) subTick =
        shouldRun clock.ActionRate subTick

    let adaptive (clock: SimulationClock) subTick =
        shouldRun clock.AdaptiveRate subTick

    let referee (clock: SimulationClock) subTick =
        shouldRun clock.PhysicsRate subTick
