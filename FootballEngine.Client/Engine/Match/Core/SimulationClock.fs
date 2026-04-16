namespace FootballEngine

open PhysicsContract

type SimulationClock =
    { SubTicksPerSecond: int
      PhysicsRate: int
      SteeringRate: int
      CognitiveRate: int
      AdaptiveRate: int }

module SimulationClock =
    let defaultClock =
        let sps = 40
        { SubTicksPerSecond = sps
          PhysicsRate = sps / 4
          SteeringRate = sps * 1
          CognitiveRate = sps * 5
          AdaptiveRate = sps * 60 }

    let dt (c: SimulationClock) : float<second> = float c.PhysicsRate / float c.SubTicksPerSecond * 1.0<second>

    let dtPlayer (c: SimulationClock) : float<second> = float c.SteeringRate / float c.SubTicksPerSecond * 1.0<second>

    let secondsToSubTicks (c: SimulationClock) (s: float) = int (s * float c.SubTicksPerSecond)

    let subTicksToSeconds (c: SimulationClock) (t: int) = float t / float c.SubTicksPerSecond

    let halfTime (c: SimulationClock) = secondsToSubTicks c (45.0 * 60.0)

    let fullTime (c: SimulationClock) = secondsToSubTicks c (95.0 * 60.0)