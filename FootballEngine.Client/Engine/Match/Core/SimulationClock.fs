namespace FootballEngine

open PhysicsContract

type SimulationClock =
    { SubTicksPerSecond: int
      PhysicsRate: int
      SteeringRate: int
      CognitiveRate: int
      ActionRate: int
      AdaptiveRate: int }

module SimulationClock =
    let defaultClock =
        let sps = 40

        { SubTicksPerSecond = sps
          PhysicsRate = 1
          SteeringRate = 2
          CognitiveRate = 40
          ActionRate = 8
          AdaptiveRate = 1200 }

    let dt (c: SimulationClock) : float<second> =
        float c.PhysicsRate / float c.SubTicksPerSecond * 1.0<second>

    let dtPlayer (c: SimulationClock) : float<second> =
        float c.SteeringRate / float c.SubTicksPerSecond * 1.0<second>

    let secondsToSubTicks (c: SimulationClock) (s: float) = int (s * float c.SubTicksPerSecond)

    let subTicksToSeconds (c: SimulationClock) (t: int) = float t / float c.SubTicksPerSecond

    let halfTime (c: SimulationClock) = secondsToSubTicks c (45.0 * 60.0)

    let fullTime (c: SimulationClock) = secondsToSubTicks c (95.0 * 60.0)

type TickDelay =
    { MeanST: int
      StdST: int
      MinST: int
      MaxST: int }

module TickDelay =
    let ofSeconds (clock: SimulationClock) mean std min max =
        { MeanST = SimulationClock.secondsToSubTicks clock mean
          StdST = SimulationClock.secondsToSubTicks clock std
          MinST = SimulationClock.secondsToSubTicks clock min
          MaxST = SimulationClock.secondsToSubTicks clock max }

    let delayFrom (clock: SimulationClock) (d: TickDelay) : int =
        Stats.normalInt (float d.MeanST) (float d.StdST) d.MinST d.MaxST
