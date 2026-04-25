namespace FootballEngine

/// Represents a probability value in [0.0, 1.0].
/// Use Probability.from to construct — the compiler prevents passing raw floats.
[<Struct>]
type Probability = private { Value: float }

module Probability =
    /// Safe constructor — clamps to [0, 1]
    let from (v: float) : Probability = { Value = PhysicsContract.clampFloat v 0.0 1.0 }

    /// For compile-time known constants only
    let unsafe (v: float) : Probability = { Value = v }

    let value (p: Probability) : float = p.Value

    let zero = { Value = 0.0 }
    let one  = { Value = 1.0 }
    let half = { Value = 0.5 }
