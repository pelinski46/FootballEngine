namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FSharp.Stats.Distributions
open MatchContext

/// Pure functions for computing effective stats and effort scores.
/// No side-effects, no mutable state — easy to unit-test in isolation.
module MatchStats =

    // ------------------------------------------------------------------ //
    //  Core sampling                                                       //
    // ------------------------------------------------------------------ //

    let nextNormalInt (mean: float) (stdDev: float) (lo: int) (hi: int) =
        let sample = Continuous.Normal.Sample mean stdDev
        Math.Clamp(int (Math.Round(sample)), lo, hi)

    /// Applies condition and morale degradation to a raw stat, then adds
    /// Gaussian noise with the given sigma.
    let inline effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let baseValue =
            float stat * (float condition / 100.0) * (0.8 + (float morale / 500.0))

        Continuous.Normal.Sample baseValue sigma

    // ------------------------------------------------------------------ //
    //  Effort formulae                                                     //
    // ------------------------------------------------------------------ //

    let attackEffort (phase: MatchPhase) (att: Player) (cond: int) =
        match phase with
        | BuildUp ->
            effectiveStat att.Technical.Passing cond att.Morale 2.0
            + effectiveStat att.Mental.Vision cond att.Morale 1.5
            + effectiveStat att.Mental.Composure cond att.Morale 1.0
        | Midfield ->
            effectiveStat att.Technical.BallControl cond att.Morale 2.0
            + effectiveStat att.Mental.Vision cond att.Morale 2.0
            + effectiveStat att.Technical.Dribbling cond att.Morale 1.5
        | Attack ->
            effectiveStat att.Technical.Dribbling cond att.Morale 2.5
            + effectiveStat att.Physical.Pace cond att.Morale 2.0
            + effectiveStat att.Physical.Agility cond att.Morale 1.5

    let defenseEffort (def: Player) (cond: int) =
        effectiveStat def.Technical.Tackling cond def.Morale 2.0
        + effectiveStat def.Mental.Positioning cond def.Morale 2.0
        + effectiveStat def.Physical.Strength cond def.Morale 1.0
        + effectiveStat def.Mental.Concentration cond def.Morale 1.0

    // ------------------------------------------------------------------ //
    //  Position predicates                                                 //
    // ------------------------------------------------------------------ //

    let isDefender (p: Player) =
        match p.Position with
        | DC
        | DL
        | DR
        | DM -> true
        | _ -> false

    let isMidfielder (p: Player) =
        match p.Position with
        | MC
        | AMC
        | AML
        | AMR -> true
        | _ -> false

    let isAttacker (p: Player) =
        match p.Position with
        | ST
        | AML
        | AMR -> true
        | _ -> false

    // ------------------------------------------------------------------ //
    //  Spatial helpers                                                     //
    // ------------------------------------------------------------------ //

    let inline distance (x1, y1) (x2, y2) =
        sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)

    /// Index of the player closest to a given point.
    let inline nearestIdx (positions: (float * float)[]) (point: float * float) =
        positions
        |> Array.mapi (fun i pos -> i, distance point pos)
        |> Array.minBy snd
        |> fst
