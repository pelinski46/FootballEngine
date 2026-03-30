namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats

module MatchCalc =

    let inline effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let base' = float stat * (float condition / 100.0) * (0.8 + float morale / 500.0)
        normalSample base' (sigma * 0.6)

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

module PitchMath =

    let inline distance (x1, y1) (x2, y2) =
        sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)

    let inline nearestIdx (positions: (float * float)[]) (point: float * float) =
        positions
        |> Array.mapi (fun i pos -> i, distance point pos)
        |> Array.minBy snd
        |> fst

    let jitter oX oY tX tY scale nx ny =
        Math.Clamp(oX + (tX - oX) * scale + normalSample 0.0 nx, 0.0, 100.0),
        Math.Clamp(oY + (tY - oY) * scale + normalSample 0.0 ny, 0.0, 100.0)

    type PlayerRole =
        | Defender
        | Midfielder
        | Attacker
        | Goalkeeper

    let playerRole (p: Player) =
        match p.Position with
        | GK -> Goalkeeper
        | DC
        | DL
        | DR
        | DM
        | WBL
        | WBR -> Defender
        | MC
        | ML
        | MR
        | AML
        | AMR
        | AMC -> Midfielder
        | ST -> Attacker
