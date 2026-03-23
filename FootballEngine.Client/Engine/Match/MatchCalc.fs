namespace FootballEngine


open FootballEngine.Domain
open FSharp.Stats.Distributions

module MatchStats =


    let inline effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let base' = float stat * (float condition / 100.0) * (0.8 + float morale / 500.0)
        Continuous.Normal.Sample base' sigma

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
