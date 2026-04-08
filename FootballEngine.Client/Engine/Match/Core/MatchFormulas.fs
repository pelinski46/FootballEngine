namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats

module MatchFormulas =

    let inline effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let normStat = PhysicsContract.normaliseAttr stat
        let normCond = PhysicsContract.normaliseCondition condition
        let normMorale = PhysicsContract.normaliseCondition morale
        let base' = normStat * normCond * (0.8 + normMorale / 2.5)
        let sample = normalSample base' (sigma * 0.1) // Reduced sigma for stability
        Math.Max(0.05, sample)

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

    let inline distanceSq (x1, y1) (x2, y2) =
        (x1 - x2) ** 2.0 + (y1 - y2) ** 2.0

    let inline nearestIdx (positions: (float * float)[]) (point: float * float) =
        positions
        |> Array.mapi (fun i pos -> i, distance point pos)
        |> Array.minBy snd
        |> fst

    let jitter oX oY tX tY scale nx ny =
        Math.Clamp(oX + (tX - oX) * scale + normalSample 0.0 nx, 0.0, PhysicsContract.PitchLength),
        Math.Clamp(oY + (tY - oY) * scale + normalSample 0.0 ny, 0.0, PhysicsContract.PitchWidth)

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

module MovementConstants =

    let positionCoefficients =
        function
        | GK -> 0.04, 0.02, 0.03
        | DC -> 0.10, 0.06, 0.15
        | DL | DR -> 0.18, 0.08, 0.30
        | WBL | WBR -> 0.25, 0.10, 0.38
        | DM -> 0.20, 0.12, 0.25
        | MC -> 0.28, 0.18, 0.28
        | ML | MR -> 0.28, 0.15, 0.38
        | AML | AMR | AMC -> 0.38, 0.22, 0.32
        | ST -> 0.42, 0.28, 0.18

    let positionModifiers =
        function
        | GK -> (fun (off, def, lat) (pos, wr, vis) -> off, def, lat)
        | DC -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.08, def + wr * 0.05, lat + vis * 0.08)
        | DL | DR -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.10, def + wr * 0.06, lat + vis * 0.12)
        | WBL | WBR -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.12, def + wr * 0.08, lat + vis * 0.10)
        | DM -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.10, def + wr * 0.08, lat + vis * 0.10)
        | MC -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.12, def + wr * 0.10, lat + vis * 0.12)
        | ML | MR -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.10, def + wr * 0.08, lat + vis * 0.12)
        | AML | AMR | AMC -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.12, def + wr * 0.08, lat + vis * 0.10)
        | ST -> (fun (off, def, lat) (pos, wr, vis) -> off + pos * 0.10, def + wr * 0.06, lat + vis * 0.08)
