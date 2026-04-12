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

    let playerRole (profile: BehavioralProfile) (position: Position) =
        if profile.DefensiveHeight > 0.6 && profile.Directness < 0.3 then Defender
        elif profile.AttackingDepth > 0.7 && profile.CreativityWeight < 0.3 then Attacker
        elif position = Domain.GK then Goalkeeper
        else Midfielder

module MovementConstants =

    let movementCoefficients (profile: BehavioralProfile) =
        let offensive = profile.AttackingDepth * 0.30 + profile.Directness * 0.25 + profile.PositionalFreedom * 0.20
        let defensive = profile.DefensiveHeight * 0.35 + profile.PressingIntensity * 0.25 + (1.0 - profile.PositionalFreedom) * 0.25
        let lateral = abs (profile.LateralTendency - 0.5) * 0.50 + profile.PositionalFreedom * 0.25
        (offensive, defensive, lateral)
