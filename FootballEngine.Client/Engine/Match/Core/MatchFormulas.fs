namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open FootballEngine.PhysicsContract

module MatchFormulas =



    let sigmoid x = 1.0 / (1.0 + exp (-x))

    let inline effectiveStat (stat: int) (condition: int) (morale: int) (weight: float) =
        let normStat = float stat / 20.0
        let normCond = float condition / 100.0
        let normMorale = float morale / 100.0

        // Curva de rendimiento: el stat base se procesa de forma no lineal
        // k=10.0 controla la agresividad de la curva, x0=0.5 es el punto de inflexión
        let rawValue = (normStat * 0.75) + (normCond * 0.15) + (normMorale * 0.10)
        let scaled = sigmoid (10.0 * (rawValue - 0.5))

        Math.Max(0.01, scaled * weight)

    let physicalVariation (cond: int) = 0.85 + (float cond / 100.0) * 0.30 // Retorno de varianza entre 0.85 y 1.15

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
        let dx = x1 - x2
        let dy = y1 - y2
        sqrt (dx * dx + dy * dy)

    let inline distanceSq (x1, y1) (x2, y2) =
        let dx = x1 - x2
        let dy = y1 - y2
        dx * dx + dy * dy

    let inline nearestIdx (positions: (float * float)[]) (point: float * float) =
        positions
        |> Array.mapi (fun i pos -> i, distance point pos)
        |> Array.minBy snd
        |> fst

    let jitter (oX: float<meter>) (oY: float<meter>) (tX: float<meter>) (tY: float<meter>) scale nx ny =
        PhysicsContract.clamp
            (oX + (tX - oX) * scale + (normalSample 0.0 nx) * 1.0<meter>)
            0.0<meter>
            PhysicsContract.PitchLength,
        PhysicsContract.clamp
            (oY + (tY - oY) * scale + (normalSample 0.0 ny) * 1.0<meter>)
            0.0<meter>
            PhysicsContract.PitchWidth

    type PlayerRole =
        | Defender
        | Midfielder
        | Attacker
        | Goalkeeper

    let playerRole (profile: BehavioralProfile) (position: Position) =
        if profile.DefensiveHeight > 0.6 && profile.Directness < 0.3 then
            Defender
        elif profile.AttackingDepth > 0.7 && profile.CreativityWeight < 0.3 then
            Attacker
        elif position = GK then
            Goalkeeper
        else
            Midfielder

module MovementConstants =

    let movementCoefficients (profile: BehavioralProfile) =
        let offensive =
            profile.AttackingDepth * 0.30
            + profile.Directness * 0.25
            + profile.PositionalFreedom * 0.20

        let defensive =
            profile.DefensiveHeight * 0.35
            + profile.PressingIntensity * 0.25
            + (1.0 - profile.PositionalFreedom) * 0.25

        let lateral =
            abs (profile.LateralTendency - 0.5) * 0.50 + profile.PositionalFreedom * 0.25

        (offensive, defensive, lateral)
