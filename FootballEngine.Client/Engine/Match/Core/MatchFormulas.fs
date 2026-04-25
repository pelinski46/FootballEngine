namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open FootballEngine.PhysicsContract

module MatchFormulas =



    let sigmoid x = 1.0 / (1.0 + exp (-x))

    let physicalVariation (cond: int) = 0.85 + (float cond / 100.0) * 0.30 // Retorno de varianza entre 0.85 y 1.15

    let attackEffort (phase: MatchPhase) (att: Player) (cond: int) =
        match phase with
        | BuildUp ->
            ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Technical.Passing cond att.Morale * 2.0
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Mental.Vision cond att.Morale * 1.5
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Mental.Composure cond att.Morale * 1.0
        | Midfield ->
            ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Technical.BallControl cond att.Morale * 2.0
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Mental.Vision cond att.Morale * 2.0
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Technical.Dribbling cond att.Morale * 1.5
        | Attack ->
            ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Technical.Dribbling cond att.Morale * 2.5
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Physical.Pace cond att.Morale * 2.0
            + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig att.Physical.Agility cond att.Morale * 1.5

    let defenseEffort (def: Player) (cond: int) =
        ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig def.Technical.Tackling cond def.Morale * 2.0
        + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig def.Mental.Positioning cond def.Morale * 2.0
        + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig def.Physical.Strength cond def.Morale * 1.0
        + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig def.Mental.Concentration cond def.Morale * 1.0

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
