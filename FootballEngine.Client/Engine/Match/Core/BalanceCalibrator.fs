module BalanceCalibrator

open FootballEngine
open FootballEngine.Domain
    open System.Threading

    let mutable private currentConfig = BalanceConfig.defaultConfig
    let private configLock = obj ()

    let getConfig () =
        lock configLock (fun () -> currentConfig)

    let setConfig cfg =
        lock configLock (fun () -> currentConfig <- cfg)

type CalibrationTargets =
    { GoalsPerMatch: float * float
      Possession: float * float
      PassAccuracy: float * float
      ShotsOnTargetPct: float * float
      HomeWinRate: float * float
      DribblesPerMatch: float * float
      CornersPerMatch: float * float
      FoulsPerMatch: float * float }

module CalibrationTargets =

    let targetsDefault: CalibrationTargets =
        { GoalsPerMatch = (2.0, 3.5)
          Possession = (0.45, 0.55)
          PassAccuracy = (0.75, 0.85)
          ShotsOnTargetPct = (0.30, 0.45)
          HomeWinRate = (0.40, 0.55)
          DribblesPerMatch = (10.0, 30.0)
          CornersPerMatch = (8.0, 12.0)
          FoulsPerMatch = (30.0, 50.0) }

    let targetMid ((a, b): float * float) = (a + b) / 2.0

type MetricDelta =
    { Metric: string
      Observed: float
      TargetMin: float
      TargetMax: float
      DeltaMid: float
      DeltaPct: float
      IsHigh: bool
      IsLow: bool }

module MetricDelta =

    let create (name: string) (obs: float) (min: float) (max: float) : MetricDelta =
        let targetMid = (min + max) / 2.0
        let range = max - min
        let deltaMid = obs - targetMid
        let deltaPct = if range > 0.0 then deltaMid / range else 0.0
        { Metric = name
          Observed = obs
          TargetMin = min
          TargetMax = max
          DeltaMid = deltaMid
          DeltaPct = deltaPct
          IsHigh = obs > max
          IsLow = obs < min }

let compare (m: MatchMetrics) (t: CalibrationTargets) : MetricDelta list =
    let goals = float(m.GoalsHome + m.GoalsAway)
    let poss = m.PossessionHome
    let passAcc = (m.PassAccuracyHome + m.PassAccuracyAway) / 2.0
    let shots = float(m.ShotsHome + m.ShotsAway)
    let shotsOnT = float(m.ShotsOnTargetHome + m.ShotsOnTargetAway)
    let shotPct = if shots > 0.0 then shotsOnT / shots else 0.0
    let homeWin = if m.IsHomeWin then 1.0 else 0.0
    let drib = float(m.DribblesHome + m.DribblesAway)
    let corn = float(m.CornersHome + m.CornersAway)
    let foul = float(m.FoulsHome + m.FoulsAway)

    [ MetricDelta.create "GoalsPerMatch" goals (fst t.GoalsPerMatch) (snd t.GoalsPerMatch)
      MetricDelta.create "PassAccuracy" passAcc (fst t.PassAccuracy) (snd t.PassAccuracy)
      MetricDelta.create "PossessionHome" poss (fst t.Possession) (snd t.Possession)
      MetricDelta.create "ShotsOnTargetPct" shotPct (fst t.ShotsOnTargetPct) (snd t.ShotsOnTargetPct)
      MetricDelta.create "HomeWinRate" homeWin (fst t.HomeWinRate) (snd t.HomeWinRate)
      MetricDelta.create "DribblesPerMatch" drib (fst t.DribblesPerMatch) (snd t.DribblesPerMatch)
      MetricDelta.create "CornersPerMatch" corn (fst t.CornersPerMatch) (snd t.CornersPerMatch)
      MetricDelta.create "FoulsPerMatch" foul (fst t.FoulsPerMatch) (snd t.FoulsPerMatch) ]

let getSignificant (thresh: float) (d: MetricDelta list) : MetricDelta list =
    d |> List.filter (fun x -> abs x.DeltaPct > thresh)

type AdjustableParam =
    | DuelSteepness
    | QualityGate
    | PassBaseMean
    | HomeStrength

type AdjustableConfig =
    { DuelSteepness: float
      QualityGate: float
      PassBaseMean: float
      HomeStrength: float }

module AdjustableConfig =

    let fromConfig (cfg: BalanceConfig) : AdjustableConfig =
        { DuelSteepness = cfg.Duel.DuelSteepness
          QualityGate = cfg.Shot.QualityGate
          PassBaseMean = cfg.Pass.BaseMean
          HomeStrength = cfg.HomeAdvantage.Strength }

    let toConfig (ac: AdjustableConfig) (cfg: BalanceConfig) : BalanceConfig =
        { cfg with
            Duel = { cfg.Duel with DuelSteepness = ac.DuelSteepness }
            Shot = { cfg.Shot with QualityGate = ac.QualityGate }
            Pass = { cfg.Pass with BaseMean = ac.PassBaseMean }
            HomeAdvantage = { cfg.HomeAdvantage with Strength = ac.HomeStrength } }

    let clampParam (p: AdjustableParam) (v: float) : float =
        match p with
        | DuelSteepness -> max 0.5 (min 3.0 v)
        | QualityGate -> max 0.02 (min 0.50 v)
        | PassBaseMean -> max 0.50 (min 0.95 v)
        | HomeStrength -> max 0.0 (min 5.0 v)

    let getValue (ac: AdjustableConfig) (p: AdjustableParam) : float =
        match p with
        | DuelSteepness -> ac.DuelSteepness
        | QualityGate -> ac.QualityGate
        | PassBaseMean -> ac.PassBaseMean
        | HomeStrength -> ac.HomeStrength

    let setValue (ac: AdjustableConfig) (p: AdjustableParam) (v: float) : AdjustableConfig =
        match p with
        | DuelSteepness -> { ac with DuelSteepness = v }
        | QualityGate -> { ac with QualityGate = v }
        | PassBaseMean -> { ac with PassBaseMean = v }
        | HomeStrength -> { ac with HomeStrength = v }

let computeAdvice (deltas: MetricDelta list) : (AdjustableParam * float) list =
    deltas
    |> List.fold (fun acc d ->
        let clamped = max -1.0 (min 1.0 d.DeltaPct)
        match d.Metric with
        | "GoalsPerMatch" | "ShotsOnTargetPct" ->
            let dir = if d.IsHigh then 0.05 elif d.IsLow then -0.05 else 0.0
            if dir <> 0.0 then (QualityGate, dir * abs clamped) :: acc else acc
        | "PassAccuracy" ->
            let dir = if d.IsHigh then -0.05 elif d.IsLow then 0.05 else 0.0
            if dir <> 0.0 then (PassBaseMean, dir * abs clamped) :: acc else acc
        | "HomeWinRate" ->
            let dir = if d.IsHigh then -0.02 elif d.IsLow then 0.02 else 0.0
            if dir <> 0.0 then (HomeStrength, dir * abs clamped) :: acc else acc
        | "DribblesPerMatch" ->
            let dir = if d.IsHigh then 0.1 elif d.IsLow then -0.1 else 0.0
            if dir <> 0.0 then (DuelSteepness, dir * abs clamped) :: acc else acc
        | _ -> acc) []

let calibrateOnce
    (config: BalanceConfig)
    (metrics: MatchMetrics)
    (targets: CalibrationTargets)
    (momentum: float)
    : BalanceConfig =

    let deltas = compare metrics targets
    let important = getSignificant 0.15 deltas

    if List.isEmpty important then config
    else
        let advice = computeAdvice important
        if List.isEmpty advice then config
        else
            let mutable ac = AdjustableConfig.fromConfig config
            for (param, delta) in advice do
                let current = AdjustableConfig.getValue ac param
                let newVal = current + delta * momentum
                let clamped = AdjustableConfig.clampParam param newVal
                ac <- AdjustableConfig.setValue ac param clamped
            let newConfig = AdjustableConfig.toConfig ac config
            setConfig newConfig
            newConfig

let calibrateWithBuffer
    (config: BalanceConfig)
    (metrics: MatchMetrics list)
    (targets: CalibrationTargets)
    (momentum: float)
    (minSamples: int)
    : BalanceConfig =

    if List.length metrics < minSamples then config
    else
        let avg = MatchMetrics.average metrics
        calibrateOnce config avg targets momentum