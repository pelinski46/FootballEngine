namespace Training

open FootballEngine
open FootballEngine.Domain
open FootballEngine.ML
open FootballEngine.Simulation

module Validator =

    let validate
        (validationMatches: int)
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (profileMap: Map<PlayerId, BehavioralProfile>)
        (targets: Training.CalibrationTargets)
        (originalWeights: EngineWeights)
        (candidateWeights: EngineWeights)
        (originalError: float)
        : Result<EngineWeights, string> =
        let _config = WeightsLoader.toBalanceConfig candidateWeights

        let results =
            SimulatorRunner.runBatch validationMatches home away players staff profileMap

        if List.isEmpty results then
            Error "Validator: no matches simulated"
        else
            let matchMetrics, _ = MetricsAggregator.aggregate results
            let newError, _ = ErrorCalculator.computeError matchMetrics targets

            if newError < originalError then
                Ok candidateWeights
            else
                Error (sprintf "Validation failed: original error %.4f, new error %.4f" originalError newError)
