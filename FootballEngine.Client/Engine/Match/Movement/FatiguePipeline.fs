namespace FootballEngine.Movement

open FootballEngine.Domain

module FatiguePipeline =

    let private sigmoid (x: float) =
        1.0 / (1.0 + System.Math.Exp(-x))

    let effectiveVisionFromCondition baseVision condition =
        let normalizedVision = float baseVision / 100.0
        let normalizedCondition = float condition / 100.0
        let threshold = 0.7
        let decay =
            if normalizedCondition >= threshold then
                1.0
            else
                let deficit = threshold - normalizedCondition
                1.0 - sigmoid (deficit * 10.0 - 2.0) * 0.6
        normalizedVision * decay

    let effectivePositioningFromCondition basePositioning condition =
        let normalizedPositioning = float basePositioning / 100.0
        let normalizedCondition = float condition / 100.0
        let threshold = 0.55
        let decay =
            if normalizedCondition >= threshold then
                1.0
            else
                let deficit = threshold - normalizedCondition
                1.0 - sigmoid (deficit * 12.0 - 2.0) * 0.7
        normalizedPositioning * decay

    let effectiveComposureFromCondition baseComposure condition =
        let normalizedComposure = float baseComposure / 100.0
        let normalizedCondition = float condition / 100.0
        let threshold = 0.4
        let decay =
            if normalizedCondition >= threshold then
                1.0
            else
                let deficit = threshold - normalizedCondition
                1.0 - sigmoid (deficit * 15.0 - 2.0) * 0.8
        normalizedComposure * decay

    let decisionQuality (player: Player) condition =
        let v = effectiveVisionFromCondition player.Mental.Vision condition
        let p = effectivePositioningFromCondition player.Mental.Positioning condition
        let c = effectiveComposureFromCondition player.Mental.Composure condition

        let visionWeight, positioningWeight, composureWeight =
            match player.Position with
            | GK -> 0.2, 0.2, 0.6
            | DR | DC | DL | WBR | WBL -> 0.25, 0.45, 0.3
            | DM | MC -> 0.35, 0.4, 0.25
            | MR | ML -> 0.35, 0.3, 0.35
            | AMR | AML | AMC -> 0.45, 0.3, 0.25
            | ST -> 0.5, 0.25, 0.25

        v * visionWeight + p * positioningWeight + c * composureWeight
