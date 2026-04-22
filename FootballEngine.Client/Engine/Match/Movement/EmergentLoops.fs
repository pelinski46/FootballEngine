namespace FootballEngine.Movement


open FootballEngine

module EmergentLoops =

    let updateCompactness (current: EmergentState) (shortPassSuccess: float) : EmergentState =
        let adjustment =
            if shortPassSuccess > 0.7 then 0.05
            elif shortPassSuccess < 0.5 then -0.05
            else 0.0

        { current with
            CompactnessLevel = System.Math.Clamp(current.CompactnessLevel + adjustment, 0.1, 1.0) }

    let updatePressing (current: EmergentState) (pressSuccessRate: float) : EmergentState =
        let adjustment =
            if pressSuccessRate > 0.6 then 0.05
            elif pressSuccessRate < 0.4 then -0.08
            else 0.0

        { current with
            PressingIntensity = System.Math.Clamp(current.PressingIntensity + adjustment, 0.1, 1.0) }

    let updateWingPlay (current: EmergentState) (flankSuccessRate: float) : EmergentState =
        let adjustment =
            if flankSuccessRate > 0.55 then 0.05
            elif flankSuccessRate < 0.4 then -0.05
            else 0.0

        { current with
            WingPlayPreference = System.Math.Clamp(current.WingPlayPreference + adjustment, 0.1, 1.0) }

    let updateFatigueSpiral (current: EmergentState) (avgCondition: float) (consecutiveLosses: int) : EmergentState =
        let conditionFactor = avgCondition / 100.0
        let lossPenalty = float consecutiveLosses * 0.03
        let fatigueEffect = max 0.0 (1.0 - conditionFactor) * 0.1 + lossPenalty

        { current with
            CompactnessLevel = max 0.1 (current.CompactnessLevel - fatigueEffect * 0.2)
            PressingIntensity = max 0.1 (current.PressingIntensity - fatigueEffect * 1.2)
            TempoLevel = max 0.1 (current.TempoLevel - fatigueEffect * 0.6)
            RiskAppetite = max 0.1 (current.RiskAppetite - fatigueEffect * 0.5) }


