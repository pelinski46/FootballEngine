namespace FootballEngine.Movement

type EmergentState =
    { CompactnessLevel: float
      PressingIntensity: float
      WingPlayPreference: float
      TempoLevel: float
      RiskAppetite: float }

module EmergentLoops =
    let initial =
        { CompactnessLevel = 0.5
          PressingIntensity = 0.5
          WingPlayPreference = 0.5
          TempoLevel = 0.5
          RiskAppetite = 0.5 }

    let updateCompactness current shortPassSuccess =
        let adjustment =
            if shortPassSuccess > 0.7 then 0.05
            elif shortPassSuccess < 0.5 then -0.05
            else 0.0
        { current with
            CompactnessLevel = System.Math.Clamp(current.CompactnessLevel + adjustment, 0.1, 1.0) }

    let updatePressing current pressSuccessRate =
        let adjustment =
            if pressSuccessRate > 0.6 then 0.05
            elif pressSuccessRate < 0.4 then -0.08
            else 0.0
        { current with
            PressingIntensity = System.Math.Clamp(current.PressingIntensity + adjustment, 0.1, 1.0) }

    let updateWingPlay current flankSuccessRate =
        let adjustment =
            if flankSuccessRate > 0.55 then 0.05
            elif flankSuccessRate < 0.4 then -0.05
            else 0.0
        { current with
            WingPlayPreference = System.Math.Clamp(current.WingPlayPreference + adjustment, 0.1, 1.0) }

    let updateFatigueSpiral current avgCondition consecutiveLosses =
        let conditionFactor = float avgCondition / 100.0
        let lossPenalty = float consecutiveLosses * 0.03
        let fatigueEffect = max 0.0 (1.0 - conditionFactor) * 0.1 + lossPenalty

        { current with
            CompactnessLevel = max 0.1 (current.CompactnessLevel - fatigueEffect * 0.2)
            PressingIntensity = max 0.1 (current.PressingIntensity - fatigueEffect * 1.2)
            TempoLevel = max 0.1 (current.TempoLevel - fatigueEffect * 0.6)
            RiskAppetite = max 0.1 (current.RiskAppetite - fatigueEffect * 0.5) }

    let toDirectiveModifiers (state: EmergentState) =
        { Shape = 1.0
          Run = state.TempoLevel
          MarkMan = state.CompactnessLevel
          MarkZone = state.CompactnessLevel
          Press = state.PressingIntensity
          Cover = state.CompactnessLevel
          Support = state.RiskAppetite
          Flank = state.WingPlayPreference
          Compact = state.CompactnessLevel
          Spread = 2.0 - state.CompactnessLevel
          ThirdMan = state.RiskAppetite }
