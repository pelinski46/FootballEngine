namespace FootballEngine.Simulation

open FootballEngine.Domain

type WeatherCondition =
    | Clear
    | LightRain
    | HeavyRain
    | Snow
    | Windy

type PitchCondition =
    | Dry
    | Damp
    | Wet
    | Waterlogged

module WeatherSystem =

    let ballSpeedModifier (weather: WeatherCondition) : float =
        match weather with
        | Clear -> 1.0
        | LightRain -> 0.95
        | HeavyRain -> 0.85
        | Snow -> 0.8
        | Windy -> 0.9

    let slipProbability (pitch: PitchCondition) (player: Player) : float =
        let agility = float player.Physical.Agility / 20.0

        let baseSlip =
            match pitch with
            | Dry -> 0.01
            | Damp -> 0.03
            | Wet -> 0.08
            | Waterlogged -> 0.15

        baseSlip * (1.0 - agility * 0.5)

type CrowdInfluence =
    { StadiumCapacity: int
      HomeSupport: float
      CurrentMomentum: float
      MatchImportance: float }

module CrowdSystem =

    let homeAdvantage (crowd: CrowdInfluence) : float =
        let capacityFactor = float crowd.StadiumCapacity / 80000.0
        let supportFactor = crowd.HomeSupport
        let momentumFactor = max 0.0 crowd.CurrentMomentum / 10.0
        let importanceFactor = crowd.MatchImportance

        (capacityFactor * 0.3
         + supportFactor * 0.3
         + momentumFactor * 0.2
         + importanceFactor * 0.2)
        * 0.15

    let pressureOnAwayTeam (crowd: CrowdInfluence) : float =
        let homeAdv = homeAdvantage crowd
        homeAdv * 0.5
