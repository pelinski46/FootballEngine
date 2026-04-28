namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract
open Stats

type xGModel = {
    DistanceToGoal: float<meter>
    AngleToGoal: float
    ShotType: ShotType
    BodyPart: string
    AssistType: string
    PressureLevel: float
    IsOneOnOne: bool
    IsSetPiece: bool
}

module xGCalculator =

    let baseXG (dist: float<meter>) (angle: float) : float =
        let d = float dist
        let a = angle
        let distFactor = max 0.0 (1.0 - d / 30.0)
        let angleFactor = a / 180.0
        distFactor * angleFactor * 0.8

    let adjustForShotType (xg: float) (shotType: ShotType) : float =
        match shotType with
        | ShotType.Header -> xg * 0.7
        | ShotType.Volley -> xg * 0.6
        | ShotType.HalfVolley -> xg * 0.75
        | ShotType.ChipShot -> xg * 0.85
        | ShotType.Curler -> xg * 0.9
        | ShotType.DrivenShot -> xg * 1.1
        | ShotType.PlacedShot -> xg * 1.0
        | ShotType.FirstTimeShot -> xg * 0.8

    let adjustForPressure (xg: float) (pressure: float) : float =
        xg * (1.0 - pressure * 0.5)

    let calculate (model: xGModel) : float =
        let xg = baseXG model.DistanceToGoal model.AngleToGoal
        let xg2 = adjustForShotType xg model.ShotType
        let xg3 = adjustForPressure xg2 model.PressureLevel
        let xg4 = if model.IsOneOnOne then xg3 * 1.3 else xg3
        let xg5 = if model.IsSetPiece then xg4 * 0.8 else xg4
        min 1.0 (max 0.0 xg5)

type MatchMomentum = {
    HomeMomentum: float
    AwayMomentum: float
    LastEventSubTick: int
    ConsecutiveHomeEvents: int
    ConsecutiveAwayEvents: int
}

module DynamicMomentum =

    let initial = {
        HomeMomentum = 0.0
        AwayMomentum = 0.0
        LastEventSubTick = 0
        ConsecutiveHomeEvents = 0
        ConsecutiveAwayEvents = 0 }

    let update (momentum: MatchMomentum) (subTick: int) (eventClub: ClubSide) : MatchMomentum =
        let delta = 0.5
        let decay = 0.02

        let (homeM, awayM, homeC, awayC) =
            match eventClub with
            | HomeClub ->
                (momentum.HomeMomentum + delta,
                 momentum.AwayMomentum - decay,
                 momentum.ConsecutiveHomeEvents + 1,
                 0)
            | AwayClub ->
                (momentum.HomeMomentum - decay,
                 momentum.AwayMomentum + delta,
                 0,
                 momentum.ConsecutiveAwayEvents + 1)

        { HomeMomentum = max -10.0 (min 10.0 homeM)
          AwayMomentum = max -10.0 (min 10.0 awayM)
          LastEventSubTick = subTick
          ConsecutiveHomeEvents = homeC
          ConsecutiveAwayEvents = awayC }

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

type CrowdInfluence = {
    StadiumCapacity: int
    HomeSupport: float
    CurrentMomentum: float
    MatchImportance: float
}

module CrowdSystem =

    let homeAdvantage (crowd: CrowdInfluence) : float =
        let capacityFactor = float crowd.StadiumCapacity / 80000.0
        let supportFactor = crowd.HomeSupport
        let momentumFactor = max 0.0 crowd.CurrentMomentum / 10.0
        let importanceFactor = crowd.MatchImportance
        (capacityFactor * 0.3 + supportFactor * 0.3 + momentumFactor * 0.2 + importanceFactor * 0.2) * 0.15

    let pressureOnAwayTeam (crowd: CrowdInfluence) : float =
        let homeAdv = homeAdvantage crowd
        homeAdv * 0.5

type SubstitutionSuggestion = {
    PlayerOut: PlayerId
    PlayerIn: PlayerId
    Reason: string
    Urgency: float
}

module SubstitutionIntelligence =

    let suggestSubs
        (subTick: int)
        (clock: SimulationClock)
        (players: Player[])
        (frame: TeamFrame)
        (score: int)
        (opponentScore: int)
        : SubstitutionSuggestion list =

        let elapsed = float subTick / float clock.SubTicksPerSecond / 60.0
        let suggestions = ResizeArray()

        if elapsed > 60.0 then
            for i = 0 to frame.SlotCount - 1 do
                match frame.Occupancy[i] with
                | OccupancyKind.Active rosterIdx ->
                    let p = players[rosterIdx]
                    if p.Condition < 50 && p.Position <> Domain.GK then
                        suggestions.Add({
                            PlayerOut = p.Id
                            PlayerIn = 0
                            Reason = "Fatigue"
                            Urgency = float (100 - p.Condition) / 100.0
                        }) |> ignore

        if score < opponentScore && elapsed > 70.0 then
            suggestions.Add({
                PlayerOut = 0
                PlayerIn = 0
                Reason = "Tactical - chasing goal"
                Urgency = 0.7
            }) |> ignore

        suggestions |> Seq.toList
