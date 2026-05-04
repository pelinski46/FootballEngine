namespace FootballEngine.Player.Actions

open FootballEngine.Domain

open FootballEngine.Types.PhysicsContract


type xGModel =
    { DistanceToGoal: float<meter>
      AngleToGoal: float
      ShotType: ShotType
      BodyPart: string
      AssistType: string
      PressureLevel: float
      IsOneOnOne: bool
      IsSetPiece: bool }

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

    let adjustForPressure (xg: float) (pressure: float) : float = xg * (1.0 - pressure * 0.5)

    let calculate (model: xGModel) : float =
        let xg = baseXG model.DistanceToGoal model.AngleToGoal
        let xg2 = adjustForShotType xg model.ShotType
        let xg3 = adjustForPressure xg2 model.PressureLevel
        let xg4 = if model.IsOneOnOne then xg3 * 1.3 else xg3
        let xg5 = if model.IsSetPiece then xg4 * 0.8 else xg4
        min 1.0 (max 0.0 xg5)
