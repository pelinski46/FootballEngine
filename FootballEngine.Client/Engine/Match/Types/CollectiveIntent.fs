namespace FootballEngine

[<Struct>]
type MovementScores =
    { MaintainShape: float
      MarkMan: float
      PressBall: float
      CoverSpace: float
      SupportAttack: float
      RecoverBall: float
      MoveToSetPiecePos: float }

[<Struct>]
type CollectiveIntent =
    { MaintainShape: float
      MarkMan: float
      PressBall: float
      CoverSpace: float
      SupportAttack: float
      RecoverBall: float }

module CollectiveIntent =
    let neutral =
        { MaintainShape = 0.5
          MarkMan = 0.5
          PressBall = 0.5
          CoverSpace = 0.5
          SupportAttack = 0.5
          RecoverBall = 0.5 }
