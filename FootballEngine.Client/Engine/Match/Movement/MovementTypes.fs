namespace FootballEngine.Movement

type RunType =
    | DeepRun
    | OverlapRun
    | UnderlapRun
    | DiagonalRun
    | CheckToBall
    | DriftWide
    | ThirdManRun
    | FalseNineDrop
    | WingBackSurge

type RunTrigger =
    | TeammateHasBall
    | TeammateStartedDribble
    | SpaceDetected
    | TacticalInstruction
    | SetPieceRoutine
    | CounterAttack

type RunTrajectory =
    | Linear of float * float * float * float
    | Waypoints of (float * float)[]

type MovementMode =
    | OpenPlay
    | SetPiece of SetPieceRole

and SetPieceRole =
    | CornerAttack
    | CornerDefend
    | FreeKickAttack
    | FreeKickDefend
    | ThrowInNear
    | ThrowInFar

type MovementRole =
    | BallCarrier
    | SupportPlayer
    | ShadowPlayer
    | PressingPlayer
    | CoveringPlayer
    | MarkerPlayer
    | RunPlayer
    | SetPiecePlayer
