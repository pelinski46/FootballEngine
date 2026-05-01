namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

type BallActionKind =
    | Pass of passerId: PlayerId * targetId: PlayerId * quality: float
    | Shot of shooterId: PlayerId * quality: float
    | Cross of crosserId: PlayerId * targetId: PlayerId * quality: float
    | LongBall of passerId: PlayerId * targetId: PlayerId * quality: float
    | Clearance of playerId: PlayerId
    | Deflection of playerId: PlayerId
    | FreeBall

type BallTrajectory =
    { OriginX: float<meter>
      OriginY: float<meter>
      TargetX: float<meter>
      TargetY: float<meter>
      LaunchSubTick: int
      EstimatedArrivalSubTick: int
      KickerId: PlayerId
      PeakHeight: float<meter>
      ActionKind: BallActionKind }

type BallPhysicsState =
    { Position: Spatial
      Spin: Spin
      Possession: Possession
      LastTouchBy: PlayerId option
      PendingOffsideSnapshot: OffsideSnapshot option
      StationarySinceSubTick: int option
      GKHoldSinceSubTick: int option
      PlayerHoldSinceSubTick: int option
      Trajectory: BallTrajectory option }

type ArrivalContext =
    { BallPos: Spatial
      TargetId: PlayerId
      Quality: float
      HomeFrame: TeamFrame
      AwayFrame: TeamFrame
      HomeRoster: PlayerRoster
      AwayRoster: PlayerRoster
      PhysicsCfg: PhysicsConfig }
