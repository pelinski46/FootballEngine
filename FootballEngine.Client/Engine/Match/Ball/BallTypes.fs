namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Player


type AimedKind =
    | RegularPass
    | LongBall
    | Cross

type InFlightIntent =
    | Aimed of passerId: PlayerId * targetId: PlayerId * quality: float * kind: AimedKind
    | Struck of shooterId: PlayerId * quality: float
    | Cleared of playerId: PlayerId
    | Uncontrolled

type BallTrajectory =
    { OriginX: float<meter>
      OriginY: float<meter>
      TargetX: float<meter>
      TargetY: float<meter>
      LaunchSubTick: int
      EstimatedArrivalSubTick: int
      KickerId: PlayerId
      PeakHeight: float<meter>
      Intent: InFlightIntent }

type BallControl =
    | Free
    | Receiving of ClubSide * PlayerId * sinceSubTick: int
    | Controlled of ClubSide * PlayerId
    | Airborne
    | Contesting of ClubSide

type BallPhysicsState =
    { Position: Spatial
      Spin: Spin
      Control: BallControl
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
