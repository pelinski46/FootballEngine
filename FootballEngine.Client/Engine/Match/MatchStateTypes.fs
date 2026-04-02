namespace FootballEngine

open FootballEngine.Domain

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

type Spatial =
    { X: float
      Y: float
      Z: float
      Vx: float
      Vy: float
      Vz: float }

type BallState =
    { Position: Spatial
      LastTouchBy: PlayerId option }

type TeamSide =
    { Players: Player[]
      Conditions: int[]
      Positions: Spatial[]
      BasePositions: Spatial[]
      Sidelined: Map<PlayerId, PlayerOut>
      Yellows: Map<PlayerId, int>
      SubsUsed: int
      Tactics: TeamTactics
      Instructions: TacticalInstructions option }

type PenaltyShootout =
    { HomeKicks: (PlayerId * bool * int) list
      AwayKicks: (PlayerId * bool * int) list
      CurrentKick: int
      IsComplete: bool }

type MatchState =
    { Home: Club
      Away: Club
      HomeCoach: Staff
      AwayCoach: Staff
      Second: int
      HomeScore: int
      AwayScore: int
      Ball: BallState
      AttackingClub: ClubSide
      Momentum: float
      HomeSide: TeamSide
      AwaySide: TeamSide
      PenaltyShootout: PenaltyShootout option
      IsExtraTime: bool
      IsKnockoutMatch: bool }

type MatchContext =
    { HomePositions: Map<PlayerId, float * float>
      AwayPositions: Map<PlayerId, float * float> }

type MatchReplay =
    { Final: MatchState
      Events: MatchEvent list
      Snapshots: MatchState[] }
