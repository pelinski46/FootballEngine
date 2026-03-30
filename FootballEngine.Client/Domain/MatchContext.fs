namespace FootballEngine.Domain

type Possession =
    | Home
    | Away

type MatchPhase =
    | BuildUp
    | Midfield
    | Attack

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

type TeamSide =
    { Players: Player[]
      Conditions: int[]
      Positions: (float * float)[]
      BasePositions: (float * float)[]
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
      BallPosition: float * float
      Possession: Possession
      Momentum: float
      HomeSide: TeamSide
      AwaySide: TeamSide
      EventsRev: MatchEvent list
      PenaltyShootout: PenaltyShootout option
      IsExtraTime: bool
      IsKnockoutMatch: bool }

type MatchContext =
    { HomePositions: Map<PlayerId, float * float>
      AwayPositions: Map<PlayerId, float * float> }

type MatchReplay =
    { Final: MatchState
      Snapshots: MatchState[] }

type ScheduledEvent =
    // Core simulation events
    | Duel
    | PositionTick
    | FatigueCheck

    // Attacking actions
    | ShotAttempt of attacker: Player
    | PassSequence of attacker: Player
    | DribbleAttempt of attacker: Player
    | CrossAttemptEvent of attacker: Player
    | LongBallAttempt of attacker: Player

    // Defensive actions
    | TackleAttempt of defender: Player

    // Set pieces
    | FreeKickAttempt of kicker: Player
    | PenaltyKick of kicker: Player * isHome: bool * kickNumber: int
    | CornerTaken

    // Administrative events
    | CardCheck of player: Player * clubId: ClubId * isPossessingTeam: bool
    | InjuryCheck of player: Player * clubId: ClubId
    | SubstitutionCheck of clubId: ClubId

    // Match lifecycle
    | MatchEnd
