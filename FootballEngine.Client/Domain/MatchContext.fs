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
    { HomeKicks: (PlayerId * bool) list  // playerId * scored
      AwayKicks: (PlayerId * bool) list
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
    | Duel
    | FatigueCheck
    | ShotAttempt of attacker: Player
    | CardCheck of player: Player * clubId: ClubId * isPossessingTeam: bool
    | InjuryCheck of player: Player * clubId: ClubId
    | SubstitutionCheck of clubId: ClubId
    | MatchEnd
    | PenaltyKick of kicker: Player * isHome: bool * kickNumber: int
    | FreeKickAttempt of kicker: Player
    | CornerTaken
