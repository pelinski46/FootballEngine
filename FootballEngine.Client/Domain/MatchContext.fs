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
      Positions: Map<PlayerId, float * float>
      BasePositions: Map<PlayerId, float * float>
      Sidelined: Map<PlayerId, PlayerOut>
      Yellows: Map<PlayerId, int>
      SubsUsed: int }

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
      EventsRev: MatchEvent list }

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
