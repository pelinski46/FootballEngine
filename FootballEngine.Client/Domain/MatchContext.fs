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

type MatchState =
    { Home: Club
      Away: Club
      Second: int
      HomeScore: int
      AwayScore: int
      BallPosition: float * float
      Possession: Possession
      Momentum: float
      HomePlayers: Player[]
      AwayPlayers: Player[]
      HomeConditions: int[]
      AwayConditions: int[]
      HomeSidelined: Map<PlayerId, PlayerOut>
      AwaySidelined: Map<PlayerId, PlayerOut>
      HomeYellows: Map<PlayerId, int>
      AwayYellows: Map<PlayerId, int>
      HomeSubsUsed: int
      AwaySubsUsed: int
      EventsRev: MatchEvent list
      HomePositions: Map<PlayerId, float * float>
      AwayPositions: Map<PlayerId, float * float>
      HomeBasePositions: Map<PlayerId, float * float>
      AwayBasePositions: Map<PlayerId, float * float> }

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

type TeamSide =
    { Players: Player[]
      Conditions: int[]
      Positions: Map<PlayerId, float * float>
      BasePositions: Map<PlayerId, float * float>
      Sidelined: Map<PlayerId, PlayerOut>
      Yellows: Map<PlayerId, int>
      SubsUsed: int }
