namespace FootballEngine.Domain

type FormationSlot =
    { Index: int
      Role: Position
      X: float
      Y: float }

type LineupSlot =
    { Index: int
      Role: Position
      X: float
      Y: float
      PlayerId: PlayerId option }

type ClubLineup =
    { Formation: Formation
      TeamTactics: string
      Slots: LineupSlot list }

type CupObjective =
    | WinDomesticCup
    | WinContinentalCup
    | WinChampionsLeague

type LeagueObjective =
    | Survival
    | MidTable
    | TopHalf
    | TopFour
    | WinLeague

type BoardObjective =
    | LeagueObjective of LeagueObjective
    | CupObjective of CupObjective
    | Promotion
    | Relegation

type Club =
    { Id: ClubId
      Name: string
      Nationality: CountryCode
      Reputation: int
      PlayerIds: PlayerId list
      CurrentLineup: ClubLineup option
      Budget: decimal
      Morale: int
      BoardObjective: BoardObjective }
