namespace FootballEngine.Domain



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
      StaffIds: StaffId list
      Budget: decimal
      Morale: int
      BoardObjective: BoardObjective }
