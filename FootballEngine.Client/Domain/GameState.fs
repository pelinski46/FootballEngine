namespace FootballEngine.Domain

open System

type GameState =
    { CurrentDate: DateTime
      Season: int
      Clubs: Map<ClubId, Club>
      Players: Map<PlayerId, Player>
      Competitions: Map<CompetitionId, Competition>
      Countries: Map<CountryCode, Country>
      UserClubId: ClubId
      ManagerName: string
      PrimaryCountry: CountryCode }
