namespace FootballEngine.Domain

open System
open FootballEngine.DomainTypes

type GameState =
    { CurrentDate: DateTime
      Season: int
      Clubs: Map<ClubId, Club>
      Players: Map<PlayerId, Player>
      Competitions: Map<CompetitionId, Competition>
      Fixtures: Map<MatchId, MatchFixture>

      KnockoutTies: Map<int, KnockoutTie>
      UserClubId: ClubId
      ManagerName: string
      PrimaryCountry: CountryCode
      Countries: Map<CountryCode, Country> }
