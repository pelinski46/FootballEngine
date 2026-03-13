namespace FootballEngine.Domain

open FootballEngine.DomainTypes

type CompetitionId = int

type LeagueLevel =
    | First
    | Second

type CupFormat =
    | SingleMatch
    | TwoLegs
    | GroupThenKnockout of groupSize: int * teamsPerGroup: int
    | StraightKnockout of legs: int


type CompetitionType =
    | NationalLeague of level: LeagueLevel
    | NationalCup of format: CupFormat
    | InternationalCup of confederation: Confederation option * format: CupFormat


type Round =
    | GroupStage of group: int
    | RoundOf32
    | RoundOf16
    | QuarterFinal
    | SemiFinal
    | Final
    | ThirdPlace


type Competition =
    { Id: CompetitionId
      Name: string
      Type: CompetitionType
      Country: CountryCode option
      Season: int
      ClubIds: ClubId list
      Settings: Map<string, string> }

type LeagueStanding =
    { ClubId: ClubId
      Played: int
      Won: int
      Drawn: int
      Lost: int
      GoalsFor: int
      GoalsAgainst: int
      Points: int }

type KnockoutTie =
    { TieId: int
      Round: Round
      HomeClubId: ClubId
      AwayClubId: ClubId
      Leg1FixtureId: MatchId option
      Leg2FixtureId: MatchId option
      AggregateScore: (int * int) option
      WinnerId: ClubId option }
