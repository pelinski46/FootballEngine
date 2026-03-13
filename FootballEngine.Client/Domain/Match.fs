namespace FootballEngine.Domain

open System
open FootballEngine.DomainTypes

type MatchEventType =
    | Goal
    | OwnGoal
    | Assist
    | YellowCard
    | RedCard
    | Injury of string
    | SubstitutionIn
    | SubstitutionOut

type MatchEvent =
    { Second: int
      PlayerId: PlayerId
      ClubId: ClubId
      Type: MatchEventType }

type MatchFixture =
    { Id: MatchId
      CompetitionId: CompetitionId
      Round: Round option
      HomeClubId: ClubId
      AwayClubId: ClubId
      ScheduledDate: DateTime
      Played: bool
      HomeScore: int option
      AwayScore: int option
      Events: MatchEvent list }
