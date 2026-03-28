namespace FootballEngine.Domain

open System

type MatchEventType =
    | Goal
    | OwnGoal
    | Assist
    | YellowCard
    | RedCard
    | Injury of description: string
    | SubstitutionIn
    | SubstitutionOut
    | PenaltyAwarded of isScored: bool
    | FreeKick of isScored: bool
    | Corner

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
