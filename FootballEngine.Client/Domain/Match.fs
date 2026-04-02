namespace FootballEngine.Domain

open System

type MatchPhase =
    | BuildUp
    | Midfield
    | Attack

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
    | PassCompleted of fromPlayer: PlayerId * toPlayer: PlayerId
    | PassIncomplete of fromPlayer: PlayerId
    | DribbleSuccess
    | DribbleFail
    | TackleSuccess
    | TackleFail
    | CrossAttempt of isSuccessful: bool
    | LongBall of isSuccessful: bool
    | ShotBlocked
    | ShotOffTarget
    | Save
    | FoulCommitted

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

module MatchFixture =

    let involves (clubId: ClubId) (f: MatchFixture) =
        f.HomeClubId = clubId || f.AwayClubId = clubId

    let isPlayed (f: MatchFixture) = f.Played

    let isPending (f: MatchFixture) = not f.Played

    let scoreFor (clubId: ClubId) (f: MatchFixture) =
        match f.HomeScore, f.AwayScore with
        | Some h, Some a ->
            if f.HomeClubId = clubId then Some(h, a)
            elif f.AwayClubId = clubId then Some(a, h)
            else None
        | _ -> None
