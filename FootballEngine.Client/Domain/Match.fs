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
    | PassDeflected of fromPlayer: PlayerId * deflectedById: PlayerId
    | PassIntercepted of fromPlayer: PlayerId * interceptorId: PlayerId
    | PassMisplaced of fromPlayer: PlayerId * toPlayer: PlayerId
    | DribbleSuccess
    | DribbleFail
    | DribbleKeep
    | TackleSuccess
    | TackleFail
    | CrossAttempt of isSuccessful: bool
    | LongBall of isSuccessful: bool
    | ShotBlocked
    | ShotOffTarget
    | Save
    | FoulCommitted


type MatchEvent =
    { SubTick: int
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

type ClubSide =
    | HomeClub
    | AwayClub

type AttackDir =
    | LeftToRight
    | RightToLeft


type PitchZone =
    | AttackingZone
    | MidfieldZone
    | DefensiveZone

module ClubSide =
    let flip =
        function
        | HomeClub -> AwayClub
        | AwayClub -> HomeClub

module AttackDir =


    let distToGoal (x: float) (dir: AttackDir) : float =
        match dir with
        | LeftToRight -> 105.0 - x
        | RightToLeft -> x

    let isInAttackingThird (x: float) (dir: AttackDir) : bool =
        match dir with
        | LeftToRight -> x >= 70.0
        | RightToLeft -> x <= 30.0

    let isInDefensiveThird (x: float) (dir: AttackDir) : bool =
        match dir with
        | LeftToRight -> x <= 30.0
        | RightToLeft -> x >= 70.0

    let forwardX (dir: AttackDir) : float =
        match dir with
        | LeftToRight -> 1.0
        | RightToLeft -> -1.0

    let momentumSign (dir: AttackDir) : float = forwardX dir

    let momentumDelta (dir: AttackDir) (delta: float) : float = momentumSign dir * delta

module PitchZone =
    let ofBallX (x: float) (dir: AttackDir) : PitchZone =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> 105.0 - x

        if effectiveX < 30.0 then DefensiveZone
        elif effectiveX <= 70.0 then MidfieldZone
        else AttackingZone

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
