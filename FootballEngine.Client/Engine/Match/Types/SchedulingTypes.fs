namespace FootballEngine.Types

open FootballEngine.Domain


type PlayerAction =
    | Shoot
    | Pass of target: Player
    | Dribble
    | Cross
    | LongBall
    | Tackle of opponent: Player
    | FreeKick
    | Corner
    | ThrowIn of side: ClubSide
    | Penalty of kicker: Player * side: ClubSide * kickNum: int

module SchedulingTypes =

    type OnBallIntent =
        | Shoot
        | Pass of target: PlayerId
        | Dribble
        | Cross
        | LongBall of target: PlayerId
        | Tackle of opponent: PlayerId
        | PassIntoSpace of targetCell: int

    type PlayerResult =
        { Events: MatchEvent list
          Transition: MatchFlow option
          PendingRefereeActions: RefereeAction list }

    type BallResult =
        { Events: MatchEvent list
          Transition: MatchFlow option
          PossessionChanged: bool
          BallInFlight: bool
          SetPieceAwarded: bool
          ReceivedByPlayer: PlayerId option
          GoalScored: ClubSide option }

    module BallResult =
        let empty =
            { Events = []
              Transition = None
              PossessionChanged = false
              BallInFlight = false
              SetPieceAwarded = false
              ReceivedByPlayer = None
              GoalScored = None }

        let ofPlayerResult (pr: PlayerResult) =
            { Events = pr.Events
              Transition = pr.Transition
              PossessionChanged = false
              BallInFlight = false
              SetPieceAwarded = false
              ReceivedByPlayer = None
              GoalScored = None }

    type RefereeResult =
        { Actions: RefereeAction list
          Restart: (SetPieceKind * ClubSide) option
          Transition: MatchFlow option }
