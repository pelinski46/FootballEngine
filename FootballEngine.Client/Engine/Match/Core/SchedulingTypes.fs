namespace FootballEngine

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

    type IntentContext =
        | NormalPlay
        | BuildUpPhase
        | PressingTrap

    type PlayerIntent =
        { Movement: MovementIntent
          Action: OnBallIntent option
          Context: IntentContext
          Urgency: float
          Confidence: float }


    type PlayerResult =
        { Events: MatchEvent list
          Transition: MatchFlow option }

    type RefereeResult =
        { Actions: RefereeAction list
          Restart: (SetPieceKind * ClubSide) option
          Transition: MatchFlow option }
