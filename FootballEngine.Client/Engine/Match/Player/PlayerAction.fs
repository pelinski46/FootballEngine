namespace FootballEngine

open FootballEngine.Domain

type PlayerAction =
    | Shoot
    | Pass       of target: Player
    | Dribble
    | Cross
    | LongBall
    | Tackle     of opponent: Player
    | FreeKick
    | Corner
    | ThrowIn    of side: ClubSide
    | Penalty    of kicker: Player * side: ClubSide * kickNum: int
    | Idle
