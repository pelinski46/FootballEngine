namespace FootballEngine.Domain

/// Fixed identity of a club in this match — never changes
type ClubSide =
    | HomeClub
    | AwayClub

/// Direction of attack on the X axis — derived from ClubSide
/// Pitch X: 0 = left goal, 100 = right goal
/// HomeClub always attacks LeftToRight, AwayClub always RightToLeft
type AttackDir =
    | LeftToRight
    | RightToLeft

/// Ball zone relative to the attacking direction — never absolute
type PitchZone =
    | AttackingZone
    | MidfieldZone
    | DefensiveZone

module ClubSide =
    let flip = function
        | HomeClub -> AwayClub
        | AwayClub -> HomeClub

module AttackDir =
    let ofClubSide (clubSide: ClubSide) : AttackDir =
        match clubSide with
        | HomeClub -> LeftToRight
        | AwayClub -> RightToLeft

    let distToGoal (x: float) (dir: AttackDir) : float =
        match dir with
        | LeftToRight -> 100.0 - x
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

    let momentumDelta (dir: AttackDir) (delta: float) : float =
        momentumSign dir * delta

module PitchZone =
    let ofBallX (x: float) (dir: AttackDir) : PitchZone =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> 100.0 - x

        if effectiveX < 30.0 then DefensiveZone
        elif effectiveX <= 70.0 then MidfieldZone
        else AttackingZone
