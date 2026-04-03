namespace FootballEngine

open FootballEngine.Domain

module GoalDetector =

    [<Literal>]
    let GoalLineHome = 100.0
    [<Literal>]
    let GoalLineAway = 0.0
    [<Literal>]
    let PostMin = 36.8
    [<Literal>]
    let PostMax = 63.2
    [<Literal>]
    let Crossbar = 2.44

    let detect (ball: BallPhysicsState) : ClubSide option =
        let pos = ball.Position
        let inGoalY = pos.Y >= PostMin && pos.Y <= PostMax
        let inGoalZ = pos.Z >= 0.0 && pos.Z <= Crossbar

        if pos.X >= GoalLineHome && inGoalY && inGoalZ then
            Some HomeClub
        elif pos.X <= GoalLineAway && inGoalY && inGoalZ then
            Some AwayClub
        else
            None

    let scorer (scoringClub: ClubSide) (ball: BallPhysicsState) (s: MatchState) : PlayerId option * bool =
        let lastTouchId = ball.LastTouchBy
        match lastTouchId with
        | Some pid ->
            let touchIsHome = s.HomeSide.Players |> Array.exists (fun p -> p.Id = pid)
            let isOwnGoal =
                match scoringClub with
                | HomeClub -> not touchIsHome
                | AwayClub -> touchIsHome
            lastTouchId, isOwnGoal
        | None -> None, false
