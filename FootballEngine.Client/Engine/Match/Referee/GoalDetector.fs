namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Types
open FootballEngine.Types.PhysicsContract
open SimStateOps

module GoalDetector =

    let detect (ball: BallPhysicsState) : ClubSide option =
        let pos = ball.Position
        let inGoalY = pos.Y >= PostNearY && pos.Y <= PostFarY
        let inGoalZ = pos.Z >= 0.0<meter> && pos.Z <= CrossbarHeight

        if pos.X >= GoalLineHome && inGoalY && inGoalZ then
            Some HomeClub
        elif pos.X <= GoalLineAway && inGoalY && inGoalZ then
            Some AwayClub
        else
            None

    let scorer
        (scoringClub: ClubSide)
        (ball: BallPhysicsState)
        (ctx: MatchContext)
        (state: SimState)
        : PlayerId option * bool =
        let lastTouchId = ball.LastTouchBy

        match lastTouchId with
        | Some pid ->
            let touchIsHome = playerOnSide ctx state HomeClub pid

            let isOwnGoal =
                match scoringClub with
                | HomeClub -> not touchIsHome
                | AwayClub -> touchIsHome

            lastTouchId, isOwnGoal
        | None -> None, false
