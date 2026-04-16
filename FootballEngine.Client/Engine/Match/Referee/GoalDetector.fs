namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

module GoalDetector =

    let detect (ball: BallPhysicsState) : ClubSide option =
        let pos = ball.Position
        let inGoalY = pos.Y >= PhysicsContract.PostNearY && pos.Y <= PhysicsContract.PostFarY
        let inGoalZ = pos.Z >= 0.0<meter> && pos.Z <= PhysicsContract.CrossbarHeight

        if pos.X >= PhysicsContract.GoalLineHome && inGoalY && inGoalZ then
            Some HomeClub
        elif pos.X <= PhysicsContract.GoalLineAway && inGoalY && inGoalZ then
            Some AwayClub
        else
            None

    let scorer
        (scoringClub: ClubSide)
        (ball: BallPhysicsState)
        (state: SimState)
        : PlayerId option * bool =
        let lastTouchId = ball.LastTouchBy

        match lastTouchId with
        | Some pid ->
            let touchIsHome =
                state.Home.Slots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = pid
                    | _ -> false)

            let isOwnGoal =
                match scoringClub with
                | HomeClub -> not touchIsHome
                | AwayClub -> touchIsHome

            lastTouchId, isOwnGoal
        | None -> None, false
