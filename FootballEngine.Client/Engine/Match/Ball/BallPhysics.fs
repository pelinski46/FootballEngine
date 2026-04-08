namespace FootballEngine

open System

module BallPhysics =

    let private applyAerodynamics (pos: Spatial) (spin: Spin) : Spatial =
        let speed = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy + pos.Vz * pos.Vz)
        let dt = PhysicsContract.Dt

        let magnusX = spin.Side * BalanceConfig.BallMagnusCoeff * pos.Vy
        let magnusY = spin.Side * BalanceConfig.BallMagnusCoeff * (-pos.Vx)
        let magnusZ = spin.Top * BalanceConfig.BallMagnusCoeff * speed

        let vx = pos.Vx * BalanceConfig.BallAirDrag + magnusX * dt
        let vy = pos.Vy * BalanceConfig.BallAirDrag + magnusY * dt
        let vz = pos.Vz + BalanceConfig.BallGravity * dt + magnusZ * dt

        { pos with
            X = pos.X + vx * dt
            Y = pos.Y + vy * dt
            Z = pos.Z + vz * dt
            Vx = vx
            Vy = vy
            Vz = vz }

    let private applyGroundCollision (pos: Spatial) : Spatial =
        if pos.Z > 0.0 then
            pos
        else
            { pos with
                Z = 0.0
                Vx = pos.Vx * BalanceConfig.BallGroundFriction
                Vy = pos.Vy * BalanceConfig.BallGroundFriction
                Vz = abs pos.Vz * BalanceConfig.BallGroundRestitution }

    let private clampToPitch (pos: Spatial) : Spatial =
        { pos with
            X = Math.Clamp(pos.X, 0.0, PhysicsContract.PitchLength)
            Y = Math.Clamp(pos.Y, 0.0, PhysicsContract.PitchWidth) }

    let private applyGoalPostHome (pos: Spatial) : Spatial =
        let inGoalY =
            pos.Y >= PhysicsContract.PostNearY && pos.Y <= PhysicsContract.PostFarY

        let inGoalZ = pos.Z >= 0.0 && pos.Z <= PhysicsContract.CrossbarHeight

        let nearPost =
            pos.X >= PhysicsContract.GoalLineHome - 0.2
            && pos.X <= PhysicsContract.GoalLineHome

        if inGoalY && inGoalZ && nearPost then
            { pos with
                X = PhysicsContract.GoalLineHome - 0.2
                Vx = -pos.Vx * BalanceConfig.BallPostRestitution }
        else
            pos

    let private applyGoalPostAway (pos: Spatial) : Spatial =
        let inGoalY =
            pos.Y >= PhysicsContract.PostNearY && pos.Y <= PhysicsContract.PostFarY

        let inGoalZ = pos.Z >= 0.0 && pos.Z <= PhysicsContract.CrossbarHeight

        let nearPost =
            pos.X <= PhysicsContract.GoalLineAway + 0.2
            && pos.X >= PhysicsContract.GoalLineAway

        if inGoalY && inGoalZ && nearPost then
            { pos with
                X = PhysicsContract.GoalLineAway + 0.2
                Vx = -pos.Vx * BalanceConfig.BallPostRestitution }
        else
            pos

    let private stopThreshold = 0.06

    let private applyStopThreshold (pos: Spatial) : Spatial =
        if pos.Z > 0.0 then
            pos
        else
            { pos with
                Vx = if abs pos.Vx < stopThreshold then 0.0 else pos.Vx
                Vy = if abs pos.Vy < stopThreshold then 0.0 else pos.Vy }

    let update (ball: BallPhysicsState) : BallPhysicsState =
        let pos = ball.Position
        let spin = ball.Spin

        // Fast path: grounded ball with no spin — skip aerodynamics, posts, gravity
        if pos.Z = 0.0 && pos.Vz = 0.0 && spin.Top = 0.0 && spin.Side = 0.0 then
            let newPos =
                { pos with
                    X = pos.X + pos.Vx * PhysicsContract.Dt
                    Y = pos.Y + pos.Vy * PhysicsContract.Dt
                    Vx = pos.Vx * BalanceConfig.BallGroundFriction
                    Vy = pos.Vy * BalanceConfig.BallGroundFriction }
                |> clampToPitch
                |> applyStopThreshold

            { ball with Position = newPos }
        else
            let newPos =
                applyAerodynamics pos spin
                |> applyGroundCollision
                |> clampToPitch
                |> applyGoalPostHome
                |> applyGoalPostAway
                |> applyStopThreshold

            let newSpin =
                { Top = spin.Top * BalanceConfig.BallSpinDecay
                  Side = spin.Side * BalanceConfig.BallSpinDecay }

            { ball with
                Position = newPos
                Spin = newSpin }
