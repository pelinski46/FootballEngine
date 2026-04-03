namespace FootballEngine

open System

module BallPhysics =

    let update (dt: float) (ball: BallPhysicsState) : BallPhysicsState =
        let pos = ball.Position

        // Magnus effect
        let speed = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy + pos.Vz * pos.Vz)
        let magnusX = ball.Spin.Side * BalanceConfig.BallMagnusCoeff * pos.Vy
        let magnusY = ball.Spin.Side * BalanceConfig.BallMagnusCoeff * (-pos.Vx)
        let magnusZ = ball.Spin.Top * BalanceConfig.BallMagnusCoeff * speed

        // Apply gravity + drag + magnus
        let vz' = pos.Vz + BalanceConfig.BallGravity * dt + magnusZ * dt
        let vx' = (pos.Vx * BalanceConfig.BallAirDrag) + magnusX * dt
        let vy' = (pos.Vy * BalanceConfig.BallAirDrag) + magnusY * dt

        // Integrate position
        let x' = pos.X + vx' * dt
        let y' = pos.Y + vy' * dt
        let z' = pos.Z + vz' * dt

        // Ground bounce
        let pos', vx'', vy'', vz'' =
            if z' <= 0.0 then
                { pos with X = x'; Y = y'; Z = 0.0 },
                vx' * BalanceConfig.BallGroundFriction,
                vy' * BalanceConfig.BallGroundFriction,
                abs vz' * BalanceConfig.BallGroundRestitution
            else
                { pos with X = x'; Y = y'; Z = z' },
                vx', vy', vz'

        // Post/crossbar bounce
        let pos'', vx''', vy''', vz''' =
            // Home goal (X ~100, Y 36.8-63.2)
            let pos', vx', vy', vz' =
                if pos'.X >= 98.0 && pos'.X <= 102.0 && pos'.Y >= 36.8 && pos'.Y <= 63.2 then
                    if pos'.Z >= 0.0 && pos'.Z <= 2.44 then
                        // Hit the goal structure — reflect X
                        { pos' with X = 98.0 },
                        -vx' * BalanceConfig.BallPostRestitution,
                        vy', vz'
                    else
                        pos', vx', vy', vz'
                else
                    pos', vx', vy', vz'

            // Away goal (X ~0, Y 36.8-63.2)
            let pos', vx', vy', vz' =
                if pos'.X >= -2.0 && pos'.X <= 0.0 && pos'.Y >= 36.8 && pos'.Y <= 63.2 then
                    if pos'.Z >= 0.0 && pos'.Z <= 2.44 then
                        { pos' with X = 0.0 },
                        -vx' * BalanceConfig.BallPostRestitution,
                        vy', vz'
                    else
                        pos', vx', vy', vz'
                else
                    pos', vx', vy', vz'

            pos', vx', vy', vz'

        // Spin decay
        let spin' =
            { Top  = ball.Spin.Top * BalanceConfig.BallSpinDecay
              Side = ball.Spin.Side * BalanceConfig.BallSpinDecay }

        { ball with
            Position = { pos'' with Vx = vx'''; Vy = vy'''; Vz = vz''' }
            Spin = spin' }
