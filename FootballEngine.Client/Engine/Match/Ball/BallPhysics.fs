namespace FootballEngine

open System

/// Pure ball physics integrator.
/// Operates in metres (SI units). dt = PhysicsContract.Dt = 0.025s.
/// One call = one physics step. No substep loop.
module BallPhysics =

    let update (ball: BallPhysicsState) : BallPhysicsState =
        let dt  = PhysicsContract.Dt
        let pos = ball.Position

        let speed   = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy + pos.Vz * pos.Vz)
        let magnusX =  ball.Spin.Side * BalanceConfig.BallMagnusCoeff * pos.Vy
        let magnusY =  ball.Spin.Side * BalanceConfig.BallMagnusCoeff * (-pos.Vx)
        let magnusZ =  ball.Spin.Top  * BalanceConfig.BallMagnusCoeff * speed

        let vz' = pos.Vz + BalanceConfig.BallGravity * dt + magnusZ * dt
        let vx' = pos.Vx * BalanceConfig.BallAirDrag  + magnusX * dt
        let vy' = pos.Vy * BalanceConfig.BallAirDrag  + magnusY * dt

        let x' = pos.X + vx' * dt
        let y' = pos.Y + vy' * dt
        let z' = pos.Z + vz' * dt

        let z'', vx'', vy'', vz'' =
            if z' <= 0.0 then
                0.0,
                vx' * BalanceConfig.BallGroundFriction,
                vy' * BalanceConfig.BallGroundFriction,
                abs vz' * BalanceConfig.BallGroundRestitution
            else
                z', vx', vy', vz'

        let x'' = Math.Clamp(x', 0.0, PhysicsContract.PitchLength)
        let y'' = Math.Clamp(y', 0.0, PhysicsContract.PitchWidth)

        let inGoalY = y'' >= PhysicsContract.PostNearY && y'' <= PhysicsContract.PostFarY
        let inGoalZ = z'' >= 0.0 && z'' <= PhysicsContract.CrossbarHeight

        let x''', vx''' =
            let frameX = PhysicsContract.GoalLineHome
            if inGoalY && inGoalZ && x'' >= frameX - 0.2 && x'' <= frameX then
                frameX - 0.2, -vx'' * BalanceConfig.BallPostRestitution
            else
                x'', vx''

        let x'''', vx'''' =
            let frameX = PhysicsContract.GoalLineAway
            if inGoalY && inGoalZ && x''' <= frameX + 0.2 && x''' >= frameX then
                frameX + 0.2, -vx''' * BalanceConfig.BallPostRestitution
            else
                x''', vx'''

        let spin' =
            { Top  = ball.Spin.Top  * BalanceConfig.BallSpinDecay
              Side = ball.Spin.Side * BalanceConfig.BallSpinDecay }

        { ball with
            Position =
                { X  = x''''
                  Y  = y''
                  Z  = z''
                  Vx = vx''''
                  Vy = vy''
                  Vz = vz'' }
            Spin = spin' }
