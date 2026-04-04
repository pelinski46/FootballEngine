namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps

module PlayerSteering =

    type Force = { Fx: float; Fy: float; Fz: float }

    module Force =
        let zero = { Fx = 0.0; Fy = 0.0; Fz = 0.0 }

        let add a b =
            { Fx = a.Fx + b.Fx
              Fy = a.Fy + b.Fy
              Fz = a.Fz + b.Fz }

        let scale s f =
            { Fx = f.Fx * s
              Fy = f.Fy * s
              Fz = f.Fz * s }

        let magnitude f =
            sqrt (f.Fx * f.Fx + f.Fy * f.Fy + f.Fz * f.Fz)

        let normalize f =
            let m = magnitude f

            if m < 0.001 then
                zero
            else
                { Fx = f.Fx / m
                  Fy = f.Fy / m
                  Fz = f.Fz / m }

        let truncate maxMag f =
            let m = magnitude f
            if m > maxMag then f |> normalize |> scale maxMag else f

    type SteeringTarget =
        | StaticPosition of x: float * y: float * z: float
        | MovingPosition of x: float * y: float * z: float * vx: float * vy: float * vz: float

    module Behaviours =

        let seek (agent: Spatial) (target: SteeringTarget) (maxSpeed: float) : Force =
            let tx, ty, tz =
                match target with
                | StaticPosition(x, y, z) -> x, y, z
                | MovingPosition(x, y, z, _, _, _) -> x, y, z

            let dx = tx - agent.X
            let dy = ty - agent.Y
            let dz = tz - agent.Z
            let dist = sqrt (dx * dx + dy * dy + dz * dz)

            if dist < 0.01 then
                Force.zero
            else
                let desiredSpeed = min maxSpeed (dist * 2.0)

                { Fx = dx / dist * desiredSpeed - agent.Vx
                  Fy = dy / dist * desiredSpeed - agent.Vy
                  Fz = dz / dist * desiredSpeed - agent.Vz }

        let arrive (agent: Spatial) (target: SteeringTarget) (maxSpeed: float) : Force =
            let tx, ty, tz =
                match target with
                | StaticPosition(x, y, z) -> x, y, z
                | MovingPosition(x, y, z, _, _, _) -> x, y, z

            let dx = tx - agent.X
            let dy = ty - agent.Y
            let dz = tz - agent.Z
            let dist = sqrt (dx * dx + dy * dy + dz * dz)

            if dist < 0.01 then
                Force.zero
            else
                let speed =
                    if dist > BalanceConfig.SteeringSlowRadius then
                        maxSpeed
                    else
                        maxSpeed * (dist / BalanceConfig.SteeringSlowRadius)

                { Fx = dx / dist * speed - agent.Vx
                  Fy = dy / dist * speed - agent.Vy
                  Fz = dz / dist * speed - agent.Vz }

        let pursuit (agent: Spatial) (target: SteeringTarget) (maxSpeed: float) : Force =
            match target with
            | MovingPosition(tx, ty, tz, tvx, tvy, tvz) ->
                let dx = tx - agent.X
                let dy = ty - agent.Y
                let dz = tz - agent.Z
                let dist = sqrt (dx * dx + dy * dy + dz * dz)
                let lookahead = min (dist / maxSpeed) 1.0
                let predX = tx + tvx * lookahead
                let predY = ty + tvy * lookahead
                let predZ = tz + tvz * lookahead
                seek agent (StaticPosition(predX, predY, predZ)) maxSpeed
            | StaticPosition(x, y, z) -> seek agent (StaticPosition(x, y, z)) maxSpeed

        let flee (agent: Spatial) (target: SteeringTarget) (maxSpeed: float) : Force =
            let tx, ty, tz =
                match target with
                | StaticPosition(x, y, z) -> x, y, z
                | MovingPosition(x, y, z, _, _, _) -> x, y, z

            let dx = agent.X - tx
            let dy = agent.Y - ty
            let dz = agent.Z - tz
            let dist = sqrt (dx * dx + dy * dy + dz * dz)

            if dist > BalanceConfig.SteeringFleeRadius then
                Force.zero
            elif dist < 0.01 then
                { Fx = 1.0 * maxSpeed
                  Fy = 0.0
                  Fz = 0.0 }
            else
                { Fx = dx / dist * maxSpeed - agent.Vx
                  Fy = dy / dist * maxSpeed - agent.Vy
                  Fz = dz / dist * maxSpeed - agent.Vz }

        let separation (agent: Spatial) (teammates: Spatial[]) (minDist: float) : Force =
            teammates
            |> Array.fold
                (fun acc tm ->
                    let dx = agent.X - tm.X
                    let dy = agent.Y - tm.Y
                    let dist = sqrt (dx * dx + dy * dy)

                    if dist < minDist && dist > 0.001 then
                        let strength = (minDist - dist) / minDist

                        { acc with
                            Fx = acc.Fx + dx / dist * strength
                            Fy = acc.Fy + dy / dist * strength }
                    else
                        acc)
                Force.zero

        let alignment (agent: Spatial) (reference: Spatial) : Force =
            { Fx = (reference.Vx - agent.Vx) * BalanceConfig.SteeringAlignmentWeight
              Fy = (reference.Vy - agent.Vy) * BalanceConfig.SteeringAlignmentWeight
              Fz = 0.0 }

    module PlayerPhysics =

        let playerMass (p: Player) : float =
            BalanceConfig.PlayerMassBase
            + float p.Weight * BalanceConfig.PlayerMassWeightCoeff
            + float p.Physical.Strength * BalanceConfig.PlayerMassStrengthCoeff

        let turnConstraintLimit (p: Player) (currentSpeed: float) : float =
            let agilityFactor =
                1.0
                - float p.Physical.Agility / 100.0 * BalanceConfig.TurnConstraintAgilityCoeff

            let speedFactor = Math.Clamp(currentSpeed / BalanceConfig.MoveSpeedMax, 0.0, 1.0)

            BalanceConfig.TurnConstraintBaseLimit
            * agilityFactor
            * (1.0 - speedFactor * 0.5)

        let maxSpeed (p: Player) (condition: int) : float =
            let pace = float p.Physical.Pace
            let accel = float p.Physical.Acceleration
            let cond = float condition / 100.0

            Math.Clamp(
                (pace * 0.6 + accel * 0.4) / 100.0 * cond * BalanceConfig.MoveSpeedBase,
                BalanceConfig.MoveSpeedMin,
                BalanceConfig.MoveSpeedMax
            )

        let steer
            (p: Player)
            (condition: int)
            (current: Spatial)
            (tacticalTarget: float * float)
            (teammates: Spatial[])
            (ballPos: Spatial)
            (hasBall: bool)
            (dt: float)
            : Spatial =
            let tx, ty = tacticalTarget
            let target = StaticPosition(tx, ty, 0.0)
            let ms = maxSpeed p condition
            let mass = playerMass p

            let arriveForce = Behaviours.arrive current target ms

            let sepForce =
                Behaviours.separation current teammates BalanceConfig.SeparationMinDistance

            let ballForce =
                if hasBall then
                    Behaviours.pursuit
                        current
                        (MovingPosition(ballPos.X, ballPos.Y, ballPos.Z, ballPos.Vx, ballPos.Vy, ballPos.Vz))
                        ms
                else
                    Force.zero

            let totalForce =
                Force.add arriveForce (Force.add sepForce ballForce)
                |> Force.truncate BalanceConfig.PlayerMaxForce

            let currentSpeed = sqrt (current.Vx * current.Vx + current.Vy * current.Vy)

            let lateralAccel =
                sqrt (totalForce.Fx * totalForce.Fx + totalForce.Fy * totalForce.Fy)

            let turnLimit = turnConstraintLimit p currentSpeed

            let constrainedForce =
                if lateralAccel > turnLimit && lateralAccel > 0.0 then
                    let scale = turnLimit / lateralAccel

                    { Fx = totalForce.Fx * scale
                      Fy = totalForce.Fy * scale
                      Fz = totalForce.Fz }
                else
                    totalForce

            let newVx = current.Vx + constrainedForce.Fx * dt
            let newVy = current.Vy + constrainedForce.Fy * dt
            let newVz = current.Vz + constrainedForce.Fz * dt

            { current with
                X = Math.Clamp(current.X + newVx * dt, 0.0, 100.0)
                Y = Math.Clamp(current.Y + newVy * dt, 0.0, 100.0)
                Z = Math.Max(0.0, current.Z + newVz * dt)
                Vx = newVx
                Vy = newVy
                Vz = newVz }
