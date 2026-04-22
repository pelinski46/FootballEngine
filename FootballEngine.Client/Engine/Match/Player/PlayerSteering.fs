namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open FootballEngine.PhysicsContract
open SimStateOps

module PlayerSteering =

    [<Struct>]
    type Force = { Fx: float<meter/second^2>; Fy: float<meter/second^2>; Fz: float<meter/second^2> }

    module Force =
        let zero = { Fx = 0.0<meter/second^2>; Fy = 0.0<meter/second^2>; Fz = 0.0<meter/second^2> }

        let add a b =
            { Fx = a.Fx + b.Fx
              Fy = a.Fy + b.Fy
              Fz = a.Fz + b.Fz }

        let scale (s: float) f =
            { Fx = f.Fx * s
              Fy = f.Fy * s
              Fz = f.Fz * s }

        let magnitude f =
            sqrt (f.Fx * f.Fx + f.Fy * f.Fy + f.Fz * f.Fz)

        let normalize (f: Force) : Force =
            let m = magnitude f

            if m < 0.001<meter/second^2> then
                zero
            else
                let scale = 1.0<meter/second^2> / m
                { Fx = f.Fx * scale
                  Fy = f.Fy * scale
                  Fz = f.Fz * scale }

        let truncate (maxMag: float<meter/second^2>) (f: Force) : Force =
            let m = magnitude f
            if m > maxMag then f |> normalize |> scale (float (maxMag / m)) else f

    type SteeringTarget =
        | StaticPosition of x: float<meter> * y: float<meter> * z: float<meter>
        | MovingPosition of x: float<meter> * y: float<meter> * z: float<meter> * vx: float<meter/second> * vy: float<meter/second> * vz: float<meter/second>

    module Behaviours =

        let seek (agent: Spatial) (target: SteeringTarget) (maxSpeed: float<meter/second>) : Force =
            let targetPos =
                match target with
                | StaticPosition(x, y, z) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | MovingPosition(x, y, z, _, _, _) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

            let dist = agent.DistTo targetPos

            if dist < 0.01<meter> then
                Force.zero
            else
                let dx = targetPos.X - agent.X
                let dy = targetPos.Y - agent.Y
                let dz = targetPos.Z - agent.Z
                let desiredSpeed = min maxSpeed (dist / 0.5<second>)
                let desiredVx = (dx / dist) * desiredSpeed
                let desiredVy = (dy / dist) * desiredSpeed
                let desiredVz = (dz / dist) * desiredSpeed

                { Fx = (desiredVx - agent.Vx) / PhysicsContract.SteeringSeekTimeConstant
                  Fy = (desiredVy - agent.Vy) / PhysicsContract.SteeringSeekTimeConstant
                  Fz = (desiredVz - agent.Vz) / PhysicsContract.SteeringSeekTimeConstant }

        let arrive (agent: Spatial) (target: SteeringTarget) (maxSpeed: float<meter/second>) : Force =
            let targetPos =
                match target with
                | StaticPosition(x, y, z) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | MovingPosition(x, y, z, _, _, _) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

            let dist = agent.DistTo targetPos

            if dist < 0.01<meter> then
                Force.zero
            else
                let dx = targetPos.X - agent.X
                let dy = targetPos.Y - agent.Y
                let dz = targetPos.Z - agent.Z
                let speed =
                    if dist > PhysicsContract.SteeringSlowRadius then
                        maxSpeed
                    else
                        maxSpeed * (dist / PhysicsContract.SteeringSlowRadius)

                let desiredVx = (dx / dist) * speed
                let desiredVy = (dy / dist) * speed
                let desiredVz = (dz / dist) * speed

                { Fx = (desiredVx - agent.Vx) / PhysicsContract.SteeringArriveTimeConstant
                  Fy = (desiredVy - agent.Vy) / PhysicsContract.SteeringArriveTimeConstant
                  Fz = (desiredVz - agent.Vz) / PhysicsContract.SteeringArriveTimeConstant }

        let pursuit (agent: Spatial) (target: SteeringTarget) (maxSpeed: float<meter/second>) : Force =
            match target with
            | MovingPosition(tx, ty, tz, tvx, tvy, tvz) ->
                let targetPos = { X = tx; Y = ty; Z = tz; Vx = tvx; Vy = tvy; Vz = tvz }
                let dist = agent.DistTo targetPos
                let lookahead = min (dist / maxSpeed) 1.0<second>
                let predX = tx + tvx * lookahead
                let predY = ty + tvy * lookahead
                let predZ = tz + tvz * lookahead
                seek agent (StaticPosition(predX, predY, predZ)) maxSpeed
            | StaticPosition(x, y, z) -> seek agent (StaticPosition(x, y, z)) maxSpeed

        let flee (config: PhysicsConfig) (agent: Spatial) (target: SteeringTarget) (maxSpeed: float<meter/second>) : Force =
            let targetPos =
                match target with
                | StaticPosition(x, y, z) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | MovingPosition(x, y, z, _, _, _) -> { X = x; Y = y; Z = z; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

            let dist = agent.DistTo targetPos

            if dist > config.SteeringFleeRadius then
                Force.zero
            elif dist < 0.01<meter> then
                let desiredVx = 1.0 * maxSpeed
                { Fx = (desiredVx - agent.Vx) / PhysicsContract.SteeringFleeTimeConstant
                  Fy = 0.0<meter/second^2>
                  Fz = 0.0<meter/second^2> }
            else
                let dx = agent.X - targetPos.X
                let dy = agent.Y - targetPos.Y
                let dz = agent.Z - targetPos.Z
                let desiredSpeed = min maxSpeed (dist / 0.5<second>)
                let desiredVx = (dx / dist) * desiredSpeed
                let desiredVy = (dy / dist) * desiredSpeed
                let desiredVz = (dz / dist) * desiredSpeed
                { Fx = (desiredVx - agent.Vx) / PhysicsContract.SteeringFleeTimeConstant
                  Fy = (desiredVy - agent.Vy) / PhysicsContract.SteeringFleeTimeConstant
                  Fz = (desiredVz - agent.Vz) / PhysicsContract.SteeringFleeTimeConstant }

        let separation (agent: Spatial) (positions: Spatial[]) (myIdx: int) (minDist: float<meter>) : Force =
            let mutable fx = 0.0
            let mutable fy = 0.0
            let minDistSq = minDist * minDist
            for i = 0 to positions.Length - 1 do
                if i <> myIdx then
                    let tm = positions[i]
                    let dSq = agent.DistSqTo2D tm
                    if dSq < minDistSq && dSq > 1e-6<meter^2> then
                        let dist = sqrt dSq
                        let strength = (minDist - dist) / minDist
                        fx <- fx + ((agent.X - tm.X) / dist) * strength
                        fy <- fy + ((agent.Y - tm.Y) / dist) * strength
            // Convert separation influence (unitless) into acceleration using PlayerAccelMax
            { Fx = fx * PhysicsContract.PlayerAccelMax
              Fy = fy * PhysicsContract.PlayerAccelMax
              Fz = 0.0<meter/second^2> }

        let alignment (config: PhysicsConfig) (agent: Spatial) (reference: Spatial) : Force =
            let dvxRaw = (reference.Vx - agent.Vx)
            let dvyRaw = (reference.Vy - agent.Vy)
            // Convert velocity difference into acceleration using alignment time constant
            let dvx = (dvxRaw / PhysicsContract.SteeringAlignmentTimeConstant) * config.SteeringAlignmentWeight
            let dvy = (dvyRaw / PhysicsContract.SteeringAlignmentTimeConstant) * config.SteeringAlignmentWeight
            { Fx = dvx
              Fy = dvy
              Fz = 0.0<meter/second^2> }

        let cohesion (agent: Spatial) (positions: Spatial[]) (myIdx: int) (maxSpeed: float<meter/second>) : Force =
            let mutable cx = 0.0<meter>
            let mutable cy = 0.0<meter>
            let mutable count = 0
            for i = 0 to positions.Length - 1 do
                if i <> myIdx then
                    cx <- cx + positions[i].X
                    cy <- cy + positions[i].Y
                    count <- count + 1
            if count = 0 then
                Force.zero
            else
                let target = { X = cx / float count; Y = cy / float count; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                let dist = agent.DistTo2D target
                if dist < 0.01<meter> then
                    Force.zero
                else
                    let speed = if dist > PhysicsContract.SteeringSlowRadius then maxSpeed else maxSpeed * (dist / PhysicsContract.SteeringSlowRadius)
                    let desiredVx = ((target.X - agent.X) / dist) * speed
                    let desiredVy = ((target.Y - agent.Y) / dist) * speed
                    { Fx = (desiredVx - agent.Vx) / PhysicsContract.SteeringCohesionTimeConstant
                      Fy = (desiredVy - agent.Vy) / PhysicsContract.SteeringCohesionTimeConstant
                      Fz = 0.0<meter/second^2> }

    module PlayerPhysics =

        let playerMass (config: PhysicsConfig) (p: Player) : float =
            config.PlayerMassBase
            + float p.Weight * config.PlayerMassWeightCoeff
            + float p.Physical.Strength * config.PlayerMassStrengthCoeff

        let turnConstraintLimit (config: PhysicsConfig) (p: Player) (currentSpeed: float<meter/second>) : float<meter/second^2> =
            let agilityFactor =
                1.0
                - PhysicsContract.normaliseAttr p.Physical.Agility * config.TurnConstraintAgilityCoeff

            let speedRatio = currentSpeed / config.MoveSpeedMax
            let speedFactor = PhysicsContract.clampFloat speedRatio 0.0 1.0

            // Convert dimensionless limit into acceleration by scaling with PlayerAccelMax
            config.TurnConstraintBaseLimit
            * agilityFactor
            * (1.0 - speedFactor * 0.5)
            * PhysicsContract.PlayerAccelMax

        let maxSpeed (p: Player) (condition: int) : float<meter/second> =
            PhysicsContract.playerMaxSpeed p.Physical.Pace condition

        let steer
            (config: PhysicsConfig)
            (p: Player)
            (condition: int)
            (current: Spatial)
            (myIdx: int)
            (positions: Spatial[])
            (tacticalTarget: float<meter> * float<meter>)
            (ballPos: Spatial)
            (hasBall: bool)
            (chasingBall: bool)
            (dt: float<second>)
            : Spatial =
            let tx, ty = tacticalTarget
            let target = StaticPosition(tx, ty, 0.0<meter>)
            let ms = maxSpeed p condition

            let arriveForce = Behaviours.arrive current target ms

            let sepDist =
                if chasingBall then
                    config.BallContestSeparationRadius
                else
                    config.SeparationMinDistance

            let sepForce =
                Behaviours.separation current positions myIdx sepDist

            let cohesionForce =
                Behaviours.cohesion current positions myIdx ms
                |> Force.scale config.CohesionWeight

            let ballForce =
                if hasBall then
                    Behaviours.pursuit
                        current
                        (MovingPosition(ballPos.X, ballPos.Y, ballPos.Z, ballPos.Vx, ballPos.Vy, ballPos.Vz))
                        ms
                else
                    Force.zero

            let totalForce =
                Force.add arriveForce (Force.add sepForce (Force.add cohesionForce ballForce))
                |> Force.truncate config.PlayerMaxForce

            let currentSpeed = current.VelMag

            let lateralAccelMag =
                sqrt (totalForce.Fx * totalForce.Fx + totalForce.Fy * totalForce.Fy)

            let turnLimit = turnConstraintLimit config p currentSpeed

            let constrainedForce =
                if lateralAccelMag > turnLimit && lateralAccelMag > 0.0<meter/second^2> then
                    let scale = turnLimit / lateralAccelMag

                    { Fx = totalForce.Fx * scale
                      Fy = totalForce.Fy * scale
                      Fz = totalForce.Fz }
                else
                    totalForce

            let newVx = current.Vx + constrainedForce.Fx * dt
            let newVy = current.Vy + constrainedForce.Fy * dt
            let newVz = current.Vz + constrainedForce.Fz * dt

            { current with
                X = PhysicsContract.clamp (current.X + newVx * dt) 0.0<meter> PhysicsContract.PitchLength
                Y = PhysicsContract.clamp (current.Y + newVy * dt) 0.0<meter> PhysicsContract.PitchWidth
                Z = max 0.0<meter> (current.Z + newVz * dt)
                Vx = newVx
                Vy = newVy
                Vz = newVz }
