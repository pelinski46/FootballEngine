namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.PlayerSteering

module SteeringPipeline =

    /// Pure steering calculation combining tactical directives with physics
    /// Called at SteeringRate (40 subTicks = 1 tick)
    let calculate
        (player: Player)
        (profile: BehavioralProfile)
        (slot: ActiveSlot)
        (dt: float<second>)
        (currentSubTick: int)
        : Spatial =

        // 1. Use cached weighted target
        let targetX, targetY = slot.CachedTarget

        // 2. Use cached movement execution
        let exec = slot.CachedExecution

        // 3. Apply fatigue multiplier (Simplified for steering tick)
        let fatigueMultiplier = float slot.Condition / 100.0

        // 4. No organic drift
        let driftedTargetX = targetX
        let driftedTargetY = targetY

        // 5. Calculate steering force with max speed from player attributes
        let baseSpeed = playerMaxSpeed player.Physical.Pace slot.Condition
        let maxSpeed = baseSpeed * exec * fatigueMultiplier

        // Use Spatial for distance calculations, SteeringTarget for steering behavior
        let targetSpatial =
            { X = driftedTargetX
              Y = driftedTargetY
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }

        let targetDirection = StaticPosition(driftedTargetX, driftedTargetY, 0.0<meter>)

        let distSq = slot.Pos.DistSqTo2D targetSpatial
        
        // Si el objetivo tiene mucha urgencia/peso (va a por la pelota), usamos SEEK para no frenar
        // El peso de Ball Chase ahora es > 10.0
        let force =
            if exec > 5.0 then 
                Behaviours.seek slot.Pos targetDirection maxSpeed
            else
                if distSq < 1.0<meter^2> then
                    Behaviours.arrive slot.Pos targetDirection maxSpeed
                else
                    Behaviours.seek slot.Pos targetDirection maxSpeed

        // 6. Integrate physics: v' = v + a*dt, p' = p + v'*dt
        let newVx = slot.Pos.Vx + force.Fx * dt
        let newVy = slot.Pos.Vy + force.Fy * dt
        let newVz = slot.Pos.Vz + force.Fz * dt

        let speedMagSq = newVx * newVx + newVy * newVy + newVz * newVz
        let maxSpeedSq = maxSpeed * maxSpeed

        let (adjVx, adjVy, adjVz) =
            if speedMagSq > maxSpeedSq then
                let speedMag = sqrt speedMagSq
                (newVx * maxSpeed / speedMag, newVy * maxSpeed / speedMag, newVz * maxSpeed / speedMag)
            else
                (newVx, newVy, newVz)

        let newX = slot.Pos.X + adjVx * dt
        let newY = slot.Pos.Y + adjVy * dt
        let newZ = slot.Pos.Z + adjVz * dt

        // 7. Clamp to field bounds
        let boundX = PhysicsContract.clamp newX 0.0<meter> PhysicsContract.PitchLength
        let boundY = PhysicsContract.clamp newY 0.0<meter> PhysicsContract.PitchWidth
        let boundZ = PhysicsContract.clamp newZ 0.0<meter> 1.0<meter>

        { X = boundX
          Y = boundY
          Z = boundZ
          Vx = adjVx
          Vy = adjVy
          Vz = adjVz }
