namespace FootballEngine.Movement

open System
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.PlayerSteering

module SteeringPipeline =

    [<Literal>]
    let MaxForce = 50.0<meter / second^2>

    [<Literal>]
    let DeadZone = 0.5<meter>

    let calculate
        (player: Player)
        (profile: BehavioralProfile)
        (slot: ActiveSlot)
        (dt: float<second>)
        (currentSubTick: int)
        (ball: BallPhysicsState)
        (state: SimState)
        : Spatial =

        let exec = slot.CachedExecution
        let baseSpeed = PhysicsContract.playerMaxSpeed player.Physical.Pace slot.Condition
        let maxSpeed = baseSpeed * exec * (float slot.Condition / 100.0)

        let isChasing =
            slot.Directives |> Array.exists (fun d -> d.Kind = Press && not (Directive.expired currentSubTick d))

        let targetPos =
            if isChasing then
                let dist = slot.Pos.DistTo ball.Position

                let lookahead =
                    Math.Min(float (dist / (maxSpeed + 1.0<meter / second>)), 1.5) * 1.0<second>

                StaticPosition(
                    ball.Position.X + ball.Position.Vx * lookahead,
                    ball.Position.Y + ball.Position.Vy * lookahead,
                    0.0<meter>
                )
            else
                let tx, ty = slot.CachedTarget
                StaticPosition(tx, ty, 0.0<meter>)

        let rawForce =
            if exec > 5.0 then
                Behaviours.seek slot.Pos targetPos maxSpeed
            else
                Behaviours.arrive slot.Pos targetPos maxSpeed

        let targetX, targetY =
            match targetPos with
            | StaticPosition(x, y, _) -> x, y
            | MovingPosition(x, y, _, _, _, _) -> x, y

        let distToTarget =
            slot.Pos.DistTo2D
                { X = targetX
                  Y = targetY
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

        let forceScale =
            if not isChasing && distToTarget < DeadZone then
                float (distToTarget / DeadZone)
            else
                1.0



        let scaledForce =
            { Fx = rawForce.Fx * forceScale
              Fy = rawForce.Fy * forceScale
              Fz = 0.0<meter / second^2> }

        let forceMag =
            sqrt (float (scaledForce.Fx * scaledForce.Fx + scaledForce.Fy * scaledForce.Fy))

        let limitedForce =
            if forceMag > float MaxForce then
                { Fx = scaledForce.Fx * (float MaxForce / forceMag)
                  Fy = scaledForce.Fy * (float MaxForce / forceMag)
                  Fz = 0.0<meter / second^2> }
            else
                scaledForce


        let newVx =
            PhysicsContract.clamp (slot.Pos.Vx + limitedForce.Fx * dt) -maxSpeed maxSpeed

        let newVy =
            PhysicsContract.clamp (slot.Pos.Vy + limitedForce.Fy * dt) -maxSpeed maxSpeed

        let finalX =
            PhysicsContract.clamp (slot.Pos.X + newVx * dt) 0.0<meter> PhysicsContract.PitchLength

        let finalY =
            PhysicsContract.clamp (slot.Pos.Y + newVy * dt) 0.0<meter> PhysicsContract.PitchWidth


        { slot.Pos with
            X = finalX
            Y = finalY
            Vx = newVx
            Vy = newVy }
