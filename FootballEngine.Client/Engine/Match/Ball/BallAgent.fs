namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PlayerSteering
open FootballEngine.SchedulingTypes


module BallAgent =

    let private contactRadius (playerHeight: int) : float =
        float playerHeight * BalanceConfig.PlayerContactRadiusFactor

    let resolvePlayerContact (ball: BallPhysicsState) (playerPos: Spatial) (playerHeight: int) : BallPhysicsState =
        let r = contactRadius playerHeight
        let bPos = ball.Position
        let dx = bPos.X - playerPos.X
        let dy = bPos.Y - playerPos.Y
        let dz = bPos.Z - playerPos.Z
        let dist = sqrt (dx * dx + dy * dy + dz * dz)

        if dist < r && dist > 0.001 then
            let nx = dx / dist
            let ny = dy / dist
            let nz = dz / dist
            let dot = bPos.Vx * nx + bPos.Vy * ny + bPos.Vz * nz

            if dot > 0.0 then
                let restitution = 0.5

                { ball with
                    Position =
                        { bPos with
                            Vx = bPos.Vx - (1.0 + restitution) * dot * nx
                            Vy = bPos.Vy - (1.0 + restitution) * dot * ny
                            Vz = bPos.Vz - (1.0 + restitution) * dot * nz } }
            else
                ball
        else
            ball

    let resolveContacts (ball: BallPhysicsState) (players: (Player * Spatial)[]) : BallPhysicsState * PlayerId option =
        let mutable closestDist = System.Double.MaxValue
        let mutable closestIdx = -1

        for i = 0 to players.Length - 1 do
            let _, pPos = players[i]
            let bPos = ball.Position
            let dx = bPos.X - pPos.X
            let dy = bPos.Y - pPos.Y
            let dz = bPos.Z - pPos.Z
            let dist = dx * dx + dy * dy + dz * dz

            if dist < closestDist then
                closestDist <- dist
                closestIdx <- i

        if closestIdx >= 0 then
            let player, pPos = players[closestIdx]
            let r = contactRadius player.Height

            if closestDist < r * r then
                let resolved = resolvePlayerContact ball pPos player.Height
                resolved, Some player.Id
            else
                ball, None
        else
            ball, None

    let update
        (dt: float)
        (players: (Player * Spatial)[])
        (ball: BallPhysicsState)
        : BallPhysicsState * PlayerId option =
        let physicsBall = BallPhysics.update dt ball
        resolveContacts physicsBall players

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        let subStepDt = 0.1
        let subSteps = 25 // simula ~2.5s de física real por tick

        let allPlayersArr =
            Array.append
                (Array.zip state.HomeSide.Players state.HomeSide.Positions)
                (Array.zip state.AwaySide.Players state.AwaySide.Positions)

        let mutable ball = state.Ball
        let mutable lastTouch = state.Ball.LastTouchBy

        for _ in 1..subSteps do
            let b, t = update subStepDt allPlayersArr ball

            ball <-
                { b with
                    LastTouchBy = t |> Option.orElse lastTouch }

            if t.IsSome then
                lastTouch <- t

        let newState =
            { state with
                Ball = ball }

        { State = newState
          Events = []
          Spawned =
            [ { Second = tick.Second + 5
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick } ]
          Transition = None }
