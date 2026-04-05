namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module BallAgent =

    let private resolveContact (ball: BallPhysicsState) (playerPos: Spatial) : BallPhysicsState =

        let r = BalanceConfig.BallContactRadius
        let bp = ball.Position
        let dx = bp.X - playerPos.X
        let dy = bp.Y - playerPos.Y
        let dz = bp.Z - playerPos.Z
        let dist = sqrt (dx * dx + dy * dy + dz * dz)

        if dist < r && dist > 0.001 then
            let nx = dx / dist
            let ny = dy / dist
            let nz = dz / dist
            let dot = bp.Vx * nx + bp.Vy * ny + bp.Vz * nz

            if dot > 0.0 then
                let restitution = 0.5

                { ball with
                    Position =
                        { bp with
                            Vx = bp.Vx - (1.0 + restitution) * dot * nx
                            Vy = bp.Vy - (1.0 + restitution) * dot * ny
                            Vz = bp.Vz - (1.0 + restitution) * dot * nz } }
            else
                ball
        else
            ball

    let private resolveContacts
        (ball: BallPhysicsState)
        (homePlayers: Player[])
        (homePositions: Spatial[])
        (awayPlayers: Player[])
        (awayPositions: Spatial[])
        : BallPhysicsState * PlayerId option =

        let r = BalanceConfig.BallContactRadius
        let rSq = r * r
        let mutable closestDistSq = System.Double.MaxValue
        let mutable closestPlayer: Player option = None
        let mutable closestPos = { X = 0.0; Y = 0.0; Z = 0.0; Vx = 0.0; Vy = 0.0; Vz = 0.0 }
        let bp = ball.Position

        let inline checkContact (player: Player) (pPos: Spatial) =
            let dx = bp.X - pPos.X
            let dy = bp.Y - pPos.Y
            let dz = bp.Z - pPos.Z
            let dSq = dx * dx + dy * dy + dz * dz
            if dSq < closestDistSq then
                closestDistSq <- dSq
                closestPlayer <- Some player
                closestPos <- pPos

        for i = 0 to homePlayers.Length - 1 do
            checkContact homePlayers[i] homePositions[i]

        for i = 0 to awayPlayers.Length - 1 do
            checkContact awayPlayers[i] awayPositions[i]

        if closestDistSq < rSq then
            match closestPlayer with
            | Some player ->
                let resolved = resolveContact ball closestPos
                let canControl = ball.Position.Z < 0.6
                let controller = if canControl then Some player.Id else None
                resolved, controller
            | None ->
                ball, None
        else
            ball, None

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        let stepped = BallPhysics.update state.Ball
        let resolved, ctrl =
            resolveContacts stepped
                state.HomeSide.Players state.HomeSide.Positions
                state.AwaySide.Players state.AwaySide.Positions

        let newBall =
            { resolved with
                ControlledBy = ctrl
                LastTouchBy = ctrl |> Option.orElse state.Ball.LastTouchBy }

        let nextSubTick = tick.SubTick + 1

        { State = { state with Ball = newBall }
          Events = []
          Spawned =
            [ { SubTick = nextSubTick
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick } ]
          Transition = None }
