namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
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
        (homeSlots: PlayerSlot[])
        (awaySlots: PlayerSlot[])
        : BallPhysicsState * PlayerId option =

        let r = BalanceConfig.BallContactRadius
        let rSq = r * r
        let mutable closestDistSq = System.Double.MaxValue
        let mutable closestPlayer: Player option = None

        let mutable closestPos =
            { X = 0.0
              Y = 0.0
              Z = 0.0
              Vx = 0.0
              Vy = 0.0
              Vz = 0.0 }

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

        for i = 0 to homeSlots.Length - 1 do
            match homeSlots[i] with
            | PlayerSlot.Active s -> checkContact s.Player s.Pos
            | _ -> ()

        for i = 0 to awaySlots.Length - 1 do
            match awaySlots[i] with
            | PlayerSlot.Active s -> checkContact s.Player s.Pos
            | _ -> ()

        if closestDistSq < rSq then
            match closestPlayer with
            | Some player ->
                let resolved = resolveContact ball closestPos
                let canControl = ball.Position.Z < 0.6
                let controller = if canControl then Some player.Id else None
                resolved, controller
            | None -> ball, None
        else
            ball, None

    let agent homeId homeSquad awaySquad tick (ctx: MatchContext) (state: SimState) : AgentOutput =
        let stepped = BallPhysics.update state.Ball
        let resolved, ctrl = resolveContacts stepped state.Home.Slots state.Away.Slots
        let prevControlled = state.Ball.ControlledBy

        state.Ball <-
            match ctrl with
            | Some _ ->
                let dampened =
                    { resolved.Position with
                        Vx = resolved.Position.Vx * 0.15
                        Vy = resolved.Position.Vy * 0.15
                        Vz = 0.0 }

                { resolved with
                    Position = dampened
                    ControlledBy = ctrl
                    LastTouchBy = ctrl |> Option.orElse state.Ball.LastTouchBy }
            | None ->
                { resolved with
                    ControlledBy = ctrl
                    LastTouchBy = ctrl |> Option.orElse state.Ball.LastTouchBy }

        let nextSubTick = tick.SubTick + PhysicsIntervalSubTicks

        let decisionSpawn =
            match prevControlled, ctrl with
            | None, Some pid ->
                [ { SubTick = nextSubTick
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DecisionTick(0, Some pid) } ]
            | _ -> []

        { Events = []
          Spawned =
            [ { SubTick = nextSubTick
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick } ] @ decisionSpawn
          Transition = None }
