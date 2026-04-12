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
        (currentController: PlayerId option)
        : BallPhysicsState * PlayerId option =

        let mutable bestDistSq = System.Double.MaxValue
        let mutable bestPlayer: Player option = None

        let mutable bestPos =
            { X = 0.0
              Y = 0.0
              Z = 0.0
              Vx = 0.0
              Vy = 0.0
              Vz = 0.0 }

        let bp = ball.Position

        let inline checkContact (player: Player) (pPos: Spatial) =
            let playerRadius = BalanceConfig.BallContactRadius + (float player.Technical.BallControl / 20.0) * 0.20
            let dx = bp.X - pPos.X
            let dy = bp.Y - pPos.Y
            let dz = bp.Z - pPos.Z
            let dSq = dx * dx + dy * dy + dz * dz
            let rSq = playerRadius * playerRadius

            if dSq < rSq && dSq < bestDistSq then
                bestDistSq <- dSq
                bestPlayer <- Some player
                bestPos <- pPos

        for i = 0 to homeSlots.Length - 1 do
            match homeSlots[i] with
            | PlayerSlot.Active s -> checkContact s.Player s.Pos
            | _ -> ()

        for i = 0 to awaySlots.Length - 1 do
            match awaySlots[i] with
            | PlayerSlot.Active s -> checkContact s.Player s.Pos
            | _ -> ()

        let findPlayerById pid =
            let mutable found: Player * Spatial * float = Unchecked.defaultof<_>
            let mutable foundIt = false

            for i = 0 to homeSlots.Length - 1 do
                match homeSlots[i] with
                | PlayerSlot.Active s when s.Player.Id = pid ->
                    let dx = bp.X - s.Pos.X
                    let dy = bp.Y - s.Pos.Y
                    let dz = bp.Z - s.Pos.Z
                    let dSq = dx * dx + dy * dy + dz * dz
                    found <- (s.Player, s.Pos, dSq)
                    foundIt <- true
                | _ -> ()

            if not foundIt then
                for i = 0 to awaySlots.Length - 1 do
                    match awaySlots[i] with
                    | PlayerSlot.Active s when s.Player.Id = pid ->
                        let dx = bp.X - s.Pos.X
                        let dy = bp.Y - s.Pos.Y
                        let dz = bp.Z - s.Pos.Z
                        let dSq = dx * dx + dy * dy + dz * dz
                        found <- (s.Player, s.Pos, dSq)
                        foundIt <- true
                    | _ -> ()

            if foundIt then Some found else None

        let hysteresis = 0.30

        match currentController, bestPlayer with
        | Some currentPid, Some best ->
            match findPlayerById currentPid with
            | Some (currentPlayer, currentPos, currentDistSq) ->
                let currentRadius = BalanceConfig.BallContactRadius + (float currentPlayer.Technical.BallControl / 20.0) * 0.20
                let currentDist = sqrt currentDistSq
                let bestDist = sqrt bestDistSq

                if currentDist < currentRadius && best.Id <> currentPid && bestDist >= (currentDist - hysteresis) then
                    let resolved = resolveContact ball currentPos
                    let canControl = ball.Position.Z < 0.6
                    resolved, (if canControl then Some currentPid else None)
                else
                    let resolved = resolveContact ball bestPos
                    let canControl = ball.Position.Z < 0.6
                    resolved, (if canControl then Some best.Id else None)
            | None ->
                let resolved = resolveContact ball bestPos
                let canControl = ball.Position.Z < 0.6
                resolved, (if canControl then Some best.Id else None)
        | _, Some player ->
            let resolved = resolveContact ball bestPos
            let canControl = ball.Position.Z < 0.6
            resolved, (if canControl then Some player.Id else None)
        | _ ->
            ball, None

    let agent homeId homeSquad awaySquad tick (ctx: MatchContext) (state: SimState) : AgentOutput =
        let prevControlled = state.Ball.ControlledBy
        let stepped = BallPhysics.update state.Ball
        let resolved, ctrl = resolveContacts stepped state.Home.Slots state.Away.Slots prevControlled

        state.Ball <-
            match ctrl with
            | Some pid ->
                let club =
                    match SimStateOps.clubSideOf state pid with
                    | Some c -> c
                    | None -> state.AttackingClub

                let playerOpt =
                    match state.Home.Slots |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                        | _ -> None) with
                    | Some p -> Some p
                    | None ->
                        state.Away.Slots |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                            | _ -> None)

                let ballControlNorm =
                    match playerOpt with
                    | Some p -> float p.Technical.BallControl / 20.0
                    | None -> 0.5

                let damp = 0.35 - ballControlNorm * 0.30

                let dampened =
                    { resolved.Position with
                        Vx = resolved.Position.Vx * damp
                        Vy = resolved.Position.Vy * damp
                        Vz = 0.0 }

                { resolved with
                    Position = dampened
                    ControlledBy = ctrl
                    LastTouchBy = ctrl |> Option.orElse state.Ball.LastTouchBy
                    Phase = PossessionPhase.InPossession club }
            | None ->
                { resolved with
                    ControlledBy = ctrl
                    LastTouchBy = ctrl |> Option.orElse state.Ball.LastTouchBy }

        match state.Ball.ControlledBy with
        | Some _ -> ()
        | None ->
            let vel =
                sqrt (state.Ball.Position.Vx * state.Ball.Position.Vx + state.Ball.Position.Vy * state.Ball.Position.Vy)

            if vel < 1.0 then
                let bpX = state.Ball.Position.X
                let bpY = state.Ball.Position.Y
                let mutable nearestDistSq = System.Double.MaxValue
                let mutable nearestPlayer: Player option = None

                for i = 0 to state.Home.Slots.Length - 1 do
                    match state.Home.Slots[i] with
                    | PlayerSlot.Active s ->
                        let dx = s.Pos.X - bpX
                        let dy = s.Pos.Y - bpY
                        let dSq = dx * dx + dy * dy
                        if dSq < nearestDistSq then
                            nearestDistSq <- dSq
                            nearestPlayer <- Some s.Player
                    | _ -> ()

                for i = 0 to state.Away.Slots.Length - 1 do
                    match state.Away.Slots[i] with
                    | PlayerSlot.Active s ->
                        let dx = s.Pos.X - bpX
                        let dy = s.Pos.Y - bpY
                        let dSq = dx * dx + dy * dy
                        if dSq < nearestDistSq then
                            nearestDistSq <- dSq
                            nearestPlayer <- Some s.Player
                    | _ -> ()

                if nearestDistSq < 25.0 then
                    match nearestPlayer with
                    | Some p ->
                        let club =
                            match SimStateOps.clubSideOf state p.Id with
                            | Some c -> c
                            | None -> state.AttackingClub

                        state.Ball <-
                            { state.Ball with
                                Position =
                                    { state.Ball.Position with
                                        Vx = state.Ball.Position.Vx * 0.20
                                        Vy = state.Ball.Position.Vy * 0.20
                                        Vz = 0.0 }
                                ControlledBy = Some p.Id
                                LastTouchBy = Some p.Id
                                Phase = PossessionPhase.InPossession club }
                    | None -> ()

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
