namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SchedulingTypes

module BallAgent =

    let private resolveContact (ball: BallPhysicsState) (playerPos: Spatial) : BallPhysicsState =

        let r = BalanceConfig.BallContactRadius
        let bp = ball.Position
        let dist = bp.DistTo playerPos

        if dist < r && dist > 0.001<meter> then
            let nx = (bp.X - playerPos.X) / dist
            let ny = (bp.Y - playerPos.Y) / dist
            let nz = (bp.Z - playerPos.Z) / dist
            let dot = bp.Vx * nx + bp.Vy * ny + bp.Vz * nz

            if dot > 0.0<meter / second> then
                let impactSpeed = bp.VelMag
                let isAirborne = bp.Z > 0.3<meter>

                let restitution =
                    if isAirborne then
                        0.35 + min 0.25 (float impactSpeed * 0.008)
                    else
                        0.50 + min 0.20 (float impactSpeed * 0.005)

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

        let mutable bestDistSq = PhysicsContract.MaxDistanceSq
        let mutable bestPlayer: Player option = None

        let mutable bestPos =
            { X = 0.0<meter>
              Y = 0.0<meter>
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }

        let bp = ball.Position

        let inline checkContact (player: Player) (pPos: Spatial) =
            let playerRadius =
                BalanceConfig.BallContactRadius
                + (float player.Technical.BallControl / 20.0) * 0.20<meter>

            let dSq = bp.DistSqTo pPos
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
            let mutable found: Player * Spatial * float<meter^2> = Unchecked.defaultof<_>
            let mutable foundIt = false

            for i = 0 to homeSlots.Length - 1 do
                match homeSlots[i] with
                | PlayerSlot.Active s when s.Player.Id = pid ->
                    found <- (s.Player, s.Pos, bp.DistSqTo s.Pos)
                    foundIt <- true
                | _ -> ()

            if not foundIt then
                for i = 0 to awaySlots.Length - 1 do
                    match awaySlots[i] with
                    | PlayerSlot.Active s when s.Player.Id = pid ->
                        found <- (s.Player, s.Pos, bp.DistSqTo s.Pos)
                        foundIt <- true
                    | _ -> ()

            if foundIt then Some found else None

        let hysteresis = 0.30<meter>

        let currentPossession =
            match ball.Possession with
            | Owned(_, pid) -> Some pid
            | _ -> None

        match currentPossession, bestPlayer with
        | _, Some best ->
            let resolved = resolveContact ball bestPos
            let impactSpeed = resolved.Position.VelMag

            let canControl =
                resolved.Position.Z < 0.6<meter> || impactSpeed < 3.0<meter / second>

            if canControl then
                let club =
                    if Array.contains best (SimStateOps.activePlayers homeSlots) then
                        HomeClub
                    else
                        AwayClub

                { resolved with
                    Possession = Owned(club, best.Id) },
                Some best.Id
            else
                resolved, None
        | _, _ -> ball, None

    let agent tick (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentOutput =
        let prevPossession = state.Ball.Possession
        let dt = SimulationClock.dt clock
        let stepped = BallPhysics.update dt state.Ball

        let resolved, ctrl =
            resolveContacts
                stepped
                state.Home.Slots
                state.Away.Slots
                (match prevPossession with
                 | Owned(_, pid) -> Some pid
                 | _ -> None)

        state.Ball <-
            match ctrl with
            | Some pid ->
                let club =
                    match SimStateOps.clubSideOf state pid with
                    | Some c -> c
                    | None -> state.AttackingClub

                let playerOpt =
                    match
                        state.Home.Slots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s.Player
                            | _ -> None)
                    with
                    | Some p -> Some p
                    | None ->
                        state.Away.Slots
                        |> Array.tryPick (function
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
                        Vz = 0.0<meter / second> }

                { resolved with
                    Position = dampened
                    Possession = Owned(club, pid)
                    LastTouchBy = Some pid }
            | None -> { resolved with Possession = Loose }

        match ctrl with
        | Some _ ->
            state.Ball <-
                { state.Ball with
                    StationarySinceSubTick = None }
        | None ->
            let vel =
                sqrt (
                    state.Ball.Position.Vx * state.Ball.Position.Vx
                    + state.Ball.Position.Vy * state.Ball.Position.Vy
                )

            state.Ball <-
                { state.Ball with
                    StationarySinceSubTick =
                        if vel < 1.0<meter / second> then
                            state.Ball.StationarySinceSubTick |> Option.orElse (Some state.SubTick)
                        else
                            None }

        let physicsRate = int (round ((float dt * 40.0)))
        let nextSubTick = tick.SubTick + physicsRate

        let decisionSpawn =
            match prevPossession, ctrl with
            | Loose, Some pid ->
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
                Kind = PhysicsTick } ]
            @ decisionSpawn
          Transition = None }
