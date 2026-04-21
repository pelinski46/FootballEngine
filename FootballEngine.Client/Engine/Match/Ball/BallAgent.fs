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

        let bp = ball.Position

        let winner, winnerPosOpt, contested =
            Interception.chooseBestInterceptor bp homeSlots awaySlots

        match winner, contested with
        | Some p, false ->
            match winnerPosOpt with
            | Some pos ->
                let resolved = resolveContact ball pos
                resolved, Some p.Id
            | None -> ball, Some p.Id
        | _, _ -> ball, None

    let private findNearestChaser
        (ballPos: Spatial)
        (homeSlots: PlayerSlot[])
        (awaySlots: PlayerSlot[])
        : PlayerId option =
        let mutable bestDistSq = PhysicsContract.MaxDistanceSq
        let mutable bestPlayerId: PlayerId option = None
        let mutable bestIsHome = true

        for i = 0 to homeSlots.Length - 1 do
            match homeSlots[i] with
            | PlayerSlot.Active s ->
                let dSq = ballPos.DistSqTo2D s.Pos

                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestPlayerId <- Some s.Player.Id
                    bestIsHome <- true
            | _ -> ()

        for i = 0 to awaySlots.Length - 1 do
            match awaySlots[i] with
            | PlayerSlot.Active s ->
                let dSq = ballPos.DistSqTo2D s.Pos

                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestPlayerId <- Some s.Player.Id
                    bestIsHome <- false
            | _ -> ()

        bestPlayerId

    let agent tick (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentOutput =
        let dt = SimulationClock.dt clock
        let physicsRate = clock.PhysicsRate
        let stepped = BallPhysics.update dt state.Ball



        let winner, winnerPosOpt, contested =
            Interception.chooseBestInterceptor stepped.Position state.Home.Slots state.Away.Slots

        let finalWinner =
            match winner with
            | Some p -> Some p
            | None ->
                let chaserId = findNearestChaser stepped.Position state.Home.Slots state.Away.Slots

                match chaserId with
                | Some pid ->
                    let chaserSlot =
                        let slots = Array.append state.Home.Slots state.Away.Slots

                        slots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s
                            | _ -> None)

                    match chaserSlot with
                    | Some s when stepped.Position.DistTo2D s.Pos < 1.0<meter> -> Some s.Player
                    | _ -> None
                | None -> None

        match finalWinner with
        | Some p ->
            let club = SimStateOps.clubSideOf state p.Id |> Option.get

            state.Ball <-
                { stepped with
                    Possession = Owned(club, p.Id)
                    LastTouchBy = Some p.Id }

            let delay = BalanceConfig.TickDelay.delayFrom BalanceConfig.duelChainDelay

            { Events =[]
              Continuation = SelfReschedule physicsRate
              Transition = Some LivePlay
              SideEffects =[
                  { SubTick = tick.SubTick + delay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DecisionTick(0, Some p.Id) }
              ] }
        | None ->
            match state.Ball.Possession with
            | Possession.SetPiece _ ->
                state.Ball <- { stepped with Possession = Loose }

                { Events = []
                  Continuation = SelfReschedule physicsRate
                  Transition = Some LivePlay
                  SideEffects = [] }
            | _ ->
                state.Ball <-
                    if contested then
                        { stepped with
                            Possession = Contest state.AttackingSide }
                    else
                        stepped

                { Events = []
                  Continuation = SelfReschedule physicsRate
                  Transition = None
                  SideEffects = [] }
