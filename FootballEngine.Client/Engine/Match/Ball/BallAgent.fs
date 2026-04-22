namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SchedulingTypes

module BallAgent =

    let private resolveContact (config: PhysicsConfig) (ball: BallPhysicsState) (playerPos: Spatial) : BallPhysicsState =

        let r = config.ContactRadius
        let bp = ball.Position
        let dist = bp.DistTo playerPos

        if dist < r && dist > 0.001<meter> then
            let nx = (bp.X - playerPos.X) / dist
            let ny = (bp.Y - playerPos.Y) / dist
            let nz = (bp.Z - playerPos.Z) / dist
            let dot = bp.Vx * nx + bp.Vy * ny + bp.Vz * nz

            if dot > 0.0<meter / second> then
                let impactSpeed = bp.VelMag
                let isAirborne = bp.Z > config.AirborneThreshold

                let restitution =
                    if isAirborne then
                        config.AirborneRestitutionBase + min config.AirborneRestitutionCoeff (float impactSpeed * config.AirborneRestitutionFloor)
                    else
                        config.GroundRestitutionBase + min config.GroundRestitutionCoeff (float impactSpeed * config.GroundRestitutionFloor)

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

    let private findNearestOfSide (ballPos: Spatial) (side: ClubSide) (homeSlots: PlayerSlot[]) (awaySlots: PlayerSlot[]) : (Player * Spatial) option =
        let slots = if side = HomeClub then homeSlots else awaySlots
        let mutable best: (Player * Spatial) option = None
        let mutable bestDistSq = PhysicsContract.MaxDistanceSq

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let dSq = ballPos.DistSqTo2D s.Pos
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    best <- Some(s.Player, s.Pos)
            | _ -> ()

        best

    let private findNearestChaser
        (config: PhysicsConfig)
        (ballPos: Spatial)
        (homeSlots: PlayerSlot[])
        (awaySlots: PlayerSlot[])
        : PlayerId option =
        let mutable bestTime = System.Double.PositiveInfinity
        let mutable bestPlayerId: PlayerId option = None

        let consider (player: Player) (pPos: Spatial) =
            let t = Interception.estimateTimeToBall config player pPos ballPos
            if t < bestTime then
                bestTime <- t
                bestPlayerId <- Some player.Id

        for i = 0 to homeSlots.Length - 1 do
            match homeSlots[i] with
            | PlayerSlot.Active s -> consider s.Player s.Pos
            | _ -> ()

        for i = awaySlots.Length - 1 downto 0 do
            match awaySlots[i] with
            | PlayerSlot.Active s -> consider s.Player s.Pos
            | _ -> ()

        bestPlayerId

    let private ballNearlyStopped (ball: BallPhysicsState) : bool =
        let vSq = ball.Position.Vx * ball.Position.Vx + ball.Position.Vy * ball.Position.Vy + ball.Position.Vz * ball.Position.Vz
        vSq < 1.0<meter/second> * 1.0<meter/second>

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentOutput =
        let pcfg = ctx.Config.Physics
        let stepped = BallPhysics.update pcfg (SimulationClock.dt clock) state.Ball

        let winner, winnerPosOpt, contested =
            Interception.chooseBestInterceptor pcfg stepped.Position state.Home.Slots state.Away.Slots

        let finalWinner =
            match winner with
            | Some p -> Some p
            | None ->
                let chaserId = findNearestChaser pcfg stepped.Position state.Home.Slots state.Away.Slots

                match chaserId with
                | Some pid ->
                    let chaserSlot =
                        let slots = Array.append state.Home.Slots state.Away.Slots

                        slots
                        |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s
                            | _ -> None)

                    match chaserSlot with
                    | Some s when stepped.Position.DistTo2D s.Pos < pcfg.ChaserProximity -> Some s.Player
                    | _ -> None
                | None -> None

        let prevPossession = state.Ball.Possession

        match finalWinner with
        | Some p ->
            let club = SimStateOps.clubSideOf state p.Id |> Option.get

            state.Ball <-
                { stepped with
                    Possession = Owned(club, p.Id)
                    LastTouchBy = Some p.Id }

            let intent =
                match prevPossession with
                | Owned(_, existingPid) when existingPid = p.Id -> NoOp
                | _ -> GiveDecisionTo p.Id

            { Events = []
              Transition = Some LivePlay
              Intent = intent }

        | None ->
            match state.Ball.Possession with
            | Possession.SetPiece _ ->
                state.Ball <- { stepped with Possession = Loose }

                { Events = []
                  Transition = Some LivePlay
                  Intent = FindNextDuel }

            | Possession.Contest side ->
                let nearest = findNearestOfSide stepped.Position side state.Home.Slots state.Away.Slots
                match nearest with
                | Some (player, _pos) ->
                    state.Ball <-
                        { stepped with
                            Possession = Owned(side, player.Id)
                            LastTouchBy = Some player.Id }

                    { Events = []
                      Transition = Some LivePlay
                      Intent = GiveDecisionTo player.Id }
                | None ->
                    state.Ball <- { stepped with Possession = Loose }

                    { Events = []
                      Transition = Some LivePlay
                      Intent = FindNextDuel }

            | Possession.InFlight _ ->
                if ballNearlyStopped stepped then
                    let chaserId = findNearestChaser pcfg stepped.Position state.Home.Slots state.Away.Slots
                    match chaserId with
                    | Some pid ->
                        let club = SimStateOps.clubSideOf state pid |> Option.defaultValue state.AttackingSide
                        let chaserSlot =
                            let slots = Array.append state.Home.Slots state.Away.Slots
                            slots |> Array.tryPick (function
                                | PlayerSlot.Active s when s.Player.Id = pid -> Some s
                                | _ -> None)

                        match chaserSlot with
                        | Some s ->
                            state.Ball <-
                                { stepped with
                                    Possession = Owned(club, s.Player.Id)
                                    LastTouchBy = Some s.Player.Id }

                            { Events = []
                              Transition = Some LivePlay
                              Intent = GiveDecisionTo s.Player.Id }
                        | None ->
                            state.Ball <- { stepped with Possession = Loose }
                            { Events = []
                              Transition = Some LivePlay
                              Intent = FindNextDuel }
                    | None ->
                        state.Ball <- { stepped with Possession = Loose }
                        { Events = []
                          Transition = Some LivePlay
                          Intent = FindNextDuel }
                else
                    state.Ball <- stepped

                    { Events = []
                      Transition = None
                      Intent = NoOp }

            | Possession.Loose ->
                let chaserId = findNearestChaser pcfg stepped.Position state.Home.Slots state.Away.Slots
                match chaserId with
                | Some pid ->
                    let club = SimStateOps.clubSideOf state pid |> Option.defaultValue state.AttackingSide
                    let chaserSlot =
                        let slots = Array.append state.Home.Slots state.Away.Slots
                        slots |> Array.tryPick (function
                            | PlayerSlot.Active s when s.Player.Id = pid -> Some s
                            | _ -> None)

                    match chaserSlot with
                    | Some s ->
                        let timeToBall = Interception.estimateTimeToBall pcfg s.Player s.Pos stepped.Position
                        if timeToBall < 2.0 then
                            state.Ball <-
                                { stepped with
                                    Possession = Owned(club, s.Player.Id)
                                    LastTouchBy = Some s.Player.Id }

                            { Events = []
                              Transition = Some LivePlay
                              Intent = GiveDecisionTo s.Player.Id }
                        else
                            state.Ball <- stepped
                            { Events = []
                              Transition = None
                              Intent = NoOp }
                    | None ->
                        state.Ball <- stepped
                        { Events = []
                          Transition = None
                          Intent = NoOp }
                | None ->
                    state.Ball <- stepped
                    { Events = []
                      Transition = None
                      Intent = NoOp }

            | Possession.Owned _ ->
                state.Ball <- stepped
                { Events = []
                  Transition = None
                  Intent = NoOp }

            | Possession.Transition _ ->
                let chaserId = findNearestChaser pcfg stepped.Position state.Home.Slots state.Away.Slots
                match chaserId with
                | Some pid ->
                    let club = SimStateOps.clubSideOf state pid |> Option.defaultValue state.AttackingSide
                    state.Ball <-
                        { stepped with
                            Possession = Owned(club, pid)
                            LastTouchBy = Some pid }

                    { Events = []
                      Transition = Some LivePlay
                      Intent = GiveDecisionTo pid }
                | None ->
                    state.Ball <- { stepped with Possession = Loose }
                    { Events = []
                      Transition = Some LivePlay
                      Intent = FindNextDuel }
