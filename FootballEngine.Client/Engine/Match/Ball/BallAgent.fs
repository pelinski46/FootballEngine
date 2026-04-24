namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SchedulingTypes
open SimStateOps

module BallAgent =

    let private resolveContact
        (config: PhysicsConfig)
        (ball: BallPhysicsState)
        (playerPos: Spatial)
        : BallPhysicsState =

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
                        config.AirborneRestitutionBase
                        + min config.AirborneRestitutionCoeff (float impactSpeed * config.AirborneRestitutionFloor)
                    else
                        config.GroundRestitutionBase
                        + min config.GroundRestitutionCoeff (float impactSpeed * config.GroundRestitutionFloor)

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

    let private findNearestChaserSoA
        (config: PhysicsConfig)
        (ballPos: Spatial)
        (homeFrame: TeamFrame)
        (awayFrame: TeamFrame)
        (homeRoster: PlayerRoster)
        (awayRoster: PlayerRoster)
        : PlayerId option =
        let mutable bestTime = System.Double.PositiveInfinity
        let mutable bestPlayerId: PlayerId option = None

        for i = 0 to homeFrame.SlotCount - 1 do
            match homeFrame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = homeRoster.Players[i]
                let px = float homeFrame.PosX[i] * 1.0<meter>
                let py = float homeFrame.PosY[i] * 1.0<meter>

                let pPos =
                    { X = px
                      Y = py
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> }

                let t = Interception.estimateTimeToBall config player pPos ballPos

                if t < bestTime then
                    bestTime <- t
                    bestPlayerId <- Some player.Id
            | _ -> ()

        for i = awayFrame.SlotCount - 1 downto 0 do
            match awayFrame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = awayRoster.Players[i]
                let px = float awayFrame.PosX[i] * 1.0<meter>
                let py = float awayFrame.PosY[i] * 1.0<meter>

                let pPos =
                    { X = px
                      Y = py
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> }

                let t = Interception.estimateTimeToBall config player pPos ballPos

                if t < bestTime then
                    bestTime <- t
                    bestPlayerId <- Some player.Id
            | _ -> ()

        bestPlayerId

    let private findPlayerByPidSoA
        (pid: PlayerId)
        (homeFrame: TeamFrame)
        (awayFrame: TeamFrame)
        (homeRoster: PlayerRoster)
        (awayRoster: PlayerRoster)
        =
        let rec searchHome i =
            if i >= homeFrame.SlotCount then
                None
            else
                match homeFrame.Occupancy[i] with
                | OccupancyKind.Active _ when homeRoster.Players[i].Id = pid ->
                    Some(
                        homeRoster.Players[i],
                        float homeFrame.PosX[i] * 1.0<meter>,
                        float homeFrame.PosY[i] * 1.0<meter>
                    )
                | _ -> searchHome (i + 1)

        let rec searchAway i =
            if i >= awayFrame.SlotCount then
                None
            else
                match awayFrame.Occupancy[i] with
                | OccupancyKind.Active _ when awayRoster.Players[i].Id = pid ->
                    Some(
                        awayRoster.Players[i],
                        float awayFrame.PosX[i] * 1.0<meter>,
                        float awayFrame.PosY[i] * 1.0<meter>
                    )
                | _ -> searchAway (i + 1)

        searchHome 0 |> Option.orElse (searchAway 0)

    let private ballNearlyStopped (ball: BallPhysicsState) : bool =
        let vSq =
            ball.Position.Vx * ball.Position.Vx
            + ball.Position.Vy * ball.Position.Vy
            + ball.Position.Vz * ball.Position.Vz

        vSq < 1.0<meter / second> * 1.0<meter / second>

    let private ballIntent: PlayerIntent =
        { Movement =
            MovementIntent.RecoverBall
                { X = 0.0<meter>
                  Y = 0.0<meter>
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }
          Action = None
          Context = NormalPlay
          Urgency = 0.0
          Confidence = 0.5 }

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : AgentResult =
        let pcfg = ctx.Config.Physics
        let dt = SimulationClock.dt clock

        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub

        // When Owned, ball is an extension of the carrier — no independent physics, no interception logic.
        // Possession only changes via ActionResolver (pass/shot → InFlight) or DuelAction (duel → Contest).
        match state.Ball.Possession with
        | Owned(_, pid) ->
            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
            | Some(_, px, py) ->
                state.Ball <-
                    { state.Ball with
                        Position =
                            { X = px
                              Y = py
                              Z = 0.0<meter>
                              Vx = 0.0<meter / second>
                              Vy = 0.0<meter / second>
                              Vz = 0.0<meter / second> } }
            | None -> state.Ball <- BallPhysics.update pcfg dt state.Ball

            { Intent = ballIntent
              NextTick = None
              Events = []
              Transition = None }
        | _ ->
            let stepped = BallPhysics.update pcfg dt state.Ball

            let winner, _winnerPosOpt, _contested =
                Interception.chooseBestInterceptorSoA pcfg stepped.Position homeFrame awayFrame homeRoster awayRoster

            let nearestChaser =
                findNearestChaserSoA pcfg stepped.Position homeFrame awayFrame homeRoster awayRoster

            // During InFlight, the player who just released the ball (LastTouchBy) can't
            // immediately reclaim it — prevents the passer/shooter from cancelling their own action.
            let inFlight =
                match state.Ball.Possession with
                | Possession.InFlight _ -> true
                | _ -> false

            let finalWinner =
                match winner with
                | Some p when inFlight && state.Ball.LastTouchBy = Some p.Id -> None
                | Some p -> Some p
                | None ->
                    match nearestChaser with
                    | Some pid ->
                        match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                        | Some(s, sx, sy) when
                            stepped.Position.DistTo2D
                                { X = sx
                                  Y = sy
                                  Z = 0.0<meter>
                                  Vx = 0.0<meter / second>
                                  Vy = 0.0<meter / second>
                                  Vz = 0.0<meter / second> } < pcfg.ChaserProximity
                            ->
                            Some s
                        | _ -> None
                    | None -> None

            let prevPossession = state.Ball.Possession

            match finalWinner with
            | Some p ->
                let club = SimStateOps.clubSideOf ctx state p.Id |> Option.get

                state.Ball <-
                    { stepped with
                        Possession = Owned(club, p.Id)
                        LastTouchBy = Some p.Id }

                let nextTick =
                    match prevPossession with
                    | Owned(_, existingPid) when existingPid = p.Id -> None
                    | _ -> None

                { Intent = ballIntent
                  NextTick = nextTick
                  Events = []
                  Transition = Some LivePlay }

            | None ->
                match state.Ball.Possession with
                | Possession.SetPiece _ ->
                    state.Ball <- { stepped with Possession = Loose }
                    let delay = TickDelay.delayFrom clock ctx.Config.Timing.DuelNextDelay

                    { Intent = ballIntent
                      NextTick = None
                      Events = []
                      Transition = Some LivePlay }

                | Possession.Contest side ->
                    let isSameSide pid =
                        match clubSideOf ctx state pid with
                        | Some c -> c = side
                        | _ -> false

                    match nearestChaser with
                    | Some pid when isSameSide pid ->
                        match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                        | Some(player, _, _) ->
                            state.Ball <-
                                { stepped with
                                    Possession = Owned(side, player.Id)
                                    LastTouchBy = Some player.Id }

                            { Intent = ballIntent
                              NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                        | None ->
                            state.Ball <- { stepped with Possession = Loose }
                            let delay = TickDelay.delayFrom clock ctx.Config.Timing.DuelNextDelay

                            { Intent = ballIntent
                              NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                    | _ ->
                        state.Ball <- { stepped with Possession = Loose }
                        let delay = TickDelay.delayFrom clock ctx.Config.Timing.DuelNextDelay

                        { Intent = ballIntent
                          NextTick = None
                          Events = []
                          Transition = Some LivePlay }

                | Possession.InFlight _ ->
                    if ballNearlyStopped stepped then
                        match nearestChaser with
                        | Some pid ->
                            let club =
                                SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                            | Some(s, _, _) ->
                                state.Ball <-
                                    { stepped with
                                        Possession = Owned(club, s.Id)
                                        LastTouchBy = Some s.Id }

                                { Intent = ballIntent
                                  NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                            | None ->
                                state.Ball <- { stepped with Possession = Loose }
                                let delay = TickDelay.delayFrom clock ctx.Config.Timing.DuelNextDelay

                                { Intent = ballIntent
                                  NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                        | None ->
                            state.Ball <- { stepped with Possession = Loose }
                            let delay = TickDelay.delayFrom clock ctx.Config.Timing.DuelNextDelay

                            { Intent = ballIntent
                              NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                    else
                        state.Ball <- stepped

                        { Intent = ballIntent
                          NextTick = None
                          Events = []
                          Transition = None }

                | Possession.Loose ->
                    match nearestChaser with
                    | Some pid ->
                        let club =
                            SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                        match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                        | Some(s, sx, sy) ->
                            let pPos =
                                { X = sx
                                  Y = sy
                                  Z = 0.0<meter>
                                  Vx = 0.0<meter / second>
                                  Vy = 0.0<meter / second>
                                  Vz = 0.0<meter / second> }

                            let timeToBall = Interception.estimateTimeToBall pcfg s pPos stepped.Position

                            if timeToBall < 2.0 then
                                state.Ball <-
                                    { stepped with
                                        Possession = Owned(club, s.Id)
                                        LastTouchBy = Some s.Id }

                                { Intent = ballIntent
                                  NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                            else
                                state.Ball <- stepped

                                { Intent = ballIntent
                                  NextTick = None
                                  Events = []
                                  Transition = None }
                        | None ->
                            state.Ball <- stepped

                            { Intent = ballIntent
                              NextTick = None
                              Events = []
                              Transition = None }
                    | None ->
                        state.Ball <- stepped

                        { Intent = ballIntent
                          NextTick = None
                          Events = []
                          Transition = None }

                | Possession.Owned _ ->
                    state.Ball <- stepped

                    { Intent = ballIntent
                      NextTick = None
                      Events = []
                      Transition = None }

                | Possession.Transition _ ->
                    match nearestChaser with
                    | Some pid ->
                        let club =
                            SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                        state.Ball <-
                            { stepped with
                                Possession = Owned(club, pid)
                                LastTouchBy = Some pid }

                        { Intent = ballIntent
                          NextTick = None
                          Events = []
                          Transition = Some LivePlay }
                    | None ->
                        state.Ball <- { stepped with Possession = Loose }


                        { Intent = ballIntent
                          NextTick = None
                          Events = []
                          Transition = Some LivePlay }
