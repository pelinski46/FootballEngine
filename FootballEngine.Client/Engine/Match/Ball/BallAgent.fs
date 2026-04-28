namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Stats
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

        for i = 0 to awayFrame.SlotCount - 1 do
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

    let private zeroVel (ball: BallPhysicsState) : BallPhysicsState =
        { ball with
            Position =
                { ball.Position with
                    Vx = 0.0<meter / second>
                    Vy = 0.0<meter / second>
                    Vz = 0.0<meter / second> } }

    let private ballNearlyStopped (ball: BallPhysicsState) : bool =
        let vSq =
            ball.Position.Vx * ball.Position.Vx
            + ball.Position.Vy * ball.Position.Vy
            + ball.Position.Vz * ball.Position.Vz

        vSq < 1.0<meter / second> * 1.0<meter / second>

    let private isInPenaltyArea (x: float<meter>) (y: float<meter>) (dir: AttackDir) : bool =
        let penX = PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth

        match dir with
        | LeftToRight -> x > penX
        | RightToLeft -> x < PhysicsContract.PenaltyAreaDepth

    let private isGoalLineCrossed (x: float<meter>) (dir: AttackDir) : bool =
        match dir with
        | LeftToRight -> x >= PhysicsContract.PitchLength
        | RightToLeft -> x <= 0.0<meter>

    let private isInGoal (x: float<meter>) (y: float<meter>) (z: float<meter>) (dir: AttackDir) : bool =
        let crossed = isGoalLineCrossed x dir
        let inY = y >= PhysicsContract.PostNearY && y <= PhysicsContract.PostFarY
        let inZ = z >= 0.0<meter> && z <= PhysicsContract.CrossbarHeight
        crossed && inY && inZ

    let private resolveInFlightOutcome
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (withStationary: BallPhysicsState)
        (player: Player)
        (playerClub: ClubSide)
        : PlayerResult =

        let ballPos = withStationary.Position
        let gkc = ctx.Config.GK
        let pc = ctx.Config.Pass

        let passerClubSide (passerId: PlayerId) : ClubSide =
            if playerOnSide ctx state HomeClub passerId then
                HomeClub
            else
                AwayClub

        let passerClubId (passerId: PlayerId) : ClubId =
            if playerOnSide ctx state HomeClub passerId then
                ctx.Home.Id
            else
                ctx.Away.Id

        let clearOffside () = clearOffsideSnapshot state

        match withStationary.Trajectory with
        | None ->
            clearOffside ()

            state.Ball <-
                { zeroVel withStationary with
                    Possession = Owned(playerClub, player.Id)
                    LastTouchBy = Some player.Id }

            { NextTick = None
              Events = []
              Transition = Some LivePlay }

        | Some traj ->
            match traj.ActionKind with
            | BallActionKind.Pass(passerId, targetId, quality) ->
                let attClubId = passerClubId passerId

                if player.Id = targetId then
                    let controlProb =
                        PhysicsContract.normaliseAttr player.Technical.BallControl
                        * gkc.CatchHandlingMult

                    if bernoulli (System.Math.Clamp(controlProb, 0.0, 0.95)) then
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Owned(playerClub, player.Id)
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some LivePlay }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some LivePlay }
                elif
                    player.Position = GK
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    let interceptProb =
                        PhysicsContract.normaliseAttr player.Mental.Positioning * gkc.CatchHandlingMult

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.95)) then
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Owned(playerClub, player.Id)
                                LastTouchBy = Some player.Id
                                GKHoldSinceSubTick = Some tick
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some LivePlay }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = []
                          Transition = Some LivePlay }
                else
                    let interceptProb = PhysicsContract.normaliseAttr player.Mental.Positioning * 0.5

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.8)) then
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Owned(playerClub, player.Id)
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some LivePlay }
                    else
                        clearOffside ()

                        state.Ball <-
                            { withStationary with
                                Possession = Contest(playerClub)
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (PassMisplaced(passerId, player.Id)) ]
                          Transition = Some LivePlay }

            | BallActionKind.LongBall(passerId, targetId, quality) ->
                let attClubId = passerClubId passerId

                if player.Id = targetId then
                    let controlProb =
                        PhysicsContract.normaliseAttr player.Technical.BallControl
                        * gkc.CatchHandlingMult
                        * 0.8

                    if bernoulli (System.Math.Clamp(controlProb, 0.0, 0.9)) then
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Owned(playerClub, player.Id)
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall true) ]
                          Transition = Some LivePlay }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                          Transition = Some LivePlay }
                elif
                    player.Position = GK
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    clearOffside ()

                    state.Ball <-
                        { zeroVel withStationary with
                            Possession = Owned(playerClub, player.Id)
                            LastTouchBy = Some player.Id
                            GKHoldSinceSubTick = Some tick
                            Trajectory = None }

                    { NextTick = None
                      Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                      Transition = Some LivePlay }
                else
                    clearOffside ()

                    state.Ball <-
                        { withStationary with
                            Possession = Contest(playerClub)
                            Trajectory = None }

                    { NextTick = None
                      Events = []
                      Transition = Some LivePlay }

            | BallActionKind.Shot(shooterId, shotQuality) ->
                let shooterClub = passerClubSide shooterId
                let shooterClubId = passerClubId shooterId
                let shootingDir = attackDirFor shooterClub state
                let gkDir = attackDirFor playerClub state

                if isInGoal ballPos.X ballPos.Y ballPos.Z shootingDir then
                    let scoringClub = shooterClub
                    clearOffside ()

                    state.Ball <-
                        { withStationary with
                            Possession = Possession.SetPiece(ClubSide.flip scoringClub, SetPieceKind.KickOff)
                            Trajectory = None }

                    { NextTick = None
                      Events = [ createEvent tick shooterId shooterClubId ShotOnTarget ]
                      Transition = Some(Stopped Goal) }
                elif player.Position = GK && isInPenaltyArea ballPos.X ballPos.Y gkDir then
                    let distToGoal =
                        if shootingDir = LeftToRight then
                            PhysicsContract.PitchLength - ballPos.X
                        else
                            ballPos.X

                    let gkReactionBonus =
                        1.0 - float distToGoal / float PhysicsContract.PenaltyAreaDepth

                    let reflexesNorm = PhysicsContract.normaliseAttr player.Goalkeeping.Reflexes
                    let saveProb = reflexesNorm * gkc.CatchHandlingMult + gkReactionBonus * 0.3

                    if bernoulli (System.Math.Clamp(saveProb, 0.0, 0.95)) then
                        let catchProb =
                            PhysicsContract.normaliseAttr player.Goalkeeping.Handling
                            * gkc.CatchHandlingMult

                        if bernoulli (System.Math.Clamp(catchProb, 0.0, 0.9)) then
                            clearOffside ()

                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Owned(playerClub, player.Id)
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = Some tick
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { NextTick = None
                              Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveCaught(shooterId, player.Id)) ]
                              Transition = Some LivePlay }
                        else
                            clearOffside ()
                            let dirSign = PhysicsContract.forwardX shootingDir
                            let parryAngle = normalSample 0.0 gkc.ParryDeflectionAngle
                            let parryVx = -dirSign * gkc.ParrySpeed * System.Math.Cos(parryAngle)
                            let parryVy = gkc.ParrySpeed * System.Math.Sin(parryAngle)

                            state.Ball <-
                                { withStationary with
                                    Position =
                                        { ballPos with
                                            Vx = parryVx
                                            Vy = parryVy
                                            Vz = 2.0<meter / second> }
                                    Possession = Loose
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = None
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { NextTick = None
                              Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveParried(shooterId, player.Id)) ]
                              Transition = Some LivePlay }
                    else
                        state.Ball <- withStationary

                        { NextTick = None
                          Events = []
                          Transition = None }
                else if isGoalLineCrossed ballPos.X shootingDir then
                    clearOffside ()

                    let lastTouchClub =
                        state.Ball.LastTouchBy
                        |> Option.bind (fun pid ->
                            if playerOnSide ctx state HomeClub pid then Some HomeClub
                            elif playerOnSide ctx state AwayClub pid then Some AwayClub
                            else None)

                    let defClub = ClubSide.flip shooterClub

                    let setpiece =
                        match lastTouchClub with
                        | Some club when club = defClub -> Possession.SetPiece(shooterClub, SetPieceKind.Corner)
                        | _ -> Possession.SetPiece(defClub, SetPieceKind.GoalKick)

                    state.Ball <-
                        { withStationary with
                            Possession = setpiece
                            Trajectory = None }

                    let transition =
                        match setpiece with
                        | Possession.SetPiece(_, SetPieceKind.Corner) -> PlayState.SetPiece SetPieceKind.Corner
                        | _ -> PlayState.SetPiece SetPieceKind.GoalKick

                    { NextTick = None
                      Events = []
                      Transition = Some transition }
                else
                    state.Ball <- withStationary

                    { NextTick = None
                      Events = []
                      Transition = None }

            | BallActionKind.Cross(crosserId, targetId, crossQuality) ->
                clearOffside ()
                let crossAttClub = passerClubSide crosserId
                let crossAttClubId = passerClubId crosserId
                let crossDir = attackDirFor crossAttClub state
                let gkDir = attackDirFor playerClub state

                if player.Position = GK && isInPenaltyArea ballPos.X ballPos.Y crossDir then
                    let gkReach =
                        PhysicsContract.normaliseAttr player.Goalkeeping.AerialReach
                        * ctx.Config.Cross.GkAerialReachMult
                        + PhysicsContract.normaliseAttr player.Physical.JumpingReach
                          * ctx.Config.Cross.GkJumpMult

                    let condFactor = PhysicsContract.normaliseCondition player.Condition
                    let claimProb = gkReach * condFactor * ctx.Config.Cross.ClaimCrossProbability

                    if bernoulli (System.Math.Clamp(claimProb, 0.0, 0.95)) then
                        let catchProb =
                            PhysicsContract.normaliseAttr player.Goalkeeping.Handling
                            * gkc.CatchHandlingMult

                        if bernoulli (System.Math.Clamp(catchProb, 0.0, 0.9)) then
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Owned(playerClub, player.Id)
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = Some tick
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { NextTick = None
                              Events =
                                [ createEvent tick crosserId crossAttClubId (CrossAttempt false)
                                  createEvent tick player.Id gkClubId Save
                                  createEvent tick crosserId crossAttClubId (SaveCaught(crosserId, player.Id)) ]
                              Transition = Some LivePlay }
                        else
                            let dirSign = PhysicsContract.forwardX crossDir
                            let parryAngle = normalSample 0.0 gkc.ParryDeflectionAngle
                            let parryVx = -dirSign * gkc.ParrySpeed * System.Math.Cos(parryAngle)
                            let parryVy = gkc.ParrySpeed * System.Math.Sin(parryAngle)

                            state.Ball <-
                                { withStationary with
                                    Position =
                                        { ballPos with
                                            Vx = parryVx
                                            Vy = parryVy
                                            Vz = 2.0<meter / second> }
                                    Possession = Loose
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = None
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { NextTick = None
                              Events = [ createEvent tick crosserId gkClubId (GKPunch player.Id) ]
                              Transition = Some LivePlay }
                    else
                        state.Ball <- withStationary

                        { NextTick = None
                          Events = []
                          Transition = None }
                elif player.Id = targetId then
                    let headerProb =
                        PhysicsContract.normaliseAttr player.Technical.Heading * 0.6
                        + PhysicsContract.normaliseAttr player.Physical.Strength * 0.4

                    if bernoulli (System.Math.Clamp(headerProb, 0.0, 0.9)) then
                        let shootingDir = attackDirFor playerClub state
                        let distToGoal = PhysicsContract.distToGoal ballPos.X shootingDir

                        if distToGoal < 15.0<meter> then
                            let shotPower = float player.Technical.Finishing / 20.0 * 10.0
                            let dirSign = PhysicsContract.forwardX shootingDir
                            let angle = normalSample 0.0 0.3
                            let vx = dirSign * shotPower * System.Math.Cos(angle)
                            let vy = shotPower * System.Math.Sin(angle)
                            let vz = abs (normalSample 2.0 1.0) * 1.0<meter / second>

                            let goalX =
                                if shootingDir = LeftToRight then
                                    PhysicsContract.PitchLength
                                else
                                    0.0<meter>

                            let targetY = ballPos.Y + (vy / (dirSign * shotPower)) * (goalX - ballPos.X)

                            state.Ball <-
                                { withStationary with
                                    Position =
                                        { ballPos with
                                            Vx = vx * 1.0<meter / second>
                                            Vy = vy * 1.0<meter / second>
                                            Vz = vz }
                                    LastTouchBy = Some player.Id
                                    Trajectory =
                                        Some
                                            { traj with
                                                ActionKind = BallActionKind.Shot(player.Id, headerProb)
                                                TargetY = targetY } }

                            { NextTick = None
                              Events =
                                [ createEvent
                                      tick
                                      player.Id
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      ShotOnTarget ]
                              Transition = None }
                        else
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Owned(playerClub, player.Id)
                                    LastTouchBy = Some player.Id
                                    Trajectory = None }

                            { NextTick = None
                              Events =
                                [ createEvent
                                      tick
                                      crosserId
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      (CrossAttempt true) ]
                              Transition = Some LivePlay }
                    else
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { NextTick = None
                          Events =
                            [ createEvent
                                  tick
                                  crosserId
                                  (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                  (CrossAttempt false) ]
                          Transition = Some LivePlay }
                else if player.Position <> GK then
                    let clearProb = 0.5 + PhysicsContract.normaliseAttr player.Physical.Strength * 0.3

                    if bernoulli (System.Math.Clamp(clearProb, 0.0, 0.9)) then
                        let dirSign = -PhysicsContract.forwardX crossDir
                        let clearAngle = normalSample 0.0 0.5
                        let clearSpeed = 8.0<meter / second>
                        let vx = dirSign * clearSpeed * System.Math.Cos(clearAngle)
                        let vy = clearSpeed * System.Math.Sin(clearAngle)

                        state.Ball <-
                            { withStationary with
                                Position =
                                    { ballPos with
                                        Vx = vx
                                        Vy = vy
                                        Vz = 3.0<meter / second> }
                                Possession = InFlight
                                LastTouchBy = Some player.Id
                                Trajectory =
                                    Some
                                        { traj with
                                            ActionKind = BallActionKind.Clearance(player.Id) } }

                        { NextTick = None
                          Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = None }
                    else
                        state.Ball <-
                            { withStationary with
                                Possession = Contest(playerClub)
                                Trajectory = None }

                        { NextTick = None
                          Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = Some LivePlay }
                else
                    state.Ball <-
                        { withStationary with
                            Possession = Contest(playerClub)
                            Trajectory = None }

                    { NextTick = None
                      Events = []
                      Transition = Some LivePlay }

            | BallActionKind.Clearance(_)
            | BallActionKind.Deflection(_)
            | BallActionKind.FreeBall ->
                state.Ball <-
                    { zeroVel withStationary with
                        Possession = Owned(playerClub, player.Id)
                        LastTouchBy = Some player.Id
                        Trajectory = None }

                { NextTick = None
                  Events = []
                  Transition = Some LivePlay }

    let agent (tick: ScheduledTick) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlayerResult =
        let pcfg = ctx.Config.Physics
        let dt = SimulationClock.dt clock

        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub

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
                              Vz = 0.0<meter / second> }
                        Trajectory = None }
            | None -> state.Ball <- BallPhysics.update pcfg dt state.Ball

            { NextTick = None
              Events = []
              Transition = None }
        | _ ->
            let withStationary = BallPhysics.update pcfg dt state.Ball

            let wasStationary = state.Ball.StationarySinceSubTick.IsSome
            let isNowStopped = ballNearlyStopped withStationary

            let withStationary =
                if isNowStopped && not wasStationary then
                    { withStationary with
                        StationarySinceSubTick = Some tick.SubTick }
                elif not isNowStopped then
                    { withStationary with
                        StationarySinceSubTick = None }
                else
                    withStationary

            let winner, _winnerPosOpt, _contested =
                Interception.chooseBestInterceptorSoA
                    pcfg
                    withStationary.Position
                    homeFrame
                    awayFrame
                    homeRoster
                    awayRoster

            let nearestChaser =
                findNearestChaserSoA pcfg withStationary.Position homeFrame awayFrame homeRoster awayRoster

            let inFlight =
                match state.Ball.Possession with
                | Possession.InFlight -> true
                | _ -> false

            let gkc = ctx.Config.GK

            let rawWinner =
                match winner with
                | Some p when inFlight && state.Ball.LastTouchBy = Some p.Id -> None
                | Some p -> Some p
                | None ->
                    match nearestChaser with
                    | Some pid ->
                        match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                        | Some(s, sx, sy) when
                            withStationary.Position.DistTo2D
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

            let gkCollectionOverride
                (candidate: Player option)
                (candidateSide: ClubSide option)
                : Player option * ClubSide option =
                let bx = withStationary.Position.X
                let by = withStationary.Position.Y

                let tryGkCollection
                    (frame: TeamFrame)
                    (roster: PlayerRoster)
                    (side: ClubSide)
                    : Player option * ClubSide option =
                    let mutable bestGk: Player option = None
                    let mutable bestGkDist = System.Double.MaxValue

                    for i = 0 to frame.SlotCount - 1 do
                        match frame.Occupancy[i] with
                        | OccupancyKind.Active _ when roster.Players[i].Position = GK ->
                            let gx = float frame.PosX[i] * 1.0<meter>
                            let gy = float frame.PosY[i] * 1.0<meter>

                            let d =
                                withStationary.Position.DistTo2D
                                    { X = gx
                                      Y = gy
                                      Z = 0.0<meter>
                                      Vx = 0.0<meter / second>
                                      Vy = 0.0<meter / second>
                                      Vz = 0.0<meter / second> }

                            if float d < float gkc.CollectionRadius && float d < bestGkDist then
                                bestGkDist <- float d
                                bestGk <- Some roster.Players[i]
                        | _ -> ()

                    match bestGk with
                    | Some gk -> Some gk, Some side
                    | None -> None, None

                match candidate with
                | Some p ->
                    let pSide = candidateSide |> Option.defaultValue HomeClub
                    let pDir = attackDirFor (ClubSide.flip pSide) state

                    if isInPenaltyArea bx by pDir then
                        match
                            tryGkCollection
                                (if pSide = HomeClub then homeFrame else awayFrame)
                                (if pSide = HomeClub then homeRoster else awayRoster)
                                pSide
                        with
                        | Some gk, Some side -> Some gk, Some side
                        | _ -> candidate, candidateSide
                    else
                        candidate, candidateSide
                | None ->
                    let homeDefDir = attackDirFor (ClubSide.flip HomeClub) state
                    let awayDefDir = attackDirFor (ClubSide.flip AwayClub) state

                    if isInPenaltyArea bx by homeDefDir then
                        match tryGkCollection homeFrame homeRoster HomeClub with
                        | Some gk, Some side -> Some gk, Some side
                        | _ ->
                            if isInPenaltyArea bx by awayDefDir then
                                tryGkCollection awayFrame awayRoster AwayClub
                            else
                                None, None
                    elif isInPenaltyArea bx by awayDefDir then
                        tryGkCollection awayFrame awayRoster AwayClub
                    else
                        None, None

            let finalWinner, finalClub =
                match rawWinner with
                | Some p ->
                    let club = SimStateOps.clubSideOf ctx state p.Id
                    gkCollectionOverride (Some p) club
                | None -> gkCollectionOverride None None

            let prevPossession = state.Ball.Possession

            match finalWinner with
            | Some p ->
                let club = finalClub |> Option.get

                if inFlight then
                    resolveInFlightOutcome tick.SubTick ctx state withStationary p club
                else
                    let isGk = p.Position = GK
                    let gkHoldTick = if isGk then Some tick.SubTick else None

                    state.Ball <-
                        { zeroVel withStationary with
                            Possession = Owned(club, p.Id)
                            LastTouchBy = Some p.Id
                            GKHoldSinceSubTick = gkHoldTick }

                    { NextTick = None
                      Events = []
                      Transition = Some LivePlay }

            | None ->
                match state.Ball.Possession with
                | Possession.SetPiece _ ->
                    state.Ball <-
                        { zeroVel withStationary with
                            Possession = Loose }

                    { NextTick = None
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
                            let isGk = player.Position = GK
                            let gkHoldTick = if isGk then Some tick.SubTick else None

                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Owned(side, player.Id)
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = gkHoldTick }

                            { NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                        | None ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                    | _ ->
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose }

                        { NextTick = None
                          Events = []
                          Transition = Some LivePlay }

                | Possession.InFlight ->
                    let bx = withStationary.Position.X
                    let by = withStationary.Position.Y
                    let bz = withStationary.Position.Z

                    let outY = by <= 0.1<meter> || by >= PhysicsContract.PitchWidth - 0.1<meter>
                    let outX = bx <= 0.1<meter> || bx >= PhysicsContract.PitchLength - 0.1<meter>

                    let lastTouchClub =
                        state.Ball.LastTouchBy
                        |> Option.bind (fun pid ->
                            if playerOnSide ctx state HomeClub pid then Some HomeClub
                            elif playerOnSide ctx state AwayClub pid then Some AwayClub
                            else None)

                    if outY then
                        clearOffsideSnapshot state

                        match lastTouchClub with
                        | Some side ->
                            state.Ball <-
                                { withStationary with
                                    Possession = Possession.SetPiece(ClubSide.flip side, SetPieceKind.ThrowIn) }

                            { NextTick = None
                              Events = []
                              Transition = Some(PlayState.SetPiece SetPieceKind.ThrowIn) }
                        | None ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                    elif outX then
                        let inGoalY = by >= PhysicsContract.PostNearY && by <= PhysicsContract.PostFarY
                        let inGoalZ = bz >= 0.0<meter> && bz <= PhysicsContract.CrossbarHeight

                        if inGoalY && inGoalZ then
                            let scoringClub =
                                match lastTouchClub with
                                | Some HomeClub -> Some AwayClub
                                | Some AwayClub -> Some HomeClub
                                | None -> None

                            match scoringClub with
                            | Some sc ->
                                clearOffsideSnapshot state

                                state.Ball <-
                                    { withStationary with
                                        Possession = Possession.SetPiece(ClubSide.flip sc, SetPieceKind.KickOff) }

                                { NextTick = None
                                  Events = []
                                  Transition = Some(Stopped Goal) }
                            | None ->
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Loose }

                                { NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                        else
                            clearOffsideSnapshot state
                            let behindHome = bx <= 0.5<meter>
                            let behindAway = bx >= PhysicsContract.PitchLength - 0.5<meter>

                            if behindHome then
                                match lastTouchClub with
                                | Some AwayClub ->
                                    state.Ball <-
                                        { withStationary with
                                            Possession = Possession.SetPiece(HomeClub, SetPieceKind.Corner) }

                                    { NextTick = None
                                      Events = []
                                      Transition = Some(PlayState.SetPiece SetPieceKind.Corner) }
                                | _ ->
                                    state.Ball <-
                                        { withStationary with
                                            Possession = Possession.SetPiece(HomeClub, SetPieceKind.GoalKick) }

                                    { NextTick = None
                                      Events = []
                                      Transition = Some(PlayState.SetPiece SetPieceKind.GoalKick) }
                            elif behindAway then
                                match lastTouchClub with
                                | Some HomeClub ->
                                    state.Ball <-
                                        { withStationary with
                                            Possession = Possession.SetPiece(AwayClub, SetPieceKind.Corner) }

                                    { NextTick = None
                                      Events = []
                                      Transition = Some(PlayState.SetPiece SetPieceKind.Corner) }
                                | _ ->
                                    state.Ball <-
                                        { withStationary with
                                            Possession = Possession.SetPiece(AwayClub, SetPieceKind.GoalKick) }

                                    { NextTick = None
                                      Events = []
                                      Transition = Some(PlayState.SetPiece SetPieceKind.GoalKick) }
                            else
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Loose }

                                { NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                    elif ballNearlyStopped withStationary then
                        match nearestChaser with
                        | Some pid ->
                            let club =
                                SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                            | Some(s, _, _) ->
                                let isGk = s.Position = GK
                                let gkHoldTick = if isGk then Some tick.SubTick else None

                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Owned(club, s.Id)
                                        LastTouchBy = Some s.Id
                                        GKHoldSinceSubTick = gkHoldTick }

                                { NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                            | None ->
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Loose }

                                { NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                        | None ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { NextTick = None
                              Events = []
                              Transition = Some LivePlay }
                    else
                        state.Ball <- withStationary

                        { NextTick = None
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

                            let timeToBall = Interception.estimateTimeToBall pcfg s pPos withStationary.Position

                            if timeToBall < 2.0 then
                                let isGk = s.Position = GK
                                let gkHoldTick = if isGk then Some tick.SubTick else None

                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Owned(club, s.Id)
                                        LastTouchBy = Some s.Id
                                        GKHoldSinceSubTick = gkHoldTick }

                                { NextTick = None
                                  Events = []
                                  Transition = Some LivePlay }
                            else
                                state.Ball <- withStationary

                                { NextTick = None
                                  Events = []
                                  Transition = None }
                        | None ->
                            state.Ball <- withStationary

                            { NextTick = None
                              Events = []
                              Transition = None }
                    | None ->
                        state.Ball <- withStationary

                        { NextTick = None
                          Events = []
                          Transition = None }

                | Possession.Owned _ ->
                    state.Ball <- withStationary

                    { NextTick = None
                      Events = []
                      Transition = None }

                | Possession.Transition _ ->
                    match nearestChaser with
                    | Some pid ->
                        let club =
                            SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                        let isGk =
                            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                            | Some(p, _, _) -> p.Position = GK
                            | None -> false

                        let gkHoldTick = if isGk then Some tick.SubTick else None

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Owned(club, pid)
                                LastTouchBy = Some pid
                                GKHoldSinceSubTick = gkHoldTick }

                        { NextTick = None
                          Events = []
                          Transition = Some LivePlay }
                    | None ->
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose }


                        { NextTick = None
                          Events = []
                          Transition = Some LivePlay }
