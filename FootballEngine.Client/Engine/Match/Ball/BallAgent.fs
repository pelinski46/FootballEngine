namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Stats
open SchedulingTypes
open SimStateOps

module BallAgent =

    let private isBallArrivingToPlayer
        (ballPos: Spatial)
        (playerPos: Spatial)
        (competitionRadius: float<meter>)
        (convergenceThreshold: float<meter / second>)
        : bool =
        let dist = ballPos.DistTo2D playerPos

        if dist > competitionRadius then
            false
        else
            let toPlayerX = playerPos.X - ballPos.X
            let toPlayerY = playerPos.Y - ballPos.Y
            let toPlayerLen = sqrt (toPlayerX * toPlayerX + toPlayerY * toPlayerY)

            if toPlayerLen < 0.001<meter> then
                true
            else
                let ballSpeed = sqrt (ballPos.Vx * ballPos.Vx + ballPos.Vy * ballPos.Vy)

                if ballSpeed < 0.5<meter / second> then
                    true
                else
                    let normX = toPlayerX / toPlayerLen
                    let normY = toPlayerY / toPlayerLen
                    let convergence = float (ballPos.Vx * normX + ballPos.Vy * normY)
                    convergence < float convergenceThreshold

    let private shouldTriggerArrival
        (ball: BallPhysicsState)
        (cfg: PhysicsConfig)
        (state: SimState)
        (homeFrame: TeamFrame)
        (awayFrame: TeamFrame)
        (homeRoster: PlayerRoster)
        (awayRoster: PlayerRoster)
        : bool =
        let ballPos = ball.Position
        let compRadius = cfg.ArrivalCompetitionRadius
        let convThreshold = cfg.ArrivalConvergenceThreshold

        let checkFrame (frame: TeamFrame) (roster: PlayerRoster) =
            let mutable result = false

            for i = 0 to frame.SlotCount - 1 do
                match frame.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let player = roster.Players[i]

                    // INVARIANT: Don't trigger arrival for the player who just kicked the ball.
                    // Give it ~10 subticks (0.25 seconds) to escape the kicker's contact radius.
                    let isKicker =
                        match ball.Trajectory with
                        | Some t -> t.KickerId = player.Id && (state.SubTick - t.LaunchSubTick < 10)
                        | None -> false

                    if not isKicker then
                        let px = float frame.PosX[i] * 1.0<meter>
                        let py = float frame.PosY[i] * 1.0<meter>

                        let pPos =
                            { X = px
                              Y = py
                              Z = 0.0<meter>
                              Vx = 0.0<meter / second>
                              Vy = 0.0<meter / second>
                              Vz = 0.0<meter / second> }

                        if isBallArrivingToPlayer ballPos pPos compRadius convThreshold then
                            result <- true
                | _ -> ()

            result

        checkFrame homeFrame homeRoster || checkFrame awayFrame awayRoster

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
            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state
            { Events = []; Transition = Some Live }

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
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some Live }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some Live }
                elif
                    player.Position = GK
                    && player.Id <> passerId // INVARIANT: GK cannot intercept their own pass
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    let interceptProb =
                        PhysicsContract.normaliseAttr player.Mental.Positioning * gkc.CatchHandlingMult

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.95)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some Live }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = []; Transition = Some Live }
                else
                    let interceptProb = PhysicsContract.normaliseAttr player.Mental.Positioning * 0.5

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.8)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some Live }
                    else
                        clearOffside ()

                        state.Ball <-
                            { withStationary with
                                Possession = Contest(playerClub)
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (PassMisplaced(passerId, player.Id)) ]
                          Transition = Some Live }

            | BallActionKind.LongBall(passerId, targetId, quality) ->
                let attClubId = passerClubId passerId

                if player.Id = targetId then
                    let controlProb =
                        PhysicsContract.normaliseAttr player.Technical.BallControl
                        * gkc.CatchHandlingMult
                        * 0.8

                    if bernoulli (System.Math.Clamp(controlProb, 0.0, 0.9)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall true) ]
                          Transition = Some Live }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                          Transition = Some Live }
                elif
                    player.Position = GK
                    && player.Id <> passerId // INVARIANT: GK cannot intercept their own long ball
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    clearOffside ()
                    givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                    { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                      Transition = Some Live }
                else
                    clearOffside ()

                    state.Ball <-
                        { withStationary with
                            Possession = Contest(playerClub)
                            Trajectory = None }

                    { Events = []; Transition = Some Live }

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

                    { Events = [ createEvent tick shooterId shooterClubId ShotOnTarget ]
                      Transition =
                        Some(
                            MatchFlow.GoalPause
                                { ScoringTeam = scoringClub
                                  ScorerId = Some shooterId
                                  IsOwnGoal = false
                                  RemainingTicks = 120
                                  VARRequested = false }
                        ) }
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
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveCaught(shooterId, player.Id)) ]
                              Transition = Some Live }
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

                            { Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveParried(shooterId, player.Id)) ]
                              Transition = Some Live }
                    else
                        state.Ball <- withStationary

                        { Events = []; Transition = None }
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
                        | Possession.SetPiece(_, SetPieceKind.Corner) ->
                            MatchFlow.RestartDelay
                                { Kind = SetPieceKind.Corner
                                  Team = shooterClub
                                  Cause = AfterBallOut
                                  RemainingTicks = 120 }
                        | _ ->
                            MatchFlow.RestartDelay
                                { Kind = SetPieceKind.GoalKick
                                  Team = defClub
                                  Cause = AfterBallOut
                                  RemainingTicks = 120 }

                    { Events = []
                      Transition = Some transition }
                else
                    state.Ball <- withStationary

                    { Events = []; Transition = None }

            | BallActionKind.Cross(crosserId, targetId, crossQuality) ->
                clearOffside ()
                let crossAttClub = passerClubSide crosserId
                let crossAttClubId = passerClubId crosserId
                let crossDir = attackDirFor crossAttClub state
                let gkDir = attackDirFor playerClub state

                if
                    player.Position = GK
                    && player.Id <> crosserId // INVARIANT: GK cannot claim their own cross
                    && isInPenaltyArea ballPos.X ballPos.Y crossDir
                then
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
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events =
                                [ createEvent tick crosserId crossAttClubId (CrossAttempt false)
                                  createEvent tick player.Id gkClubId Save
                                  createEvent tick crosserId crossAttClubId (SaveCaught(crosserId, player.Id)) ]
                              Transition = Some Live }
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

                            { Events = [ createEvent tick crosserId gkClubId (GKPunch player.Id) ]
                              Transition = Some Live }
                    else
                        state.Ball <- withStationary

                        { Events = []; Transition = None }
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

                            { Events =
                                [ createEvent
                                      tick
                                      player.Id
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      ShotOnTarget ]
                              Transition = None }
                        else
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            { Events =
                                [ createEvent
                                      tick
                                      crosserId
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      (CrossAttempt true) ]
                              Transition = Some Live }
                    else
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events =
                            [ createEvent
                                  tick
                                  crosserId
                                  (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                  (CrossAttempt false) ]
                          Transition = Some Live }
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

                        { Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = None }
                    else
                        state.Ball <-
                            { withStationary with
                                Possession = Contest(playerClub)
                                Trajectory = None }

                        { Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = Some Live }
                else
                    state.Ball <-
                        { withStationary with
                            Possession = Contest(playerClub)
                            Trajectory = None }

                    { Events = []; Transition = Some Live }

            | BallActionKind.Clearance(_)
            | BallActionKind.Deflection(_)
            | BallActionKind.FreeBall ->
                givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state
                { Events = []; Transition = Some Live }

    let private resolveShot
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (ball: BallPhysicsState)
        (traj: BallTrajectory)
        : PlayerResult =
        let ballPos = ball.Position

        let shooterClub =
            if playerOnSide ctx state HomeClub traj.KickerId then
                HomeClub
            else
                AwayClub

        let shootingDir = attackDirFor shooterClub state

        if isInGoal ballPos.X ballPos.Y ballPos.Z shootingDir then
            clearOffsideSnapshot state
            let receivingClub = ClubSide.flip shooterClub

            state.Ball <-
                { ball with
                    Possession = Possession.SetPiece(receivingClub, SetPieceKind.KickOff)
                    Trajectory = None }

            { Events = []
              Transition =
                Some(
                    MatchFlow.GoalPause
                        { ScoringTeam = shooterClub
                          ScorerId = None
                          IsOwnGoal = true
                          RemainingTicks = 120
                          VARRequested = false }
                ) }
        else
            let homeFrame = getFrame state HomeClub
            let awayFrame = getFrame state AwayClub
            let homeRoster = getRoster ctx HomeClub
            let awayRoster = getRoster ctx AwayClub
            let defClub = ClubSide.flip shooterClub
            let defFrame = if defClub = HomeClub then homeFrame else awayFrame
            let defRoster = if defClub = HomeClub then homeRoster else awayRoster
            let gkDir = attackDirFor defClub state
            let mutable gkPlayer: Player option = None

            for i = 0 to defFrame.SlotCount - 1 do
                match defFrame.Occupancy[i] with
                | OccupancyKind.Active _ when defRoster.Players[i].Position = GK ->
                    if isInPenaltyArea ballPos.X ballPos.Y gkDir then
                        gkPlayer <- Some defRoster.Players[i]
                | _ -> ()

            match gkPlayer with
            | Some gk -> resolveInFlightOutcome tick ctx state ball gk defClub
            | None ->
                if isGoalLineCrossed ballPos.X shootingDir then
                    clearOffsideSnapshot state

                    let lastTouchClub =
                        ball.LastTouchBy
                        |> Option.bind (fun pid ->
                            if playerOnSide ctx state HomeClub pid then Some HomeClub
                            elif playerOnSide ctx state AwayClub pid then Some AwayClub
                            else None)

                    let setpiece =
                        match lastTouchClub with
                        | Some club when club = defClub -> Possession.SetPiece(shooterClub, SetPieceKind.Corner)
                        | _ -> Possession.SetPiece(defClub, SetPieceKind.GoalKick)

                    state.Ball <-
                        { ball with
                            Possession = setpiece
                            Trajectory = None }

                    let transition =
                        match setpiece with
                        | Possession.SetPiece(_, SetPieceKind.Corner) ->
                            MatchFlow.RestartDelay
                                { Kind = SetPieceKind.Corner
                                  Team = ClubSide.flip defClub
                                  Cause = AfterBallOut
                                  RemainingTicks = 120 }
                        | _ ->
                            MatchFlow.RestartDelay
                                { Kind = SetPieceKind.GoalKick
                                  Team = ClubSide.flip defClub
                                  Cause = AfterBallOut
                                  RemainingTicks = 120 }

                    { Events = []
                      Transition = Some transition }
                else
                    state.Ball <- ball

                    { Events = []; Transition = None }

    let private resolveArrival
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (ball: BallPhysicsState)
        (traj: BallTrajectory)
        : PlayerResult =
        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub

        match traj.ActionKind with
        | BallActionKind.Shot _ -> resolveShot tick ctx state ball traj

        | BallActionKind.Pass(passerId, targetId, quality) ->
            let arrivalCtx: ArrivalContext =
                { BallPos = ball.Position
                  TargetId = targetId
                  Quality = quality
                  HomeFrame = homeFrame
                  AwayFrame = awayFrame
                  HomeRoster = homeRoster
                  AwayRoster = awayRoster
                  PhysicsCfg = ctx.Config.Physics }

            match Interception.evaluateArrival arrivalCtx with
            | NoOneInRange ->
                state.Ball <- ball

                { Events = []; Transition = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Possession = Contest(attClub)
                        Trajectory = None }

                { Events = []; Transition = Some Live }

        | BallActionKind.LongBall(passerId, targetId, quality) ->
            let arrivalCtx: ArrivalContext =
                { BallPos = ball.Position
                  TargetId = targetId
                  Quality = quality
                  HomeFrame = homeFrame
                  AwayFrame = awayFrame
                  HomeRoster = homeRoster
                  AwayRoster = awayRoster
                  PhysicsCfg = ctx.Config.Physics }

            match Interception.evaluateArrival arrivalCtx with
            | NoOneInRange ->
                state.Ball <- ball

                { Events = []; Transition = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Possession = Contest(attClub)
                        Trajectory = None }

                { Events = []; Transition = Some Live }

        | BallActionKind.Cross(crosserId, targetId, quality) ->
            let arrivalCtx: ArrivalContext =
                { BallPos = ball.Position
                  TargetId = targetId
                  Quality = quality
                  HomeFrame = homeFrame
                  AwayFrame = awayFrame
                  HomeRoster = homeRoster
                  AwayRoster = awayRoster
                  PhysicsCfg = ctx.Config.Physics }

            match Interception.evaluateArrival arrivalCtx with
            | NoOneInRange ->
                state.Ball <- ball

                { Events = []; Transition = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub crosserId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Possession = Contest(attClub)
                        Trajectory = None }

                { Events = []; Transition = Some Live }

        | BallActionKind.Clearance _
        | BallActionKind.Deflection _
        | BallActionKind.FreeBall ->
            state.Ball <- ball

            { Events = []; Transition = None }

    let agent (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlayerResult =
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

            { Events = []; Transition = None }
        | _ ->
            let withStationary = BallPhysics.update pcfg dt state.Ball

            let wasStationary = state.Ball.StationarySinceSubTick.IsSome
            let isNowStopped = ballNearlyStopped withStationary

            let withStationary =
                if isNowStopped && not wasStationary then
                    { withStationary with
                        StationarySinceSubTick = Some state.SubTick }
                elif not isNowStopped then
                    { withStationary with
                        StationarySinceSubTick = None }
                else
                    withStationary

            let isLive =
                match state.Flow with
                | Live -> true
                | _ -> false

            if isLive then
                // ARRIVAL SYSTEM: si el balón está chegando a alguien, resolver arrival
                let nearestChaser =
                    findNearestChaserSoA pcfg withStationary.Position homeFrame awayFrame homeRoster awayRoster

                // 1. Chequeos de out-of-bounds primero
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
                        let restartTeam = ClubSide.flip side

                        state.Ball <-
                            { withStationary with
                                Possession = Possession.SetPiece(restartTeam, SetPieceKind.ThrowIn) }

                        { Events = []
                          Transition =
                            Some(
                                MatchFlow.RestartDelay
                                    { Kind = SetPieceKind.ThrowIn
                                      Team = restartTeam
                                      Cause = AfterBallOut
                                      RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.ThrowInDelay }
                            ) }
                    | None ->
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose }

                        { Events = []
                          Transition = Some MatchFlow.Live }
                elif outX then
                    let inGoalY = by >= PhysicsContract.PostNearY && by <= PhysicsContract.PostFarY
                    let inGoalZ = bz >= 0.0<meter> && bz <= PhysicsContract.CrossbarHeight

                    if inGoalY && inGoalZ then
                        let scoringClub =
                            match lastTouchClub with
                            | Some HomeClub -> Some HomeClub
                            | Some AwayClub -> Some AwayClub
                            | None -> None

                        match scoringClub with
                        | Some sc ->
                            clearOffsideSnapshot state

                            state.Ball <-
                                { withStationary with
                                    Possession = Possession.SetPiece(ClubSide.flip sc, SetPieceKind.KickOff) }

                            { Events = []
                              Transition =
                                Some(
                                    MatchFlow.GoalPause
                                        { ScoringTeam = sc
                                          ScorerId = state.Ball.LastTouchBy
                                          IsOwnGoal = false
                                          RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.GoalDelay
                                          VARRequested = false }
                                ) }
                        | None ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { Events = []
                              Transition = Some MatchFlow.Live }
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

                                { Events = []
                                  Transition =
                                    Some(
                                        MatchFlow.RestartDelay
                                            { Kind = SetPieceKind.Corner
                                              Team = HomeClub
                                              Cause = AfterBallOut
                                              RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.CornerDelay }
                                    ) }
                            | _ ->
                                state.Ball <-
                                    { withStationary with
                                        Possession = Possession.SetPiece(HomeClub, SetPieceKind.GoalKick) }

                                { Events = []
                                  Transition =
                                    Some(
                                        MatchFlow.RestartDelay
                                            { Kind = SetPieceKind.GoalKick
                                              Team = HomeClub
                                              Cause = AfterBallOut
                                              RemainingTicks =
                                                TickDelay.delayFrom clock state.Config.Timing.GoalKickDelay }
                                    ) }
                        elif behindAway then
                            match lastTouchClub with
                            | Some HomeClub ->
                                state.Ball <-
                                    { withStationary with
                                        Possession = Possession.SetPiece(AwayClub, SetPieceKind.Corner) }

                                { Events = []
                                  Transition =
                                    Some(
                                        MatchFlow.RestartDelay
                                            { Kind = SetPieceKind.Corner
                                              Team = AwayClub
                                              Cause = AfterBallOut
                                              RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.CornerDelay }
                                    ) }
                            | _ ->
                                state.Ball <-
                                    { withStationary with
                                        Possession = Possession.SetPiece(AwayClub, SetPieceKind.GoalKick) }

                                { Events = []
                                  Transition =
                                    Some(
                                        MatchFlow.RestartDelay
                                            { Kind = SetPieceKind.GoalKick
                                              Team = AwayClub
                                              Cause = AfterBallOut
                                              RemainingTicks =
                                                TickDelay.delayFrom clock state.Config.Timing.GoalKickDelay }
                                    ) }
                        else
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { Events = []
                              Transition = Some MatchFlow.Live }
                // 2. Arrival condition
                elif shouldTriggerArrival withStationary pcfg state homeFrame awayFrame homeRoster awayRoster then
                    match withStationary.Trajectory with
                    | Some traj -> resolveArrival state.SubTick ctx state withStationary traj
                    | None ->
                        let winner, _, _ =
                            Interception.chooseBestInterceptorSoA
                                pcfg
                                withStationary.Position
                                homeFrame
                                awayFrame
                                homeRoster
                                awayRoster

                        match winner with
                        | Some p ->
                            let club =
                                SimStateOps.clubSideOf ctx state p.Id |> Option.defaultValue state.AttackingSide

                            givePossessionTo club p.Id (p.Position = GK) state.SubTick withStationary state

                            { Events = []
                              Transition = Some MatchFlow.Live }
                        | None ->
                            state.Ball <- withStationary
                            { Events = []; Transition = None }

                elif ballNearlyStopped withStationary then
                    state.Ball <-
                        { zeroVel withStationary with
                            Possession = Loose }

                    { Events = []
                      Transition = Some MatchFlow.Live }

                else
                    match state.Ball.Possession with
                    | Possession.SetPiece _ ->
                        state.Ball <-
                            { zeroVel withStationary with
                                Possession = Loose }

                        { Events = []
                          Transition = Some MatchFlow.Live }

                    | Possession.Contest side ->
                        let isSameSide pid =
                            match clubSideOf ctx state pid with
                            | Some c -> c = side
                            | _ -> false

                        match nearestChaser with
                        | Some pid when isSameSide pid ->
                            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                            | Some(player, _, _) ->
                                givePossessionTo side player.Id (player.Position = GK) state.SubTick withStationary state

                                { Events = []
                                  Transition = Some MatchFlow.Live }
                            | None ->
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Possession = Loose }

                                { Events = []
                                  Transition = Some MatchFlow.Live }
                        | _ ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Possession = Loose }

                            { Events = []
                              Transition = Some MatchFlow.Live }

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
                                    givePossessionTo club s.Id (s.Position = GK) state.SubTick withStationary state

                                    { Events = []
                                      Transition = Some MatchFlow.Live }
                                else
                                    state.Ball <- withStationary

                                    { Events = []; Transition = None }
                            | None ->
                                state.Ball <- withStationary

                                { Events = []; Transition = None }
                        | None ->
                            state.Ball <- withStationary

                            { Events = []; Transition = None }

                    | Possession.InFlight ->
                        state.Ball <- withStationary

                        { Events = []; Transition = None }

                    | Possession.Owned _ ->
                        state.Ball <- withStationary

                        { Events = []; Transition = None }

                    | Possession.Transition _ ->
                        match nearestChaser with
                        | Some pid ->
                            let club =
                                SimStateOps.clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

                            match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                            | Some(s, _, _) ->
                                givePossessionTo club s.Id (s.Position = GK) state.SubTick withStationary state

                                { Events = []
                                  Transition = Some MatchFlow.Live }
                            | None ->
                                state.Ball <- withStationary

                                { Events = []; Transition = None }
                        | None ->
                            state.Ball <- withStationary

                            { Events = []; Transition = None }
            else
                state.Ball <- withStationary
                { Events = []; Transition = None }
