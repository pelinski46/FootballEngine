namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open FootballEngine.Types
open FootballEngine.Types.PhysicsContract
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
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let player = roster.Players[i]

                    // INVARIANT: Don't trigger arrival for the player who just kicked the ball.
                    // Give it ~10 subticks (0.25 seconds) to escape the kicker's contact radius.
                    let isKicker =
                        match ball.Trajectory with
                        | Some t -> t.KickerId = player.Id && (state.SubTick - t.LaunchSubTick < 10)
                        | None -> false

                    if not isKicker then
                        let px = float frame.Physics.PosX[i] * 1.0<meter>
                        let py = float frame.Physics.PosY[i] * 1.0<meter>

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
            match homeFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = homeRoster.Players[i]
                let px = float homeFrame.Physics.PosX[i] * 1.0<meter>
                let py = float homeFrame.Physics.PosY[i] * 1.0<meter>

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
            match awayFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = awayRoster.Players[i]
                let px = float awayFrame.Physics.PosX[i] * 1.0<meter>
                let py = float awayFrame.Physics.PosY[i] * 1.0<meter>

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
                match homeFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ when homeRoster.Players[i].Id = pid ->
                    Some(
                        homeRoster.Players[i],
                        float homeFrame.Physics.PosX[i] * 1.0<meter>,
                        float homeFrame.Physics.PosY[i] * 1.0<meter>
                    )
                | _ -> searchHome (i + 1)

        let rec searchAway i =
            if i >= awayFrame.SlotCount then
                None
            else
                match awayFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ when awayRoster.Players[i].Id = pid ->
                    Some(
                        awayRoster.Players[i],
                        float awayFrame.Physics.PosX[i] * 1.0<meter>,
                        float awayFrame.Physics.PosY[i] * 1.0<meter>
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
        let penX = PitchLength - PenaltyAreaDepth

        match dir with
        | LeftToRight -> x > penX
        | RightToLeft -> x < PenaltyAreaDepth

    let private isGoalLineCrossed (x: float<meter>) (dir: AttackDir) : bool =
        match dir with
        | LeftToRight -> x >= PitchLength
        | RightToLeft -> x <= 0.0<meter>

    let private isInGoal (x: float<meter>) (y: float<meter>) (z: float<meter>) (dir: AttackDir) : bool =
        let crossed = isGoalLineCrossed x dir
        let inY = y >= PostNearY && y <= PostFarY
        let inZ = z >= 0.0<meter> && z <= CrossbarHeight
        crossed && inY && inZ

    let private resolveInFlightOutcome
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (withStationary: BallPhysicsState)
        (player: Player)
        (playerClub: ClubSide)
        : BallResult =

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

            { Events = []
              Transition = Some Live
              PossessionChanged = true
              BallInFlight = false
              SetPieceAwarded = false
              ReceivedByPlayer = Some player.Id
              GoalScored = None }

        | Some traj ->
            match traj.Intent with
            | Aimed(passerId, targetId, quality, RegularPass) ->
                let attClubId = passerClubId passerId

                if player.Id = targetId then
                    let controlProb = normaliseAttr player.Technical.BallControl * gkc.CatchHandlingMult

                    if bernoulli (System.Math.Clamp(controlProb, 0.0, 0.95)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = Some player.Id
                          GoalScored = None }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Control = Free
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (PassCompleted(passerId, targetId)) ]
                          Transition = Some Live
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                elif
                    player.Position = GK
                    && player.Id <> passerId // INVARIANT: GK cannot intercept their own pass
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    let interceptProb = normaliseAttr player.Mental.Positioning * gkc.CatchHandlingMult

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.95)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = Some player.Id
                          GoalScored = None }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Control = Free
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = []
                          Transition = Some Live
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                else
                    let interceptProb = normaliseAttr player.Mental.Positioning * 0.5

                    if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.8)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (PassIntercepted(passerId, player.Id)) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = Some player.Id
                          GoalScored = None }
                    else
                        clearOffside ()

                        state.Ball <-
                            { withStationary with
                                Control = Contesting playerClub
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (PassMisplaced(passerId, player.Id)) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }

            | Aimed(passerId, targetId, quality, AimedKind.LongBall) ->
                let attClubId = passerClubId passerId

                if player.Id = targetId then
                    let controlProb =
                        normaliseAttr player.Technical.BallControl * gkc.CatchHandlingMult * 0.8

                    if bernoulli (System.Math.Clamp(controlProb, 0.0, 0.9)) then
                        clearOffside ()
                        givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                        { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall true) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = Some player.Id
                          GoalScored = None }
                    else
                        clearOffside ()

                        state.Ball <-
                            { zeroVel withStationary with
                                Control = Free
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                          Transition = Some Live
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                elif
                    player.Position = GK
                    && player.Id <> passerId // INVARIANT: GK cannot intercept their own long ball
                    && isInPenaltyArea ballPos.X ballPos.Y (attackDirFor playerClub state)
                then
                    clearOffside ()
                    givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                    { Events = [ createEvent tick passerId attClubId (MatchEventType.LongBall false) ]
                      Transition = Some Live
                      PossessionChanged = true
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = Some player.Id
                      GoalScored = None }
                else
                    clearOffside ()

                    state.Ball <-
                        { withStationary with
                            Control = Contesting playerClub
                            Trajectory = None }

                    { Events = []
                      Transition = Some Live
                      PossessionChanged = true
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = None
                      GoalScored = None }

            | Struck(shooterId, shotQuality) ->
                let shooterClub = passerClubSide shooterId
                let shooterClubId = passerClubId shooterId
                let shootingDir = attackDirFor shooterClub state
                let gkDir = attackDirFor playerClub state

                if isInGoal ballPos.X ballPos.Y ballPos.Z shootingDir then
                    let scoringClub = shooterClub
                    clearOffside ()

                    state.Ball <-
                        { withStationary with
                            Control = Free
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
                        )
                      PossessionChanged = true
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = None
                      GoalScored = None }
                elif player.Position = GK && isInPenaltyArea ballPos.X ballPos.Y gkDir then
                    let distToGoal =
                        if shootingDir = LeftToRight then
                            PitchLength - ballPos.X
                        else
                            ballPos.X

                    let gkReactionBonus = 1.0 - float distToGoal / float PenaltyAreaDepth

                    let reflexesNorm = normaliseAttr player.Goalkeeping.Reflexes
                    let saveProb = reflexesNorm * gkc.CatchHandlingMult + gkReactionBonus * 0.3

                    if bernoulli (System.Math.Clamp(saveProb, 0.0, 0.95)) then
                        let catchProb = normaliseAttr player.Goalkeeping.Handling * gkc.CatchHandlingMult

                        if bernoulli (System.Math.Clamp(catchProb, 0.0, 0.9)) then
                            clearOffside ()
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveCaught(shooterId, player.Id)) ]
                              Transition = Some Live
                              PossessionChanged = true
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = Some player.Id
                              GoalScored = None }
                        else
                            clearOffside ()
                            let dirSign = forwardX shootingDir
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
                                    Control = Free
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = None
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events =
                                [ createEvent tick shooterId gkClubId Save
                                  createEvent tick shooterId gkClubId (SaveParried(shooterId, player.Id)) ]
                              Transition = Some Live
                              PossessionChanged = false
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }
                    else
                        state.Ball <- withStationary

                        { Events = []
                          Transition = None
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                else if isGoalLineCrossed ballPos.X shootingDir then
                    clearOffside ()

                    let lastTouchClub =
                        state.Ball.LastTouchBy
                        |> Option.bind (fun pid ->
                            if playerOnSide ctx state HomeClub pid then Some HomeClub
                            elif playerOnSide ctx state AwayClub pid then Some AwayClub
                            else None)

                    let defClub = ClubSide.flip shooterClub

                    state.Ball <-
                        { withStationary with
                            Control = Free
                            Trajectory = None }

                    let transition =
                        match lastTouchClub with
                        | Some club when club = defClub ->
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
                      Transition = Some transition
                      PossessionChanged = false
                      BallInFlight = false
                      SetPieceAwarded = true
                      ReceivedByPlayer = None
                      GoalScored = None }
                else
                    state.Ball <- withStationary

                    { Events = []
                      Transition = None
                      PossessionChanged = false
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = None
                      GoalScored = None }

            | Aimed(crosserId, targetId, crossQuality, AimedKind.Cross) ->
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
                        normaliseAttr player.Goalkeeping.AerialReach
                        * ctx.Config.Cross.GkAerialReachMult
                        + normaliseAttr player.Physical.JumpingReach * ctx.Config.Cross.GkJumpMult

                    let condFactor = normaliseCondition player.Condition
                    let claimProb = gkReach * condFactor * ctx.Config.Cross.ClaimCrossProbability

                    if bernoulli (System.Math.Clamp(claimProb, 0.0, 0.95)) then
                        let catchProb = normaliseAttr player.Goalkeeping.Handling * gkc.CatchHandlingMult

                        if bernoulli (System.Math.Clamp(catchProb, 0.0, 0.9)) then
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events =
                                [ createEvent tick crosserId crossAttClubId (CrossAttempt false)
                                  createEvent tick player.Id gkClubId Save
                                  createEvent tick crosserId crossAttClubId (SaveCaught(crosserId, player.Id)) ]
                              Transition = Some Live
                              PossessionChanged = true
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = Some player.Id
                              GoalScored = None }
                        else
                            let dirSign = forwardX crossDir
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
                                    Control = Free
                                    LastTouchBy = Some player.Id
                                    GKHoldSinceSubTick = None
                                    Trajectory = None }

                            let gkClubId = if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

                            { Events = [ createEvent tick crosserId gkClubId (GKPunch player.Id) ]
                              Transition = Some Live
                              PossessionChanged = false
                              BallInFlight = true
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }
                    else
                        state.Ball <- withStationary

                        { Events = []
                          Transition = None
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                elif player.Id = targetId then
                    let headerProb =
                        normaliseAttr player.Technical.Heading * 0.6
                        + normaliseAttr player.Physical.Strength * 0.4

                    if bernoulli (System.Math.Clamp(headerProb, 0.0, 0.9)) then
                        let shootingDir = attackDirFor playerClub state
                        let distToGoal = PhysicsContract.distToGoal ballPos.X shootingDir

                        if distToGoal < 15.0<meter> then
                            let shotPower = float player.Technical.Finishing / 20.0 * 10.0
                            let dirSign = forwardX shootingDir
                            let angle = normalSample 0.0 0.3
                            let vx = dirSign * shotPower * System.Math.Cos(angle)
                            let vy = shotPower * System.Math.Sin(angle)
                            let vz = abs (normalSample 2.0 1.0) * 1.0<meter / second>

                            let goalX =
                                if shootingDir = LeftToRight then
                                    PitchLength
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
                                                Intent = Struck(player.Id, headerProb)
                                                TargetY = targetY } }

                            { Events =
                                [ createEvent
                                      tick
                                      player.Id
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      ShotOnTarget ]
                              Transition = None
                              PossessionChanged = false
                              BallInFlight = true
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }
                        else
                            givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                            { Events =
                                [ createEvent
                                      tick
                                      crosserId
                                      (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                      (CrossAttempt true) ]
                              Transition = Some Live
                              PossessionChanged = true
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = Some player.Id
                              GoalScored = None }
                    else
                        state.Ball <-
                            { zeroVel withStationary with
                                Control = Free
                                LastTouchBy = Some player.Id
                                Trajectory = None }

                        { Events =
                            [ createEvent
                                  tick
                                  crosserId
                                  (if playerClub = HomeClub then ctx.Home.Id else ctx.Away.Id)
                                  (CrossAttempt false) ]
                          Transition = Some Live
                          PossessionChanged = false
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                else if player.Position <> GK then
                    let clearProb = 0.5 + normaliseAttr player.Physical.Strength * 0.3

                    if bernoulli (System.Math.Clamp(clearProb, 0.0, 0.9)) then
                        let dirSign = -forwardX crossDir
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
                                Control = Airborne
                                LastTouchBy = Some player.Id
                                Trajectory = Some { traj with Intent = Cleared player.Id } }

                        { Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = None
                          PossessionChanged = false
                          BallInFlight = true
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                    else
                        state.Ball <-
                            { withStationary with
                                Control = Contesting playerClub
                                Trajectory = None }

                        { Events = [ createEvent tick crosserId crossAttClubId (CrossAttempt false) ]
                          Transition = Some Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }
                else
                    state.Ball <-
                        { withStationary with
                            Control = Contesting playerClub
                            Trajectory = None }

                    { Events = []
                      Transition = Some Live
                      PossessionChanged = true
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = None
                      GoalScored = None }

            | Cleared(_)
            | Uncontrolled ->
                givePossessionTo playerClub player.Id (player.Position = GK) tick withStationary state

                { Events = []
                  Transition = Some Live
                  PossessionChanged = true
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = Some player.Id
                  GoalScored = None }

    let private resolveShot
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (ball: BallPhysicsState)
        (traj: BallTrajectory)
        : BallResult =
        let ballPos = ball.Position

        let shooterClub =
            if playerOnSide ctx state HomeClub traj.KickerId then
                HomeClub
            else
                AwayClub

        let shootingDir = attackDirFor shooterClub state
        let defClub = ClubSide.flip shooterClub

        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub
        let defFrame = if defClub = HomeClub then homeFrame else awayFrame
        let defRoster = if defClub = HomeClub then homeRoster else awayRoster
        let gkDir = attackDirFor defClub state
        let mutable gkPlayer: Player option = None

        for i = 0 to defFrame.SlotCount - 1 do
            match defFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ when defRoster.Players[i].Position = GK ->
                if isInPenaltyArea ballPos.X ballPos.Y gkDir then
                    gkPlayer <- Some defRoster.Players[i]
            | _ -> ()

        match gkPlayer with
        | Some gk -> resolveInFlightOutcome tick ctx state ball gk defClub
        | None ->
            if isInGoal ballPos.X ballPos.Y ballPos.Z shootingDir then
                clearOffsideSnapshot state

                state.Ball <-
                    { ball with
                        Control = Free
                        Trajectory = None }

                { BallResult.empty with
                    GoalScored = Some shooterClub }

            elif isGoalLineCrossed ballPos.X shootingDir then
                clearOffsideSnapshot state

                let lastTouchClub =
                    ball.LastTouchBy
                    |> Option.bind (fun pid ->
                        if playerOnSide ctx state HomeClub pid then Some HomeClub
                        elif playerOnSide ctx state AwayClub pid then Some AwayClub
                        else None)

                state.Ball <-
                    { ball with
                        Control = Free
                        Trajectory = None }

                let transition =
                    match lastTouchClub with
                    | Some club when club = defClub ->
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

                { BallResult.empty with
                    Transition = Some transition
                    SetPieceAwarded = true }

            else
                state.Ball <- ball
                BallResult.empty

    let private resolveInterception
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (ball: BallPhysicsState)
        (interceptor: Player)
        (interceptorClub: ClubSide)
        (passerId: PlayerId)
        (passerClub: ClubSide)
        : BallResult =

        let passerClubId = if passerClub = HomeClub then ctx.Home.Id else ctx.Away.Id
        let passerFrame = getFrame state passerClub
        let passerRoster = getRoster ctx passerClub

        match findIdxByPid passerId passerFrame passerRoster with
        | ValueSome idx -> MatchMemory.recordPassFailure passerClub idx state.MatchMemory
        | ValueNone -> ()

        clearOffsideSnapshot state

        givePossessionTo interceptorClub interceptor.Id (interceptor.Position = GK) tick ball state

        { Events = [ createEvent tick passerId passerClubId (PassIntercepted(passerId, interceptor.Id)) ]
          Transition = Some Live
          PossessionChanged = true
          BallInFlight = false
          SetPieceAwarded = false
          ReceivedByPlayer = Some interceptor.Id
          GoalScored = None }

    let private resolveArrival
        (tick: int)
        (ctx: MatchContext)
        (state: SimState)
        (ball: BallPhysicsState)
        (traj: BallTrajectory)
        : BallResult =
        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub

        match traj.Intent with
        | Struck _ -> resolveShot tick ctx state ball traj

        | Aimed(passerId, targetId, quality, RegularPass) ->
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

                { Events = []
                  Transition = None
                  PossessionChanged = false
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) ->
                let passerClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                resolveInterception tick ctx state ball player club passerId passerClub
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Control = Contesting attClub
                        Trajectory = None }

                { Events = []
                  Transition = Some Live
                  PossessionChanged = true
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }

        | Aimed(passerId, targetId, quality, AimedKind.LongBall) ->
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

                { Events = []
                  Transition = None
                  PossessionChanged = false
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) ->
                let passerClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                resolveInterception tick ctx state ball player club passerId passerClub
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub passerId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Control = Contesting attClub
                        Trajectory = None }

                { Events = []
                  Transition = Some Live
                  PossessionChanged = true
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }

        | Aimed(crosserId, targetId, quality, AimedKind.Cross) ->
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

                { Events = []
                  Transition = None
                  PossessionChanged = false
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }
            | IntendedTarget(player, club) -> resolveInFlightOutcome tick ctx state ball player club
            | Intercepted(player, club) ->
                let passerClub =
                    if playerOnSide ctx state HomeClub crosserId then
                        HomeClub
                    else
                        AwayClub

                resolveInterception tick ctx state ball player club crosserId passerClub
            | Contested ->
                let attClub =
                    if playerOnSide ctx state HomeClub crosserId then
                        HomeClub
                    else
                        AwayClub

                state.Ball <-
                    { zeroVel ball with
                        Control = Contesting attClub
                        Trajectory = None }

                { Events = []
                  Transition = Some Live
                  PossessionChanged = true
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }

        | Cleared _
        | Uncontrolled ->
            state.Ball <- ball

            { Events = []
              Transition = None
              PossessionChanged = false
              BallInFlight = false
              SetPieceAwarded = false
              ReceivedByPlayer = None
              GoalScored = None }

    let agent (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : BallResult =
        let pcfg = ctx.Config.Physics
        let dt = SimulationClock.dt clock

        let trajectoryBefore = state.Ball.Trajectory

        let homeFrame = getFrame state HomeClub
        let awayFrame = getFrame state AwayClub
        let homeRoster = getRoster ctx HomeClub
        let awayRoster = getRoster ctx AwayClub

        let result =
            match state.Ball.Control with
            | Controlled(_, pid)
            | Receiving(_, pid, _) ->
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
                | None ->

                    ()

                { Events = []
                  Transition = None
                  PossessionChanged = false
                  BallInFlight = false
                  SetPieceAwarded = false
                  ReceivedByPlayer = None
                  GoalScored = None }
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

                    let nearestChaser =
                        findNearestChaserSoA pcfg withStationary.Position homeFrame awayFrame homeRoster awayRoster

                    let bx = withStationary.Position.X
                    let by = withStationary.Position.Y
                    let bz = withStationary.Position.Z

                    let outY = by <= 0.1<meter> || by >= PitchWidth - 0.1<meter>
                    let outX = bx <= 0.1<meter> || bx >= PitchLength - 0.1<meter>

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

                            state.Ball <- { withStationary with Control = Free }

                            { Events = []
                              Transition =
                                Some(
                                    MatchFlow.RestartDelay
                                        { Kind = SetPieceKind.ThrowIn
                                          Team = restartTeam
                                          Cause = AfterBallOut
                                          RemainingTicks = TickDelay.delayFrom clock state.Config.Timing.ThrowInDelay }
                                )
                              PossessionChanged = false
                              BallInFlight = false
                              SetPieceAwarded = true
                              ReceivedByPlayer = None
                              GoalScored = None }

                        | None ->
                            state.Ball <-
                                { zeroVel withStationary with
                                    Control = Free }

                            { Events = []
                              Transition = Some MatchFlow.Live
                              PossessionChanged = true
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }
                    elif outX then
                        let inGoalY = by >= PostNearY && by <= PostFarY
                        let inGoalZ = bz >= 0.0<meter> && bz <= CrossbarHeight

                        if inGoalY && inGoalZ then
                            let scoringClub =
                                match lastTouchClub with
                                | Some HomeClub -> Some HomeClub
                                | Some AwayClub -> Some AwayClub
                                | None -> None

                            match scoringClub with
                            | Some sc ->
                                clearOffsideSnapshot state

                                state.Ball <- { withStationary with Control = Free }

                                let goalResult =
                                    { BallResult.empty with
                                        GoalScored = Some sc }

                                goalResult


                            | None ->
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Control = Free }

                                { Events = []
                                  Transition = Some MatchFlow.Live
                                  PossessionChanged = true
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }
                        else
                            clearOffsideSnapshot state
                            let behindHome = bx <= 0.5<meter>
                            let behindAway = bx >= PitchLength - 0.5<meter>

                            if behindHome then
                                match lastTouchClub with
                                | Some AwayClub ->
                                    state.Ball <- { withStationary with Control = Free }

                                    { Events = []
                                      Transition =
                                        Some(
                                            MatchFlow.RestartDelay
                                                { Kind = SetPieceKind.Corner
                                                  Team = HomeClub
                                                  Cause = AfterBallOut
                                                  RemainingTicks =
                                                    TickDelay.delayFrom clock state.Config.Timing.CornerDelay }
                                        )
                                      PossessionChanged = false
                                      BallInFlight = false
                                      SetPieceAwarded = true
                                      ReceivedByPlayer = None
                                      GoalScored = None }
                                | _ ->
                                    state.Ball <- { withStationary with Control = Free }

                                    { Events = []
                                      Transition =
                                        Some(
                                            MatchFlow.RestartDelay
                                                { Kind = SetPieceKind.GoalKick
                                                  Team = HomeClub
                                                  Cause = AfterBallOut
                                                  RemainingTicks =
                                                    TickDelay.delayFrom clock state.Config.Timing.GoalKickDelay }
                                        )
                                      PossessionChanged = false
                                      BallInFlight = false
                                      SetPieceAwarded = true
                                      ReceivedByPlayer = None
                                      GoalScored = None }

                            elif behindAway then
                                match lastTouchClub with
                                | Some HomeClub ->
                                    state.Ball <- { withStationary with Control = Free }

                                    { Events = []
                                      Transition =
                                        Some(
                                            MatchFlow.RestartDelay
                                                { Kind = SetPieceKind.Corner
                                                  Team = AwayClub
                                                  Cause = AfterBallOut
                                                  RemainingTicks =
                                                    TickDelay.delayFrom clock state.Config.Timing.CornerDelay }
                                        )
                                      PossessionChanged = false
                                      BallInFlight = false
                                      SetPieceAwarded = true
                                      ReceivedByPlayer = None
                                      GoalScored = None }

                                | _ ->
                                    state.Ball <- { withStationary with Control = Free }

                                    { Events = []
                                      Transition =
                                        Some(
                                            MatchFlow.RestartDelay
                                                { Kind = SetPieceKind.GoalKick
                                                  Team = AwayClub
                                                  Cause = AfterBallOut
                                                  RemainingTicks =
                                                    TickDelay.delayFrom clock state.Config.Timing.GoalKickDelay }
                                        )
                                      PossessionChanged = false
                                      BallInFlight = false
                                      SetPieceAwarded = true
                                      ReceivedByPlayer = None
                                      GoalScored = None }

                            else
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Control = Free }

                                { Events = []
                                  Transition = Some MatchFlow.Live
                                  PossessionChanged = true
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }
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
                                let club = clubSideOf ctx state p.Id |> Option.defaultValue state.AttackingSide

                                givePossessionTo club p.Id (p.Position = GK) state.SubTick withStationary state

                                { Events = []
                                  Transition = Some MatchFlow.Live
                                  PossessionChanged = true
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }
                            | None ->
                                state.Ball <- withStationary

                                { Events = []
                                  Transition = None
                                  PossessionChanged = false
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }

                    elif ballNearlyStopped withStationary then
                        state.Ball <-
                            { zeroVel withStationary with
                                Control = Free }

                        { Events = []
                          Transition = Some MatchFlow.Live
                          PossessionChanged = true
                          BallInFlight = false
                          SetPieceAwarded = false
                          ReceivedByPlayer = None
                          GoalScored = None }

                    else
                        match state.Ball.Control with
                        | Contesting side ->
                            let isSameSide pid =
                                match clubSideOf ctx state pid with
                                | Some c -> c = side
                                | _ -> false

                            match nearestChaser with
                            | Some pid when isSameSide pid ->
                                match findPlayerByPidSoA pid homeFrame awayFrame homeRoster awayRoster with
                                | Some(player, _, _) ->
                                    givePossessionTo
                                        side
                                        player.Id
                                        (player.Position = GK)
                                        state.SubTick
                                        withStationary
                                        state

                                    { Events = []
                                      Transition = Some MatchFlow.Live
                                      PossessionChanged = true
                                      BallInFlight = false
                                      SetPieceAwarded = false
                                      ReceivedByPlayer = None
                                      GoalScored = None }
                                | None ->
                                    state.Ball <-
                                        { zeroVel withStationary with
                                            Control = Free }

                                    { Events = []
                                      Transition = Some MatchFlow.Live
                                      PossessionChanged = true
                                      BallInFlight = false
                                      SetPieceAwarded = false
                                      ReceivedByPlayer = None
                                      GoalScored = None }
                            | _ ->
                                state.Ball <-
                                    { zeroVel withStationary with
                                        Control = Free }

                                { Events = []
                                  Transition = Some MatchFlow.Live
                                  PossessionChanged = true
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }

                        | Free ->
                            match nearestChaser with
                            | Some pid ->
                                let club = clubSideOf ctx state pid |> Option.defaultValue state.AttackingSide

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

                                    if timeToBall < 1.0 then
                                        givePossessionTo club s.Id (s.Position = GK) state.SubTick withStationary state

                                        { Events = []
                                          Transition = Some MatchFlow.Live
                                          PossessionChanged = true
                                          BallInFlight = false
                                          SetPieceAwarded = false
                                          ReceivedByPlayer = None
                                          GoalScored = None }
                                    else
                                        state.Ball <- withStationary

                                        { Events = []
                                          Transition = None
                                          PossessionChanged = false
                                          BallInFlight = false
                                          SetPieceAwarded = false
                                          ReceivedByPlayer = None
                                          GoalScored = None }
                                | None ->
                                    state.Ball <- withStationary

                                    { Events = []
                                      Transition = None
                                      PossessionChanged = false
                                      BallInFlight = false
                                      SetPieceAwarded = false
                                      ReceivedByPlayer = None
                                      GoalScored = None }
                            | None ->
                                state.Ball <- withStationary

                                { Events = []
                                  Transition = None
                                  PossessionChanged = false
                                  BallInFlight = false
                                  SetPieceAwarded = false
                                  ReceivedByPlayer = None
                                  GoalScored = None }

                        | Airborne ->
                            state.Ball <- withStationary

                            { Events = []
                              Transition = None
                              PossessionChanged = false
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }

                        | Controlled _
                        | Receiving _ ->
                            state.Ball <- withStationary

                            { Events = []
                              Transition = None
                              PossessionChanged = false
                              BallInFlight = false
                              SetPieceAwarded = false
                              ReceivedByPlayer = None
                              GoalScored = None }


                else
                    state.Ball <- withStationary

                    { Events = []
                      Transition = None
                      PossessionChanged = false
                      BallInFlight = false
                      SetPieceAwarded = false
                      ReceivedByPlayer = None
                      GoalScored = None }

        // Phase 4: detect if a new pass/shot trajectory was launched THIS tick (BallInFlight trigger)
        // ActionResolver may have set Trajectory between the previous agent tick and now.
        // If Trajectory appeared or changed and possession is InFlight, a launch just happened.
        let trajectoryAfter = state.Ball.Trajectory

        let launchDetected =
            match trajectoryBefore, trajectoryAfter with
            | None, Some _ -> true
            | Some t1, Some t2 -> t1.LaunchSubTick <> t2.LaunchSubTick
            | _ -> false

        if launchDetected && not result.BallInFlight then
            { result with BallInFlight = true }
        else
            result
