namespace FootballEngine.Player.Actions

open System
open FootballEngine
open FootballEngine.Domain

open FootballEngine.MatchSpatial
open FootballEngine.Player.Decision

open FootballEngine.SimStateOps
open FootballEngine.Stats
open FootballEngine.Types
open FootballEngine.Types.MatchMemory
open FootballEngine.Types.PhysicsContract
open SimStateOps
open MatchSpatial




module PassAction =

    let private predictLeadPosition
        (leadFactor: float)
        (passerX: float<meter>)
        (passerY: float<meter>)
        (targetX: float<meter>)
        (targetY: float<meter>)
        (targetVx: float<meter / second>)
        (targetVy: float<meter / second>)
        (passSpeed: float<meter / second>)
        : float<meter> * float<meter> =
        let dx = targetX - passerX
        let dy = targetY - passerY
        let dist = sqrt (dx * dx + dy * dy)

        let flightTime =
            if passSpeed > 0.0<meter / second> then
                dist / passSpeed
            else
                0.5<second>

        let effectiveTime = flightTime * leadFactor
        let leadX = targetX + targetVx * effectiveTime
        let leadY = targetY + targetVy * effectiveTime
        clamp leadX 0.0<meter> PitchLength, clamp leadY 0.0<meter> PitchWidth

    let private makeTrajectory
        (subTick: int)
        (clock: SimulationClock)
        (passerId: PlayerId)
        (ox: float<meter>)
        (oy: float<meter>)
        (tx: float<meter>)
        (ty: float<meter>)
        (speed: float<meter / second>)
        (vz: float<meter / second>)
        (intent: InFlightIntent)
        : BallTrajectory =
        let dist = sqrt ((tx - ox) * (tx - ox) + (ty - oy) * (ty - oy))

        let flightTime =
            if speed > 0.0<meter / second> then
                dist / speed
            else
                0.5<second>

        let arrivalSubTick =
            subTick + int (float (flightTime / 1.0<second>) * float clock.SubTicksPerSecond)

        let peakHeight =
            if vz > 0.0<meter / second> then
                vz * vz / (2.0 * 9.80665<meter / second^2>)
            else
                0.0<meter>

        { OriginX = ox
          OriginY = oy
          TargetX = tx
          TargetY = ty
          LaunchSubTick = subTick
          EstimatedArrivalSubTick = arrivalSubTick
          KickerId = passerId
          PeakHeight = peakHeight
          Intent = intent }

    let resolve
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (target: Player)
        : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = getRoster ctx actx.Att.ClubSide

        let targetIdx =
            match findIdxByPid target.Id attFrame attRoster with
            | ValueSome i -> i
            | ValueNone -> -1

        if targetIdx < 0 then
            []
        else
            let targetCurrX = float attFrame.Physics.PosX[targetIdx] * 1.0<meter>
            let targetCurrY = float attFrame.Physics.PosY[targetIdx] * 1.0<meter>
            let targetVx = float attFrame.Physics.VelX[targetIdx] * 1.0<meter / second>
            let targetVy = float attFrame.Physics.VelY[targetIdx] * 1.0<meter / second>

            let targetSp =
                { X = targetCurrX
                  Y = targetCurrY
                  Z = 0.0<meter>
                  Vx = targetVx
                  Vy = targetVy
                  Vz = 0.0<meter / second> }

            let passerIdx =
                match state.Ball.Control with
                | Controlled(_, ctrlId) ->
                    match findIdxByPid ctrlId attFrame attRoster with
                    | ValueSome i -> Some i
                    | ValueNone -> None
                | _ ->
                    match state.Ball.LastTouchBy with
                    | Some pid ->
                        match findIdxByPid pid attFrame attRoster with
                        | ValueSome i -> Some i
                        | ValueNone -> None
                    | None -> None

            match passerIdx with
            | None -> []
            | Some pIdx ->
                updateMatchStats state actx.Att.ClubSide (fun s ->
                    { s with
                        PassAttempts = s.PassAttempts + 1 })

                let passer = attRoster.Players[pIdx]
                let passerCond = int attFrame.Condition[pIdx]
                let passerProfile = attRoster.Profiles[pIdx]

                let passerSp =
                    { X = float attFrame.Physics.PosX[pIdx] * 1.0<meter>
                      Y = float attFrame.Physics.PosY[pIdx] * 1.0<meter>
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> }

                let condNorm = normaliseCondition passerCond

                let chemistry = getChemistry ctx actx.Att.ClubSide

                let familiarity =
                    if pIdx < chemistry.PlayerCount && targetIdx < chemistry.PlayerCount then
                        chemistry.Familiarity[pIdx, targetIdx]
                    else
                        0.5

                let passDist = float (passerSp.DistTo2D targetSp)

                let distPenalty =
                    if passDist <= 10.0 then
                        0.0
                    elif passDist <= 30.0 then
                        (passDist - 10.0) * pc.DistancePenaltyPerMeter
                    else
                        20.0 * pc.DistancePenaltyPerMeter
                        + (passDist - 30.0) * pc.LongPassPenaltyPerMeter

                let passMean =
                    pc.BaseMean
                    + toScalar (normaliseAttr passer.Technical.Passing) * pc.TechnicalWeight
                    + toScalar (normaliseAttr passer.Mental.Vision) * pc.VisionWeight
                    + actx.Att.Bonus.PassAcc
                    + (if actx.Zone = DefensiveZone then
                           passerProfile.CreativityWeight * pc.CreativityWeight
                           + (1.0 - passerProfile.Directness) * pc.DirectnessWeight
                       else
                           0.0)
                    - distPenalty
                    + (familiarity - 0.5) * 0.15

                let passMeanCapped = Math.Clamp(passMean, pc.MeanMin, pc.MeanMax)
                let familiarityBonus (familiarity: float) : float = familiarity * 0.15
                let chemBonus = familiarityBonus familiarity

                let quality =
                    betaSample
                        (passMeanCapped + chemBonus)
                        (pc.SuccessShapeAlpha + condNorm * pc.SuccessConditionMultiplier)

                let nearestDefDist =
                    match nearestActiveSlotInFrame defFrame passerSp.X passerSp.Y with
                    | ValueSome dIdx ->
                        let defX = float defFrame.Physics.PosX[dIdx] * 1.0<meter>
                        let defY = float defFrame.Physics.PosY[dIdx] * 1.0<meter>

                        passerSp.DistTo2D
                            { X = defX
                              Y = defY
                              Z = 0.0<meter>
                              Vx = 0.0<meter / second>
                              Vy = 0.0<meter / second>
                              Vz = 0.0<meter / second> }
                    | ValueNone -> pc.DefaultNearestDefDist

                let pressureFactor =
                    Math.Clamp(1.0 - float (nearestDefDist / pc.PressureDistance), 0.0, 1.0)

                let offside =
                    isOffsideFrame targetIdx attFrame attRoster defFrame state actx.Att.ClubSide

                if offside then
                    losePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.OffsideMomentum) state
                    [ createEvent subTick target.Id attClubId (PassIncomplete target.Id) ]
                else
                    let snapshot =
                        snapshotAtPassFrame pIdx targetIdx attFrame attRoster defFrame state actx.Att.AttackDir

                    let heavyTouchChance =
                        (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor)
                        * pc.HeavyTouchMultiplier

                    let accuracyNoise = pc.JitterStdDev * (1.0 - quality)

                    let leadX, leadY =
                        predictLeadPosition
                            pc.PassLeadFactor
                            passerSp.X
                            passerSp.Y
                            targetSp.X
                            targetSp.Y
                            targetSp.Vx
                            targetSp.Vy
                            pc.Speed

                    let actualTargetX, actualTargetY =
                        if bernoulli heavyTouchChance then
                            clamp (leadX + normalSample 0.0 accuracyNoise * 1.0<meter>) 0.0<meter> PitchLength,
                            clamp (leadY + normalSample 0.0 accuracyNoise * 1.0<meter>) 0.0<meter> PitchWidth
                        else
                            leadX, leadY

                    ballTowards passerSp.X passerSp.Y actualTargetX actualTargetY pc.Speed pc.Vz state

                    let intent = Aimed(passer.Id, target.Id, quality, RegularPass)

                    let traj =
                        makeTrajectory
                            subTick
                            clock
                            passer.Id
                            passerSp.X
                            passerSp.Y
                            actualTargetX
                            actualTargetY
                            pc.Speed
                            pc.Vz
                            intent

                    state.Ball <-
                        { state.Ball with
                            Spin =
                                { Top = 0.0<radianPerSecond>
                                  Side = 0.0<radianPerSecond> }
                            Control = Airborne
                            PendingOffsideSnapshot = Some snapshot
                            Trajectory = Some traj }

                    adjustMomentum actx.Att.AttackDir pc.SuccessMomentum state
                    recordSuccess actx.Att.ClubSide pIdx state.MatchMemory
                    [ createEvent subTick passer.Id attClubId (PassLaunched(passer.Id, target.Id)) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = getRoster ctx actx.Att.ClubSide

        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        match nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> []
        | ValueSome passerIdx ->
            let passer = attRoster.Players[passerIdx]
            let passerCond = int attFrame.Condition[passerIdx]

            let passerSp =
                { X = float attFrame.Physics.PosX[passerIdx] * 1.0<meter>
                  Y = float attFrame.Physics.PosY[passerIdx] * 1.0<meter>
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

            let condNorm = normaliseCondition passerCond

            let mutable bestFwdIdx = -1
            let mutable bestFwdX = 0.0f
            let mutable bestFwdY = 0.0f

            for i = 0 to attFrame.SlotCount - 1 do
                match attFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let profile = attRoster.Profiles[i]

                    if
                        profile.AttackingDepth > pc.ForwardDepthThreshold
                        || profile.CreativityWeight > pc.ForwardCreativityThreshold
                    then
                        if bestFwdIdx < 0 then
                            bestFwdIdx <- i
                            bestFwdX <- attFrame.Physics.PosX[i]
                            bestFwdY <- attFrame.Physics.PosY[i]
                | _ -> ()

            let longMean =
                pc.LongBallBaseMean
                + toScalar (normaliseAttr passer.Technical.LongShots) * pc.LongBallLongShotsWeight
                + toScalar (normaliseAttr passer.Technical.Passing) * pc.LongBallPassingWeight
                + toScalar (normaliseAttr passer.Mental.Vision) * pc.LongBallVisionWeight
                + actx.Att.Bonus.SetPlay
                - (if
                       bestFwdIdx >= 0
                       && bPos.DistTo2D
                           { X = float bestFwdX * 1.0<meter>
                             Y = float bestFwdY * 1.0<meter>
                             Z = 0.0<meter>
                             Vx = 0.0<meter / second>
                             Vy = 0.0<meter / second>
                             Vz = 0.0<meter / second> } > 30.0<meter>
                   then
                       0.2
                   else
                       0.0)

            let quality =
                betaSample longMean (pc.LongBallSuccessShapeAlpha + condNorm * pc.LongBallSuccessConditionMultiplier)

            if bestFwdIdx < 0 then
                losePossession state
                adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                [ createEvent subTick passer.Id attClubId (MatchEventType.LongBall false) ]
            else
                let target = attRoster.Players[bestFwdIdx]
                let targetCurrX = float bestFwdX * 1.0<meter>
                let targetCurrY = float bestFwdY * 1.0<meter>
                let targetVx = float attFrame.Physics.VelX[bestFwdIdx] * 1.0<meter / second>
                let targetVy = float attFrame.Physics.VelY[bestFwdIdx] * 1.0<meter / second>
                let targetIdx = bestFwdIdx

                let offside =
                    isOffsideFrame targetIdx attFrame attRoster defFrame state actx.Att.ClubSide

                if offside then
                    losePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallOffsideMomentum) state
                    [ createEvent subTick passer.Id attClubId (MatchEventType.LongBall false) ]
                else
                    let snapshot =
                        snapshotAtPassFrame passerIdx targetIdx attFrame attRoster defFrame state actx.Att.AttackDir

                    let accuracyNoise =
                        pc.JitterStdDev * pc.LongBallScrambleJitterMult * (1.0 - quality)

                    let heavyTouchChance =
                        (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor)
                        * pc.HeavyTouchMultiplier

                    let leadX, leadY =
                        predictLeadPosition
                            pc.PassLeadFactor
                            passerSp.X
                            passerSp.Y
                            targetCurrX
                            targetCurrY
                            targetVx
                            targetVy
                            pc.LongBallSpeed

                    let actualTargetX, actualTargetY =
                        if bernoulli heavyTouchChance then
                            clamp (leadX + normalSample 0.0 accuracyNoise * 1.0<meter>) 0.0<meter> PitchLength,
                            clamp (leadY + normalSample 0.0 accuracyNoise * 1.0<meter>) 0.0<meter> PitchWidth
                        else
                            leadX, leadY

                    ballTowards passerSp.X passerSp.Y actualTargetX actualTargetY pc.LongBallSpeed pc.LongBallVz state

                    let intent = Aimed(passer.Id, target.Id, quality, AimedKind.LongBall)

                    let traj =
                        makeTrajectory
                            subTick
                            clock
                            passer.Id
                            passerSp.X
                            passerSp.Y
                            actualTargetX
                            actualTargetY
                            pc.LongBallSpeed
                            pc.LongBallVz
                            intent

                    state.Ball <-
                        { state.Ball with
                            Control = Airborne
                            PendingOffsideSnapshot = Some snapshot
                            Trajectory = Some traj }

                    adjustMomentum actx.Att.AttackDir pc.LongBallSuccessMomentum state
                    [ createEvent subTick passer.Id attClubId (MatchEventType.LongBall true) ]

    /// Resolve a pass into space — ball goes to a grid cell, not a player.
    /// The best teammate converging on that cell becomes the receiver.
    let resolveIntoSpace
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (targetCell: int)
        : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = getRoster ctx actx.Att.ClubSide

        // Get the target cell center position
        let targetX, targetY = InfluenceTypes.cellToCenter targetCell

        let targetSpatial =
            { X = float targetX * 1.0<meter>
              Y = float targetY * 1.0<meter>
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }

        // Find the passer
        let passerIdx =
            match state.Ball.Control with
            | Controlled(_, ctrlId) ->
                match findIdxByPid ctrlId attFrame attRoster with
                | ValueSome i -> Some i
                | ValueNone -> None
            | _ ->
                match state.Ball.LastTouchBy with
                | Some pid ->
                    match findIdxByPid pid attFrame attRoster with
                    | ValueSome i -> Some i
                    | ValueNone -> None
                | None -> None

        match passerIdx with
        | None -> []
        | Some pIdx ->
            updateMatchStats state actx.Att.ClubSide (fun s ->
                { s with
                    PassAttempts = s.PassAttempts + 1 })

            let passer = attRoster.Players[pIdx]

            let passerSp =
                { X = float attFrame.Physics.PosX[pIdx] * 1.0<meter>
                  Y = float attFrame.Physics.PosY[pIdx] * 1.0<meter>
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

            // Find the best teammate converging on the target cell
            let mutable bestReceiverIdx = -1
            let mutable bestReceiverScore = -1.0

            for i = 0 to attFrame.SlotCount - 1 do
                if i <> pIdx then
                    match attFrame.Physics.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let px = float attFrame.Physics.PosX[i] * 1.0<meter>
                        let py = float attFrame.Physics.PosY[i] * 1.0<meter>
                        let vx = float attFrame.Physics.VelX[i] * 1.0<meter / second>
                        let vy = float attFrame.Physics.VelY[i] * 1.0<meter / second>

                        let dist =
                            sqrt (
                                (px - targetSpatial.X) * (px - targetSpatial.X)
                                + (py - targetSpatial.Y) * (py - targetSpatial.Y)
                            )

                        // Proximity score
                        let proximityScore = max 0.0 (1.0 - float (dist / 40.0<meter>))

                        // Velocity alignment: is player moving toward target?
                        let toTargetX = float (targetSpatial.X - px)
                        let toTargetY = float (targetSpatial.Y - py)
                        let toTargetDist = sqrt (toTargetX * toTargetX + toTargetY * toTargetY)

                        let velAlign =
                            if toTargetDist < 0.01 then
                                0.0
                            else
                                let dot = float vx * toTargetX / toTargetDist + float vy * toTargetY / toTargetDist
                                max 0.0 (dot / 8.0)

                        let convergence = proximityScore * 0.4 + velAlign * 0.6

                        // BallControl bonus for receiver quality
                        let receiver = attRoster.Players[i]
                        let ballControlBonus = float receiver.Technical.BallControl / 20.0 * 0.2

                        let totalScore = convergence + ballControlBonus

                        if totalScore > bestReceiverScore then
                            bestReceiverScore <- totalScore
                            bestReceiverIdx <- i
                    | _ -> ()

            // Fallback: if no teammate is converging, pass to nearest teammate
            if bestReceiverScore < 0.15 then
                // Fallback to regular pass to nearest teammate
                let mutable nearestIdx = -1
                let mutable nearestDist = Double.MaxValue

                for i = 0 to attFrame.SlotCount - 1 do
                    if i <> pIdx then
                        match attFrame.Physics.Occupancy[i] with
                        | OccupancyKind.Active _ ->
                            let dx = float attFrame.Physics.PosX[i] - float attFrame.Physics.PosX[pIdx]
                            let dy = float attFrame.Physics.PosY[i] - float attFrame.Physics.PosY[pIdx]
                            let d = dx * dx + dy * dy

                            if d < nearestDist then
                                nearestDist <- d
                                nearestIdx <- i
                        | _ -> ()

                if nearestIdx < 0 then
                    losePossession state
                    [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
                else
                    let target = attRoster.Players[nearestIdx]
                    resolve subTick ctx state clock target
            else
                // Execute space pass
                let receiver = attRoster.Players[bestReceiverIdx]
                let receiverCurrX = float attFrame.Physics.PosX[bestReceiverIdx] * 1.0<meter>
                let receiverCurrY = float attFrame.Physics.PosY[bestReceiverIdx] * 1.0<meter>

                // Offside check using receiver's current position
                let offside =
                    isOffsideFrame bestReceiverIdx attFrame attRoster defFrame state actx.Att.ClubSide

                if offside then
                    losePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.OffsideMomentum) state
                    [ createEvent subTick receiver.Id attClubId (PassIncomplete receiver.Id) ]
                else
                    let snapshot =
                        snapshotAtPassFrame pIdx bestReceiverIdx attFrame attRoster defFrame state actx.Att.AttackDir

                    // Ball trajectory targets the space cell, not the receiver
                    let actualTargetX, actualTargetY =
                        clamp
                            (targetSpatial.X + normalSample 0.0 (pc.JitterStdDev * 0.5) * 1.0<meter>)
                            0.0<meter>
                            PitchLength,
                        clamp
                            (targetSpatial.Y + normalSample 0.0 (pc.JitterStdDev * 0.5) * 1.0<meter>)
                            0.0<meter>
                            PitchWidth

                    ballTowards passerSp.X passerSp.Y actualTargetX actualTargetY pc.Speed pc.Vz state

                    let intent = Aimed(passer.Id, receiver.Id, 0.7, RegularPass)

                    let traj =
                        makeTrajectory
                            subTick
                            clock
                            passer.Id
                            passerSp.X
                            passerSp.Y
                            actualTargetX
                            actualTargetY
                            pc.Speed
                            pc.Vz
                            intent

                    state.Ball <-
                        { state.Ball with
                            Spin =
                                { Top = 0.0<radianPerSecond>
                                  Side = 0.0<radianPerSecond> }
                            Control = Airborne
                            PendingOffsideSnapshot = Some snapshot
                            Trajectory = Some traj }

                    // Create RunAssignment for the receiver to run toward the ball landing position
                    let runDuration = max 60 (traj.EstimatedArrivalSubTick - subTick + 20)

                    let assignment =
                        RunAssignment.create
                            RunType.DeepRun
                            receiverCurrX
                            receiverCurrY
                            actualTargetX
                            actualTargetY
                            receiver.Id
                            subTick
                            runDuration

                    // Update the assignment trigger to SpaceDetected
                    let spaceAssignment =
                        { assignment with
                            Trigger = SpaceDetected
                            Priority = 2 }

                    // Add the run to the team's active runs
                    let team = getTeam state actx.Att.ClubSide
                    team.ActiveRuns <- spaceAssignment :: team.ActiveRuns

                    adjustMomentum actx.Att.AttackDir pc.SuccessMomentum state
                    recordSuccess actx.Att.ClubSide pIdx state.MatchMemory
                    [ createEvent subTick passer.Id attClubId (PassLaunched(passer.Id, receiver.Id)) ]
