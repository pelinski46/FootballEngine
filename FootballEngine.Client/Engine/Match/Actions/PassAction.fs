namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract
open FootballEngine.MatchMemory

module PassAction =

    let private interceptChance
        (cfg: PassConfig)
        (defender: Player)
        (defX: float<meter>)
        (defY: float<meter>)
        (passerX: float<meter>)
        (passerY: float<meter>)
        (targetX: float<meter>)
        (targetY: float<meter>)
        (passerVisionNorm: float)
        =
        let passerPos = { X = passerX; Y = passerY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let targetPos = { X = targetX; Y = targetY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let lenSq = passerPos.DistSqTo2D targetPos

        if lenSq < 1.0<meterSquared> then 0.0
        else
            let dx = targetX - passerX
            let dy = targetY - passerY
            let tdx = defX - passerX
            let tdy = defY - passerY
            let dot = tdx * dx + tdy * dy
            let tNorm = dot / lenSq
            let t = PhysicsContract.clampFloat tNorm 0.0 1.0

            let intercept = { X = passerX + t * dx; Y = passerY + t * dy; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
            let perpDist = { X = defX; Y = defY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }.DistTo2D intercept

            if perpDist > cfg.InterceptionRadius then 0.0
            else
                let distFactor = 1.0 - (perpDist / cfg.InterceptionRadius)
                let positioningNorm = PhysicsContract.normaliseAttr defender.Mental.Positioning
                cfg.InterceptBaseRate + distFactor * cfg.InterceptDistFactorWeight + positioningNorm * cfg.InterceptPositioningContrib - passerVisionNorm * cfg.InterceptVisionContrib

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (target: Player) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

        let targetIdx =
            match SimStateOps.findIdxByPid target.Id attFrame attRoster with
            | ValueSome i -> i
            | ValueNone -> -1

        if targetIdx < 0 then []
        else
            let targetSp = { X = float attFrame.PosX[targetIdx] * 1.0<meter>; Y = float attFrame.PosY[targetIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

            let passerIdx =
                match state.Ball.Possession with
                | Owned(_, ctrlId) ->
                    match SimStateOps.findIdxByPid ctrlId attFrame attRoster with
                    | ValueSome i -> Some i
                    | ValueNone -> None
                | Loose | InFlight _ | SetPiece _ | Contest _ | Transition _ ->
                    match state.Ball.LastTouchBy with
                    | Some pid ->
                        match SimStateOps.findIdxByPid pid attFrame attRoster with
                        | ValueSome i -> Some i
                        | ValueNone -> None
                    | None -> None

            match passerIdx with
            | None -> []
            | Some pIdx ->
                SimStateOps.updateMatchStats state actx.Att.ClubSide (fun s -> { s with PassAttempts = s.PassAttempts + 1 })
                let passer = attRoster.Players[pIdx]
                let passerCond = int attFrame.Condition[pIdx]
                let passerProfile = attRoster.Profiles[pIdx]
                let passerSp = { X = float attFrame.PosX[pIdx] * 1.0<meter>; Y = float attFrame.PosY[pIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

                let condNorm = PhysicsContract.normaliseCondition passerCond
                let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

                let passDist = float (passerSp.DistTo2D targetSp)

                let distPenalty =
                    if passDist <= 10.0 then 0.0
                    elif passDist <= 30.0 then (passDist - 10.0) * pc.DistancePenaltyPerMeter
                    else 20.0 * pc.DistancePenaltyPerMeter + (passDist - 30.0) * pc.LongPassPenaltyPerMeter

                let passMean =
                    pc.BaseMean
                    + PhysicsContract.toScalar (PhysicsContract.normaliseAttr passer.Technical.Passing) * pc.TechnicalWeight
                    + PhysicsContract.toScalar (PhysicsContract.normaliseAttr passer.Mental.Vision) * pc.VisionWeight
                    + actx.Att.Bonus.PassAcc
                    + (if actx.Zone = DefensiveZone then
                           passerProfile.CreativityWeight * pc.CreativityWeight + (1.0 - passerProfile.Directness) * pc.DirectnessWeight
                       else 0.0)
                    - distPenalty

                let passMeanCapped = Math.Clamp(passMean, pc.MeanMin, pc.MeanMax)

                let successChance =
                    betaSample passMeanCapped (pc.SuccessShapeAlpha + condNorm * pc.SuccessConditionMultiplier)

                let nearestDefDist =
                    match SimStateOps.nearestActiveSlotInFrame defFrame passerSp.X passerSp.Y with
                    | ValueSome dIdx ->
                        let defX = float defFrame.PosX[dIdx] * 1.0<meter>
                        let defY = float defFrame.PosY[dIdx] * 1.0<meter>
                        passerSp.DistTo2D { X = defX; Y = defY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    | ValueNone -> pc.DefaultNearestDefDist

                let pressureFactor = Math.Clamp(1.0 - float (nearestDefDist / pc.PressureDistance), 0.0, 1.0)

                let adjustedSuccess = successChance - pressureFactor * pc.DeflectPressureMultiplier * 0.5

                let deflectRate =
                    pc.DeflectBaseRate + pressureFactor * pc.DeflectPressureMultiplier * pc.DefaultTackling

                let defClubId = actx.Def.ClubId

                if bernoulli adjustedSuccess then
                    let offside = isOffsideFrame targetIdx attFrame attRoster defFrame state actx.Att.ClubSide

                    if offside then
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.OffsideMomentum) state
                        [ createEvent subTick target.Id attClubId (PassIncomplete target.Id) ]
                    else
                        let snapshot = snapshotAtPassFrame pIdx targetIdx attFrame attRoster defFrame state actx.Att.AttackDir

                        let heavyTouchChance = (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor) * pc.HeavyTouchMultiplier

                        if bernoulli heavyTouchChance then
                            let jitterX = PhysicsContract.clamp (targetSp.X + normalSample 0.0 pc.JitterStdDev * 1.0<meter>) 0.0<meter> PhysicsContract.PitchLength
                            let jitterY = PhysicsContract.clamp (targetSp.Y + normalSample 0.0 pc.JitterStdDev * 1.0<meter>) 0.0<meter> PhysicsContract.PitchWidth
                            ballTowards passerSp.X passerSp.Y jitterX jitterY pc.Speed pc.Vz state
                        else
                            ballTowards passerSp.X passerSp.Y targetSp.X targetSp.Y pc.Speed pc.Vz state

                        adjustMomentum actx.Att.AttackDir pc.SuccessMomentum state

                        state.Ball <-
                            { state.Ball with
                                Spin = { Top = 0.0<radianPerSecond>; Side = 0.0<radianPerSecond> }
                                Possession = InFlight(actx.Att.ClubSide, passer.Id)
                                PendingOffsideSnapshot = Some snapshot }
                        MatchMemory.recordSuccess actx.Att.ClubSide pIdx state.MatchMemory
                        SimStateOps.updateMatchStats state actx.Att.ClubSide (fun s -> { s with PassSuccesses = s.PassSuccesses + 1 })
                        [ createEvent subTick passer.Id attClubId (PassCompleted(passer.Id, target.Id)) ]
                elif bernoulli deflectRate then
                    let deflectedById =
                        match SimStateOps.nearestActiveSlotInFrame defFrame targetSp.X targetSp.Y with
                        | ValueSome dIdx -> defRoster.Players[dIdx].Id
                        | ValueNone -> passer.Id

                    let jitterX = targetSp.X + normalSample 0.0 (float pc.ScrambleJitter) * 1.0<meter> |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength
                    let jitterY = targetSp.Y + normalSample 0.0 (float pc.ScrambleJitter) * 1.0<meter> |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                    ballTowards passerSp.X passerSp.Y jitterX jitterY (pc.Speed * pc.DeflectedSpeedMult) (pc.Vz * pc.DeflectedVzMult) state
                    state.Ball <- { state.Ball with Possession = Loose }
                    [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
                elif nearestDefDist < pc.PressureDistance then
                    let defIdx =
                        match SimStateOps.nearestActiveSlotInFrame defFrame passerSp.X passerSp.Y with
                        | ValueSome i -> Some i
                        | ValueNone -> None

                    match defIdx with
                    | Some dIdx ->
                        let def = defRoster.Players[dIdx]
                        let defX = float defFrame.PosX[dIdx] * 1.0<meter>
                        let defY = float defFrame.PosY[dIdx] * 1.0<meter>
                        let interceptProb = interceptChance pc def defX defY passerSp.X passerSp.Y targetSp.X targetSp.Y passerVisionNorm

                        if bernoulli (Math.Clamp(interceptProb, 0.0, pc.InterceptProbMax)) then
                            adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                            state.Ball <- { state.Ball with Position = { state.Ball.Position with Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }; LastTouchBy = Some def.Id }
                            givePossessionTo actx.Def.ClubSide def.Id state
                            MatchMemory.recordPassFailure actx.Att.ClubSide pIdx state.MatchMemory
                            [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id)); createEvent subTick def.Id defClubId TackleSuccess ]
                        else
                            match nearestActiveSlotInFrameExcluding attFrame pIdx targetSp.X targetSp.Y with
                            | ValueSome tmIdx ->
                                let actualTarget = attRoster.Players[tmIdx]
                                let actualSp = { X = float attFrame.PosX[tmIdx] * 1.0<meter>; Y = float attFrame.PosY[tmIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                                ballTowards passerSp.X passerSp.Y actualSp.X actualSp.Y (pc.Speed * pc.MisplacedSpeedMult) pc.Vz state
                                loosePossession state
                                adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                                MatchMemory.recordPassFailure actx.Att.ClubSide pIdx state.MatchMemory
                                [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                            | ValueNone ->
                                loosePossession state
                                adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                                MatchMemory.recordPassFailure actx.Att.ClubSide pIdx state.MatchMemory
                                [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
                    | None ->
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                        [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
                else
                    match nearestActiveSlotInFrameExcluding attFrame pIdx targetSp.X targetSp.Y with
                    | ValueSome tmIdx ->
                        let actualTarget = attRoster.Players[tmIdx]
                        let actualSp = { X = float attFrame.PosX[tmIdx] * 1.0<meter>; Y = float attFrame.PosY[tmIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                        ballTowards passerSp.X passerSp.Y actualSp.X actualSp.Y (pc.Speed * pc.MisplacedSpeedMult) pc.Vz state
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                        MatchMemory.recordPassFailure actx.Att.ClubSide pIdx state.MatchMemory
                        [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                    | ValueNone ->
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                        MatchMemory.recordPassFailure actx.Att.ClubSide pIdx state.MatchMemory
                        [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> []
        | ValueSome passerIdx ->
            let passer = attRoster.Players[passerIdx]
            let passerCond = int attFrame.Condition[passerIdx]
            let passerSp = { X = float attFrame.PosX[passerIdx] * 1.0<meter>; Y = float attFrame.PosY[passerIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

            let condNorm = PhysicsContract.normaliseCondition passerCond
            let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

            let mutable bestFwdIdx = -1
            let mutable bestFwdX = 0.0f
            let mutable bestFwdY = 0.0f

            for i = 0 to attFrame.SlotCount - 1 do
                match attFrame.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let profile = attRoster.Profiles[i]
                    if profile.AttackingDepth > pc.ForwardDepthThreshold || profile.CreativityWeight > pc.ForwardCreativityThreshold then
                        if bestFwdIdx < 0 then
                            bestFwdIdx <- i
                            bestFwdX <- attFrame.PosX[i]
                            bestFwdY <- attFrame.PosY[i]
                | _ -> ()

            let longMean =
                pc.LongBallBaseMean
                + PhysicsContract.toScalar (PhysicsContract.normaliseAttr passer.Technical.LongShots) * pc.LongBallLongShotsWeight
                + PhysicsContract.toScalar (PhysicsContract.normaliseAttr passer.Technical.Passing) * pc.LongBallPassingWeight
                + PhysicsContract.toScalar (PhysicsContract.normaliseAttr passer.Mental.Vision) * pc.LongBallVisionWeight
                + actx.Att.Bonus.SetPlay
                - (if bestFwdIdx >= 0 && bPos.DistTo2D { X = float bestFwdX * 1.0<meter>; Y = float bestFwdY * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> } > 30.0<meter> then 0.2 else 0.0)

            let successChance =
                betaSample longMean (pc.LongBallSuccessShapeAlpha + condNorm * pc.LongBallSuccessConditionMultiplier)

            let nearestDefDist =
                match SimStateOps.nearestActiveSlotInFrame defFrame bX bY with
                | ValueSome dIdx ->
                    let defX = float defFrame.PosX[dIdx] * 1.0<meter>
                    let defY = float defFrame.PosY[dIdx] * 1.0<meter>
                    bPos.DistTo2D { X = defX; Y = defY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | ValueNone -> pc.DefaultNearestDefDist

            let pressureFactor = Math.Clamp(1.0 - nearestDefDist / pc.PressureDistance, 0.0, 1.0)
            let adjustedSuccess = successChance - pressureFactor * pc.DeflectPressureMultiplier * pc.LongBallPressureContrib
            let deflectRate = pc.DeflectBaseRate * pc.LongBallDeflectMult + pressureFactor * pc.DeflectPressureMultiplier
            let interceptRate = pc.InterceptBaseRate * pc.LongBallInterceptMult

            if bernoulli adjustedSuccess && bestFwdIdx >= 0 then
                let target = attRoster.Players[bestFwdIdx]
                let targetSp = { X = float bestFwdX * 1.0<meter>; Y = float bestFwdY * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                let targetIdx = bestFwdIdx
                let offside = isOffsideFrame targetIdx attFrame attRoster defFrame state actx.Att.ClubSide

                if offside then
                    loosePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallOffsideMomentum) state
                    [ createEvent subTick passer.Id attClubId (LongBall false) ]
                else
                    let snapshot = snapshotAtPassFrame passerIdx targetIdx attFrame attRoster defFrame state actx.Att.AttackDir
                    let heavyTouchChance = (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor) * pc.HeavyTouchMultiplier

                    if bernoulli heavyTouchChance then
                        let jitterX = PhysicsContract.clamp (targetSp.X + normalSample 0.0 pc.JitterStdDev * 1.0<meter>) 0.0<meter> PhysicsContract.PitchLength
                        let jitterY = PhysicsContract.clamp (targetSp.Y + normalSample 0.0 pc.JitterStdDev * 1.0<meter>) 0.0<meter> PhysicsContract.PitchWidth
                        ballTowards passerSp.X passerSp.Y jitterX jitterY pc.LongBallSpeed pc.LongBallVz state
                    else
                        ballTowards passerSp.X passerSp.Y targetSp.X targetSp.Y pc.LongBallSpeed pc.LongBallVz state

                    state.Ball <- { state.Ball with Possession = InFlight(actx.Att.ClubSide, passer.Id); PendingOffsideSnapshot = Some snapshot }
                    adjustMomentum actx.Att.AttackDir pc.LongBallSuccessMomentum state
                    [ createEvent subTick passer.Id attClubId (LongBall true) ]
            elif bernoulli deflectRate then
                let deflectedById =
                    match SimStateOps.nearestActiveSlotInFrame defFrame bX bY with
                    | ValueSome dIdx -> defRoster.Players[dIdx].Id
                    | ValueNone -> passer.Id

                let jitterX = bX + normalSample 0.0 (float pc.ScrambleJitter * pc.LongBallScrambleJitterMult) * 1.0<meter> |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength
                let jitterY = bY + normalSample 0.0 (float pc.ScrambleJitter * pc.LongBallScrambleJitterMult) * 1.0<meter> |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                ballTowards passerSp.X passerSp.Y jitterX jitterY (pc.LongBallSpeed * pc.DeflectedSpeedMult) (pc.LongBallVz * pc.DeflectedVzMult) state
                state.Ball <- { state.Ball with Possession = InFlight(actx.Att.ClubSide, passer.Id) }
                [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
            elif bernoulli interceptRate then
                match SimStateOps.nearestActiveSlotInFrame defFrame bX bY with
                | ValueSome dIdx ->
                    let def = defRoster.Players[dIdx]
                    let defSp = { X = float defFrame.PosX[dIdx] * 1.0<meter>; Y = float defFrame.PosY[dIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    let defClubId = actx.Def.ClubId
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                    state.Ball <- { state.Ball with Position = { defSp with Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }; LastTouchBy = Some def.Id }
                    givePossessionTo actx.Def.ClubSide def.Id state
                    [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id)); createEvent subTick def.Id defClubId TackleSuccess ]
                | ValueNone ->
                    loosePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                    [ createEvent subTick passer.Id attClubId (LongBall false) ]
            else
                loosePossession state
                adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
