namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module PassAction =

    let private interceptChance
        (cfg: PassConfig)
        (defender: Player)
        (defPos: Spatial)
        (passerX: float<meter>)
        (passerY: float<meter>)
        (targetX: float<meter>)
        (targetY: float<meter>)
        (passerVisionNorm: float)
        =
        let passerPos =
            { X = passerX
              Y = passerY
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }

        let targetPos =
            { X = targetX
              Y = targetY
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }

        let lenSq = passerPos.DistSqTo2D targetPos

        if lenSq < 1.0<meterSquared> then
            0.0
        else
            let dx = targetX - passerX
            let dy = targetY - passerY
            let tdx = defPos.X - passerX
            let tdy = defPos.Y - passerY
            let dot = tdx * dx + tdy * dy
            let tNorm = dot / lenSq
            let t = PhysicsContract.clampFloat tNorm 0.0 1.0

            let intercept =
                { X = passerX + t * dx
                  Y = passerY + t * dy
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

            let perpDist = defPos.DistTo2D intercept

            if perpDist > cfg.InterceptionRadius then
                0.0
            else
                let distFactor = 1.0 - (perpDist / cfg.InterceptionRadius)
                let positioningNorm = PhysicsContract.normaliseAttr defender.Mental.Positioning

                cfg.InterceptBaseRate + distFactor * cfg.InterceptDistFactorWeight + positioningNorm * cfg.InterceptPositioningContrib
                - passerVisionNorm * cfg.InterceptVisionContrib

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (target: Player) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId
        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        let targetSlot = attSlots |> Array.tryPick (function | PlayerSlot.Active s when s.Player.Id = target.Id -> Some s | _ -> None)

        match targetSlot with
        | None -> []
        | Some targetSlot ->
            let targetSp = targetSlot.Pos
            
            let passerOpt =
                match state.Ball.Possession with
                | Owned(_, ctrlId) -> attSlots |> Array.tryPick (function | PlayerSlot.Active s when s.Player.Id = ctrlId -> Some s | _ -> None)
                | Loose | InFlight _ | SetPiece _ | Contest _ | Transition _ -> attSlots |> Array.tryPick (function | PlayerSlot.Active s when state.Ball.LastTouchBy = Some s.Player.Id -> Some s | _ -> None)

            match passerOpt with
            | None -> []
            | Some passerSlot ->
                let passer, passerCond, passerProfile = passerSlot.Player, passerSlot.Condition, passerSlot.Profile
                let passerSp = passerSlot.Pos
                
                let condNorm = PhysicsContract.normaliseCondition passerCond
                let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

                let passMean =
                    pc.BaseMean
                    + PhysicsContract.normaliseAttr passer.Technical.Passing
                      * pc.TechnicalWeight
                    + PhysicsContract.normaliseAttr passer.Mental.Vision
                      * pc.VisionWeight
                    + actx.Att.Bonus.PassAcc
                    + (if actx.Zone = DefensiveZone then
                           passerProfile.CreativityWeight * pc.CreativityWeight + (1.0 - passerProfile.Directness) * pc.DirectnessWeight
                       else
                           0.0)

                let passMeanCapped = Math.Clamp(passMean, pc.MeanMin, pc.MeanMax)

                let successChance =
                    betaSample
                        passMeanCapped
                        (pc.SuccessShapeAlpha
                         + condNorm * pc.SuccessConditionMultiplier)

                let nearestDefDist, nearestDef =
                    match nearestActiveSlot defSlots passerSp.X passerSp.Y with
                    | ValueSome defSlot -> passerSp.DistTo2D defSlot.Pos, Some(defSlot.Player, defSlot.Pos)
                    | ValueNone -> pc.DefaultNearestDefDist, None

                let pressureFactor =
                    Math.Clamp(1.0 - float (nearestDefDist / pc.PressureDistance), 0.0, 1.0)

                let adjustedSuccess =
                    successChance
                    - pressureFactor * pc.DeflectPressureMultiplier * 0.5

                let deflectRate =
                    pc.DeflectBaseRate
                    + pressureFactor
                      * pc.DeflectPressureMultiplier
                      * (nearestDef
                         |> Option.map (fun (d, _) -> PhysicsContract.normaliseAttr d.Technical.Tackling)
                         |> Option.defaultValue pc.DefaultTackling)

                let defClubId = actx.Def.ClubId

                if bernoulli adjustedSuccess then
                    let offside = isOffside target targetSp.X state actx.Att.ClubSide

                    if offside then
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.OffsideMomentum) state
                        [ createEvent subTick target.Id attClubId (PassIncomplete target.Id) ]
                    else
                        let snapshot = snapshotAtPass passer target state actx.Att.AttackDir

                        let heavyTouchChance = (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor) * pc.HeavyTouchMultiplier

                        if bernoulli heavyTouchChance then
                            let jitterX =
                                PhysicsContract.clamp
                                    (targetSp.X + normalSample 0.0 pc.JitterStdDev * 1.0<meter>)
                                    0.0<meter>
                                    PhysicsContract.PitchLength

                            let jitterY =
                                PhysicsContract.clamp
                                    (targetSp.Y + normalSample 0.0 pc.JitterStdDev * 1.0<meter>)
                                    0.0<meter>
                                    PhysicsContract.PitchWidth

                            ballTowards
                                passerSp.X
                                passerSp.Y
                                jitterX
                                jitterY
                                pc.Speed
                                pc.Vz
                                state
                        else
                            ballTowards
                                passerSp.X
                                passerSp.Y
                                targetSp.X
                                targetSp.Y
                                pc.Speed
                                pc.Vz
                                state

                        adjustMomentum actx.Att.AttackDir pc.SuccessMomentum state

                        state.Ball <-
                            { state.Ball with
                                Spin =
                                    { Top = 0.0<radianPerSecond>
                                      Side = 0.0<radianPerSecond> }
                                Possession = InFlight(actx.Att.ClubSide, passer.Id)
                                PendingOffsideSnapshot = Some snapshot }
                        [ createEvent subTick passer.Id attClubId (PassCompleted(passer.Id, target.Id)) ]
                elif bernoulli deflectRate then
                    let deflectedById =
                        match nearestDef with
                        | Some(d, _) -> d.Id
                        | None ->
                            match nearestActiveSlot defSlots targetSp.X targetSp.Y with
                            | ValueSome defSlot -> defSlot.Player.Id
                            | ValueNone -> passer.Id

                    let jitterX =
                        targetSp.X
                        + normalSample 0.0 (float pc.ScrambleJitter) * 1.0<meter>
                        |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength

                    let jitterY =
                        targetSp.Y
                        + normalSample 0.0 (float pc.ScrambleJitter) * 1.0<meter>
                        |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                    ballTowards
                        passerSp.X
                        passerSp.Y
                        jitterX
                        jitterY
                        (pc.Speed * pc.DeflectedSpeedMult)
                        (pc.Vz * pc.DeflectedVzMult)
                        state

                    state.Ball <- { state.Ball with Possession = Loose }

                    [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
                elif nearestDef.IsSome then
                    let def, defPos = nearestDef.Value

                    let interceptProb =
                        interceptChance pc def defPos passerSp.X passerSp.Y targetSp.X targetSp.Y passerVisionNorm

                    if bernoulli (Math.Clamp(interceptProb, 0.0, pc.InterceptProbMax)) then
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state

                        state.Ball <-
                            { state.Ball with
                                Position =
                                    { state.Ball.Position with
                                        Vx = 0.0<meter / second>
                                        Vy = 0.0<meter / second>
                                        Vz = 0.0<meter / second> }
                                LastTouchBy = Some def.Id }

                        givePossessionTo actx.Def.ClubSide def.Id state

                        [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                          createEvent subTick def.Id defClubId TackleSuccess ]
                    else
                        match nearestActiveSlotExcluding attSlots passer.Id targetSp.X targetSp.Y with
                        | ValueSome teammateSlot ->
                            let actualTarget = teammateSlot.Player
                            let actualSp = teammateSlot.Pos
                            ballTowards
                                passerSp.X
                                passerSp.Y
                                actualSp.X
                                actualSp.Y
                                (pc.Speed * pc.MisplacedSpeedMult)
                                pc.Vz
                                state

                            loosePossession state
                            adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state

                            [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                        | ValueNone ->
                            loosePossession state
                            adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                            [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
                else
                    match nearestActiveSlotExcluding attSlots passer.Id targetSp.X targetSp.Y with
                    | ValueSome teammateSlot ->
                        let actualTarget = teammateSlot.Player
                        let actualSp = teammateSlot.Pos
                        ballTowards
                            passerSp.X
                            passerSp.Y
                            actualSp.X
                            actualSp.Y
                            (pc.Speed * pc.MisplacedSpeedMult)
                            pc.Vz
                            state

                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state

                        [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                    | ValueNone ->
                        loosePossession state
                        adjustMomentum actx.Att.AttackDir (-pc.FailMomentum) state
                        [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let pc = ctx.Config.Pass
        let attClubId = actx.Att.ClubId

        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        let passerSlotOpt = MatchSpatial.nearestActiveSlot attSlots bX bY

        match passerSlotOpt with
        | ValueNone -> []
        | ValueSome passerSlot ->
            let passer, passerCond, passerSp = passerSlot.Player, passerSlot.Condition, passerSlot.Pos
            
            let condNorm = PhysicsContract.normaliseCondition passerCond
            let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

            let longMean =
                pc.LongBallBaseMean
                + PhysicsContract.normaliseAttr passer.Technical.LongShots
                  * pc.LongBallLongShotsWeight
                + PhysicsContract.normaliseAttr passer.Technical.Passing
                  * pc.LongBallPassingWeight
                + PhysicsContract.normaliseAttr passer.Mental.Vision
                  * pc.LongBallVisionWeight
                + actx.Att.Bonus.SetPlay

            let successChance =
                betaSample
                    longMean
                    (pc.LongBallSuccessShapeAlpha
                     + condNorm * pc.LongBallSuccessConditionMultiplier)

            let forwards =
                attSlots
                |> Array.map (function
                    | PlayerSlot.Active s -> Some(s.Player, s.Pos, s.Profile)
                    | _ -> None)
                |> Array.choose id
                |> Array.filter (fun (_, _, profile) -> profile.AttackingDepth > pc.ForwardDepthThreshold || profile.CreativityWeight > pc.ForwardCreativityThreshold)
                |> Array.map (fun (p, pos, _) -> p, pos)

            let nearestDefDist =
                match nearestActiveSlot defSlots bX bY with
                | ValueSome defSlot -> bPos.DistTo2D defSlot.Pos
                | ValueNone -> pc.DefaultNearestDefDist

            let pressureFactor =
                Math.Clamp(1.0 - nearestDefDist / pc.PressureDistance, 0.0, 1.0)

            let adjustedSuccess =
                successChance
                - pressureFactor * pc.DeflectPressureMultiplier * pc.LongBallPressureContrib

            let deflectRate =
                pc.DeflectBaseRate * pc.LongBallDeflectMult
                + pressureFactor * pc.DeflectPressureMultiplier

            let interceptRate = pc.InterceptBaseRate * pc.LongBallInterceptMult

            if bernoulli adjustedSuccess && forwards.Length > 0 then
                let target, targetSp = forwards[0]
                let offside = isOffside target targetSp.X state actx.Att.ClubSide

                if offside then
                    loosePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallOffsideMomentum) state
                    [ createEvent subTick passer.Id attClubId (LongBall false) ]
                else
                    let snapshot = snapshotAtPass passer target state actx.Att.AttackDir

                    let heavyTouchChance = (1.0 - float target.Technical.BallControl / pc.HeavyTouchDivisor) * pc.HeavyTouchMultiplier

                    if bernoulli heavyTouchChance then
                        let jitterX =
                            PhysicsContract.clamp
                                (targetSp.X + normalSample 0.0 pc.JitterStdDev * 1.0<meter>)
                                0.0<meter>
                                PhysicsContract.PitchLength

                        let jitterY =
                            PhysicsContract.clamp
                                (targetSp.Y + normalSample 0.0 pc.JitterStdDev * 1.0<meter>)
                                0.0<meter>
                                PhysicsContract.PitchWidth

                        ballTowards
                            passerSp.X
                            passerSp.Y
                            jitterX
                            jitterY
                            pc.LongBallSpeed
                            pc.LongBallVz
                            state
                    else
                        ballTowards
                            passerSp.X
                            passerSp.Y
                            targetSp.X
                            targetSp.Y
                            pc.LongBallSpeed
                            pc.LongBallVz
                            state

                    state.Ball <-
                        { state.Ball with
                            Possession = InFlight(actx.Att.ClubSide, passer.Id)
                            PendingOffsideSnapshot = Some snapshot }

                    adjustMomentum actx.Att.AttackDir pc.LongBallSuccessMomentum state

                    [ createEvent subTick passer.Id attClubId (LongBall true) ]
            elif bernoulli deflectRate then
                let deflectedById =
                    match nearestActiveSlot defSlots bX bY with
                    | ValueSome defSlot -> defSlot.Player.Id
                    | ValueNone -> passer.Id

                let jitterX =
                    bX
                    + normalSample 0.0 (float pc.ScrambleJitter * pc.LongBallScrambleJitterMult) * 1.0<meter>
                    |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength

                let jitterY =
                    bY
                    + normalSample 0.0 (float pc.ScrambleJitter * pc.LongBallScrambleJitterMult) * 1.0<meter>
                    |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                ballTowards
                    passerSp.X
                    passerSp.Y
                    jitterX
                    jitterY
                    (pc.LongBallSpeed * pc.DeflectedSpeedMult)
                    (pc.LongBallVz * pc.DeflectedVzMult)
                    state

                state.Ball <-
                    { state.Ball with
                        Possession = InFlight(actx.Att.ClubSide, passer.Id) }

                [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
            elif bernoulli interceptRate then
                match nearestActiveSlot defSlots bX bY with
                | ValueSome defSlot ->
                    let def = defSlot.Player
                    let defSp = defSlot.Pos
                    let defClubId = actx.Def.ClubId
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { defSp with
                                    Vx = 0.0<meter / second>
                                    Vy = 0.0<meter / second>
                                    Vz = 0.0<meter / second> }
                            LastTouchBy = Some def.Id }

                    givePossessionTo actx.Def.ClubSide def.Id state

                    [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                      createEvent subTick def.Id defClubId TackleSuccess ]
                | ValueNone ->
                    loosePossession state
                    adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                    [ createEvent subTick passer.Id attClubId (LongBall false) ]
            else
                loosePossession state
                adjustMomentum actx.Att.AttackDir (-pc.LongBallFailMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
