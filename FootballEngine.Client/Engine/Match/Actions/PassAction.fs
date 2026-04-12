namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial

module PassAction =

    let private interceptChance
        (defender: Player)
        (defPos: Spatial)
        (passerX: float)
        (passerY: float)
        (targetX: float)
        (targetY: float)
        (passerVisionNorm: float)
        =
        let dx = targetX - passerX
        let dy = targetY - passerY
        let lenSq = dx * dx + dy * dy

        if lenSq < 1.0 then
            0.0
        else
            let tdx = defPos.X - passerX
            let tdy = defPos.Y - passerY
            let t = System.Math.Clamp((tdx * dx + tdy * dy) / lenSq, 0.0, 1.0)

            let interceptX = passerX + t * dx
            let interceptY = passerY + t * dy

            let perpDist =
                sqrt ((defPos.X - interceptX) ** 2.0 + (defPos.Y - interceptY) ** 2.0)

            if perpDist > BalanceConfig.PassInterceptionRadius then
                0.0
            else
                let distFactor = 1.0 - perpDist / BalanceConfig.PassInterceptionRadius
                let positioningNorm = PhysicsContract.normaliseAttr defender.Mental.Positioning

                BalanceConfig.PassInterceptBaseRate + distFactor * 0.3 + positioningNorm * 0.15
                - passerVisionNorm * 0.10

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (target: Player) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let defClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let mutable targetIdx = -1
        let mutable targetSp = kickOffSpatial

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = target.Id ->
                targetIdx <- i
                targetSp <- s.Pos
            | _ -> ()

        if targetIdx < 0 then
            []
        else
            let mutable passerIdx = -1
            let mutable passerFound = false

            match state.Ball.ControlledBy with
            | Some ctrlId ->
                for i = 0 to attSlots.Length - 1 do
                    match attSlots[i] with
                    | PlayerSlot.Active s when s.Player.Id = ctrlId ->
                        passerIdx <- i
                        passerFound <- true
                    | _ -> ()
            | None ->
                for i = 0 to attSlots.Length - 1 do
                    match attSlots[i] with
                    | PlayerSlot.Active s when state.Ball.LastTouchBy = Some s.Player.Id ->
                        passerIdx <- i
                        passerFound <- true
                    | _ -> ()

            let passer, passerCond, passerProfile =
                if passerFound && passerIdx >= 0 then
                    match attSlots[passerIdx] with
                    | PlayerSlot.Active s -> s.Player, s.Condition, s.Profile
                    | _ -> Unchecked.defaultof<Player>, 70, BehavioralProfile.neutral
                else
                    match attSlots[0] with
                    | PlayerSlot.Active s -> s.Player, s.Condition, s.Profile
                    | _ -> Unchecked.defaultof<Player>, 70, BehavioralProfile.neutral

            let passerSp =
                if passerFound && passerIdx >= 0 then
                    match attSlots[passerIdx] with
                    | PlayerSlot.Active s -> s.Pos
                    | _ -> kickOffSpatial
                else
                    match attSlots[0] with
                    | PlayerSlot.Active s -> s.Pos
                    | _ -> kickOffSpatial

            let condNorm = PhysicsContract.normaliseCondition passerCond
            let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

            let passMean =
                BalanceConfig.PassBaseMean
                + PhysicsContract.normaliseAttr passer.Technical.Passing
                  * BalanceConfig.PassTechnicalWeight
                + PhysicsContract.normaliseAttr passer.Mental.Vision
                  * BalanceConfig.PassVisionWeight
                + actx.AttBonus.PassAcc
                + (if actx.Zone = DefensiveZone then
                       passerProfile.CreativityWeight * 0.06
                       + (1.0 - passerProfile.Directness) * 0.04
                   else
                       0.0)

            let passMeanCapped = Math.Clamp(passMean, 0.01, 0.99)

            let successChance =
                betaSample
                    passMeanCapped
                    (BalanceConfig.PassSuccessShapeAlpha
                     + condNorm * BalanceConfig.PassSuccessConditionMultiplier)

            let nearestDefDist, nearestDef =
                match findNearestOpponentToPos passerSp.X passerSp.Y ctx state with
                | Some(d, dSp) ->
                    let dx = dSp.X - passerSp.X
                    let dy = dSp.Y - passerSp.Y
                    sqrt (dx * dx + dy * dy), Some(d, dSp)
                | None -> 10.0, None

            let pressureFactor =
                System.Math.Clamp(1.0 - nearestDefDist / BalanceConfig.PassPressureDistance, 0.0, 1.0)

            let adjustedSuccess =
                successChance
                - pressureFactor * BalanceConfig.PassDeflectPressureMultiplier * 0.5

            let deflectRate =
                BalanceConfig.PassDeflectBaseRate
                + pressureFactor
                  * BalanceConfig.PassDeflectPressureMultiplier
                  * (nearestDef
                     |> Option.map (fun (d, _) -> PhysicsContract.normaliseAttr d.Technical.Tackling)
                     |> Option.defaultValue 0.5)

            let misplacedRate = BalanceConfig.PassMisplacedBaseRate

            if bernoulli adjustedSuccess then
                let offside = isOffside target targetSp.X ctx state actx.Dir

                if offside then
                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassOffsideMomentum) state
                    [ createEvent subTick target.Id attClubId (PassIncomplete target.Id) ]
                else
                    let snapshot = snapshotAtPass passer target ctx state actx.Dir

                    let heavyTouchChance = (1.0 - float target.Technical.BallControl / 20.0) * 0.25

                    if bernoulli heavyTouchChance then
                        let jitterX = Math.Clamp(targetSp.X + normalSample 0.0 2.0, 0.0, PhysicsContract.PitchLength)
                        let jitterY = Math.Clamp(targetSp.Y + normalSample 0.0 2.0, 0.0, PhysicsContract.PitchWidth)

                        MatchSpatial.ballTowards
                            passerSp.X
                            passerSp.Y
                            jitterX
                            jitterY
                            BalanceConfig.PassSpeed
                            BalanceConfig.PassVz
                            state
                    else
                        MatchSpatial.ballTowards
                            passerSp.X
                            passerSp.Y
                            targetSp.X
                            targetSp.Y
                            BalanceConfig.PassSpeed
                            BalanceConfig.PassVz
                            state

                    adjustMomentum actx.Dir BalanceConfig.PassSuccessMomentum state

                    state.Ball <-
                        { state.Ball with
                            Spin = { Top = 0.0; Side = 0.0 }
                            ControlledBy = None
                            Phase = PossessionPhase.InFlight actx.AttSide
                            PendingOffsideSnapshot = Some snapshot }

                    [ createEvent subTick passer.Id attClubId (PassCompleted(passer.Id, target.Id)) ]
            elif bernoulli deflectRate then
                let deflectedById =
                    match nearestDef with
                    | Some(d, _) -> d.Id
                    | None ->
                        match findNearestOpponentToPos targetSp.X targetSp.Y ctx state with
                        | Some(d, _) -> d.Id
                        | None -> passer.Id

                let jitterX =
                    targetSp.X + normalSample 0.0 BalanceConfig.PassScrambleJitter
                    |> fun x -> Math.Clamp(x, 0.0, PhysicsContract.PitchLength)

                let jitterY =
                    targetSp.Y + normalSample 0.0 BalanceConfig.PassScrambleJitter
                    |> fun y -> Math.Clamp(y, 0.0, PhysicsContract.PitchWidth)

                MatchSpatial.ballTowards
                    passerSp.X
                    passerSp.Y
                    jitterX
                    jitterY
                    (BalanceConfig.PassSpeed * 0.6)
                    (BalanceConfig.PassVz * 0.5)
                    state

                state.Ball <-
                    { state.Ball with
                        ControlledBy = None
                        Phase = PossessionPhase.Contest actx.AttSide }

                [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
            elif nearestDef.IsSome then
                let def, defPos = nearestDef.Value

                let interceptProb =
                    interceptChance def defPos passerSp.X passerSp.Y targetSp.X targetSp.Y passerVisionNorm

                if bernoulli (System.Math.Clamp(interceptProb, 0.0, 0.8)) then
                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    Vx = 0.0
                                    Vy = 0.0
                                    Vz = 0.0 }
                            ControlledBy = Some def.Id
                            LastTouchBy = Some def.Id
                            Phase = PossessionPhase.Transition actx.DefSide }

                    [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                      createEvent subTick def.Id defClubId TackleSuccess ]
                else
                    match findNearestTeammateToPos passer.Id targetSp.X targetSp.Y state actx.AttSide with
                    | Some(actualTarget, actualSp) ->
                        MatchSpatial.ballTowards
                            passerSp.X
                            passerSp.Y
                            actualSp.X
                            actualSp.Y
                            (BalanceConfig.PassSpeed * 0.7)
                            BalanceConfig.PassVz
                            state

                        flipPossession state
                        adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                        [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                    | None ->
                        flipPossession state
                        adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state
                        [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
            else
                match findNearestTeammateToPos passer.Id targetSp.X targetSp.Y state actx.AttSide with
                | Some(actualTarget, actualSp) ->
                    MatchSpatial.ballTowards
                        passerSp.X
                        passerSp.Y
                        actualSp.X
                        actualSp.Y
                        (BalanceConfig.PassSpeed * 0.7)
                        BalanceConfig.PassVz
                        state

                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                    [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                | None ->
                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state
                    [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let mutable passerIdx = 0
        let mutable passerDistSq = System.Double.MaxValue

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - bX
                let dy = s.Pos.Y - bY
                let dSq = dx * dx + dy * dy

                if dSq < passerDistSq then
                    passerDistSq <- dSq
                    passerIdx <- i
            | _ -> ()

        let passer, passerCond, passerSp =
            match attSlots[passerIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition, s.Pos
            | _ -> Unchecked.defaultof<Player>, 0, kickOffSpatial

        let condNorm = PhysicsContract.normaliseCondition passerCond
        let passerVisionNorm = PhysicsContract.normaliseAttr passer.Mental.Vision

        let longMean =
            BalanceConfig.LongBallBaseMean
            + PhysicsContract.normaliseAttr passer.Technical.LongShots
              * BalanceConfig.LongBallLongShotsWeight
            + PhysicsContract.normaliseAttr passer.Technical.Passing
              * BalanceConfig.LongBallPassingWeight
            + PhysicsContract.normaliseAttr passer.Mental.Vision
              * BalanceConfig.LongBallVisionWeight
            + actx.AttBonus.SetPlay

        let successChance =
            betaSample
                longMean
                (BalanceConfig.LongBallSuccessShapeAlpha
                 + condNorm * BalanceConfig.LongBallSuccessConditionMultiplier)

        let forwards =
            attSlots
            |> Array.mapi (fun i slot ->
                match slot with
                | PlayerSlot.Active s -> Some(s.Player, s.Pos, s.Profile)
                | _ -> None)
            |> Array.choose id
            |> Array.filter (fun (p, _, profile) ->
                profile.AttackingDepth > 0.5 || profile.CreativityWeight > 0.4)
            |> Array.map (fun (p, pos, _) -> p, pos)

        let nearestDefDist =
            match findNearestOpponentToPos bX bY ctx state with
            | Some(_, dSp) ->
                let dx = dSp.X - bX
                let dy = dSp.Y - bY
                sqrt (dx * dx + dy * dy)
            | None -> 10.0

        let pressureFactor =
            System.Math.Clamp(1.0 - nearestDefDist / BalanceConfig.PassPressureDistance, 0.0, 1.0)

        let adjustedSuccess =
            successChance
            - pressureFactor * BalanceConfig.PassDeflectPressureMultiplier * 0.3

        let deflectRate =
            BalanceConfig.PassDeflectBaseRate * 1.5
            + pressureFactor * BalanceConfig.PassDeflectPressureMultiplier

        let interceptRate = BalanceConfig.PassInterceptBaseRate * 1.5

        if bernoulli adjustedSuccess && forwards.Length > 0 then
            let target, targetSp = forwards[0]
            let offside = isOffside target targetSp.X ctx state actx.Dir

            if offside then
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallOffsideMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
            else
                let snapshot = snapshotAtPass passer target ctx state actx.Dir

                let heavyTouchChance = (1.0 - float target.Technical.BallControl / 20.0) * 0.25

                if bernoulli heavyTouchChance then
                    let jitterX = Math.Clamp(targetSp.X + normalSample 0.0 2.0, 0.0, PhysicsContract.PitchLength)
                    let jitterY = Math.Clamp(targetSp.Y + normalSample 0.0 2.0, 0.0, PhysicsContract.PitchWidth)

                    MatchSpatial.ballTowards
                        passerSp.X
                        passerSp.Y
                        jitterX
                        jitterY
                        BalanceConfig.LongBallSpeed
                        BalanceConfig.LongBallVz
                        state
                else
                    MatchSpatial.ballTowards
                        passerSp.X
                        passerSp.Y
                        targetSp.X
                        targetSp.Y
                        BalanceConfig.LongBallSpeed
                        BalanceConfig.LongBallVz
                        state

                state.Ball <-
                    { state.Ball with
                        ControlledBy = None
                        Phase = PossessionPhase.InFlight actx.AttSide
                        PendingOffsideSnapshot = Some snapshot }

                adjustMomentum actx.Dir BalanceConfig.LongBallSuccessMomentum state

                [ createEvent subTick passer.Id attClubId (LongBall true) ]
        elif bernoulli deflectRate then
            let deflectedById =
                match findNearestOpponentToPos bX bY ctx state with
                | Some(d, _) -> d.Id
                | None -> passer.Id

            let jitterX =
                bX + normalSample 0.0 (BalanceConfig.PassScrambleJitter * 2.0)
                |> fun x -> Math.Clamp(x, 0.0, PhysicsContract.PitchLength)

            let jitterY =
                bY + normalSample 0.0 (BalanceConfig.PassScrambleJitter * 2.0)
                |> fun y -> Math.Clamp(y, 0.0, PhysicsContract.PitchWidth)

            MatchSpatial.ballTowards
                passerSp.X
                passerSp.Y
                jitterX
                jitterY
                (BalanceConfig.LongBallSpeed * 0.6)
                (BalanceConfig.LongBallVz * 0.5)
                state

            state.Ball <-
                { state.Ball with
                    ControlledBy = None
                    Phase = PossessionPhase.Contest actx.AttSide }

            [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
        elif bernoulli interceptRate then
            match findNearestOpponentToPos bX bY ctx state with
            | Some(def, defSp) ->
                let defClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state

                state.Ball <-
                    { state.Ball with
                        Position =
                            { defSp with
                                Vx = 0.0
                                Vy = 0.0
                                Vz = 0.0 }
                        ControlledBy = Some def.Id
                        LastTouchBy = Some def.Id
                        Phase = PossessionPhase.Transition actx.DefSide }

                [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                  createEvent subTick def.Id defClubId TackleSuccess ]
            | None ->
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
        else
            flipPossession state
            adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state
            [ createEvent subTick passer.Id attClubId (LongBall false) ]
