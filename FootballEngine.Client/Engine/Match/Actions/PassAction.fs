namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module PassAction =

    let private interceptChance
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

            if perpDist > BalanceConfig.PassInterceptionRadius then
                0.0
            else
                let distFactor = 1.0 - (perpDist / BalanceConfig.PassInterceptionRadius)
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

            match state.Ball.Possession with
            | Owned(_, ctrlId) ->
                for i = 0 to attSlots.Length - 1 do
                    match attSlots[i] with
                    | PlayerSlot.Active s when s.Player.Id = ctrlId ->
                        passerIdx <- i
                        passerFound <- true
                    | _ -> ()
            | Loose
            | InFlight _
            | SetPiece _ ->
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
                       passerProfile.CreativityWeight * 0.06 + (1.0 - passerProfile.Directness) * 0.04
                   else
                       0.0)

            let passMeanCapped = Math.Clamp(passMean, 0.01, 0.99)

            let successChance =
                betaSample
                    passMeanCapped
                    (BalanceConfig.PassSuccessShapeAlpha
                     + condNorm * BalanceConfig.PassSuccessConditionMultiplier)

            let nearestDefDist, nearestDef =
                match findNearestOpponentToPos passerSp.X passerSp.Y state with
                | Some(d, dSp) -> passerSp.DistTo2D dSp, Some(d, dSp)
                | None -> 10.0<meter>, None

            let pressureFactor =
                Math.Clamp(1.0 - float (nearestDefDist / BalanceConfig.PassPressureDistance), 0.0, 1.0)

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
                let offside = isOffside target targetSp.X state actx.Dir

                if offside then
                    loosePossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassOffsideMomentum) state
                    [ createEvent subTick target.Id attClubId (PassIncomplete target.Id) ]
                else
                    let snapshot = snapshotAtPass passer target state actx.Dir

                    let heavyTouchChance = (1.0 - float target.Technical.BallControl / 20.0) * 0.25

                    if bernoulli heavyTouchChance then
                        let jitterX =
                            PhysicsContract.clamp
                                (targetSp.X + normalSample 0.0 2.0 * 1.0<meter>)
                                0.0<meter>
                                PhysicsContract.PitchLength

                        let jitterY =
                            PhysicsContract.clamp
                                (targetSp.Y + normalSample 0.0 2.0 * 1.0<meter>)
                                0.0<meter>
                                PhysicsContract.PitchWidth

                        ballTowards
                            passerSp.X
                            passerSp.Y
                            jitterX
                            jitterY
                            BalanceConfig.PassSpeed
                            BalanceConfig.PassVz
                            state
                    else
                        ballTowards
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
                            Spin =
                                { Top = 0.0<radianPerSecond>
                                  Side = 0.0<radianPerSecond> }
                            Possession = InFlight(actx.AttSide, target.Id)
                            PendingOffsideSnapshot = Some snapshot }

                    [ createEvent subTick passer.Id attClubId (PassCompleted(passer.Id, target.Id)) ]
            elif bernoulli deflectRate then
                let deflectedById =
                    match nearestDef with
                    | Some(d, _) -> d.Id
                    | None ->
                        match findNearestOpponentToPos targetSp.X targetSp.Y state with
                        | Some(d, _) -> d.Id
                        | None -> passer.Id

                let jitterX =
                    targetSp.X
                    + normalSample 0.0 (float BalanceConfig.PassScrambleJitter) * 1.0<meter>
                    |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength

                let jitterY =
                    targetSp.Y
                    + normalSample 0.0 (float BalanceConfig.PassScrambleJitter) * 1.0<meter>
                    |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                ballTowards
                    passerSp.X
                    passerSp.Y
                    jitterX
                    jitterY
                    (BalanceConfig.PassSpeed * 0.6)
                    (BalanceConfig.PassVz * 0.5)
                    state

                state.Ball <- { state.Ball with Possession = Loose }

                ballTowards
                    passerSp.X
                    passerSp.Y
                    jitterX
                    jitterY
                    (BalanceConfig.PassSpeed * 0.6)
                    (BalanceConfig.PassVz * 0.5)
                    state

                state.Ball <- { state.Ball with Possession = Loose }

                [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
            elif nearestDef.IsSome then
                let def, defPos = nearestDef.Value

                let interceptProb =
                    interceptChance def defPos passerSp.X passerSp.Y targetSp.X targetSp.Y passerVisionNorm

                if bernoulli (Math.Clamp(interceptProb, 0.0, 0.8)) then
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    Vx = 0.0<meter / second>
                                    Vy = 0.0<meter / second>
                                    Vz = 0.0<meter / second> }
                            LastTouchBy = Some def.Id }

                    givePossessionTo actx.DefSide def.Id state

                    [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                      createEvent subTick def.Id defClubId TackleSuccess ]
                else
                    match findNearestTeammateToPos passer.Id targetSp.X targetSp.Y state actx.AttSide with
                    | Some(actualTarget, actualSp) ->
                        ballTowards
                            passerSp.X
                            passerSp.Y
                            actualSp.X
                            actualSp.Y
                            (BalanceConfig.PassSpeed * 0.7)
                            BalanceConfig.PassVz
                            state

                        loosePossession state
                        adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                        [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                    | None ->
                        loosePossession state
                        adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state
                        [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]
            else
                match findNearestTeammateToPos passer.Id targetSp.X targetSp.Y state actx.AttSide with
                | Some(actualTarget, actualSp) ->
                    ballTowards
                        passerSp.X
                        passerSp.Y
                        actualSp.X
                        actualSp.Y
                        (BalanceConfig.PassSpeed * 0.7)
                        BalanceConfig.PassVz
                        state

                    loosePossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state

                    [ createEvent subTick passer.Id attClubId (PassMisplaced(passer.Id, actualTarget.Id)) ]
                | None ->
                    loosePossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state
                    [ createEvent subTick passer.Id attClubId (PassIncomplete passer.Id) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide

        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        let mutable passerIdx = 0
        let mutable passerDistSq = PhysicsContract.MaxDistanceSq

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s ->
                let dSq = s.Pos.DistSqTo2D bPos

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
            |> Array.map (function
                | PlayerSlot.Active s -> Some(s.Player, s.Pos, s.Profile)
                | _ -> None)
            |> Array.choose id
            |> Array.filter (fun (_, _, profile) -> profile.AttackingDepth > 0.5 || profile.CreativityWeight > 0.4)
            |> Array.map (fun (p, pos, _) -> p, pos)

        let nearestDefDist =
            match findNearestOpponentToPos bX bY state with
            | Some(_, dSp) -> bPos.DistTo2D dSp
            | None -> 10.0<meter>

        let pressureFactor =
            Math.Clamp(1.0 - nearestDefDist / BalanceConfig.PassPressureDistance, 0.0, 1.0)

        let adjustedSuccess =
            successChance
            - pressureFactor * BalanceConfig.PassDeflectPressureMultiplier * 0.3

        let deflectRate =
            BalanceConfig.PassDeflectBaseRate * 1.5
            + pressureFactor * BalanceConfig.PassDeflectPressureMultiplier

        let interceptRate = BalanceConfig.PassInterceptBaseRate * 1.5

        if bernoulli adjustedSuccess && forwards.Length > 0 then
            let target, targetSp = forwards[0]
            let offside = isOffside target targetSp.X state actx.Dir

            if offside then
                loosePossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallOffsideMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
            else
                let snapshot = snapshotAtPass passer target state actx.Dir

                let heavyTouchChance = (1.0 - float target.Technical.BallControl / 20.0) * 0.25

                if bernoulli heavyTouchChance then
                    let jitterX =
                        PhysicsContract.clamp
                            (targetSp.X + normalSample 0.0 2.0 * 1.0<meter>)
                            0.0<meter>
                            PhysicsContract.PitchLength

                    let jitterY =
                        PhysicsContract.clamp
                            (targetSp.Y + normalSample 0.0 2.0 * 1.0<meter>)
                            0.0<meter>
                            PhysicsContract.PitchWidth

                    ballTowards
                        passerSp.X
                        passerSp.Y
                        jitterX
                        jitterY
                        BalanceConfig.LongBallSpeed
                        BalanceConfig.LongBallVz
                        state
                else
                    ballTowards
                        passerSp.X
                        passerSp.Y
                        targetSp.X
                        targetSp.Y
                        BalanceConfig.LongBallSpeed
                        BalanceConfig.LongBallVz
                        state

                state.Ball <-
                    { state.Ball with
                        Possession = InFlight(actx.AttSide, passer.Id)
                        PendingOffsideSnapshot = Some snapshot }

                adjustMomentum actx.Dir BalanceConfig.LongBallSuccessMomentum state

                [ createEvent subTick passer.Id attClubId (LongBall true) ]
        elif bernoulli deflectRate then
            let deflectedById =
                match findNearestOpponentToPos bX bY state with
                | Some(d, _) -> d.Id
                | None -> passer.Id

            let jitterX =
                bX
                + normalSample 0.0 (float BalanceConfig.PassScrambleJitter * 2.0) * 1.0<meter>
                |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength

            let jitterY =
                bY
                + normalSample 0.0 (float BalanceConfig.PassScrambleJitter * 2.0) * 1.0<meter>
                |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

            ballTowards
                passerSp.X
                passerSp.Y
                jitterX
                jitterY
                (BalanceConfig.LongBallSpeed * 0.6)
                (BalanceConfig.LongBallVz * 0.5)
                state

            state.Ball <-
                { state.Ball with
                    Possession = InFlight(actx.AttSide, passer.Id) }

            [ createEvent subTick passer.Id attClubId (PassDeflected(passer.Id, deflectedById)) ]
        elif bernoulli interceptRate then
            match findNearestOpponentToPos bX bY state with
            | Some(def, defSp) ->
                let defClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id
                adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state

                state.Ball <-
                    { state.Ball with
                        Position =
                            { defSp with
                                Vx = 0.0<meter / second>
                                Vy = 0.0<meter / second>
                                Vz = 0.0<meter / second> }
                        LastTouchBy = Some def.Id }

                givePossessionTo actx.DefSide def.Id state

                [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                  createEvent subTick def.Id defClubId TackleSuccess ]
            | None ->
                loosePossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state
                [ createEvent subTick passer.Id attClubId (LongBall false) ]
        else
            loosePossession state
            adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state
            [ createEvent subTick passer.Id attClubId (LongBall false) ]
