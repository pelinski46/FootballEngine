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
            let mutable passerIdx = 0
            let mutable passerFound = false

            for i = 0 to attSlots.Length - 1 do
                match attSlots[i] with
                | PlayerSlot.Active s when state.Ball.LastTouchBy = Some s.Player.Id ->
                    passerIdx <- i
                    passerFound <- true
                | _ -> ()

            let passer, passerCond =
                if passerFound then
                    match attSlots[passerIdx] with
                    | PlayerSlot.Active s -> s.Player, s.Condition
                    | _ -> Unchecked.defaultof<Player>, 70
                else
                    match attSlots[0] with
                    | PlayerSlot.Active s -> s.Player, s.Condition
                    | _ -> Unchecked.defaultof<Player>, 70

            let passerSp =
                if passerFound then
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
                       BalanceConfig.BuildUpPassSuccessBonus
                   else
                       0.0)
                + (if passer.Position = GK && actx.Zone = DefensiveZone then
                       BalanceConfig.BuildUpGKDistributionBonus
                   else
                       0.0)
                + (if passer.Position = DC && actx.Zone = DefensiveZone then
                       BalanceConfig.BuildUpDCPassingBonus
                   else
                       0.0)

            let successChance =
                betaSample
                    passMean
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

                    MatchSpatial.ballTowards targetSp.X targetSp.Y BalanceConfig.PassSpeed BalanceConfig.PassVz state
                    adjustMomentum actx.Dir BalanceConfig.PassSuccessMomentum state
                    state.PendingOffsideSnapshot <- Some snapshot

                    state.Ball <-
                        { state.Ball with
                            Spin = { Top = 0.0; Side = 0.0 } }

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
                    jitterX
                    jitterY
                    (BalanceConfig.PassSpeed * 0.6)
                    (BalanceConfig.PassVz * 0.5)
                    state

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
                            LastTouchBy = Some def.Id }

                    [ createEvent subTick passer.Id attClubId (PassIntercepted(passer.Id, def.Id))
                      createEvent subTick def.Id defClubId TackleSuccess ]
                else
                    match findNearestTeammateToPos passer.Id targetSp.X targetSp.Y state actx.AttSide with
                    | Some(actualTarget, actualSp) ->
                        MatchSpatial.ballTowards
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

        let passer, passerCond =
            match attSlots[passerIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition
            | _ -> Unchecked.defaultof<Player>, 0

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
                | PlayerSlot.Active s -> Some(s.Player, s.Pos)
                | _ -> None)
            |> Array.choose id
            |> Array.filter (fun (p, _) -> p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

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

                MatchSpatial.ballTowards
                    targetSp.X
                    targetSp.Y
                    BalanceConfig.LongBallSpeed
                    BalanceConfig.LongBallVz
                    state

                adjustMomentum actx.Dir BalanceConfig.LongBallSuccessMomentum state
                state.PendingOffsideSnapshot <- Some snapshot

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
                jitterX
                jitterY
                (BalanceConfig.LongBallSpeed * 0.6)
                (BalanceConfig.LongBallVz * 0.5)
                state

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
                        LastTouchBy = Some def.Id }

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
