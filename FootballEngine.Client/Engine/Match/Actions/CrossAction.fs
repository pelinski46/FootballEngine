namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module CrossAction =

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let cc = ctx.Config.Cross
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide
        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y
        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> []
        | ValueSome crosserIdx ->
            let crosser = attRoster.Players[crosserIdx]
            let crosserCond = int attFrame.Condition[crosserIdx]
            let crosserPos = { X = float attFrame.PosX[crosserIdx] * 1.0<meter>; Y = float attFrame.PosY[crosserIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
            state.Ball <- { state.Ball with Possession = InFlight (state.AttackingSide, crosser.Id) }

            let attClubId = actx.Att.ClubId
            let condNorm = PhysicsContract.normaliseCondition crosserCond

            let crossMean =
                cc.BaseMean
                + PhysicsContract.normaliseAttr crosser.Technical.Crossing * cc.CrossingWeight
                + PhysicsContract.normaliseAttr crosser.Technical.Passing * cc.PassingWeight
                + actx.Att.Bonus.SetPlay

            let successChance = betaSample crossMean (cc.SuccessShapeAlpha + condNorm * cc.SuccessConditionMultiplier)

            let defOutfield =
                [| for i = 0 to defFrame.SlotCount - 1 do
                     match defFrame.Occupancy[i] with
                     | OccupancyKind.Active _ when defRoster.Players[i].Position <> GK ->
                         let sp = { X = float defFrame.PosX[i] * 1.0<meter>; Y = float defFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                         yield (defRoster.Players[i], sp)
                     | _ -> () |]

            let targets =
                [| for i = 0 to attFrame.SlotCount - 1 do
                     match attFrame.Occupancy[i] with
                     | OccupancyKind.Active _ ->
                         let profile = attRoster.Profiles[i]
                         if profile.AerialThreat > cc.AerialThreatThreshold || profile.AttackingDepth > cc.AttackingDepthThreshold then
                             let sp = { X = float attFrame.PosX[i] * 1.0<meter>; Y = float attFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                             let defDist =
                                 if defOutfield.Length = 0 then 999.0<meterSquared>
                                 else
                                     defOutfield
                                     |> Array.map (fun (_, dSp) ->
                                         let dx = dSp.X - sp.X
                                         let dy = dSp.Y - sp.Y
                                         dx * dx + dy * dy)
                                     |> Array.min
                             yield (attRoster.Players[i], sp, -defDist)
                     | _ -> () |]
                |> Array.sortBy (fun (_, _, d) -> d)
                |> Array.map (fun (p, sp, _) -> p, sp)

            if targets.Length > 0 && bernoulli successChance then
                let target, targetSp = targets[0]

                let gkIdx =
                    let mutable idx = -1
                    for i = 0 to defFrame.SlotCount - 1 do
                        match defFrame.Occupancy[i] with
                        | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> idx <- i
                        | _ -> ()
                    idx

                let gk = if gkIdx >= 0 then Some defRoster.Players[gkIdx] else None
                let gkSkill = gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue cc.GkSkillDefault

                // Stage 1: Cross Quality
                let crossQuality = betaSample successChance (cc.SuccessShapeAlpha + condNorm * cc.SuccessConditionMultiplier)
                if not (bernoulli crossQuality) then
                    let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth else PhysicsContract.PenaltyAreaDepth
                    let defClub = ClubSide.flip actx.Att.ClubSide
                    ballTowards crosserPos.X crosserPos.Y targetX (PhysicsContract.PitchWidth / 2.0) cc.FallbackSpeed cc.FallbackVz state
                    state.Ball <- { state.Ball with Possession = Contest(defClub) }
                    clearOffsideSnapshot state
                    [ createEvent subTick crosser.Id attClubId (CrossAttempt false) ]
                else
                    let headerScore = PhysicsContract.normaliseAttr (min 20 (target.Physical.Strength + target.Technical.Heading)) * physicalVariation crosserCond
                    let gkScore = gkSkill / cc.GkSkillDivisor
                    let nearDefs = defOutfield |> Array.sumBy (fun (p, _) -> PhysicsContract.normaliseAttr p.Mental.Positioning) |> fun v -> v / float (max 1 defOutfield.Length)
                    let spin = { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinTopMult * 1.0<radianPerSecond>; Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinSideMult * 1.0<radianPerSecond> }

                    // Stages 2, 3, 4
                    let headerDuel = headerScore - gkScore - nearDefs
                    let headerAccuracy = cc.HeaderAccuracyBase + PhysicsContract.normaliseAttr target.Technical.Heading * cc.HeaderAccuracySkillMult
                    let saveProb = cc.GkSaveBase + PhysicsContract.normaliseAttr (gk |> Option.map (fun g -> g.Goalkeeping.Reflexes) |> Option.defaultValue 10) * cc.GkReflexesMult

                    if logisticBernoulli headerDuel cc.HeaderDuelSteepness && bernoulli headerAccuracy then
                        if bernoulli saveProb then
                            (createEvent subTick crosser.Id attClubId (CrossAttempt true)) :: [createEvent subTick (gk |> Option.map (fun g -> g.Id) |> Option.defaultValue 0) actx.Def.ClubId Save]
                        else
                            let goalEvents = awardGoal actx.Att.ClubSide (Some target.Id) subTick ctx state
                            (createEvent subTick crosser.Id attClubId (CrossAttempt true)) :: goalEvents
                    else
                        let blockPos =
                            defOutfield
                            |> Array.map (fun (_, dSp) ->
                                let dx = dSp.X - targetSp.X
                                let dy = dSp.Y - targetSp.Y
                                dx * dx + dy * dy, (dSp.X, dSp.Y))
                            |> Array.minBy fst
                            |> snd
                        let bx, by = blockPos
                        ballTowards crosserPos.X crosserPos.Y bx by cc.Speed cc.Vz state
                        let defClub = ClubSide.flip actx.Att.ClubSide
                        state.Ball <- { state.Ball with Possession = Contest(defClub); Spin = spin }
                        clearOffsideSnapshot state
                        adjustMomentum actx.Att.AttackDir (-cc.FailMomentum) state
                        [ createEvent subTick crosser.Id attClubId (CrossAttempt true) ]
            else
                let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth else PhysicsContract.PenaltyAreaDepth
                let defClub = ClubSide.flip actx.Att.ClubSide
                ballTowards crosserPos.X crosserPos.Y targetX (PhysicsContract.PitchWidth / 2.0) cc.FallbackSpeed cc.FallbackVz state
                state.Ball <- { state.Ball with Possession = Contest(defClub) }
                clearOffsideSnapshot state
                [ createEvent subTick crosser.Id attClubId (CrossAttempt false) ]
