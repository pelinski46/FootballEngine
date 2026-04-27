namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open MatchSpatial
open SimulationClock
open FootballEngine.PhysicsContract

module SetPlayAction =

    let resolveFreeKick (subTick: int) (ctx: MatchContext) (state: SimState) : ActionResult =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let clubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide
        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> ActionResult.empty
        | ValueSome kickerIdx ->
            let kicker = attRoster.Players[kickerIdx]
            let kickerCond = int attFrame.Condition[kickerIdx]

            let gkIdx =
                let mutable idx = -1
                for i = 0 to defFrame.SlotCount - 1 do
                    match defFrame.Occupancy[i] with
                    | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> idx <- i
                    | _ -> ()
                idx

            let gk = if gkIdx >= 0 then Some defRoster.Players[gkIdx] else None

            let kickerScore =
                ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig kicker.Technical.Finishing kickerCond kicker.Morale
                + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig kicker.Technical.LongShots kickerCond kicker.Morale * 0.5
                + actx.Att.Bonus.FreeKick

            let gkScore =
                match gk with
                | Some g ->
                    ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig g.Goalkeeping.Reflexes g.Condition g.Morale
                    + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig g.Goalkeeping.OneOnOne g.Condition g.Morale * 0.5
                | None -> 0.5

            let fkDiff = kickerScore - gkScore
            let scored = logisticBernoulli fkDiff spc.FreeKickSteepness

            if scored then
                state.Ball <- { state.Ball with Possession = InFlight(actx.Att.ClubSide, kicker.Id); LastTouchBy = Some kicker.Id }
                ActionResult.withGoal actx.Att.ClubSide (Some kicker.Id) [ createEvent subTick kicker.Id clubId (FreeKick true) ]
            else
                let targetX = if actx.Att.AttackDir = LeftToRight then spc.FreeKickTargetX else PhysicsContract.PitchLength - spc.FreeKickTargetX
                let bX = state.Ball.Position.X
                let bY = state.Ball.Position.Y
                ballTowards bX bY targetX bY spc.FreeKickSpeed spc.FreeKickVz state
                loosePossession state
                let gkClubId = actx.Def.ClubId
                let events = [ yield createEvent subTick kicker.Id clubId (FreeKick false); match gk with | Some g -> yield createEvent subTick g.Id gkClubId Save | None -> () ]
                if ActionMath.engineBernoulli (Probability.from spc.PostShotClearProbability) then
                    let clearY = PhysicsContract.PitchWidth / 2.0 + (normalSample 0.0 spc.ClearYStdDev) * 1.0<meter>
                    ballTowards state.Ball.Position.X state.Ball.Position.Y PhysicsContract.HalfwayLineX clearY spc.ClearSpeed spc.ClearVz state
                ActionResult.ofEvents events

    let resolveCorner (subTick: int) (ctx: MatchContext) (state: SimState) : ActionResult =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let cc = ctx.Config.Cross
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

        let activeAtts = [| for i = 0 to attFrame.SlotCount - 1 do match attFrame.Occupancy[i] with | OccupancyKind.Active _ -> yield attRoster.Players[i] | _ -> () |]

        if activeAtts.Length = 0 then ActionResult.empty
        else
            let taker =
                activeAtts |> Array.tryFind (fun p -> p.Position = ML || p.Position = MR || p.Position = AML || p.Position = AMR) |> Option.defaultValue activeAtts[0]

            let boxThreshold = spc.CornerBoxXThreshold
            let defBoxThreshold = spc.CornerDefenderBoxThreshold

            let attackersInBox =
                [| for i = 0 to attFrame.SlotCount - 1 do
                     match attFrame.Occupancy[i] with
                     | OccupancyKind.Active _ ->
                         let p = attRoster.Players[i]
                         let sp = { X = float attFrame.PosX[i] * 1.0<meter>; Y = float attFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                         let cond = int attFrame.Condition[i]
                         if (p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC || p.Position = MC || p.Position = DC)
                            && (if actx.Att.AttackDir = LeftToRight then sp.X > boxThreshold else sp.X < (PhysicsContract.PitchLength - boxThreshold)) then
                             yield (p, sp, cond)
                     | _ -> () |]

            let defendersInBox =
                [| for i = 0 to defFrame.SlotCount - 1 do
                     match defFrame.Occupancy[i] with
                     | OccupancyKind.Active _ when defRoster.Players[i].Position <> GK ->
                         let sp = { X = float defFrame.PosX[i] * 1.0<meter>; Y = float defFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                         if (if actx.Att.AttackDir = LeftToRight then sp.X > defBoxThreshold else sp.X < (PhysicsContract.PitchLength - defBoxThreshold)) then
                             yield (defRoster.Players[i], sp)
                     | _ -> () |]

            let gk =
                let mutable gkOpt: Player option = None
                for i = 0 to defFrame.SlotCount - 1 do
                    match defFrame.Occupancy[i] with
                    | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> gkOpt <- Some defRoster.Players[i]
                    | _ -> ()
                gkOpt

            if attackersInBox.Length = 0 then
                let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth else PhysicsContract.PenaltyAreaDepth
                ballTowards state.Ball.Position.X state.Ball.Position.Y targetX (PhysicsContract.PitchWidth / 2.0) spc.CornerSpeed spc.CornerVz state
                loosePossession state
                ActionResult.ofEvents [ createEvent subTick taker.Id attClubId Corner ]
            else
                let bestAttacker, _bestAttackerSp, bestAttackerCond = attackersInBox |> Array.maxBy (fun (p, _, _) -> p.Physical.Strength + p.Technical.Heading)
                let bestDefender = defendersInBox |> Array.sortByDescending (fun (d, _) -> d.Physical.Strength + d.Mental.Positioning) |> Array.tryHead

                let crossQuality =
                    activeAtts
                    |> Array.tryPick (fun p ->
                        if p.Position = ML || p.Position = MR || p.Position = AML || p.Position = AMR || p.Position = MC then
                            Some (PhysicsContract.normaliseAttr p.Technical.Crossing * cc.CrossingWeight
                                  + PhysicsContract.normaliseAttr p.Technical.Passing * cc.PassingWeight)
                        else None)
                    |> Option.defaultValue cc.BaseMean

                if not (ActionMath.engineBernoulli (Probability.from crossQuality)) then
                    let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth else PhysicsContract.PenaltyAreaDepth
                    ballTowards state.Ball.Position.X state.Ball.Position.Y targetX (PhysicsContract.PitchWidth / 2.0) spc.CornerSpeed spc.CornerVz state
                    loosePossession state
                    ActionResult.ofEvents [ createEvent subTick taker.Id attClubId Corner ]
                else
                    let headerScore = PhysicsContract.normaliseAttr (min 20 (bestAttacker.Physical.Strength + bestAttacker.Technical.Heading)) * physicalVariation bestAttackerCond
                    let defScore = bestDefender |> Option.map (fun (d, _) -> PhysicsContract.normaliseAttr (min 20 (d.Physical.Strength + d.Mental.Positioning))) |> Option.defaultValue spc.CornerDefScoreDefault
                    let numDefenders = float (max 1 defendersInBox.Length)
                    let densityPenalty = (numDefenders - spc.CornerDensityBase) * spc.CornerDensityPenalty
                    let headerDuel = headerScore - defScore - densityPenalty

                    if not (ActionMath.engineLogisticBernoulli headerDuel cc.HeaderDuelSteepness) then
                        let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0<meter> else PhysicsContract.PenaltyAreaDepth + 5.0<meter>
                        ballTowards state.Ball.Position.X state.Ball.Position.Y targetX (PhysicsContract.PitchWidth / 2.0) spc.CornerSpeed spc.CornerVz state
                        if ActionMath.engineBernoulli (Probability.from spc.CornerKeepPossessionProbability) then clearOffsideSnapshot state else loosePossession state
                        ActionResult.ofEvents [ createEvent subTick taker.Id attClubId Corner ]
                    else
                        let headerAccuracy = cc.HeaderAccuracyBase + PhysicsContract.normaliseAttr bestAttacker.Technical.Heading * cc.HeaderAccuracySkillMult

                        if not (ActionMath.engineBernoulli (Probability.from headerAccuracy)) then
                            let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0<meter> else PhysicsContract.PenaltyAreaDepth + 5.0<meter>
                            ballTowards state.Ball.Position.X state.Ball.Position.Y targetX (PhysicsContract.PitchWidth / 2.0) spc.CornerSpeed spc.CornerVz state
                            loosePossession state
                            ActionResult.ofEvents [ createEvent subTick taker.Id attClubId Corner ]
                        else
                            let saveProb = cc.GkSaveBase + (gk |> Option.map (fun g -> PhysicsContract.normaliseAttr g.Goalkeeping.Reflexes * cc.GkReflexesMult) |> Option.defaultValue 0.0)

                            if ActionMath.engineBernoulli (Probability.from saveProb) then
                                let gkClubId = actx.Def.ClubId
                                let events = [ createEvent subTick taker.Id attClubId Corner ]
                                match gk with
                                | Some g -> ActionResult.ofEvents (events @ [ createEvent subTick g.Id gkClubId Save ])
                                | None -> ActionResult.ofEvents events
                            else
                                state.Ball <- { state.Ball with Possession = InFlight(actx.Att.ClubSide, bestAttacker.Id); LastTouchBy = Some bestAttacker.Id }
                                ActionResult.withGoal actx.Att.ClubSide (Some bestAttacker.Id) [ createEvent subTick taker.Id attClubId Corner ]

    let resolveThrowIn (subTick: int) (ctx: MatchContext) (state: SimState) (throwClub: ClubSide) : ActionResult =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let clubId = if throwClub = HomeClub then ctx.Home.Id else ctx.Away.Id
        let throwFrame = SimStateOps.getFrame state throwClub
        let throwRoster = SimStateOps.getRoster ctx throwClub

        let activeCount = throwFrame.Occupancy |> Array.sumBy (function | OccupancyKind.Active _ -> 1 | _ -> 0)
        if activeCount = 0 then ActionResult.empty
        else
            let throwerIdx =
                let mutable bestIdx = 0
                let mutable bestScore = 999
                for i = 0 to throwFrame.SlotCount - 1 do
                    match throwFrame.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let profile = throwRoster.Profiles[i]
                        let score = if profile.LateralTendency > 0.3 || profile.LateralTendency < -0.3 then 0 else 1
                        if score < bestScore then bestScore <- score; bestIdx <- i
                    | _ -> ()
                bestIdx

            let thrower = throwRoster.Players[throwerIdx]
            let bX = state.Ball.Position.X
            let bY = state.Ball.Position.Y

            match MatchSpatial.nearestActiveSlotInFrameExcluding throwFrame throwerIdx bX bY with
            | ValueNone -> ActionResult.empty
            | ValueSome tmIdx ->
                let teammate = throwRoster.Players[tmIdx]
                let tX = float throwFrame.PosX[tmIdx] * 1.0<meter>
                let tY = float throwFrame.PosY[tmIdx] * 1.0<meter>
                ballTowards state.Ball.Position.X state.Ball.Position.Y tX tY spc.ThrowInSpeed spc.ThrowInVz state
                adjustMomentum actx.Att.AttackDir spc.ThrowInMomentum state
                ActionResult.ofEvents [ createEvent subTick teammate.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ]

    let resolvePenalty
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (kicker: Player)
        (kickerClub: ClubSide)
        (clock: SimulationClock)
        : ActionResult =
        let spc = ctx.Config.SetPiece
        let clubId = if kickerClub = HomeClub then ctx.Home.Id else ctx.Away.Id
        let defFrame = SimStateOps.getFrame state (ClubSide.flip kickerClub)
        let defRoster = SimStateOps.getRoster ctx (ClubSide.flip kickerClub)

        let gk =
            let mutable gkOpt: Player option = None
            for i = 0 to defFrame.SlotCount - 1 do
                match defFrame.Occupancy[i] with
                | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> gkOpt <- Some defRoster.Players[i]
                | _ -> ()
            gkOpt

        let gkIdx =
            let mutable idx = -1
            for i = 0 to defFrame.SlotCount - 1 do
                match defFrame.Occupancy[i] with
                | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> idx <- i
                | _ -> ()
            idx

        let dirSign = PhysicsContract.forwardX (attackDirFor kickerClub state)
        let finishingNorm = PhysicsContract.normaliseAttr kicker.Technical.Finishing
        let angleSpread = spc.PenaltyAngleSpread * (1.0 - finishingNorm)
        let angle = normalSample 0.0 angleSpread
        let speed = spc.PenaltySpeed
        let vz = abs (normalSample (float spc.PenaltyVzBase) spc.PenaltyVzVariance) * 1.0<meter/second>
        let vx = dirSign * float speed * System.Math.Cos(angle)
        let vy = float speed * System.Math.Sin(angle)

        let kickerSkill = float kicker.CurrentSkill
        let kickerCond = float kicker.Condition
        let kickerMorale = float kicker.Morale
        let pressNoise = if state.AttackingSide = kickerClub then 0.0 else pressureNoise kicker.Mental.Composure spc.PenaltyComposureNoise

        let kickerPower =
            kickerSkill / spc.PenaltySkillDivisor * (0.5 + kickerCond / spc.PenaltyCondDivisor) * (spc.PenaltyMoraleBase + kickerMorale / spc.PenaltyMoraleDivisor)
            + (if kickerClub = HomeClub then ctx.Config.HomeAdvantage.PenaltyBonus else 0.0)
            - pressNoise * spc.PenaltyPressureMultiplier

        let gkSaves =
            match gk with
            | Some g ->
                let gkCond = if gkIdx >= 0 then int defFrame.Condition[gkIdx] else 50
                let savePower =
                    ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig (PhysicsContract.normaliseAttr g.Goalkeeping.Reflexes) gkCond g.Morale * spc.PenaltyGkReflexesMult
                    + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig (PhysicsContract.normaliseAttr g.Goalkeeping.Handling) gkCond g.Morale * spc.PenaltyGkHandlingMult
                ActionMath.engineBernoulli (Probability.from (savePower / (savePower + kickerPower + 1.0)))
            | None -> false

        state.Ball <-
            { state.Ball with
                Position =
                    { state.Ball.Position with
                        Vx = vx * 1.0<meter / second>
                        Vy = vy * 1.0<meter / second>
                        Vz = vz }
                Possession = InFlight(kickerClub, kicker.Id)
                LastTouchBy = Some kicker.Id }

        if gkSaves then
            let gkClubId = ClubSide.flip kickerClub |> fun side -> if side = HomeClub then ctx.Home.Id else ctx.Away.Id
            ActionResult.ofEvents
                [ createEvent subTick kicker.Id clubId (PenaltyAwarded false)
                  createEvent subTick (gk |> Option.map _.Id |> Option.defaultValue 0) gkClubId Save ]
        else
            ActionResult.withGoal kickerClub (Some kicker.Id) [ createEvent subTick kicker.Id clubId (PenaltyAwarded true) ]
