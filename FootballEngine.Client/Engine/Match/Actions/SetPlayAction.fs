namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open MatchSpatial
open SimulationClock
open FootballEngine.PhysicsContract

module SetPlayAction =

    let resolveFreeKick (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let clubId = actx.Att.ClubId

        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        match MatchSpatial.nearestActiveSlot attSlots bX bY with
        | ValueNone -> []
        | ValueSome kickerSlot ->
            let kicker, kickerCond = kickerSlot.Player, kickerSlot.Condition
            let defPlayers = playersArray defSlots

            let shotPower =
                effectiveStat kicker.Technical.Finishing kickerCond kicker.Morale 2.0
                + effectiveStat kicker.Mental.Composure kickerCond kicker.Morale (1.5 + actx.Att.Bonus.FreeKick)
                + effectiveStat kicker.Technical.LongShots kickerCond kicker.Morale 1.0
                + pressureNoise kicker.Mental.Composure spc.PenaltyComposureNoise

            let gk = defPlayers |> Array.tryFind (fun p -> p.Position = GK)

            let savePower =
                match gk with
                | Some g ->
                    effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                    + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                    + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
                | None -> normalSample 50.0 10.0

            let spin =
                { Top =
                    -(PhysicsContract.normaliseAttr kicker.Technical.FreeKick)
                    * spc.FreeKickSpinTopMult * 1.0<radianPerSecond>
                  Side = (PhysicsContract.normaliseAttr kicker.Technical.FreeKick) * spc.FreeKickSpinSideMult * 1.0<radianPerSecond> }

            let scored =
                shotPower > savePower
                            + spc.FreeKickSavePowerThreshold
                            + normalSample 0.0 spc.FreeKickSaveVariance

            if scored then
                let goalEvents = awardGoal actx.Att.ClubSide (Some kicker.Id) subTick ctx state
                state.Ball <- { state.Ball with Spin = spin }
                (createEvent subTick kicker.Id clubId (FreeKick true)) :: goalEvents
            else
                let targetX =
                    if actx.Att.AttackDir = LeftToRight then
                        spc.FreeKickTargetX
                    else
                        PhysicsContract.PitchLength - spc.FreeKickTargetX

                let bX = state.Ball.Position.X
                let bY = state.Ball.Position.Y

                ballTowards bX bY targetX bY spc.FreeKickSpeed spc.FreeKickVz state
                loosePossession state
                state.Ball <- { state.Ball with Spin = spin }

                let gkClubId = actx.Def.ClubId

                let events =
                    [ yield createEvent subTick kicker.Id clubId (FreeKick false)
                      match gk with
                      | Some g -> yield createEvent subTick g.Id gkClubId Save
                      | None -> () ]

                if bernoulli spc.PostShotClearProbability then
                    let clearY = PhysicsContract.PitchWidth / 2.0 + (normalSample 0.0 spc.ClearYStdDev) * 1.0<meter>

                    ballTowards
                        state.Ball.Position.X
                        state.Ball.Position.Y
                        PhysicsContract.HalfwayLineX
                        clearY
                        spc.ClearSpeed
                        spc.ClearVz
                        state

                events

    let resolveCorner (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let cc = ctx.Config.Cross
        let attClubId = actx.Att.ClubId

        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        let activeAtts = activePlayers attSlots

        if activeAtts.Length = 0 then
            []
        else
            let taker =
                activeAtts
                |> Array.tryFind (fun p -> p.Position = ML || p.Position = MR || p.Position = AML || p.Position = AMR)
                |> Option.defaultValue activeAtts[0]

            let boxThreshold = spc.CornerBoxXThreshold
            let defBoxThreshold = spc.CornerDefenderBoxThreshold

            let attackersInBox =
                attSlots
                |> Array.choose (function
                    | PlayerSlot.Active s -> Some(s.Player, s.Pos, s.Condition)
                    | _ -> None)
                |> Array.filter (fun (p, sp, _) ->
                    (p.Position = ST
                     || p.Position = AML
                     || p.Position = AMR
                     || p.Position = AMC
                     || p.Position = MC
                     || p.Position = DC)
                    && (if actx.Att.AttackDir = LeftToRight then
                            sp.X > boxThreshold
                        else
                            sp.X < (PhysicsContract.PitchLength - boxThreshold)))

            let defendersInBox =
                defSlots
                |> Array.choose (function
                    | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                    | _ -> None)
                |> Array.filter (fun (_, sp) ->
                    if actx.Att.AttackDir = LeftToRight then
                        sp.X > defBoxThreshold
                    else
                        sp.X < (PhysicsContract.PitchLength - defBoxThreshold))

            let gk =
                defSlots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Position = GK -> Some s.Player
                    | _ -> None)

            if attackersInBox.Length = 0 then
                let targetX =
                    if actx.Att.AttackDir = LeftToRight then
                        PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                    else
                        PhysicsContract.PenaltyAreaDepth

                ballTowards
                    state.Ball.Position.X
                    state.Ball.Position.Y
                    targetX
                    (PhysicsContract.PitchWidth / 2.0)
                    spc.CornerSpeed
                    spc.CornerVz
                    state

                loosePossession state

                [ createEvent subTick taker.Id attClubId Corner ]
            else
                let bestAttacker, _bestAttackerSp, bestAttackerCond =
                    attackersInBox
                    |> Array.maxBy (fun (p, _, _) -> p.Physical.Strength + p.Technical.Heading)

                let bestDefender =
                    defendersInBox
                    |> Array.sortByDescending (fun (d, _) -> d.Physical.Strength + d.Mental.Positioning)
                    |> Array.tryHead

                let attackScore =
                    PhysicsContract.normaliseAttr (
                        min 20 (bestAttacker.Physical.Strength + bestAttacker.Technical.Heading)
                    )
                    * physicalVariation bestAttackerCond

                let defScore =
                    bestDefender
                    |> Option.map (fun (d, _) ->
                        PhysicsContract.normaliseAttr (min 20 (d.Physical.Strength + d.Mental.Positioning)))
                    |> Option.defaultValue spc.CornerDefScoreDefault

                let gkBonus =
                    gk
                    |> Option.map (fun g -> effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 1.0 / 100.0)
                    |> Option.defaultValue 0.0

                let numDefenders = float (max 1 defendersInBox.Length)
                let densityPenalty = (numDefenders - spc.CornerDensityBase) * spc.CornerDensityPenalty

                let crossQuality =
                    activeAtts
                    |> Array.tryPick (fun p ->
                        if
                            p.Position = ML
                            || p.Position = MR
                            || p.Position = AML
                            || p.Position = AMR
                            || p.Position = MC
                        then
                            Some(
                                PhysicsContract.normaliseAttr p.Technical.Crossing
                                * cc.CrossingWeight
                                + PhysicsContract.normaliseAttr p.Technical.Passing
                                  * cc.PassingWeight
                            )
                        else
                            None)
                    |> Option.defaultValue cc.BaseMean

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) spc.CornerLogisticSteepness

                if scored then
                    let goalEvents = awardGoal actx.Att.ClubSide (Some bestAttacker.Id) subTick ctx state
                    (createEvent subTick taker.Id attClubId Corner) :: goalEvents
                else
                    let targetX =
                        if actx.Att.AttackDir = LeftToRight then
                            PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0<meter>
                        else
                            PhysicsContract.PenaltyAreaDepth + 5.0<meter>

                    ballTowards
                        state.Ball.Position.X
                        state.Ball.Position.Y
                        targetX
                        (PhysicsContract.PitchWidth / 2.0)
                        spc.CornerSpeed
                        spc.CornerVz
                        state

                    if bernoulli spc.CornerKeepPossessionProbability then
                        clearOffsideSnapshot state
                    else
                        loosePossession state

                    [ createEvent subTick taker.Id attClubId Corner ]



    let resolveThrowIn (subTick: int) (ctx: MatchContext) (state: SimState) (throwClub: ClubSide) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let spc = ctx.Config.SetPiece
        let clubId = if throwClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        let throwSlots = getSlots state throwClub

        let activeCount =
            throwSlots
            |> Array.sumBy (function
                | PlayerSlot.Active _ -> 1
                | _ -> 0)

        if activeCount = 0 then
            []
        else
            let thrower =
                throwSlots
                |> Array.choose (function
                    | PlayerSlot.Active s -> Some(s.Player, s.Profile)
                    | _ -> None)
                |> Array.sortBy (fun (_, profile) ->
                    if profile.LateralTendency > 0.3 || profile.LateralTendency < -0.3 then
                        0
                    else
                        1)
                |> Array.map fst
                |> Array.head

            let attSlots = getSlots state throwClub
            let bX = state.Ball.Position.X
            let bY = state.Ball.Position.Y
            let nearestTeammate = nearestActiveSlotExcluding attSlots thrower.Id bX bY

            match nearestTeammate with
            | ValueSome slot ->
                let teammate = slot.Player
                let _teammateId = slot.Player.Id
                let tX = slot.Pos.X
                let tY = slot.Pos.Y
                ballTowards
                    state.Ball.Position.X
                    state.Ball.Position.Y
                    tX
                    tY
                    spc.ThrowInSpeed
                    spc.ThrowInVz
                    state

                adjustMomentum actx.Att.AttackDir spc.ThrowInMomentum state

                [ createEvent subTick teammate.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ]
            | ValueNone -> []

    let resolvePenalty
        (ctx: MatchContext)
        (state: SimState)
        (kicker: Player)
        (kickerClub: ClubSide)
        (kickNum: int)
        (clock: SimulationClock)
        : MatchEvent list =
        let spc = ctx.Config.SetPiece
        let clubId = if kickerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        let gkSlots =
            let defSide = ClubSide.flip kickerClub
            getSlots state defSide

        let gk =
            gkSlots
            |> Array.tryPick (function
                | PlayerSlot.Active s when s.Player.Position = GK -> Some s.Player
                | _ -> None)

        let gkSkill =
            gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

        let gkCondition = gk |> Option.map (fun g -> g.Condition) |> Option.defaultValue 80

        let kickerSkill = float kicker.CurrentSkill
        let kickerCond = float kicker.Condition
        let kickerMorale = float kicker.Morale

        let pressNoise =
            if state.AttackingSide = kickerClub then 0.0
            else pressureNoise kicker.Mental.Composure spc.PenaltyComposureNoise

        let gkBonus =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale spc.PenaltyGkReflexesMult
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale spc.PenaltyGkHandlingMult
            | None -> 0.0

        let score =
            kickerSkill / spc.PenaltySkillDivisor * (0.5 + kickerCond / spc.PenaltyCondDivisor) * (spc.PenaltyMoraleBase + kickerMorale / spc.PenaltyMoraleDivisor)
            - gkSkill / spc.PenaltyGkSkillDivisor
            + gkBonus
            + pressNoise * spc.PenaltyPressureMultiplier
            + (if kickerClub = HomeClub then
                   ctx.Config.HomeAdvantage.PenaltyBonus
               else
                   0.0)

        let scored = logisticBernoulli score spc.PenaltyLogisticBase
        let penaltySubTick = (fullTime clock) + kickNum

        if scored then
            let goalEvents = awardGoal kickerClub (Some kicker.Id) penaltySubTick ctx state

            let penaltyEvent =
                createEvent penaltySubTick kicker.Id clubId (PenaltyAwarded scored)

            goalEvents @ [ penaltyEvent ]
        else
            resetBallForKickOff HomeClub state
            loosePossession state

            let penaltyEvent =
                createEvent penaltySubTick kicker.Id clubId (PenaltyAwarded scored)

            [ penaltyEvent ]
