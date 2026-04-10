namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open MatchSpatial

module SetPlayAction =

    let resolveFreeKick (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let clubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let kickerIdx = nearestIdxToBall attSlots bX bY

        let kicker, kickerCond =
            match attSlots[kickerIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition
            | _ -> Unchecked.defaultof<Player>, 0

        let defPlayers = playersArray defSlots

        let shotPower =
            effectiveStat kicker.Technical.Finishing kickerCond kicker.Morale 2.0
            + effectiveStat kicker.Mental.Composure kickerCond kicker.Morale (1.5 + actx.AttBonus.FreeKick)
            + effectiveStat kicker.Technical.LongShots kickerCond kicker.Morale 1.0
            + pressureNoise kicker.Mental.Composure BalanceConfig.PenaltyComposureNoise

        let gk = defPlayers |> Array.tryFind (fun p -> p.Position = GK)

        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> normalSample 50.0 10.0

        let spin =
            { Top = -(PhysicsContract.normaliseAttr kicker.Technical.FreeKick) * 0.5
              Side = (PhysicsContract.normaliseAttr kicker.Technical.FreeKick) * 0.9 }

        let scored =
            shotPower > savePower
                        + BalanceConfig.FreeKickSavePowerThreshold
                        + normalSample 0.0 BalanceConfig.FreeKickSaveVariance

        if scored then
            let goalEvents = awardGoal actx.AttSide (Some kicker.Id) subTick ctx state
            state.Ball <- { state.Ball with Spin = spin }
            (createEvent subTick kicker.Id clubId (FreeKick true)) :: goalEvents
        else
            let targetX =
                if actx.Dir = LeftToRight then
                    BalanceConfig.FreeKickTargetX
                else
                    PhysicsContract.PitchLength - BalanceConfig.FreeKickTargetX

            let bX = state.Ball.Position.X
            let bY = state.Ball.Position.Y

            MatchSpatial.ballTowards bX bY targetX bY BalanceConfig.FreeKickSpeed BalanceConfig.FreeKickVz state
            flipPossession state
            state.Ball <- { state.Ball with Spin = spin }

            let gkClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

            let events =
                [ yield createEvent subTick kicker.Id clubId (FreeKick false)
                  match gk with
                  | Some g -> yield createEvent subTick g.Id gkClubId Save
                  | None -> () ]

            if bernoulli BalanceConfig.PostShotClearProbability then
                let clearY = PhysicsContract.PitchWidth / 2.0 + normalSample 0.0 10.0
                MatchSpatial.ballTowards state.Ball.Position.X state.Ball.Position.Y PhysicsContract.HalfwayLineX clearY 16.0 1.5 state

            events

    let resolveCorner (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let activeAtts = activePlayers attSlots

        if activeAtts.Length = 0 then
            []
        else
            let taker =
                activeAtts
                |> Array.tryFind (fun p -> p.Position = ML || p.Position = MR || p.Position = AML || p.Position = AMR)
                |> Option.defaultValue activeAtts[0]

            let boxThreshold = BalanceConfig.CornerBoxXThreshold
            let defBoxThreshold = BalanceConfig.CornerDefenderBoxThreshold

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
                    && (if actx.Dir = LeftToRight then
                            sp.X > boxThreshold
                        else
                            sp.X < (PhysicsContract.PitchLength - boxThreshold)))

            let defendersInBox =
                defSlots
                |> Array.choose (function
                    | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                    | _ -> None)
                |> Array.filter (fun (_, sp) ->
                    if actx.Dir = LeftToRight then
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
                    if actx.Dir = LeftToRight then
                        PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                    else
                        PhysicsContract.PenaltyAreaDepth

                MatchSpatial.ballTowards
                    state.Ball.Position.X
                    state.Ball.Position.Y
                    targetX
                    (PhysicsContract.PitchWidth / 2.0)
                    BalanceConfig.CornerSpeed
                    BalanceConfig.CornerVz
                    state

                flipPossession state

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
                    |> Option.defaultValue 0.5

                let gkBonus =
                    gk
                    |> Option.map (fun g -> effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 1.0 / 100.0)
                    |> Option.defaultValue 0.0

                let numDefenders = float (max 1 defendersInBox.Length)
                let densityPenalty = (numDefenders - 3.0) * 0.05

                let crossQuality =
                    activeAtts
                    |> Array.tryPick (fun p ->
                        if p.Position = ML || p.Position = MR || p.Position = AML || p.Position = AMR || p.Position = MC then
                            Some(
                                PhysicsContract.normaliseAttr p.Technical.Crossing
                                * BalanceConfig.CrossCrossingWeight
                                + PhysicsContract.normaliseAttr p.Technical.Passing
                                  * BalanceConfig.CrossPassingWeight
                            )
                        else None)
                    |> Option.defaultValue BalanceConfig.CrossBaseMean

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) 2.5

                if scored then
                    let goalEvents = awardGoal actx.AttSide (Some bestAttacker.Id) subTick ctx state
                    (createEvent subTick taker.Id attClubId Corner) :: goalEvents
                else
                    let targetX =
                        if actx.Dir = LeftToRight then
                            PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0
                        else
                            PhysicsContract.PenaltyAreaDepth + 5.0

                    MatchSpatial.ballTowards
                        state.Ball.Position.X
                        state.Ball.Position.Y
                        targetX
                        (PhysicsContract.PitchWidth / 2.0)
                        BalanceConfig.CornerSpeed
                        BalanceConfig.CornerVz
                        state

                    if bernoulli BalanceConfig.CornerKeepPossessionProbability then
                        clearOffsideSnapshot state
                    else
                        flipPossession state

                    [ createEvent subTick taker.Id attClubId Corner ]



    let resolveThrowIn (subTick: int) (ctx: MatchContext) (state: SimState) (throwClub: ClubSide) : MatchEvent list =
        let actx = ActionContext.build state
        let clubId = if throwClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        let throwSlots = SimStateOps.getSlots state throwClub

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
                    | PlayerSlot.Active s -> Some s.Player
                    | _ -> None)
                |> Array.sortBy (fun p ->
                    match p.Position with
                    | DL
                    | DR
                    | WBL
                    | WBR -> 0
                    | _ -> 1)
                |> Array.head

            let nearestTeammate = findNearestTeammate thrower ctx state actx.Dir

            match nearestTeammate with
            | Some(teammate, teammateId, _) ->
                let targetX =
                    if actx.Dir = LeftToRight then
                        PhysicsContract.PenaltyAreaDepth / 2.0
                    else
                        PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth / 2.0

                withBallVelocity
                    ((targetX - state.Ball.Position.X) * BalanceConfig.ThrowInSpeed)
                    0.0
                    BalanceConfig.ThrowInVz
                    state

                adjustMomentum actx.Dir BalanceConfig.ThrowInMomentum state

                [ createEvent subTick thrower.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ]
            | None -> []

    let resolvePenalty
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (kicker: Player)
        (kickerClub: ClubSide)
        (kickNum: int)
        : MatchEvent list =
        let clubId = if kickerClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        let gkSlots =
            let defSide = ClubSide.flip kickerClub
            SimStateOps.getSlots state defSide

        let gk =
            gkSlots
            |> Array.tryPick (function
                | PlayerSlot.Active s when s.Player.Position = GK -> Some s.Player
                | _ -> None)

        let gkSkill =
            gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

        let pressNoise =
            pressureNoise kicker.Mental.Composure BalanceConfig.PenaltyComposureNoise

        let score =
            (float kicker.CurrentSkill - gkSkill) * BalanceConfig.PenaltySkillMultiplier
            + float kicker.Morale * BalanceConfig.PenaltyMoraleMultiplier
            + pressNoise * BalanceConfig.PenaltyPressureMultiplier
            + (if kickerClub = HomeClub then
                   BalanceConfig.HomePenaltyBonus
               else
                   0.0)

        let scored = logisticBernoulli score BalanceConfig.PenaltyLogisticBase
        let penaltySubTick = PhysicsContract.FullTimeSubTick + kickNum

        if scored then
            let goalEvents = awardGoal kickerClub (Some kicker.Id) penaltySubTick ctx state
            let penaltyEvent = createEvent penaltySubTick kicker.Id clubId (PenaltyAwarded scored)
            goalEvents @ [ penaltyEvent ]
        else
            resetBallToCenter state
            flipPossession state

            let penaltyEvent = createEvent penaltySubTick kicker.Id clubId (PenaltyAwarded scored)
            [ penaltyEvent ]
