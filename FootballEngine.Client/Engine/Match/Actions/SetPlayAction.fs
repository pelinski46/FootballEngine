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
        let actx = ActionContext.build state
        let clubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        match MatchSpatial.nearestActiveSlot attSlots bX bY with
        | ValueNone -> []
        | ValueSome kickerSlot ->
            let kicker, kickerCond = kickerSlot.Player, kickerSlot.Condition
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
                { Top =
                    -(PhysicsContract.normaliseAttr kicker.Technical.FreeKick)
                    * 0.5<radianPerSecond>
                  Side = (PhysicsContract.normaliseAttr kicker.Technical.FreeKick) * 0.9<radianPerSecond> }

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

                ballTowards bX bY targetX bY BalanceConfig.FreeKickSpeed BalanceConfig.FreeKickVz state
                loosePossession state
                state.Ball <- { state.Ball with Spin = spin }

                let gkClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

                let events =
                    [ yield createEvent subTick kicker.Id clubId (FreeKick false)
                      match gk with
                      | Some g -> yield createEvent subTick g.Id gkClubId Save
                      | None -> () ]

                if bernoulli BalanceConfig.PostShotClearProbability then
                    let clearY = PhysicsContract.PitchWidth / 2.0 + (normalSample 0.0 10.0) * 1.0<meter>

                    ballTowards
                        state.Ball.Position.X
                        state.Ball.Position.Y
                        PhysicsContract.HalfwayLineX
                        clearY
                        16.0<meter / second>
                        1.5<meter / second>
                        state

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

                ballTowards
                    state.Ball.Position.X
                    state.Ball.Position.Y
                    targetX
                    (PhysicsContract.PitchWidth / 2.0)
                    BalanceConfig.CornerSpeed
                    BalanceConfig.CornerVz
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
                        if
                            p.Position = ML
                            || p.Position = MR
                            || p.Position = AML
                            || p.Position = AMR
                            || p.Position = MC
                        then
                            Some(
                                PhysicsContract.normaliseAttr p.Technical.Crossing
                                * BalanceConfig.CrossCrossingWeight
                                + PhysicsContract.normaliseAttr p.Technical.Passing
                                  * BalanceConfig.CrossPassingWeight
                            )
                        else
                            None)
                    |> Option.defaultValue BalanceConfig.CrossBaseMean

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) 2.5

                if scored then
                    let goalEvents = awardGoal actx.AttSide (Some bestAttacker.Id) subTick ctx state
                    (createEvent subTick taker.Id attClubId Corner) :: goalEvents
                else
                    let targetX =
                        if actx.Dir = LeftToRight then
                            PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0<meter>
                        else
                            PhysicsContract.PenaltyAreaDepth + 5.0<meter>

                    ballTowards
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
                        loosePossession state

                    [ createEvent subTick taker.Id attClubId Corner ]



    let resolveThrowIn (subTick: int) (ctx: MatchContext) (state: SimState) (throwClub: ClubSide) : MatchEvent list =
        let actx = ActionContext.build state
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
                    BalanceConfig.ThrowInSpeed
                    BalanceConfig.ThrowInVz
                    state

                adjustMomentum actx.Dir BalanceConfig.ThrowInMomentum state

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
            match state.AttackingClub with
            | c when c = kickerClub -> 0.0
            | _ -> pressureNoise kicker.Mental.Composure 1.5

        let gkBonus =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> 0.0

        let score =
            kickerSkill / 20.0 * (0.5 + kickerCond / 200.0) * (0.7 + kickerMorale / 166.6)
            - gkSkill / 40.0
            + gkBonus
            + pressNoise * BalanceConfig.PenaltyPressureMultiplier
            + (if kickerClub = HomeClub then
                   BalanceConfig.HomePenaltyBonus
               else
                   0.0)

        let scored = logisticBernoulli score BalanceConfig.PenaltyLogisticBase
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
