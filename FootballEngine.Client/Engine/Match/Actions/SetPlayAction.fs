namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open MatchSpatial

module SetPlayAction =

    let private event subTick playerId clubId t =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let private ballTowards (targetX: float) (targetY: float) (speed: float) (vz: float) (state: SimState) =
        let bX = state.Ball.Position.X
        let bY = state.Ball.Position.Y
        let dx = targetX - bX
        let dy = targetY - bY
        let dist = sqrt (dx * dx + dy * dy)

        if dist < 0.01 then
            withBallVelocity 0.0 0.0 vz state
        else
            withBallVelocity (dx / dist * speed) (dy / dist * speed) vz state

    let resolveFreeKick (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let clubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let defSlots =
            if actx.DefSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let mutable kickerIdx = 0
        let mutable kickerDistSq = System.Double.MaxValue

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - bX
                let dy = s.Pos.Y - bY
                let dSq = dx * dx + dy * dy

                if dSq < kickerDistSq then
                    kickerDistSq <- dSq
                    kickerIdx <- i
            | _ -> ()

        let kicker, kickerCond =
            match attSlots[kickerIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition
            | _ -> Unchecked.defaultof<Player>, 0

        let defPlayers =
            Array.init defSlots.Length (fun i ->
                match defSlots[i] with
                | PlayerSlot.Active s -> s.Player
                | _ -> Unchecked.defaultof<Player>)

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
            (event subTick kicker.Id clubId (FreeKick true)) :: goalEvents
        else
            let targetX =
                if actx.Dir = LeftToRight then
                    BalanceConfig.FreeKickTargetX
                else
                    PhysicsContract.PitchLength - BalanceConfig.FreeKickTargetX

            ballTowards targetX bY BalanceConfig.FreeKickSpeed BalanceConfig.FreeKickVz state
            flipPossession state
            state.Ball <- { state.Ball with Spin = spin }

            let gkClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

            let events =
                [ yield event subTick kicker.Id clubId (FreeKick false)
                  match gk with
                  | Some g -> yield event subTick g.Id gkClubId Save
                  | None -> () ]

            if bernoulli BalanceConfig.PostShotClearProbability then
                let clearY = PhysicsContract.PitchWidth / 2.0 + normalSample 0.0 10.0
                ballTowards PhysicsContract.HalfwayLineX clearY 16.0 1.5 state

            events

    let resolveCorner (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let defSlots =
            if actx.DefSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let activeCount =
            attSlots
            |> Array.sumBy (function
                | PlayerSlot.Active _ -> 1
                | _ -> 0)

        if activeCount = 0 then
            let firstPlayer =
                match attSlots[0] with
                | PlayerSlot.Active s -> s.Player
                | _ -> Unchecked.defaultof<Player>

            [ event subTick firstPlayer.Id attClubId Corner ]
        else
            let boxThreshold = BalanceConfig.CornerBoxXThreshold
            let defBoxThreshold = BalanceConfig.CornerDefenderBoxThreshold

            let attackersInBox =
                attSlots
                |> Array.mapi (fun i slot ->
                    match slot with
                    | PlayerSlot.Active s -> Some(s.Player, s.Pos, s.Condition)
                    | _ -> None)
                |> Array.choose id
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
                |> Array.mapi (fun i slot ->
                    match slot with
                    | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                    | _ -> None)
                |> Array.choose id
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
                        PhysicsContract.PenaltyAreaDepth
                    else
                        PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth

                ballTowards
                    targetX
                    (PhysicsContract.PitchWidth / 2.0)
                    BalanceConfig.CornerSpeed
                    BalanceConfig.CornerVz
                    state

                flipPossession state

                let firstPlayer =
                    match attSlots[0] with
                    | PlayerSlot.Active s -> s.Player
                    | _ -> Unchecked.defaultof<Player>

                [ event subTick firstPlayer.Id attClubId Corner ]
            else
                let bestAttacker, bestAttackerSp, bestAttackerCond =
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
                    attSlots
                    |> Array.tryPick (fun slot ->
                        match slot with
                        | PlayerSlot.Active s when
                            s.Player.Position = ML
                            || s.Player.Position = MR
                            || s.Player.Position = AML
                            || s.Player.Position = AMR
                            || s.Player.Position = MC
                            ->
                            Some(
                                PhysicsContract.normaliseAttr s.Player.Technical.Crossing
                                * BalanceConfig.CrossCrossingWeight
                                + PhysicsContract.normaliseAttr s.Player.Technical.Passing
                                  * BalanceConfig.CrossPassingWeight
                            )
                        | _ -> None)
                    |> Option.defaultValue BalanceConfig.CrossBaseMean

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) 2.5

                if scored then
                    let goalEvents = awardGoal actx.AttSide (Some bestAttacker.Id) subTick ctx state
                    (event subTick bestAttacker.Id attClubId Corner) :: goalEvents
                else
                    let targetX =
                        if actx.Dir = LeftToRight then
                            PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth - 5.0
                        else
                            PhysicsContract.PenaltyAreaDepth + 5.0

                    ballTowards
                        targetX
                        (PhysicsContract.PitchWidth / 2.0)
                        BalanceConfig.CornerSpeed
                        BalanceConfig.CornerVz
                        state

                    if bernoulli BalanceConfig.CornerKeepPossessionProbability then
                        state.PendingOffsideSnapshot <- None
                    else
                        flipPossession state

                    [ event subTick bestAttacker.Id attClubId Corner ]

    let resolveThrowIn (subTick: int) (ctx: MatchContext) (state: SimState) (throwClub: ClubSide) : MatchEvent list =
        let actx = ActionContext.build state
        let clubId = if throwClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        let throwSlots =
            if throwClub = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

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

                [ event subTick thrower.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ]
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
            if kickerClub = HomeClub then
                state.AwaySlots
            else
                state.HomeSlots

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
            let penaltyEvent = event penaltySubTick kicker.Id clubId (PenaltyAwarded scored)
            goalEvents @ [ penaltyEvent ]
        else
            resetBallToCenter state
            flipPossession state

            let penaltyEvent = event penaltySubTick kicker.Id clubId (PenaltyAwarded scored)
            [ penaltyEvent ]
