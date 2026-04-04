namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchCalc
open MatchSpatial

module SetPlayAction =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let private ballTowards (targetX: float) (targetY: float) (speed: float) (vz: float) (s: MatchState) =
        let bX = s.Ball.Position.X
        let bY = s.Ball.Position.Y
        let dx = targetX - bX
        let dy = targetY - bY
        let dist = sqrt (dx * dx + dy * dy)

        if dist < 0.01 then
            s |> withBallVelocity 0.0 0.0 vz
        else
            s |> withBallVelocity (dx / dist * speed) (dy / dist * speed) vz

    let resolveFreeKick (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let clubId = ClubSide.toClubId ctx.AttSide s
        let bY = s.Ball.Position.Y
        let attSide = side clubId s

        let bX, _ = ballXY s

        let kickerIdx =
            attSide.Positions
            |> Array.mapi (fun i _ -> i)
            |> Array.minBy (fun i ->
                let dx = attSide.Positions[i].X - bX
                let dy = attSide.Positions[i].Y - bY
                dx * dx + dy * dy)

        let kicker = attSide.Players[kickerIdx]
        let kickerCond = attSide.Conditions[kickerIdx]

        let shotPower =
            effectiveStat kicker.Technical.Finishing kickerCond kicker.Morale 2.0
            + effectiveStat kicker.Mental.Composure kickerCond kicker.Morale (1.5 + ctx.AttBonus.FreeKick)
            + effectiveStat kicker.Technical.LongShots kickerCond kicker.Morale 1.0
            + pressureNoise kicker.Mental.Composure BalanceConfig.PenaltyComposureNoise

        let gk =
            side (ClubSide.toClubId (ClubSide.flip ctx.AttSide) s) s
            |> fun side -> side.Players |> Array.tryFind (fun p -> p.Position = GK)

        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> normalSample 50.0 10.0

        let spin =
            { Top = -(float kicker.Technical.FreeKick / 20.0) * 0.5
              Side = (float kicker.Technical.FreeKick / 20.0) * 0.9 }

        let scored =
            shotPower > savePower
                        + BalanceConfig.FreeKickSavePowerThreshold
                        + normalSample 0.0 BalanceConfig.FreeKickSaveVariance

        if scored then
            let s1, goalEvents = awardGoal ctx.AttSide (Some kicker.Id) second s

            let s2 =
                { s1 with
                    Ball = { s1.Ball with Spin = spin } }

            s2, (event second kicker.Id clubId (FreeKick true)) :: goalEvents
        else
            let targetX =
                if ctx.Dir = LeftToRight then
                    BalanceConfig.FreeKickTargetX
                else
                    25.0

            let s' =
                s
                |> ballTowards targetX bY BalanceConfig.FreeKickSpeed BalanceConfig.FreeKickVz
                |> flipPossessionAndClearOffside ctx.AttSide
                |> fun s'' ->
                    { s'' with
                        Ball = { s''.Ball with Spin = spin } }

            let gkClubId = ClubSide.toClubId (ClubSide.flip ctx.AttSide) s

            let events =
                [ yield event second kicker.Id clubId (FreeKick false)
                  match gk with
                  | Some g -> yield event second g.Id gkClubId Save
                  | None -> () ]

            let s'' =
                if bernoulli BalanceConfig.PostShotClearProbability then
                    let clearY = 50.0 + normalSample 0.0 10.0
                    s' |> ballTowards 50.0 clearY 16.0 1.5
                else
                    s'

            s'', events

    let resolveCorner (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s
        let clubId = attClubId

        if attSide.Players.Length = 0 then
            s, []
        else
            let attackersInBox =
                teamRoster attSide
                |> Array.filter (fun (p, sp, _) ->
                    (p.Position = ST
                     || p.Position = AML
                     || p.Position = AMR
                     || p.Position = AMC
                     || p.Position = MC
                     || p.Position = DC)
                    && (if ctx.Dir = LeftToRight then
                            sp.X > BalanceConfig.CornerBoxXThreshold
                        else
                            sp.X < BalanceConfig.CornerOutsideXThreshold))

            let defendersInBox =
                outfieldRoster defSide
                |> Array.filter (fun (_, sp, _) ->
                    if ctx.Dir = LeftToRight then
                        sp.X > BalanceConfig.CornerDefenderBoxThreshold
                    else
                        sp.X < 30.0)

            let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

            if attackersInBox.Length = 0 then
                let targetX = if ctx.Dir = LeftToRight then 20.0 else 80.0

                let s' =
                    s
                    |> ballTowards targetX 50.0 BalanceConfig.CornerSpeed BalanceConfig.CornerVz
                    |> flipPossessionAndClearOffside ctx.AttSide

                s', [ event second attSide.Players[0].Id clubId Corner ]
            else
                let bestAttacker, bestAttackerSp, bestAttackerCond =
                    attackersInBox
                    |> Array.maxBy (fun (p, _, _) -> p.Physical.Strength + p.Technical.Heading)

                let bestDefender =
                    defendersInBox
                    |> Array.sortByDescending (fun (p, _, _) -> p.Physical.Strength + p.Mental.Positioning)
                    |> Array.tryHead

                let attackScore =
                    float (bestAttacker.Physical.Strength + bestAttacker.Technical.Heading) / 200.0
                    * physicalVariation bestAttackerCond

                let defScore =
                    bestDefender
                    |> Option.map (fun (d, _, _) -> float (d.Physical.Strength + d.Mental.Positioning) / 200.0)
                    |> Option.defaultValue 0.5

                let gkBonus =
                    gk
                    |> Option.map (fun g -> effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 1.0 / 100.0)
                    |> Option.defaultValue 0.0

                let numDefenders = float (max 1 defendersInBox.Length)
                let densityPenalty = (numDefenders - 3.0) * 0.05

                let crossQuality =
                    attSide.Players
                    |> Array.tryFind (fun p ->
                        p.Position = ML
                        || p.Position = MR
                        || p.Position = AML
                        || p.Position = AMR
                        || p.Position = MC)
                    |> Option.map (fun p ->
                        float p.Technical.Crossing / 100.0 * BalanceConfig.CrossCrossingWeight
                        + float p.Technical.Passing / 100.0 * BalanceConfig.CrossPassingWeight)
                    |> Option.defaultValue BalanceConfig.CrossBaseMean

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) 2.5

                if scored then
                    let s1, goalEvents = awardGoal ctx.AttSide (Some bestAttacker.Id) second s

                    s1, (event second bestAttacker.Id clubId Corner) :: goalEvents
                else


                    let targetX = if ctx.Dir = LeftToRight then 65.0 else 35.0

                    let s' =
                        s
                        |> ballTowards targetX 50.0 BalanceConfig.CornerSpeed BalanceConfig.CornerVz
                        |> fun s'' ->
                            if bernoulli BalanceConfig.CornerKeepPossessionProbability then
                                { s'' with
                                    PendingOffsideSnapshot = None }
                            else
                                s'' |> flipPossessionAndClearOffside ctx.AttSide

                    s', [ event second bestAttacker.Id clubId Corner ]

    let resolveThrowIn (second: int) (s: MatchState) (throwClub: ClubSide) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attSide = side (ClubSide.toClubId throwClub s) s
        let clubId = ClubSide.toClubId throwClub s

        if attSide.Players.Length = 0 then
            s, []
        else
            let thrower =
                attSide.Players
                |> Array.sortBy (fun p ->
                    match p.Position with
                    | DL
                    | DR
                    | WBL
                    | WBR -> 0
                    | _ -> 1)
                |> Array.head

            let nearestTeammate = findNearestTeammate thrower s ctx.Dir

            match nearestTeammate with
            | Some(teammate, teammateId, _) ->
                let targetX = if ctx.Dir = LeftToRight then 5.0 else 95.0

                let teammateIdx =
                    attSide |> teamRoster |> Array.findIndex (fun (p, _, _) -> p.Id = teammate.Id)

                let s' =
                    s
                    |> withBallVelocity
                        ((targetX - s.Ball.Position.X) * BalanceConfig.ThrowInSpeed)
                        0.0
                        BalanceConfig.ThrowInVz
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.ThrowInMomentum,
                                    -10.0,
                                    10.0
                                ) }

                s', [ event second thrower.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ]
            | None -> s, []

    let resolvePenalty
        (second: int)
        (s: MatchState)
        (kicker: Player)
        (kickerClub: ClubSide)
        (kickNum: int)
        : MatchState * MatchEvent list =
        let clubId = ClubSide.toClubId kickerClub s

        let gk =
            side (ClubSide.toClubId (ClubSide.flip kickerClub) s) s
            |> fun side -> side.Players |> Array.tryFind (fun p -> p.Position = GK)

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

        if scored then
            let s1, goalEvents = awardGoal kickerClub (Some kicker.Id) (95 * 60 + kickNum) s

            let penaltyEvent =
                event (95 * 60 + kickNum) kicker.Id clubId (PenaltyAwarded scored)

            s1, goalEvents @ [ penaltyEvent ]
        else
            let s' =
                { s with
                    Ball =
                        { Position = defaultSpatial 50.0 50.0
                          Spin = Spin.zero
                          LastTouchBy = None
                          IsInPlay = true } }
                |> flipPossessionAndClearOffside kickerClub

            let penaltyEvent =
                event (95 * 60 + kickNum) kicker.Id clubId (PenaltyAwarded scored)

            s', [ penaltyEvent ]
