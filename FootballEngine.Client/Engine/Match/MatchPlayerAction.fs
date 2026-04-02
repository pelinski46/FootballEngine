namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.MatchCalc
open FootballEngine.Stats
open MatchStateOps
open MatchSpatial
open MatchPlayerDecision

module MatchPlayerAction =

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

    let private finishingBonusForPosition =
        function
        | ST
        | AMC
        | AML
        | AMR -> 2.0
        | MC -> 1.2
        | _ -> 0.5

    let private calcShotPower
        (player: Player)
        (condition: int)
        (composure: float)
        (urgency: float)
        (dist: float)
        (homeComposure: float)
        =
        let bonus = finishingBonusForPosition player.Position

        effectiveStat player.Technical.Finishing condition player.Morale (bonus * 0.8)
        + effectiveStat player.Mental.Composure condition player.Morale (1.5 * composure * urgency + homeComposure)
        + effectiveStat player.Technical.LongShots condition player.Morale 1.0
        + effectiveStat player.Physical.Pace condition player.Morale 0.5
        - dist

    let private resolveShotOutcome
        (second: int)
        (ctx: ActionContext)
        (attSide: TeamSide)
        (defSide: TeamSide)
        (shooter: Player)
        (shooterId: PlayerId)
        (shooterCond: int)
        (bX: float)
        (s: MatchState)
        =
        let shooterIdx = attSide.Players |> Array.findIndex (fun p -> p.Id = shooterId)
        let attClubId = ClubSide.toClubId ctx.AttSide s

        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
        let composure = pressureMultiplier attClubId s * tacticsCfg.UrgencyMultiplier
        let u = MatchManager.urgency attClubId s * tacticsCfg.UrgencyMultiplier

        let dist =
            AttackDir.distToGoal bX ctx.Dir * BalanceConfig.ShotDistanceToGoalMultiplier

        let clubId = attClubId
        let homeComposure = ctx.AttBonus.ShotCompos

        let finishing = calcShotPower shooter shooterCond composure u dist homeComposure

        let dirSign = AttackDir.forwardX ctx.Dir

        let finishingNorm =
            Math.Clamp(finishing / 300.0, BalanceConfig.ShotFinishingMin, BalanceConfig.ShotFinishingMax)

        let speed =
            BalanceConfig.ShotBaseSpeed + finishingNorm * BalanceConfig.ShotSpeedMultiplier

        let angleSpread = BalanceConfig.ShotAngleSpreadBase * (1.0 - finishingNorm)
        let angle = normalSample 0.0 angleSpread
        let vx = dirSign * speed * Math.Cos(angle)
        let vy = speed * Math.Sin(angle)

        let vz =
            normalSample BalanceConfig.ShotVzBase BalanceConfig.ShotVzVariance |> Math.Abs

        let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

        let onTarget =
            bernoulli (
                BalanceConfig.ShotOnTargetBase
                + finishingNorm * BalanceConfig.ShotOnTargetMultiplier
            )

        let gkSaves =
            onTarget
            && match gk with
               | Some g ->
                   let savePower =
                       effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                       + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5

                   let adjustedSave = savePower + BalanceConfig.GkSaveBonus
                   bernoulli (adjustedSave / (adjustedSave + finishing + 1.0))
               | None -> false

        let s' =
            s
            |> withBallVelocity vx vy vz
            |> fun s'' ->
                { s'' with
                    Ball =
                        { s''.Ball with
                            Position =
                                { s''.Ball.Position with
                                    X = bX
                                    Y = s''.Ball.Position.Y }
                            LastTouchBy = Some shooter.Id }
                    Momentum =
                        Math.Clamp(
                            s''.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.DuelMomentumBonus,
                            -10.0,
                            10.0
                        ) }

        let gkClubId = ClubSide.toClubId (ClubSide.flip ctx.AttSide) s

        let events =
            match gk with
            | Some g when gkSaves -> [ event second shooter.Id clubId ShotBlocked; event second g.Id gkClubId Save ]
            | _ when not onTarget -> [ event second shooter.Id clubId ShotOffTarget ]
            | _ -> []

        s', events, PlayerIdle

    let private resolveDuel (homeId: ClubId) (second: int) (att: Player) (def: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = ClubSide.teamSide ctx.AttSide s
        let defSide = ClubSide.teamSide ctx.DefSide s

        let ai =
            attSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = att.Id)
            |> Option.defaultValue 0

        let di =
            defSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = def.Id)
            |> Option.defaultValue 0

        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions

        let attackingBonus = ctx.AttBonus.AttackDuel
        let homeDefBonus = ctx.DefBonus.DefendDuel

        let skillBonus =
            float (att.CurrentSkill - def.CurrentSkill)
            * BalanceConfig.DuelSkillBonusMultiplier

        let moraleBonus =
            humanPerformance 1 att.Morale att.Morale
            * BalanceConfig.DuelMoraleBonusMultiplier

        let condBonus =
            physicalVariation att.Condition * BalanceConfig.DuelConditionBonusMultiplier

        let repBonus =
            let attRep = (if attClubId = s.Home.Id then s.Home else s.Away).Reputation
            let defRep = (if defClubId = s.Home.Id then s.Home else s.Away).Reputation
            float (attRep - defRep) * BalanceConfig.DuelReputationBonusMultiplier

        let momentum = ctx.Momentum

        let pressure = (pressureMultiplier attClubId s - 1.0) * 5.0
        let u = MatchManager.urgency attClubId s * tacticsCfg.UrgencyMultiplier
        let pressNoise = pressureNoise att.Mental.Composure u
        let bX, bY = ballXY s
        let aX, aY = spatialXY attSide.Positions[ai]
        let dX, dY = spatialXY defSide.Positions[di]

        let diff =
            attackEffort (phaseFromBallZone ctx.Dir bX) att attSide.Conditions[ai] * u
            + attackingBonus
            - homeDefBonus
            + skillBonus
            + moraleBonus
            + condBonus
            + repBonus
            + momentum
            + pressure
            + pressNoise
            - defenseEffort def defSide.Conditions[di]

        let s' =
            if logisticBernoulli diff BalanceConfig.DuelWinProbabilityBase then
                let nx, ny =
                    PitchMath.jitter bX bY aX aY 0.5 BalanceConfig.DuelJitterWin BalanceConfig.DuelJitterWin

                { s with
                    Ball =
                        { s.Ball with
                            Position = { s.Ball.Position with X = nx; Y = ny } }
                    Momentum = Math.Clamp(s.Momentum + BalanceConfig.DuelMomentumBonus, -10.0, 10.0) }
            elif logisticBernoulli (-diff) BalanceConfig.DuelRecoverProbabilityBase then
                let nx, ny =
                    PitchMath.jitter bX bY dX dY 0.5 BalanceConfig.DuelJitterRecover BalanceConfig.DuelJitterRecover

                { s with
                    AttackingClub = ClubSide.flip ctx.AttSide
                    Ball =
                        { s.Ball with
                            Position = { s.Ball.Position with X = nx; Y = ny } }
                    Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }
            else
                let nx, ny =
                    PitchMath.jitter bX bY bX bY 0.0 BalanceConfig.DuelJitterKeep BalanceConfig.DuelJitterKeep

                s |> ballTowards nx ny BalanceConfig.DuelSpeedKeep BalanceConfig.DuelSpeedKeepVz

        let next =
            if s'.AttackingClub = ctx.AttSide then
                decideNextAttack att s'
            else
                PlayerIdle

        s', [], next

    let private resolveShot (second: int) (attacker: Player) (s: MatchState) =
        let bX, _ = ballXY s
        let ctx = ActionContext.build s

        let inChance = ctx.Zone = AttackingZone || ctx.Zone = MidfieldZone

        if not inChance then
            s, [], PlayerIdle
        else
            let attClubId = ClubSide.toClubId ctx.AttSide s
            let quality = shotQuality bX attacker ctx.Dir

            if quality < BalanceConfig.ShotQualityGate then
                s, [], ExecutePass attacker
            else
                let attSide = ClubSide.teamSide ctx.AttSide s
                let defClubId = ClubSide.toClubId ctx.DefSide s
                let defSide = ClubSide.teamSide ctx.DefSide s

                match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
                | None -> s, [], PlayerIdle
                | Some info ->
                    resolveShotOutcome second ctx attSide defSide info.Player info.PlayerId info.Condition bX s

    let private resolvePass (second: int) (attacker: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = ClubSide.teamSide ctx.AttSide s
        let defSide = ClubSide.teamSide ctx.DefSide s
        let clubId = attClubId

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> s, [], PlayerIdle
        | Some info ->
            let condition = float info.Condition / 100.0

            let passMean =
                BalanceConfig.PassBaseMean
                + float attacker.Technical.Passing / 100.0 * BalanceConfig.PassTechnicalWeight
                + float attacker.Mental.Vision / 100.0 * BalanceConfig.PassVisionWeight
                + ctx.AttBonus.PassAcc

            let successChance =
                betaSample
                    passMean
                    (BalanceConfig.PassSuccessShapeAlpha
                     + condition * BalanceConfig.PassSuccessConditionMultiplier)

            match findBestPassTarget attacker s ctx.Dir with
            | Some(teammate, teammateId, (teammateSpX, teammateSpY)) ->
                let offside = isOffside teammate teammateSpX s ctx.Dir

                if offside then
                    { s with
                        AttackingClub = ClubSide.flip ctx.AttSide
                        Momentum =
                            Math.Clamp(
                                s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.PassOffsideMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second attacker.Id clubId (PassIncomplete attacker.Id) ],
                    ExecuteTackle attacker
                elif bernoulli successChance then

                    let s' =
                        s
                        |> ballTowards teammateSpX teammateSpY BalanceConfig.PassSpeed BalanceConfig.PassVz
                        |> fun s'' ->
                            { s'' with
                                Momentum =
                                    Math.Clamp(
                                        s''.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.PassSuccessMomentum,
                                        -10.0,
                                        10.0
                                    ) }

                    let next = decideNextAttack teammate s'
                    s', [ event second attacker.Id clubId (PassCompleted(attacker.Id, teammate.Id)) ], next
                else
                    { s with
                        AttackingClub = ClubSide.flip ctx.AttSide
                        Momentum =
                            Math.Clamp(
                                s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.PassFailMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second attacker.Id clubId (PassIncomplete attacker.Id) ],
                    ExecuteTackle attacker
            | None -> s, [], ExecuteDribble attacker

    let private resolveDribble (second: int) (attacker: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = ClubSide.teamSide ctx.AttSide s
        let clubId = attClubId

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> s, [], PlayerIdle
        | Some info ->
            let ax, ay = spatialXY info.Pos
            let condition = float info.Condition / 100.0
            let physVar = physicalVariation attacker.Condition

            let attScore =
                float attacker.Technical.Dribbling * BalanceConfig.DribbleTechnicalWeight
                + float attacker.Physical.Agility * BalanceConfig.DribbleAgilityWeight
                + float attacker.Physical.Balance * BalanceConfig.DribbleBalanceWeight
                + ctx.AttBonus.SetPlay

            match findNearestOpponent attacker s ctx.Dir with
            | None -> s, [], PlayerIdle
            | Some(defender, _, _) ->
                let defScore =
                    float defender.Technical.Tackling * BalanceConfig.TackleTechnicalWeight
                    + float defender.Physical.Strength * BalanceConfig.TackleStrengthWeight

                let duelScore = (attScore - defScore) / 100.0 * condition * physVar

                if logisticBernoulli duelScore 1.5 then
                    let targetX =
                        Math.Clamp(ax + AttackDir.forwardX ctx.Dir * BalanceConfig.DribbleForwardDistance, 5.0, 95.0)

                    let s' =
                        s
                        |> ballTowards targetX ay BalanceConfig.DribbleSpeed BalanceConfig.DribbleVz
                        |> fun s'' ->
                            { s'' with
                                Momentum =
                                    Math.Clamp(
                                        s''.Momentum
                                        + AttackDir.momentumDelta ctx.Dir BalanceConfig.DribbleSuccessMomentum,
                                        -10.0,
                                        10.0
                                    ) }

                    let next =
                        pickWeighted
                            [ BalanceConfig.DribbleCrossProbability, ExecuteCross attacker
                              BalanceConfig.DribblePassProbability, ExecutePass attacker
                              BalanceConfig.DribbleShotProbability, ExecuteShot attacker ]

                    s', [ event second attacker.Id clubId DribbleSuccess ], next
                else
                    { s with
                        AttackingClub = ClubSide.flip ctx.AttSide
                        Momentum =
                            Math.Clamp(
                                s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.DribbleFailMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second attacker.Id clubId DribbleFail ],
                    ExecuteTackle defender

    let private resolveCross (second: int) (attacker: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = ClubSide.teamSide ctx.AttSide s
        let defSide = ClubSide.teamSide ctx.DefSide s
        let clubId = attClubId

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> s, [], PlayerIdle
        | Some info ->
            let condition = float info.Condition / 100.0

            let crossMean =
                BalanceConfig.CrossBaseMean
                + float attacker.Technical.Crossing / 100.0 * BalanceConfig.CrossCrossingWeight
                + float attacker.Technical.Passing / 100.0 * BalanceConfig.CrossPassingWeight
                + ctx.AttBonus.SetPlay

            let successChance =
                betaSample
                    crossMean
                    (BalanceConfig.CrossSuccessShapeAlpha
                     + condition * BalanceConfig.CrossSuccessConditionMultiplier)

            let targets =
                teamRoster attSide
                |> Array.filter (fun (p, _, _) ->
                    p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)
                |> Array.sortBy (fun (_, sp, _) ->
                    let defDist =
                        outfieldRoster defSide
                        |> Array.map (fun (_, dSp, _) ->
                            let dx = dSp.X - sp.X
                            let dy = dSp.Y - sp.Y
                            dx * dx + dy * dy)
                        |> Array.min

                    -(defDist))

            if targets.Length > 0 && bernoulli successChance then
                let target, targetSp, _ = targets[0]
                let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

                let gkSkill =
                    gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

                let headerScore =
                    float (target.Physical.Strength + target.Technical.Heading) / 200.0
                    * physicalVariation info.Condition

                let gkScore = gkSkill / 150.0

                let nearDefs =
                    outfieldRoster defSide
                    |> Array.sumBy (fun (p, _, _) -> float p.Mental.Positioning / 200.0)
                    |> fun v -> v / float (max 1 (outfieldRoster defSide).Length)

                let blockPos =
                    nearestOutfield defSide targetSp.X targetSp.Y
                    |> Option.map (fun (_, dSp) -> dSp.X, dSp.Y)
                    |> Option.defaultValue (if ctx.Dir = LeftToRight then (20.0, 50.0) else (80.0, 50.0))

                if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                    awardGoal ctx.AttSide (Some target.Id) second s
                    |> fun (s', evs) -> s', (event second attacker.Id clubId (CrossAttempt true)) :: evs, PlayerIdle
                else
                    let bx, by = blockPos

                    let s' =
                        s
                        |> ballTowards bx by BalanceConfig.CrossSpeed BalanceConfig.CrossVz
                        |> fun s'' ->
                            { s'' with
                                AttackingClub = ClubSide.flip ctx.AttSide
                                Momentum =
                                    Math.Clamp(
                                        s''.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.CrossFailMomentum,
                                        -10.0,
                                        10.0
                                    ) }

                    s',
                    [ event second attacker.Id clubId (CrossAttempt true)
                      event second target.Id clubId ShotBlocked ],
                    ExecuteTackle target
            else
                let targetX = if ctx.Dir = LeftToRight then 85.0 else 15.0

                let s' =
                    s
                    |> ballTowards targetX 50.0 15.0 2.0
                    |> fun s'' ->
                        { s'' with
                            AttackingClub = ClubSide.flip ctx.AttSide }

                s',
                [ event second attacker.Id clubId (CrossAttempt false) ],
                if bernoulli BalanceConfig.CornerOnFailedCross then
                    ExecuteCorner
                else
                    PlayerIdle

    let private resolveLongBall (second: int) (attacker: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = ClubSide.teamSide ctx.AttSide s
        let clubId = attClubId

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> s, [], PlayerIdle
        | Some info ->
            let condition = float info.Condition / 100.0

            let longMean =
                BalanceConfig.LongBallBaseMean
                + float attacker.Technical.LongShots / 100.0
                  * BalanceConfig.LongBallLongShotsWeight
                + float attacker.Technical.Passing / 100.0 * BalanceConfig.LongBallPassingWeight
                + float attacker.Mental.Vision / 100.0 * BalanceConfig.LongBallVisionWeight
                + ctx.AttBonus.SetPlay

            let successChance =
                betaSample
                    longMean
                    (BalanceConfig.LongBallSuccessShapeAlpha
                     + condition * BalanceConfig.LongBallSuccessConditionMultiplier)

            let forwards =
                teamRoster attSide
                |> Array.filter (fun (p, _, _) ->
                    p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

            if bernoulli successChance && forwards.Length > 0 then
                let target, targetSp, _ = forwards[0]
                let offside = isOffside target targetSp.X s ctx.Dir

                if offside then
                    { s with
                        AttackingClub = ClubSide.flip ctx.AttSide
                        Momentum =
                            Math.Clamp(
                                s.Momentum
                                - AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallOffsideMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second attacker.Id clubId (LongBall false) ],
                    ExecuteTackle attacker
                else
                    let s' =
                        s
                        |> ballTowards targetSp.X targetSp.Y BalanceConfig.LongBallSpeed BalanceConfig.LongBallVz
                        |> fun s'' ->
                            { s'' with
                                Momentum =
                                    Math.Clamp(
                                        s''.Momentum
                                        + AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallSuccessMomentum,
                                        -10.0,
                                        10.0
                                    ) }

                    s', [ event second attacker.Id clubId (LongBall true) ], ExecuteDribble target
            else
                { s with
                    AttackingClub = ClubSide.flip ctx.AttSide
                    Momentum =
                        Math.Clamp(
                            s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallFailMomentum,
                            -10.0,
                            10.0
                        ) },
                [ event second attacker.Id clubId (LongBall false) ],
                ExecuteTackle attacker

    let private resolveTackle (second: int) (defender: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let possessorClubId = ClubSide.toClubId ctx.AttSide s
        let defenderClubId = ClubSide.toClubId ctx.DefSide s
        let clubId = defenderClubId

        let defenderSide = ClubSide.teamSide ctx.DefSide s
        let attackerSide = ClubSide.teamSide ctx.AttSide s

        match getPlayerInfo defenderSide.Players defenderSide.Positions defenderSide.Conditions defender.Id with
        | None -> s, [], PlayerIdle
        | Some info ->
            let condition = float info.Condition / 100.0

            let defScore =
                float defender.Technical.Tackling * BalanceConfig.TackleTechnicalWeight
                + float defender.Mental.Positioning * BalanceConfig.TacklePositioningWeight
                + float defender.Physical.Strength * BalanceConfig.TackleStrengthWeight
                + ctx.DefBonus.Tackle

            let bX, bY = ballXY s

            let attacker =
                nearestOutfield attackerSide bX bY
                |> Option.map (fun (p, _) -> p)
                |> Option.defaultValue (attackerSide.Players[0])

            let attScore =
                float attacker.Technical.Dribbling * BalanceConfig.DribbleTechnicalWeight
                + float attacker.Physical.Agility * BalanceConfig.DribbleAgilityWeight
                + float attacker.Physical.Balance * BalanceConfig.DribbleBalanceWeight

            let physVar = physicalVariation defender.Condition
            let duelScore = (defScore - attScore) / 100.0 * condition * physVar

            if logisticBernoulli duelScore 1.5 then
                let aggression = float defender.Mental.Aggression / 100.0
                let positioning = float defender.Mental.Positioning / 100.0

                let baseFoulRate =
                    BalanceConfig.FoulBaseRate + aggression * BalanceConfig.TackleAggressionWeight
                    - positioning * BalanceConfig.TacklePositioningReduction
                    |> Math.Abs

                let adjustedFoulRate = baseFoulRate * (1.0 - ctx.DefBonus.CardReduc)

                let foulChance = betaSample adjustedFoulRate BalanceConfig.TackleFoulShapeBeta

                if bernoulli foulChance then
                    { s with
                        AttackingClub = ClubSide.flip ctx.AttSide
                        Momentum =
                            Math.Clamp(
                                s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleFoulMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second defender.Id clubId FoulCommitted ],
                    PlayerIdle
                else
                    { s with
                        Momentum =
                            Math.Clamp(
                                s.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleSuccessMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second defender.Id clubId TackleSuccess ],
                    ExecutePass defender
            else
                { s with
                    Momentum =
                        Math.Clamp(
                            s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleFailMomentum,
                            -10.0,
                            10.0
                        ) },
                [ event second defender.Id clubId TackleFail ],
                PlayerIdle

    let private resolveFreeKick (second: int) (kicker: Player) (s: MatchState) =
        let ctx = ActionContext.build s
        let clubId = ClubSide.toClubId ctx.AttSide s
        let bY = s.Ball.Position.Y

        let shotPower =
            effectiveStat
                kicker.Technical.Finishing
                kicker.Condition
                kicker.Morale
                (finishingBonusForPosition kicker.Position)
            + effectiveStat kicker.Mental.Composure kicker.Condition kicker.Morale (1.5 + ctx.AttBonus.FreeKick)
            + effectiveStat kicker.Technical.LongShots kicker.Condition kicker.Morale 1.0
            + pressureNoise kicker.Mental.Composure BalanceConfig.PenaltyComposureNoise

        let gk =
            ClubSide.teamSide (ClubSide.flip ctx.AttSide) s
            |> fun side -> side.Players |> Array.tryFind (fun p -> p.Position = GK)

        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> normalSample 50.0 10.0

        let scored =
            shotPower > savePower
                        + BalanceConfig.FreeKickSavePowerThreshold
                        + normalSample 0.0 BalanceConfig.FreeKickSaveVariance

        if scored then
            let s1, goalEvents = awardGoal ctx.AttSide (Some kicker.Id) second s
            let fkEvent = event s.Second kicker.Id clubId (FreeKick true)
            s1, fkEvent :: goalEvents, PlayerIdle
        else
            let targetX =
                if ctx.Dir = LeftToRight then
                    BalanceConfig.FreeKickTargetX
                else
                    25.0

            let s' =
                s
                |> ballTowards targetX bY BalanceConfig.FreeKickSpeed BalanceConfig.FreeKickVz
                |> fun s'' ->
                    { s'' with
                        AttackingClub = ClubSide.flip ctx.AttSide }

            let gkClubId = ClubSide.toClubId (ClubSide.flip ctx.AttSide) s

            let events =
                [ event s.Second kicker.Id clubId (FreeKick false)
                  match gk with
                  | Some g -> event s.Second g.Id gkClubId Save
                  | None -> () ]

            let s'' =
                if bernoulli BalanceConfig.PostShotClearProbability then
                    let clearY = 50.0 + normalSample 0.0 10.0
                    s' |> ballTowards 50.0 clearY 16.0 1.5
                else
                    s'

            s'', events, PlayerIdle

    let private resolveCorner (second: int) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let clubId = attClubId
        let attSide = ClubSide.teamSide ctx.AttSide s
        let defSide = ClubSide.teamSide ctx.DefSide s

        if attSide.Players.Length = 0 then
            s, [], PlayerIdle
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
                    |> fun s'' ->
                        { s'' with
                            AttackingClub = ClubSide.flip ctx.AttSide }

                s', [ event second attSide.Players[0].Id clubId Corner ], PlayerIdle
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
                    awardGoal ctx.AttSide (Some bestAttacker.Id) second s
                    |> fun (s', evs) -> s', (event second bestAttacker.Id clubId Corner) :: evs, PlayerIdle
                else
                    let secondPhase =
                        if bernoulli BalanceConfig.CornerSecondPhaseProbability then
                            let outsidePlayers =
                                teamRoster attSide
                                |> Array.filter (fun (p, sp, _) ->
                                    (if ctx.Dir = LeftToRight then sp.X <= 75.0 else sp.X >= 25.0)
                                    && (p.Position = MC || p.Position = DM || p.Position = DC))

                            if outsidePlayers.Length > 0 then
                                let p, _, _ = outsidePlayers[0]
                                ExecuteShot p
                            else
                                ExecutePass bestAttacker
                        else
                            ExecuteTackle bestAttacker

                    let targetX = if ctx.Dir = LeftToRight then 65.0 else 35.0

                    let s' =
                        s
                        |> ballTowards targetX 50.0 BalanceConfig.CornerSpeed BalanceConfig.CornerVz
                        |> fun s'' ->
                            { s'' with
                                AttackingClub =
                                    if bernoulli BalanceConfig.CornerKeepPossessionProbability then
                                        s''.AttackingClub
                                    else
                                        ClubSide.flip s''.AttackingClub }

                    s', [ event second bestAttacker.Id clubId Corner ], secondPhase

    let private resolveThrowIn (second: int) (throwClub: ClubSide) (s: MatchState) =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId throwClub s
        let attSide = ClubSide.teamSide throwClub s
        let clubId = attClubId

        if attSide.Players.Length = 0 then
            s, [], PlayerIdle
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
            | Some(teammate, _, _) ->
                let targetX = if ctx.Dir = LeftToRight then 5.0 else 95.0

                let s' =
                    s
                    |> ballTowards targetX s.Ball.Position.Y BalanceConfig.ThrowInSpeed BalanceConfig.ThrowInVz
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.ThrowInMomentum,
                                    -10.0,
                                    10.0
                                ) }

                s', [ event second thrower.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ], ExecutePass teammate
            | None -> s, [], PlayerIdle

    let private resolvePenalty (second: int) (kicker: Player) (kickerClub: ClubSide) (kickNum: int) (s: MatchState) =
        let clubId = ClubSide.toClubId kickerClub s

        let gk =
            ClubSide.teamSide (ClubSide.flip kickerClub) s
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

        let result =
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
                              LastTouchBy = None }
                        AttackingClub = ClubSide.flip kickerClub }

                let penaltyEvent =
                    event (95 * 60 + kickNum) kicker.Id clubId (PenaltyAwarded scored)

                s', [ penaltyEvent ]

        fst result, snd result, PlayerIdle

    let resolve
        (homeId: ClubId)
        (second: int)
        (intent: PlayerIntent)
        (s: MatchState)
        : MatchState * MatchEvent list * PlayerIntent =

        match intent with

        | PlayerIdle -> s, [], PlayerIdle
        | ResolveDuel(att, def) -> resolveDuel homeId second att def s
        | ExecuteShot attacker -> resolveShot second attacker s
        | ExecutePass attacker -> resolvePass second attacker s
        | ExecuteDribble attacker -> resolveDribble second attacker s
        | ExecuteCross attacker -> resolveCross second attacker s
        | ExecuteLongBall attacker -> resolveLongBall second attacker s
        | ExecuteTackle defender -> resolveTackle second defender s
        | ExecuteFreeKick kicker -> resolveFreeKick second kicker s
        | ExecuteCorner -> resolveCorner second s
        | ExecuteThrowIn throwClub -> resolveThrowIn second throwClub s
        | ExecutePenalty(kicker, kickerClub, kickNum) -> resolvePenalty second kicker kickerClub kickNum s
