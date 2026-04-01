namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.MatchCalc
open FootballEngine.Stats
open MatchState
open MatchSpatial
open MatchPlayerDecision

module MatchPlayerAction =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

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
        (attIsHome: bool)
        (attSide: TeamSide)
        (defSide: TeamSide)
        (shooter: Player)
        (shooterId: PlayerId)
        (shooterCond: int)
        (bX: float)
        (s: MatchState)
        =
        let shooterIdx = attSide.Players |> Array.findIndex (fun p -> p.Id = shooterId)

        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
        let composure = pressureMultiplier attIsHome s * tacticsCfg.UrgencyMultiplier
        let u = MatchManager.urgency attIsHome s * tacticsCfg.UrgencyMultiplier
        let dist = (if attIsHome then 100.0 - bX else bX) * 0.15
        let clubId = if attIsHome then s.Home.Id else s.Away.Id

        let homeComposure =
            if attIsHome then
                BalanceConfig.HomeShotComposureBonus
            else
                0.0

        let finishing = calcShotPower shooter shooterCond composure u dist homeComposure

        let dirSign = if attIsHome then 1.0 else -1.0
        let finishingNorm = Math.Clamp(finishing / 300.0, 0.2, 1.0)
        let speed = 20.0 + finishingNorm * 30.0
        let angleSpread = 0.3 * (1.0 - finishingNorm)
        let angle = normalSample 0.0 angleSpread
        let vx = dirSign * speed * Math.Cos(angle)
        let vy = speed * Math.Sin(angle)
        let vz = normalSample 4.0 1.5 |> Math.Abs

        let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)
        let onTarget = bernoulli (0.5 + finishingNorm * 0.3)

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
                            LastTouchBy = Some shooter.Id }
                    Momentum = Math.Clamp(s''.Momentum + (if attIsHome then 0.5 else -0.5), -10.0, 10.0) }

        let gkClubId = if attIsHome then s.Away.Id else s.Home.Id

        let events =
            match gk with
            | Some g when gkSaves -> [ event second shooter.Id clubId ShotBlocked; event second g.Id gkClubId Save ]
            | _ when not onTarget -> [ event second shooter.Id clubId ShotOffTarget ]
            | _ -> []

        s', events, PlayerIdle

    let private resolveDuel
        (homeId: ClubId)
        (second: int)
        (att: Player)
        (def: Player)
        (s: MatchState)
        =
        let attIsHome = s.Possession = Home
        let attSide = side attIsHome s
        let defSide = side (not attIsHome) s
        let ai = attSide.Players |> Array.tryFindIndex (fun p -> p.Id = att.Id) |> Option.defaultValue 0
        let di = defSide.Players |> Array.tryFindIndex (fun p -> p.Id = def.Id) |> Option.defaultValue 0
        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
        let homeBonus = if attIsHome then BalanceConfig.HomeDuelAttackBonus else 0.0

        let homeDefBonus =
            if not attIsHome then
                BalanceConfig.HomeDuelDefenseBonus
            else
                0.0

        let skillBonus = float (att.CurrentSkill - def.CurrentSkill) * 0.12
        let moraleBonus = humanPerformance 1 att.Morale att.Morale * 0.05
        let condBonus = physicalVariation att.Condition * 3.0

        let attClubId = clubIdOf att s
        let defClubId = clubIdOf def s

        let repBonus =
            let attRep = (if attClubId = s.Home.Id then s.Home else s.Away).Reputation
            let defRep = (if defClubId = s.Home.Id then s.Home else s.Away).Reputation
            float (attRep - defRep) * 0.002

        let momentum = if s.Possession = Home then s.Momentum else -s.Momentum
        let pressure = (pressureMultiplier attIsHome s - 1.0) * 5.0
        let u = MatchManager.urgency attIsHome s * tacticsCfg.UrgencyMultiplier
        let pressNoise = pressureNoise att.Mental.Composure u
        let bX, bY = ballXY s
        let aX, aY = spatialXY attSide.Positions[ai]
        let dX, dY = spatialXY defSide.Positions[di]

        let diff =
            attackEffort (phaseFromBallZone bX) att attSide.Conditions[ai] * u + homeBonus
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
            if logisticBernoulli diff 0.50 then
                let nx, ny = PitchMath.jitter bX bY aX aY 0.5 10.0 10.0

                { s with
                    Ball =
                        { s.Ball with
                            Position = { s.Ball.Position with X = nx; Y = ny } }
                    Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
            elif logisticBernoulli (-diff) 0.35 then
                let nx, ny = PitchMath.jitter bX bY dX dY 0.5 2.0 2.0

                { s with
                    Possession = flipPossession s.Possession
                    Ball =
                        { s.Ball with
                            Position = { s.Ball.Position with X = nx; Y = ny } }
                    Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }
            else
                let nx, ny = PitchMath.jitter bX bY bX bY 0.0 3.0 3.0

                { s with
                    Ball =
                        { s.Ball with
                            Position = { s.Ball.Position with X = nx; Y = ny } } }

        let next =
            if s'.Possession = s.Possession then
                decideNextAttack att s'
            else
                PlayerIdle

        s', [], next

    let private resolveShot (second: int) (attacker: Player) (s: MatchState) =
        let bX, _ = ballXY s

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s, [], PlayerIdle
        else
            let attIsHome = s.Possession = Home
            let quality = shotQuality bX attacker attIsHome

            if quality < BalanceConfig.ShotQualityGate then
                s, [], ExecutePass attacker
            else
                let attSide = side attIsHome s
                let defSide = side (not attIsHome) s

                let info =
                    getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

                resolveShotOutcome second attIsHome attSide defSide info.Player info.PlayerId info.Condition bX s

    let private resolvePass (second: int) (attacker: Player) (s: MatchState) =
        let isHome = s.Possession = Home
        let attSide = side isHome s
        let defSide = side (not isHome) s
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let condition = float info.Condition / 100.0

        let passMean =
            0.65
            + float attacker.Technical.Passing / 100.0 * 0.2
            + float attacker.Mental.Vision / 100.0 * 0.1
            + if isHome then BalanceConfig.HomePassAccuracyBonus else 0.0

        let successChance = betaSample passMean (8.0 + condition * 12.0)

        match findBestPassTarget attacker s isHome with
        | Some(teammate, teammateId, _) ->
            let teammateIdx = attSide.Players |> Array.findIndex (fun p -> p.Id = teammateId)
            let offside = isOffsideCheck teammate attSide.Positions[teammateIdx].X s isHome

            if offside then
                { s with
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) },
                [ event second attacker.Id clubId (PassIncomplete attacker.Id) ],
                ExecuteTackle attacker
            elif bernoulli successChance then
                let sp = attSide.Positions[teammateIdx]

                let s' =
                    { s with
                        Ball =
                            { s.Ball with
                                Position =
                                    { s.Ball.Position with
                                        X = sp.X
                                        Y = sp.Y
                                        Z = 0.0 } }
                        Momentum = Math.Clamp(s.Momentum + (if isHome then 0.3 else -0.3), -10.0, 10.0) }

                let next = decideNextAttack teammate s'

                s', [ event second attacker.Id clubId (PassCompleted(attacker.Id, teammate.Id)) ], next
            else
                { s with
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.5 else -0.5), -10.0, 10.0) },
                [ event second attacker.Id clubId (PassIncomplete attacker.Id) ],
                ExecuteTackle attacker
        | None -> s, [], ExecuteDribble attacker

    let private resolveDribble (second: int) (attacker: Player) (s: MatchState) =
        let isHome = s.Possession = Home
        let attSide = side isHome s
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let ax, ay = spatialXY info.Pos
        let condition = float info.Condition / 100.0
        let physVar = physicalVariation attacker.Condition

        let attScore =
            float attacker.Technical.Dribbling * 0.5
            + float attacker.Physical.Agility * 0.3
            + float attacker.Physical.Balance * 0.2
            + if isHome then BalanceConfig.HomeDribbleBonus else 0.0

        let defender, _, _ = findNearestOpponent attacker s isHome

        let defScore =
            float defender.Technical.Tackling * 0.6 + float defender.Physical.Strength * 0.4

        let duelScore = (attScore - defScore) / 100.0 * condition * physVar

        if logisticBernoulli duelScore 1.5 then
            let pushX = if isHome then 5.0 else -5.0

            let s' =
                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { s.Ball.Position with
                                    X = Math.Clamp(ax + pushX, 5.0, 95.0)
                                    Y = ay } }
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.4 else -0.4), -10.0, 10.0) }

            let next =
                pickWeighted
                    [ 0.10, ExecuteCross attacker
                      0.60, ExecutePass attacker
                      0.30, ExecuteShot attacker ]

            s', [ event second attacker.Id clubId DribbleSuccess ], next
        else
            { s with
                Possession = flipPossession s.Possession
                Momentum = Math.Clamp(s.Momentum - (if isHome then 0.6 else -0.6), -10.0, 10.0) },
            [ event second attacker.Id clubId DribbleFail ],
            ExecuteTackle defender

    let private resolveCross (second: int) (attacker: Player) (s: MatchState) =
        let isHome = s.Possession = Home
        let attSide = side isHome s
        let defSide = side (not isHome) s
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let condition = float info.Condition / 100.0

        let crossMean =
            0.50
            + float attacker.Technical.Crossing / 100.0 * 0.25
            + float attacker.Technical.Passing / 100.0 * 0.10
            + if isHome then
                  BalanceConfig.HomeSetPlayAccuracyBonus
              else
                  0.0

        let successChance = betaSample crossMean (6.0 + condition * 8.0)

        let targets =
            attSide.Players
            |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
            |> Array.filter (fun (_, p, _) ->
                p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)
            |> Array.sortBy (fun (_, _, sp) ->
                let defDist =
                    defSide.Players
                    |> Array.filter (fun p -> p.Position <> GK)
                    |> Array.map (fun d ->
                        let dIdx =
                            defSide.Players
                            |> Array.tryFindIndex (fun x -> x.Id = d.Id)
                            |> Option.defaultValue 0

                        let dSp = defSide.Positions[dIdx]
                        let dx = dSp.X - sp.X
                        let dy = dSp.Y - sp.Y
                        dx * dx + dy * dy)
                    |> Array.min

                -(defDist))

        if targets.Length > 0 && bernoulli successChance then
            let targetIdx, target, targetSp = targets[0]
            let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

            let gkSkill =
                gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

            let headerScore =
                float (target.Physical.Strength + target.Technical.Heading) / 200.0
                * physicalVariation attSide.Conditions[targetIdx]

            let gkScore = gkSkill / 150.0

            let nearDefs =
                defSide.Players
                |> Array.filter (fun p -> p.Position <> GK)
                |> Array.sumBy (fun p -> float p.Mental.Positioning / 200.0)
                |> fun v -> v / float (max 1 defSide.Players.Length)

            if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                awardGoal (if isHome then Home else Away) (Some target.Id) second s
                |> fun (s', evs) -> s', (event second attacker.Id clubId (CrossAttempt true)) :: evs, PlayerIdle
            else
                { s with
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) },
                [ event second attacker.Id clubId (CrossAttempt true)
                  event second target.Id clubId ShotBlocked ],
                ExecuteTackle target
        else
            { s with
                Possession = flipPossession s.Possession
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = (if isHome then 85.0 else 15.0)
                                Y = 50.0 } } },
            [ event second attacker.Id clubId (CrossAttempt false) ],
            if bernoulli BalanceConfig.CornerOnFailedCross then
                ExecuteCorner
            else
                PlayerIdle

    let private resolveLongBall (second: int) (attacker: Player) (s: MatchState) =
        let isHome = s.Possession = Home
        let attSide = side isHome s
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let condition = float info.Condition / 100.0

        let longMean =
            0.40
            + float attacker.Technical.LongShots / 100.0 * 0.2
            + float attacker.Technical.Passing / 100.0 * 0.2
            + float attacker.Mental.Vision / 100.0 * 0.15
            + if isHome then
                  BalanceConfig.HomeSetPlayAccuracyBonus
              else
                  0.0

        let successChance = betaSample longMean (5.0 + condition * 10.0)

        let forwards =
            attSide.Players
            |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
            |> Array.filter (fun (_, p, _) ->
                p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

        if bernoulli successChance && forwards.Length > 0 then
            let targetIdx, target, targetSp = forwards[0]
            let offside = isOffsideCheck target targetSp.X s isHome

            if offside then
                { s with
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.4 else -0.4), -10.0, 10.0) },
                [ event second attacker.Id clubId (LongBall false) ],
                ExecuteTackle attacker
            else
                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { s.Ball.Position with
                                    X = targetSp.X
                                    Y = targetSp.Y } }
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.5 else -0.5), -10.0, 10.0) },
                [ event second attacker.Id clubId (LongBall true) ],
                ExecuteDribble target
        else
            { s with
                Possession = flipPossession s.Possession
                Momentum = Math.Clamp(s.Momentum - (if isHome then 0.4 else -0.4), -10.0, 10.0) },
            [ event second attacker.Id clubId (LongBall false) ],
            ExecuteTackle attacker

    let private resolveTackle (second: int) (defender: Player) (s: MatchState) =
        let isHome = s.Possession = Home
        let clubId = if isHome then s.Away.Id else s.Home.Id

        let defenderIsHome = clubIdOf defender s = s.Home.Id
        let defenderSide = side defenderIsHome s
        let attackerSide = side (not defenderIsHome) s

        let info =
            getPlayerInfo defenderSide.Players defenderSide.Positions defenderSide.Conditions defender.Id

        let condition = float info.Condition / 100.0

        let defScore =
            float defender.Technical.Tackling * 0.5
            + float defender.Mental.Positioning * 0.3
            + float defender.Physical.Strength * 0.2
            + if not isHome then BalanceConfig.HomeTackleBonus else 0.0

        let bX, bY = ballXY s

        let _, attacker, _ =
            attackerSide.Players
            |> Array.mapi (fun i p -> i, p, attackerSide.Positions[i])
            |> Array.minBy (fun (_, _, sp) -> let dx, dy = sp.X - bX, sp.Y - bY in sqrt (dx * dx + dy * dy))

        let attScore =
            float attacker.Technical.Dribbling * 0.5
            + float attacker.Physical.Agility * 0.3
            + float attacker.Physical.Balance * 0.2

        let physVar = physicalVariation defender.Condition
        let duelScore = (defScore - attScore) / 100.0 * condition * physVar

        if logisticBernoulli duelScore 1.5 then
            let aggression = float defender.Mental.Aggression / 100.0
            let positioning = float defender.Mental.Positioning / 100.0

            let foulChance =
                betaSample (BalanceConfig.FoulBaseRate + aggression * 0.15 - positioning * 0.1 |> Math.Abs) 10.0

            if bernoulli foulChance then
                { s with
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) },
                [ event second defender.Id clubId FoulCommitted ],
                ExecuteFreeKick attacker
            else
                { s with
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.8 else -0.8), -10.0, 10.0) },
                [ event second defender.Id clubId TackleSuccess ],
                ExecutePass defender
        else
            { s with
                Momentum = Math.Clamp(s.Momentum - (if isHome then 0.5 else -0.5), -10.0, 10.0) },
            [ event second defender.Id clubId TackleFail ],
            PlayerIdle

    let private resolveFreeKick (second: int) (kicker: Player) (s: MatchState) =
        let clubId = clubIdOf kicker s
        let bY = s.Ball.Position.Y
        let isHomeKicker = clubId = s.Home.Id

        let shotPower =
            effectiveStat
                kicker.Technical.Finishing
                kicker.Condition
                kicker.Morale
                (finishingBonusForPosition kicker.Position)
            + effectiveStat
                kicker.Mental.Composure
                kicker.Condition
                kicker.Morale
                (1.5
                 + if isHomeKicker then
                       BalanceConfig.HomeFreeKickComposure
                   else
                       0.0)
            + effectiveStat kicker.Technical.LongShots kicker.Condition kicker.Morale 1.0
            + pressureNoise kicker.Mental.Composure 1.1

        let gk =
            if isHomeKicker then
                s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK)
            else
                s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)

        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> normalSample 50.0 10.0

        let scored = shotPower > savePower + 120.0 + normalSample 0.0 25.0

        let s' =
            if scored then
                let scoringTeam = if isHomeKicker then Home else Away
                let s1, _ = awardGoal scoringTeam (Some kicker.Id) second s
                s1
            else
                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { s.Ball.Position with
                                    X = (if isHomeKicker then 75.0 else 25.0)
                                    Y = bY } }
                    Possession = if isHomeKicker then Away else Home }

        let gkClubId = if isHomeKicker then s.Away.Id else s.Home.Id

        let events =
            [ yield event s.Second kicker.Id clubId (FreeKick scored)
              if scored then
                  yield event s.Second kicker.Id clubId Goal
              elif not scored then
                  match gk with
                  | Some g -> yield event s.Second g.Id gkClubId Save
                  | None -> () ]

        let s'' =
            if bernoulli BalanceConfig.PostShotClearProbability then
                { s' with
                    Ball =
                        { s'.Ball with
                            Position =
                                { s'.Ball.Position with
                                    X = 50.0
                                    Y = 50.0 + normalSample 0.0 10.0
                                    Vx = 0.0
                                    Vy = 0.0
                                    Vz = 0.0 } } }
            else
                s'

        s'', events, PlayerIdle

    let private resolveCorner (second: int) (s: MatchState) =
        let isHomeCorner = s.Possession = Home
        let clubId = if isHomeCorner then s.Home.Id else s.Away.Id
        let attSide = side isHomeCorner s
        let defSide = side (not isHomeCorner) s

        if attSide.Players.Length = 0 then
            s, [], PlayerIdle
        else
            let attackersInBox =
                attSide.Players
                |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
                |> Array.filter (fun (_, p, sp) ->
                    (p.Position = ST
                     || p.Position = AML
                     || p.Position = AMR
                     || p.Position = AMC
                     || p.Position = MC
                     || p.Position = DC)
                    && sp.X > (if isHomeCorner then 75.0 else 25.0))

            let defendersInBox =
                defSide.Players
                |> Array.filter (fun p -> p.Position <> GK)
                |> Array.mapi (fun i p -> i, p, defSide.Positions[i])
                |> Array.filter (fun (_, _, sp) -> sp.X > (if isHomeCorner then 70.0 else 30.0))

            let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

            if attackersInBox.Length = 0 then
                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { s.Ball.Position with
                                    X = (if isHomeCorner then 20.0 else 80.0)
                                    Y = 50.0 } }
                    Possession = flipPossession s.Possession },
                [ event second attSide.Players[0].Id clubId Corner ],
                PlayerIdle
            else
                let bestAttackerIdx, bestAttacker, bestAttackerSp =
                    attackersInBox
                    |> Array.maxBy (fun (_, p, _) -> p.Physical.Strength + p.Technical.Heading)

                let bestDefender =
                    defendersInBox
                    |> Array.sortByDescending (fun (_, p, _) -> p.Physical.Strength + p.Mental.Positioning)
                    |> Array.tryHead

                let attackScore =
                    float (bestAttacker.Physical.Strength + bestAttacker.Technical.Heading) / 200.0
                    * physicalVariation attSide.Conditions[bestAttackerIdx]

                let defScore =
                    bestDefender
                    |> Option.map (fun (_, d, _) -> float (d.Physical.Strength + d.Mental.Positioning) / 200.0)
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
                        float p.Technical.Crossing / 100.0 * 0.3
                        + float p.Technical.Passing / 100.0 * 0.2)
                    |> Option.defaultValue 0.3

                let scored =
                    logisticBernoulli (attackScore - defScore - gkBonus - densityPenalty + crossQuality) 2.5

                if scored then
                    awardGoal (if isHomeCorner then Home else Away) (Some bestAttacker.Id) second s
                    |> fun (s', evs) -> s', (event second bestAttacker.Id clubId Corner) :: evs, PlayerIdle
                else
                    let secondPhase =
                        if bernoulli 0.35 then
                            let outsidePlayers =
                                attSide.Players
                                |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
                                |> Array.filter (fun (_, p, sp) ->
                                    sp.X <= (if isHomeCorner then 75.0 else 25.0)
                                    && (p.Position = MC || p.Position = DM || p.Position = DC))

                            if outsidePlayers.Length > 0 then
                                let _, p, sp = outsidePlayers[0]
                                ExecuteShot p
                            else
                                ExecutePass bestAttacker
                        else
                            ExecuteTackle bestAttacker

                    { s with
                        Ball =
                            { s.Ball with
                                Position =
                                    { s.Ball.Position with
                                        X = (if isHomeCorner then 65.0 else 35.0)
                                        Y = 50.0 } }
                        Possession =
                            if bernoulli 0.55 then
                                s.Possession
                            else
                                flipPossession s.Possession },
                    [ event second bestAttacker.Id clubId Corner ],
                    secondPhase

    let private resolveThrowIn (second: int) (throwTeam: Possession) (s: MatchState) =
        let isHome = throwTeam = Home
        let attSide = side isHome s
        let clubId = if isHome then s.Home.Id else s.Away.Id

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

            let nearestTeammate = findNearestTeammate thrower s isHome

            match nearestTeammate with
            | Some(teammate, _, _) ->
                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { s.Ball.Position with
                                    X = (if isHome then 5.0 else 95.0)
                                    Y = s.Ball.Position.Y } }
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.1 else -0.1), -10.0, 10.0) },
                [ event second thrower.Id clubId (PassCompleted(thrower.Id, teammate.Id)) ],
                ExecutePass teammate
            | None -> s, [], PlayerIdle

    let private resolvePenalty (second: int) (kicker: Player) (isHome: bool) (kickNum: int) (s: MatchState) =
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let gk =
            if isHome then
                s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK)
            else
                s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)

        let gkSkill =
            gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

        let pressNoise = pressureNoise kicker.Mental.Composure 1.4

        let score =
            (float kicker.CurrentSkill - gkSkill) * 0.04
            + float kicker.Morale * 0.01
            + pressNoise * 0.01
            + if isHome then BalanceConfig.HomePenaltyBonus else 0.0

        let scored = logisticBernoulli score 0.8

        let s' =
            if scored then
                let scoringTeam = if isHome then Home else Away
                let s1, _ = awardGoal scoringTeam (Some kicker.Id) (95 * 60 + kickNum) s
                s1
            else
                { s with
                    Ball =
                        { Position = defaultSpatial 50.0 50.0
                          LastTouchBy = None }
                    Possession = if isHome then Away else Home }

        s', [ event (95 * 60 + kickNum) kicker.Id clubId (PenaltyAwarded scored) ], PlayerIdle

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
        | ExecuteThrowIn throwTeam -> resolveThrowIn second throwTeam s
        | ExecutePenalty(kicker, isHome, kickNum) -> resolvePenalty second kicker isHome kickNum s
