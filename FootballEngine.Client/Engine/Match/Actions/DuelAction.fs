namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchCalc
open MatchSpatial

module DuelAction =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let private fatigueMultiplier (condition: int) : float =
        if condition < BalanceConfig.DuelFatigueExponentialThreshold then
            let deficit = float (BalanceConfig.DuelFatigueExponentialThreshold - condition)
            exp (-deficit * BalanceConfig.DuelFatigueExponentialDecay)
        else
            1.0

    let private attributeScore (attacker: Player) (attackerCond: int) (defender: Player) (defenderCond: int) : float =
        let attRaw =
            float attacker.Technical.Dribbling * BalanceConfig.DuelAttackerDribblingWeight
            + float attacker.Physical.Agility * BalanceConfig.DuelAttackerAgilityWeight
            + float attacker.Physical.Balance * BalanceConfig.DuelAttackerBalanceWeight

        let defRaw =
            float defender.Technical.Tackling * BalanceConfig.DuelDefenderTacklingWeight
            + float defender.Physical.Strength * BalanceConfig.DuelDefenderStrengthWeight
            + float defender.Mental.Positioning * BalanceConfig.DuelDefenderPositionWeight

        let attFatigue = fatigueMultiplier attackerCond
        let defFatigue = fatigueMultiplier defenderCond

        attRaw * attFatigue - defRaw * defFatigue

    let resolve (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s

        let bX, bY = ballXY s

        let attIdx =
            attSide.Positions
            |> Array.mapi (fun i _ -> i)
            |> Array.minBy (fun i ->
                let dx = attSide.Positions[i].X - bX
                let dy = attSide.Positions[i].Y - bY
                dx * dx + dy * dy)

        let defIdx =
            defSide.Positions
            |> Array.mapi (fun i _ -> i)
            |> Array.minBy (fun i ->
                let dx = defSide.Positions[i].X - bX
                let dy = defSide.Positions[i].Y - bY
                dx * dx + dy * dy)

        let att = attSide.Players[attIdx]
        let def = defSide.Players[defIdx]

        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions

        let attackingBonus = ctx.AttBonus.AttackDuel
        let homeDefBonus = ctx.DefBonus.DefendDuel

        let attrDiff =
            attributeScore att attSide.Conditions[attIdx] def defSide.Conditions[defIdx]

        let moraleBonus =
            humanPerformance 1 att.Morale att.Morale
            * BalanceConfig.DuelMoraleBonusMultiplier

        let repBonus =
            let attRep = (if attClubId = s.Home.Id then s.Home else s.Away).Reputation
            let defRep = (if defClubId = s.Home.Id then s.Home else s.Away).Reputation
            float (attRep - defRep) * BalanceConfig.DuelReputationBonusMultiplier

        let momentum = ctx.Momentum
        let pressure = (pressureMultiplier attClubId s - 1.0) * 5.0
        let u = matchUrgency attClubId s * tacticsCfg.UrgencyMultiplier
        let pressNoise = pressureNoise att.Mental.Composure u

        let diff =
            attackEffort (phaseFromBallZone ctx.Dir bX) att attSide.Conditions[attIdx] * u
            + attackingBonus
            - homeDefBonus
            + attrDiff
            + moraleBonus
            + repBonus
            + momentum
            + pressure
            + pressNoise
            - defenseEffort def defSide.Conditions[defIdx]

        let aX, aY = spatialXY attSide.Positions[attIdx]
        let dX, dY = spatialXY defSide.Positions[defIdx]

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

                s
                |> flipPossessionAndClearOffside ctx.AttSide
                |> fun s'' ->
                    { s'' with
                        Ball =
                            { s''.Ball with
                                Position =
                                    { s''.Ball.Position with
                                        X = nx
                                        Y = ny } }
                        Momentum = Math.Clamp(s''.Momentum - 1.0, -10.0, 10.0) }
            else
                let nx, ny =
                    PitchMath.jitter bX bY bX bY 0.0 BalanceConfig.DuelJitterKeep BalanceConfig.DuelJitterKeep

                s
                |> withBallVelocity
                    ((nx - bX) * BalanceConfig.DuelSpeedKeep)
                    ((ny - bY) * BalanceConfig.DuelSpeedKeep)
                    BalanceConfig.DuelSpeedKeepVz

        s', []

    let resolveTackle (second: int) (s: MatchState) (defender: Player) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let defClubId = clubIdOf defender s
        let clubId = defClubId
        let defenderSide = side (ClubSide.toClubId ctx.DefSide s) s
        let attackerSide = side (ClubSide.toClubId ctx.AttSide s) s

        match defenderSide.Players |> Array.tryFindIndex (fun p -> p.Id = defender.Id) with
        | None -> s, []
        | Some di ->
            let condition = float defenderSide.Conditions[di] / 100.0

            let defScore =
                float defender.Technical.Tackling * BalanceConfig.TackleTechnicalWeight
                + float defender.Mental.Positioning * BalanceConfig.TacklePositioningWeight
                + float defender.Physical.Strength * BalanceConfig.TackleStrengthWeight
                + ctx.DefBonus.Tackle

            let bX, bY = ballXY s

            let attacker =
                nearestOutfield attackerSide bX bY
                |> Option.map fst
                |> Option.defaultValue attackerSide.Players[0]

            let attScore =
                float attacker.Technical.Dribbling * BalanceConfig.DribbleTechnicalWeight
                + float attacker.Physical.Agility * BalanceConfig.DribbleAgilityWeight
                + float attacker.Physical.Balance * BalanceConfig.DribbleBalanceWeight

            let physVar = physicalVariation defenderSide.Conditions[di]
            let duelScore = (defScore - attScore) / 100.0 * condition * physVar

            if logisticBernoulli duelScore 1.5 then
                let aggression = float defender.Mental.Aggression / 100.0
                let positioning = float defender.Mental.Positioning / 100.0

                let baseFoulRate =
                    BalanceConfig.FoulBaseRate + aggression * BalanceConfig.TackleAggressionWeight
                    - positioning * BalanceConfig.TacklePositioningReduction
                    |> abs

                let adjustedFoulRate = baseFoulRate * (1.0 - ctx.DefBonus.CardReduc)
                let foulChance = betaSample adjustedFoulRate BalanceConfig.TackleFoulShapeBeta

                if bernoulli foulChance then
                    s
                    |> flipPossessionAndClearOffside ctx.AttSide
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleFoulMomentum,
                                    -10.0,
                                    10.0
                                ) },
                        [ event second defender.Id clubId FoulCommitted ]

                else
                    { s with
                        Momentum =
                            Math.Clamp(
                                s.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleSuccessMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second defender.Id clubId TackleSuccess ]

            else
                { s with
                    Momentum =
                        Math.Clamp(
                            s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleFailMomentum,
                            -10.0,
                            10.0
                        )
                    PendingOffsideSnapshot = None },
                [ event second defender.Id clubId TackleFail ]
