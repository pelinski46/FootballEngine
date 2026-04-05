namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchCalc
open MatchSpatial

module DuelAction =

    // Phase 0: Second -> SubTick in event constructor
    let private event subTick playerId clubId t =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    // Phase 3: normaliseCondition for fatigue threshold
    let private fatigueMultiplier (condition: int) : float =
        let norm = PhysicsContract.normaliseCondition condition
        let threshold = PhysicsContract.normaliseCondition BalanceConfig.DuelFatigueThreshold
        if norm < threshold then
            let deficit = threshold - norm
            exp (-deficit * BalanceConfig.DuelFatigueDecay / PhysicsContract.normaliseCondition 1)
        else
            1.0

    // Phase 3: normaliseAttr for all attribute accesses
    let private attributeScore (attacker: Player) (attackerCond: int) (defender: Player) (defenderCond: int) : float =
        let attRaw =
            PhysicsContract.normaliseAttr attacker.Technical.Dribbling * BalanceConfig.DuelAttackerDribblingWeight
            + PhysicsContract.normaliseAttr attacker.Physical.Agility * BalanceConfig.DuelAttackerAgilityWeight
            + PhysicsContract.normaliseAttr attacker.Physical.Balance * BalanceConfig.DuelAttackerBalanceWeight

        let defRaw =
            PhysicsContract.normaliseAttr defender.Technical.Tackling * BalanceConfig.DuelDefenderTacklingWeight
            + PhysicsContract.normaliseAttr defender.Physical.Strength * BalanceConfig.DuelDefenderStrengthWeight
            + PhysicsContract.normaliseAttr defender.Mental.Positioning * BalanceConfig.DuelDefenderPositionWeight

        let attFatigue = fatigueMultiplier attackerCond
        let defFatigue = fatigueMultiplier defenderCond

        attRaw * attFatigue - defRaw * defFatigue

    // Phase 5: JIT — attacker/defender resolved at execution time from current positions
    let resolve (subTick: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s

        let bX, bY = ballXY s

        // JIT: resolve nearest attacker/defender from CURRENT positions at execution time
        // For-loop — no Array.mapi/minBy allocation
        let mutable attIdx = 0
        let mutable attDistSq = System.Double.MaxValue
        for i = 0 to attSide.Positions.Length - 1 do
            let dx = attSide.Positions[i].X - bX
            let dy = attSide.Positions[i].Y - bY
            let dSq = dx * dx + dy * dy
            if dSq < attDistSq then
                attDistSq <- dSq
                attIdx <- i

        let mutable defIdx = 0
        let mutable defDistSq = System.Double.MaxValue
        for i = 0 to defSide.Positions.Length - 1 do
            let dx = defSide.Positions[i].X - bX
            let dy = defSide.Positions[i].Y - bY
            let dSq = dx * dx + dy * dy
            if dSq < defDistSq then
                defDistSq <- dSq
                defIdx <- i

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

    // Phase 5: JIT tackle — defender resolved at execution time
    // Phase 3: normaliseAttr for attribute scores
    let resolveTackle (subTick: int) (s: MatchState) (defender: Player) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let defClubId = clubIdOf defender s
        let clubId = defClubId
        let defenderSide = side (ClubSide.toClubId ctx.DefSide s) s
        let attackerSide = side (ClubSide.toClubId ctx.AttSide s) s

        match defenderSide.Players |> Array.tryFindIndex (fun p -> p.Id = defender.Id) with
        | None -> s, []
        | Some di ->
            let condNorm = PhysicsContract.normaliseCondition defenderSide.Conditions[di]

            let defScore =
                PhysicsContract.normaliseAttr defender.Technical.Tackling * BalanceConfig.TackleTechnicalWeight
                + PhysicsContract.normaliseAttr defender.Mental.Positioning * BalanceConfig.TacklePositioningWeight
                + PhysicsContract.normaliseAttr defender.Physical.Strength * BalanceConfig.TackleStrengthWeight
                + ctx.DefBonus.Tackle

            let bX, bY = ballXY s

            let attacker =
                nearestOutfield attackerSide bX bY
                |> Option.map fst
                |> Option.defaultValue attackerSide.Players[0]

            let attScore =
                PhysicsContract.normaliseAttr attacker.Technical.Dribbling * BalanceConfig.DribbleTechnicalWeight
                + PhysicsContract.normaliseAttr attacker.Physical.Agility * BalanceConfig.DribbleAgilityWeight
                + PhysicsContract.normaliseAttr attacker.Physical.Balance * BalanceConfig.DribbleBalanceWeight

            let physVar = physicalVariation defenderSide.Conditions[di]
            let duelScore = (defScore - attScore) * condNorm * physVar

            if logisticBernoulli duelScore 1.5 then
                let aggressionNorm = PhysicsContract.normaliseAttr defender.Mental.Aggression
                let positioningNorm = PhysicsContract.normaliseAttr defender.Mental.Positioning

                let baseFoulRate =
                    BalanceConfig.FoulBaseRate + aggressionNorm * BalanceConfig.TackleAggressionWeight
                    - positioningNorm * BalanceConfig.TacklePositioningReduction
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
                        [ event subTick defender.Id clubId FoulCommitted ]

                else
                    { s with
                        Momentum =
                            Math.Clamp(
                                s.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleSuccessMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event subTick defender.Id clubId TackleSuccess ]

            else
                { s with
                    Momentum =
                        Math.Clamp(
                            s.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.TackleFailMomentum,
                            -10.0,
                            10.0
                        )
                    PendingOffsideSnapshot = None },
                [ event subTick defender.Id clubId TackleFail ]
