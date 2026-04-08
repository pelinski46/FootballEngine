namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open MatchSpatial

module DuelAction =

    let private fatigueMultiplier (condition: int) : float =
        let norm = PhysicsContract.normaliseCondition condition

        let threshold =
            PhysicsContract.normaliseCondition BalanceConfig.DuelFatigueThreshold

        if norm < threshold then
            let deficit = threshold - norm
            exp (-deficit * BalanceConfig.DuelFatigueDecay / PhysicsContract.normaliseCondition 1)
        else
            1.0

    let private attributeScore (attacker: Player) (attackerCond: int) (defender: Player) (defenderCond: int) : float =
        let attRaw =
            PhysicsContract.normaliseAttr attacker.Technical.Dribbling
            * BalanceConfig.DuelAttackerDribblingWeight
            + PhysicsContract.normaliseAttr attacker.Physical.Agility
              * BalanceConfig.DuelAttackerAgilityWeight
            + PhysicsContract.normaliseAttr attacker.Physical.Balance
              * BalanceConfig.DuelAttackerBalanceWeight

        let defRaw =
            PhysicsContract.normaliseAttr defender.Technical.Tackling
            * BalanceConfig.DuelDefenderTacklingWeight
            + PhysicsContract.normaliseAttr defender.Physical.Strength
              * BalanceConfig.DuelDefenderStrengthWeight
            + PhysicsContract.normaliseAttr defender.Mental.Positioning
              * BalanceConfig.DuelDefenderPositionWeight

        let attFatigue = fatigueMultiplier attackerCond
        let defFatigue = fatigueMultiplier defenderCond

        attRaw * attFatigue - defRaw * defFatigue

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let defClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let attIdx = nearestIdxToBall attSlots bX bY
        let defIdx = nearestIdxToBall defSlots bX bY

        let att =
            match attSlots[attIdx] with
            | PlayerSlot.Active s -> s.Player
            | _ -> Unchecked.defaultof<Player>

        let def =
            match defSlots[defIdx] with
            | PlayerSlot.Active s -> s.Player
            | _ -> Unchecked.defaultof<Player>

        let attConditions = conditionsArray attSlots
        let defConditions = conditionsArray defSlots

        let attTactics =
            if actx.AttSide = HomeClub then
                state.HomeTactics
            else
                state.AwayTactics

        let attInstructions =
            if actx.AttSide = HomeClub then
                state.HomeInstructions
            else
                state.AwayInstructions

        let tacticsCfg = tacticsConfig attTactics attInstructions

        let attackingBonus = actx.AttBonus.AttackDuel
        let homeDefBonus = actx.DefBonus.DefendDuel

        let attrDiff = attributeScore att attConditions[attIdx] def defConditions[defIdx]

        let moraleBonus =
            humanPerformance 1 att.Morale att.Morale
            * BalanceConfig.DuelMoraleBonusMultiplier

        let repBonus =
            let attRep = (if attClubId = ctx.Home.Id then ctx.Home else ctx.Away).Reputation
            let defRep = (if defClubId = ctx.Home.Id then ctx.Home else ctx.Away).Reputation
            float (attRep - defRep) * BalanceConfig.DuelReputationBonusMultiplier

        let momentum = actx.Momentum
        let pressure = (pressureMultiplier attClubId ctx state - 1.0) * 5.0
        let u = matchUrgency attClubId ctx state * tacticsCfg.UrgencyMultiplier
        let pressNoise = pressureNoise att.Mental.Composure u

        let diff =
            attackEffort (phaseFromBallZone actx.Dir bX) att attConditions[attIdx] * u
            + attackingBonus
            - homeDefBonus
            + attrDiff
            + moraleBonus
            + repBonus
            + momentum
            + pressure
            + pressNoise
            - defenseEffort def defConditions[defIdx]

        let aX, aY =
            match attSlots[attIdx] with
            | PlayerSlot.Active s -> s.Pos.X, s.Pos.Y
            | _ -> bX, bY

        let dX, dY =
            match defSlots[defIdx] with
            | PlayerSlot.Active s -> s.Pos.X, s.Pos.Y
            | _ -> bX, bY

        let duelEvents =
            if logisticBernoulli diff BalanceConfig.DuelWinProbabilityBase then
                let nx, ny =
                    PitchMath.jitter bX bY aX aY 0.5 BalanceConfig.DuelJitterWin BalanceConfig.DuelJitterWin

                state.Ball <-
                    { state.Ball with
                        Position =
                            { state.Ball.Position with
                                X = nx
                                Y = ny } }

                state.Momentum <- Math.Clamp(state.Momentum + BalanceConfig.DuelMomentumBonus, -10.0, 10.0)
                [ createEvent subTick att.Id attClubId DribbleSuccess ]
            elif logisticBernoulli (-diff) BalanceConfig.DuelRecoverProbabilityBase then
                let nx, ny =
                    PitchMath.jitter bX bY dX dY 0.5 BalanceConfig.DuelJitterRecover BalanceConfig.DuelJitterRecover

                flipPossession state

                state.Ball <-
                    { state.Ball with
                        Position =
                            { state.Ball.Position with
                                X = nx
                                Y = ny } }

                state.Momentum <- Math.Clamp(state.Momentum - 1.0, -10.0, 10.0)
                [ createEvent subTick att.Id attClubId DribbleFail ]
            else
                let nx, ny =
                    PitchMath.jitter bX bY bX bY 0.0 BalanceConfig.DuelJitterKeep BalanceConfig.DuelJitterKeep

                withBallVelocity
                    ((nx - bX) * BalanceConfig.DuelSpeedKeep)
                    ((ny - bY) * BalanceConfig.DuelSpeedKeep)
                    BalanceConfig.DuelSpeedKeepVz
                    state

                [ createEvent subTick att.Id attClubId DribbleKeep ]

        let aggressionNorm = PhysicsContract.normaliseAttr def.Mental.Aggression
        let foulChance = 0.06 + aggressionNorm * 0.10

        if bernoulli foulChance then
            if state.AttackingClub = actx.AttSide then
                flipPossession state
            adjustMomentum actx.Dir (-BalanceConfig.TackleFoulMomentum) state
            duelEvents @ [ createEvent subTick def.Id defClubId FoulCommitted ]
        else
            duelEvents

    let resolveTackle (subTick: int) (ctx: MatchContext) (state: SimState) (defender: Player) : MatchEvent list =
        let actx = ActionContext.build state

        let defClubId =
            if
                state.HomeSlots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = defender.Id
                    | _ -> false)
            then
                ctx.Home.Id
            else
                ctx.Away.Id

        let clubId = defClubId

        let defSlots =
            if actx.DefSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let defConditions =
            Array.init defSlots.Length (fun i ->
                match defSlots[i] with
                | PlayerSlot.Active s -> s.Condition
                | _ -> 0)

        let mutable di = 0
        let mutable found = false

        for i = 0 to defSlots.Length - 1 do
            match defSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = defender.Id ->
                di <- i
                found <- true
            | _ -> ()

        if not found then
            []
        else
            let condNorm = PhysicsContract.normaliseCondition defConditions[di]

            let defScore =
                PhysicsContract.normaliseAttr defender.Technical.Tackling
                * BalanceConfig.TackleTechnicalWeight
                + PhysicsContract.normaliseAttr defender.Mental.Positioning
                  * BalanceConfig.TacklePositioningWeight
                + PhysicsContract.normaliseAttr defender.Physical.Strength
                  * BalanceConfig.TackleStrengthWeight
                + actx.DefBonus.Tackle

            let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

            let mutable bestPlayer: Player option = None
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to attSlots.Length - 1 do
                match attSlots[i] with
                | PlayerSlot.Active s when s.Player.Position <> GK ->
                    let dx = s.Pos.X - bX
                    let dy = s.Pos.Y - bY
                    let dSq = dx * dx + dy * dy

                    if dSq < bestDistSq then
                        bestDistSq <- dSq
                        bestPlayer <- Some s.Player
                | _ -> ()

            let attacker =
                match bestPlayer with
                | Some p -> p
                | None ->
                    match attSlots[0] with
                    | PlayerSlot.Active s -> s.Player
                    | _ -> Unchecked.defaultof<Player>

            let attScore =
                PhysicsContract.normaliseAttr attacker.Technical.Dribbling
                * BalanceConfig.DribbleTechnicalWeight
                + PhysicsContract.normaliseAttr attacker.Physical.Agility
                  * BalanceConfig.DribbleAgilityWeight
                + PhysicsContract.normaliseAttr attacker.Physical.Balance
                  * BalanceConfig.DribbleBalanceWeight

            let physVar = physicalVariation defConditions[di]
            let duelScore = (defScore - attScore) * condNorm * physVar

            if logisticBernoulli duelScore 1.5 then
                let aggressionNorm = PhysicsContract.normaliseAttr defender.Mental.Aggression
                let positioningNorm = PhysicsContract.normaliseAttr defender.Mental.Positioning

                let baseFoulRate =
                    BalanceConfig.FoulBaseRate
                    + aggressionNorm * BalanceConfig.TackleAggressionWeight
                    - positioningNorm * BalanceConfig.TacklePositioningReduction
                    |> abs

                let adjustedFoulRate = baseFoulRate * (1.0 - actx.DefBonus.CardReduc)
                let foulChance = betaSample adjustedFoulRate BalanceConfig.TackleFoulShapeBeta

                if bernoulli foulChance then
                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.TackleFoulMomentum) state
                    [ createEvent subTick defender.Id clubId FoulCommitted ]
                else
                    adjustMomentum actx.Dir BalanceConfig.TackleSuccessMomentum state
                    [ createEvent subTick defender.Id clubId TackleSuccess ]
            else
                adjustMomentum actx.Dir (-BalanceConfig.TackleFailMomentum) state
                state.PendingOffsideSnapshot <- None
                [ createEvent subTick defender.Id clubId TackleFail ]
