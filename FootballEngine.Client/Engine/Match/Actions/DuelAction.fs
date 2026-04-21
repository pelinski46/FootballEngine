namespace FootballEngine

open System
open FootballEngine.ActionMath
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
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


    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let defClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        match MatchSpatial.nearestActiveSlot attSlots bX bY, MatchSpatial.nearestActiveSlot defSlots bX bY with
        | ValueSome att, ValueSome def ->
            let attConditions = att.Condition
            let defConditions = def.Condition
            let attP = att.Player
            let defP = def.Player

            // Build attacker feature-weight list (normalized values)
            let attFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr attP.Technical.Dribbling, BalanceConfig.DuelAttackerDribblingWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Agility, BalanceConfig.DuelAttackerAgilityWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Balance, BalanceConfig.DuelAttackerBalanceWeight) ]

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defP.Technical.Tackling, BalanceConfig.DuelDefenderTacklingWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Strength, BalanceConfig.DuelDefenderStrengthWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Agility, BalanceConfig.DuelDefenderStrengthWeight) ]
            
            let attScore = ActionMath.evalWeighted attFeatures attConditions 0.05
            let defScore = ActionMath.evalWeighted defFeatures defConditions 0.05

            let aggressionNorm = def.Mental.AggressionLevel
            let foulChance = 0.06 + aggressionNorm * 0.10

            if bernoulli foulChance then
                // Bug 5 Fix: Foul check first, mutual exclusion with duel result
                state.Ball <- { state.Ball with Possession = Contest(actx.DefSide) }
                adjustMomentum actx.Dir (-BalanceConfig.TackleFoulMomentum) state
                [ createEvent subTick def.Player.Id defClubId FoulCommitted ]
            else
                let diff = float attScore - float defScore

                let aX, aY = att.Pos.X, att.Pos.Y
                let dX, dY = def.Pos.X, def.Pos.Y

                if logisticBernoulli diff BalanceConfig.DuelWinProbabilityBase then
                    let nx, ny =
                        PitchMath.jitter bX bY aX aY 0.5 BalanceConfig.DuelJitterWin BalanceConfig.DuelJitterWin

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    X = nx
                                    Y = ny }
                            Possession = Owned(actx.AttSide, att.Player.Id) }

                    state.Momentum <- Math.Clamp(state.Momentum + BalanceConfig.DuelMomentumBonus, -10.0, 10.0)
                    [ createEvent subTick att.Player.Id attClubId DribbleSuccess ]
                elif logisticBernoulli (-diff) BalanceConfig.DuelRecoverProbabilityBase then
                    let nx, ny =
                        PitchMath.jitter bX bY dX dY 0.5 BalanceConfig.DuelJitterRecover BalanceConfig.DuelJitterRecover

                    loosePossession state

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    X = nx
                                    Y = ny } }

                    state.Momentum <- Math.Clamp(state.Momentum - 1.0, -10.0, 10.0)
                    [ createEvent subTick att.Player.Id attClubId DribbleFail ]
                else
                    let nx, ny =
                        PitchMath.jitter bX bY bX bY 0.0 BalanceConfig.DuelJitterKeep BalanceConfig.DuelJitterKeep

                    withBallVelocity
                        (float (nx - bX) * BalanceConfig.DuelSpeedKeep)
                        (float (ny - bY) * BalanceConfig.DuelSpeedKeep)
                        BalanceConfig.DuelSpeedKeepVz
                        state

                    [ createEvent subTick att.Player.Id attClubId DribbleKeep ]
        | _ -> []

    let resolveTackle (subTick: int) (ctx: MatchContext) (state: SimState) (defender: Player) : MatchEvent list =
        let actx = ActionContext.build state

        let defClubId =
            if
                state.Home.Slots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = defender.Id
                    | _ -> false)
            then
                ctx.Home.Id
            else
                ctx.Away.Id

        let clubId = defClubId

        let defSlots = getSlots state actx.DefSide
        let attSlots = getSlots state actx.AttSide

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

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defender.Technical.Tackling, BalanceConfig.TackleTechnicalWeight)
                  (PhysicsContract.normaliseAttr defender.Mental.Positioning, BalanceConfig.TacklePositioningWeight)
                  (PhysicsContract.normaliseAttr defender.Physical.Strength, BalanceConfig.TackleStrengthWeight) ]

            let defScore = ActionMath.evalWeighted defFeatures defConditions[di] 0.05 + actx.DefBonus.Tackle

            let bPos = state.Ball.Position

            let mutable bestPlayer: Player option = None
            let mutable bestDistSq = PhysicsContract.MaxDistanceSq

            for i = 0 to attSlots.Length - 1 do
                match attSlots[i] with
                | PlayerSlot.Active s when s.Player.Position <> GK ->
                    let dSq = s.Pos.DistSqTo2D bPos

                    if dSq < bestDistSq then
                        bestDistSq <- dSq
                        bestPlayer <- Some s.Player
                | _ -> ()

            let attacker =
                match bestPlayer with
                | Some p -> p
                | None ->
                    match attSlots |> Array.tryFind (function | PlayerSlot.Active _ -> true | _ -> false) with
                    | Some (PlayerSlot.Active s) -> s.Player
                    | _ -> failwith "No active players found in attacker slots"

            let attFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr attacker.Technical.Dribbling, BalanceConfig.DribbleTechnicalWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Agility, BalanceConfig.DribbleAgilityWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Balance, BalanceConfig.DribbleBalanceWeight) ]

            let attCond =
                let mutable ac = 0
                for i = 0 to attSlots.Length - 1 do
                    match attSlots[i] with
                    | PlayerSlot.Active s when s.Player.Id = attacker.Id -> ac <- s.Condition
                    | _ -> ()
                ac

            let attScore = ActionMath.evalWeighted attFeatures attCond 0.05

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
                    state.Ball <- { state.Ball with Possession = SetPiece(actx.AttSide, SetPieceKind.FreeKick) }
                    adjustMomentum actx.Dir (-BalanceConfig.TackleFoulMomentum) state
                    [ createEvent subTick defender.Id clubId FoulCommitted ]
                else
                    adjustMomentum actx.Dir BalanceConfig.TackleSuccessMomentum state
                    [ createEvent subTick defender.Id clubId TackleSuccess ]
            else
                adjustMomentum actx.Dir (-BalanceConfig.TackleFailMomentum) state
                clearOffsideSnapshot state
                [ createEvent subTick defender.Id clubId TackleFail ]
