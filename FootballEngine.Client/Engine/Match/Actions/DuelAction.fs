namespace FootballEngine

open System
open FootballEngine.ActionMath
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract


module DuelAction =

    let private fatigueMultiplier (config: DuelConfig) (condition: int) : float =
        let norm = PhysicsContract.normaliseCondition condition

        let threshold =
            PhysicsContract.normaliseCondition config.FatigueThreshold

        if norm < threshold then
            let deficit = threshold - norm
            exp (-deficit * config.FatigueDecay / PhysicsContract.normaliseCondition 1)
        else
            1.0


    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let attClubId = actx.Att.ClubId
        let defClubId = actx.Def.ClubId
        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        match MatchSpatial.nearestActiveSlot attSlots bX bY, MatchSpatial.nearestActiveSlot defSlots bX bY with
        | ValueSome att, ValueSome def ->
            let attConditions = att.Condition
            let defConditions = def.Condition
            let attP = att.Player
            let defP = def.Player

            let cfg = ctx.Config.Duel

            let attFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr attP.Technical.Dribbling, cfg.AttackerDribblingWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Agility, cfg.AttackerAgilityWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Balance, cfg.AttackerBalanceWeight) ]

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defP.Technical.Tackling, cfg.DefenderTacklingWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Strength, cfg.DefenderStrengthWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Agility, cfg.DefenderPositionWeight) ]
            
            let attScore = ActionMath.evalWeighted attFeatures attConditions 0.05
            let defScore = ActionMath.evalWeighted defFeatures defConditions 0.05

            let aggressionNorm = def.Mental.AggressionLevel
            let foulChance = 0.06 + aggressionNorm * 0.10

            if bernoulli foulChance then
                state.Ball <- { state.Ball with Possession = Contest(actx.Def.ClubSide) }
                adjustMomentum actx.Att.AttackDir (-ctx.Config.Tackle.FoulMomentum) state
                [ createEvent subTick def.Player.Id defClubId FoulCommitted ]
            else
                let diff = float attScore - float defScore

                let aX, aY = att.Pos.X, att.Pos.Y
                let dX, dY = def.Pos.X, def.Pos.Y

                if logisticBernoulli diff cfg.WinProbabilityBase then
                    let nx, ny =
                        PitchMath.jitter bX bY aX aY 0.5 cfg.JitterWin cfg.JitterWin

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    X = nx
                                    Y = ny }
                            Possession = Owned(actx.Att.ClubSide, att.Player.Id) }

                    state.Momentum <- Math.Clamp(state.Momentum + cfg.MomentumBonus, -10.0, 10.0)
                    [ createEvent subTick att.Player.Id attClubId DribbleSuccess ]
                elif logisticBernoulli (-diff) cfg.RecoverProbabilityBase then
                    let nx, ny =
                        PitchMath.jitter bX bY dX dY 0.5 cfg.JitterRecover cfg.JitterRecover

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
                        PitchMath.jitter bX bY bX bY 0.0 cfg.JitterKeep cfg.JitterKeep

                    withBallVelocity
                        ((nx - bX) / 1.0<meter> * cfg.SpeedKeep)
                        ((ny - bY) / 1.0<meter> * cfg.SpeedKeep)
                        cfg.SpeedKeepVz
                        state

                    [ createEvent subTick att.Player.Id attClubId DribbleKeep ]
        | _ -> []

    let resolveTackle (subTick: int) (ctx: MatchContext) (state: SimState) (defender: Player) : MatchEvent list =
        let actx = ActionContext.build ctx state

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

        let defSlots = actx.Def.OwnSlots
        let attSlots = actx.Att.OwnSlots

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

            let tCfg = ctx.Config.Tackle
            let dCfg = ctx.Config.Duel

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defender.Technical.Tackling, tCfg.TechnicalWeight)
                  (PhysicsContract.normaliseAttr defender.Mental.Positioning, tCfg.PositioningWeight)
                  (PhysicsContract.normaliseAttr defender.Physical.Strength, tCfg.StrengthWeight) ]

            let defScore = ActionMath.evalWeighted defFeatures defConditions[di] 0.05 + actx.Def.Bonus.Tackle

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
                [ (PhysicsContract.normaliseAttr attacker.Technical.Dribbling, dCfg.AttackerDribblingWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Agility, dCfg.AttackerAgilityWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Balance, dCfg.AttackerBalanceWeight) ]

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
                    ctx.Config.SetPiece.FoulBaseRate
                    + aggressionNorm * tCfg.AggressionWeight
                    - positioningNorm * tCfg.PositioningReduction
                    |> abs

                let adjustedFoulRate = baseFoulRate * (1.0 - actx.Def.Bonus.CardReduc)
                let foulChance = betaSample adjustedFoulRate tCfg.FoulShapeBeta

                if bernoulli foulChance then
                    state.Ball <- { state.Ball with Possession = SetPiece(actx.Att.ClubSide, SetPieceKind.FreeKick) }
                    adjustMomentum actx.Att.AttackDir (-tCfg.FoulMomentum) state
                    [ createEvent subTick defender.Id clubId FoulCommitted ]
                else
                    adjustMomentum actx.Att.AttackDir tCfg.SuccessMomentum state
                    [ createEvent subTick defender.Id clubId TackleSuccess ]
            else
                adjustMomentum actx.Att.AttackDir (-tCfg.FailMomentum) state
                clearOffsideSnapshot state
                [ createEvent subTick defender.Id clubId TackleFail ]
