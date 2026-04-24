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
        let threshold = PhysicsContract.normaliseCondition config.FatigueThreshold
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

        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY, SimStateOps.nearestActiveSlotInFrame defFrame bX bY with
        | ValueSome attIdx, ValueSome defIdx ->
            let attP = attRoster.Players[attIdx]
            let defP = defRoster.Players[defIdx]
            let attCond = int attFrame.Condition[attIdx]
            let defCond = int defFrame.Condition[defIdx]

            let cfg = ctx.Config.Duel

            let attFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr attP.Technical.Dribbling, cfg.AttackerDribblingWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Agility, cfg.AttackerAgilityWeight)
                  (PhysicsContract.normaliseAttr attP.Physical.Balance, cfg.AttackerBalanceWeight) ]

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defP.Technical.Tackling, cfg.DefenderTacklingWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Strength, cfg.DefenderStrengthWeight)
                  (PhysicsContract.normaliseAttr defP.Physical.Agility, cfg.DefenderPositionWeight) ]
            
            let attScore = ActionMath.evalWeighted attFeatures attCond 0.05
            let defScore = ActionMath.evalWeighted defFeatures defCond 0.05

            let aggressionNorm = float defFrame.AggressionLevel[defIdx]
            let foulChance = 0.06 + aggressionNorm * 0.10

            if bernoulli foulChance then
                state.Ball <- { state.Ball with Possession = Contest(actx.Def.ClubSide) }
                adjustMomentum actx.Att.AttackDir (-ctx.Config.Tackle.FoulMomentum) state
                [ createEvent subTick defP.Id defClubId FoulCommitted ]
            else
                let diff = float attScore - float defScore

                let aX = float attFrame.PosX[attIdx] * 1.0<meter>
                let aY = float attFrame.PosY[attIdx] * 1.0<meter>
                let dX = float defFrame.PosX[defIdx] * 1.0<meter>
                let dY = float defFrame.PosY[defIdx] * 1.0<meter>

                if logisticBernoulli diff cfg.WinProbabilityBase then
                    let nx, ny =
                        PitchMath.jitter bX bY aX aY 0.5 cfg.JitterWin cfg.JitterWin

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    X = nx
                                    Y = ny }
                            Possession = Owned(actx.Att.ClubSide, attP.Id) }

                    state.Momentum <- Math.Clamp(state.Momentum + cfg.MomentumBonus, -10.0, 10.0)
                    [ createEvent subTick attP.Id attClubId DribbleSuccess ]
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
                    [ createEvent subTick attP.Id attClubId DribbleFail ]
                else
                    let nx, ny =
                        PitchMath.jitter bX bY bX bY 0.0 cfg.JitterKeep cfg.JitterKeep

                    withBallVelocity
                        ((nx - bX) / 1.0<meter> * cfg.SpeedKeep)
                        ((ny - bY) / 1.0<meter> * cfg.SpeedKeep)
                        cfg.SpeedKeepVz
                        state

                    [ createEvent subTick attP.Id attClubId DribbleKeep ]
        | _ -> []

    let resolveTackle (subTick: int) (ctx: MatchContext) (state: SimState) (defender: Player) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let defFrame = actx.Def.OwnFrame
        let attFrame = actx.Att.OwnFrame
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide

        let defClubId =
            match SimStateOps.findIdxByPid defender.Id defFrame defRoster with
            | ValueSome _ -> defRoster.Players |> Array.tryFind (fun p -> p.Id = defender.Id) |> Option.map (fun _ -> defRoster) |> Option.map (fun _ -> actx.Def.ClubId) |> Option.defaultValue ctx.Away.Id
            | ValueNone ->
                match SimStateOps.findIdxByPid defender.Id (SimStateOps.getFrame state HomeClub) (SimStateOps.getRoster ctx HomeClub) with
                | ValueSome _ -> ctx.Home.Id
                | ValueNone -> ctx.Away.Id

        let defIdx =
            match SimStateOps.findIdxByPid defender.Id defFrame defRoster with
            | ValueSome i -> i
            | ValueNone -> -1

        if defIdx < 0 then []
        else
            let condNorm = PhysicsContract.normaliseCondition (int defFrame.Condition[defIdx])

            let tCfg = ctx.Config.Tackle
            let dCfg = ctx.Config.Duel

            let defFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr defender.Technical.Tackling, tCfg.TechnicalWeight)
                  (PhysicsContract.normaliseAttr defender.Mental.Positioning, tCfg.PositioningWeight)
                  (PhysicsContract.normaliseAttr defender.Physical.Strength, tCfg.StrengthWeight) ]

            let defScore = ActionMath.evalWeighted defFeatures (int defFrame.Condition[defIdx]) 0.05 + actx.Def.Bonus.Tackle

            let bPos = state.Ball.Position

            let mutable bestIdx = ValueNone
            let mutable bestDistSq = System.Single.MaxValue
            let bX32 = float32 bPos.X
            let bY32 = float32 bPos.Y

            for i = 0 to attFrame.SlotCount - 1 do
                match attFrame.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let player = attRoster.Players[i]
                    if player.Position <> GK then
                        let dx = attFrame.PosX[i] - bX32
                        let dy = attFrame.PosY[i] - bY32
                        let dSq = dx * dx + dy * dy
                        if dSq < bestDistSq then
                            bestDistSq <- dSq
                            bestIdx <- ValueSome i
                | _ -> ()

            let attacker =
                match bestIdx with
                | ValueSome idx -> attRoster.Players[idx]
                | ValueNone ->
                    attRoster.Players |> Array.tryFind (fun p -> p.Position <> GK)
                    |> Option.defaultValue (failwith "No active players found in attacker team")

            let attFeatures : (float * float) list =
                [ (PhysicsContract.normaliseAttr attacker.Technical.Dribbling, dCfg.AttackerDribblingWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Agility, dCfg.AttackerAgilityWeight)
                  (PhysicsContract.normaliseAttr attacker.Physical.Balance, dCfg.AttackerBalanceWeight) ]

            let attIdx =
                match SimStateOps.findIdxByPid attacker.Id attFrame attRoster with
                | ValueSome i -> i
                | ValueNone -> 0

            let attCond = int attFrame.Condition[attIdx]
            let attScore = ActionMath.evalWeighted attFeatures attCond 0.05

            let physVar = physicalVariation (int defFrame.Condition[defIdx])
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
                    [ createEvent subTick defender.Id defClubId FoulCommitted ]
                else
                    adjustMomentum actx.Att.AttackDir tCfg.SuccessMomentum state
                    [ createEvent subTick defender.Id defClubId TackleSuccess ]
            else
                adjustMomentum actx.Att.AttackDir (-tCfg.FailMomentum) state
                clearOffsideSnapshot state
                [ createEvent subTick defender.Id defClubId TackleFail ]
