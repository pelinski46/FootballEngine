namespace FootballEngine.Player.Actions

open System
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSpatial
open FootballEngine.Player.Decision

open FootballEngine.SimStateOps
open FootballEngine.Stats
open FootballEngine.Types
open FootballEngine.Types.MatchMemory
open FootballEngine.Types.PhysicsContract




module DuelAction =

    let private fatigueMultiplier (config: DuelConfig) (condition: int) : float =
        let norm = normaliseCondition condition
        let threshold = normaliseCondition config.FatigueThreshold

        if norm < threshold then
            let deficit = threshold - norm
            exp (-deficit * config.FatigueDecay / normaliseCondition 1)
        else
            1.0


    let resolve
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : MatchEvent list * RefereeAction list =
        let actx = ActionContext.build ctx state
        let attClubId = actx.Att.ClubId
        let defClubId = actx.Def.ClubId
        let bPos = state.Ball.Position
        let bX, bY = bPos.X, bPos.Y

        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = getRoster ctx actx.Att.ClubSide
        let defRoster = getRoster ctx actx.Def.ClubSide

        match state.Ball.Control with
        | Receiving _ -> [], []
        | _ ->

            match nearestActiveSlotInFrame attFrame bX bY, nearestActiveSlotInFrame defFrame bX bY with
            | ValueSome attIdx, ValueSome defIdx ->
                let attP = attRoster.Players[attIdx]
                let defP = defRoster.Players[defIdx]
                let attCond = int attFrame.Condition[attIdx]
                let defCond = int defFrame.Condition[defIdx]

                let cfg = ctx.Config.Duel

                let attScore =
                    ActionMath.evalPerformance
                        PerformanceDefaults.duelPerformanceConfig
                        attP.Technical.Dribbling
                        attCond
                        0

                let defScore =
                    ActionMath.evalPerformance
                        PerformanceDefaults.duelPerformanceConfig
                        defP.Technical.Tackling
                        defCond
                        0

                let aggressionNorm = float defFrame.AggressionLevel[defIdx]
                let foulChance = 0.06 + aggressionNorm * 0.10

                if bernoulli foulChance then
                    state.Ball <-
                        { state.Ball with
                            Control = Contesting(actx.Def.ClubSide) }

                    adjustMomentum actx.Att.AttackDir (-ctx.Config.Tackle.FoulMomentum) state

                    let reckless = bernoulli (aggressionNorm * 0.3)
                    let excessiveForce = bernoulli (aggressionNorm * 0.15)

                    let foulCtx =
                        FoulAnalysis.assess
                            bX
                            bY
                            actx.Att.AttackDir
                            actx.Att.ClubSide
                            defP.Mental.Aggression
                            reckless
                            excessiveForce
                            attFrame
                            attRoster
                            defFrame
                            defRoster

                    let severity = FoulAnalysis.classifySeverity foulCtx

                    let yellows =
                        getYellows state actx.Def.ClubSide
                        |> Map.tryFind defP.Id
                        |> Option.defaultValue 0

                    let cardActions =
                        match FoulAnalysis.decideCard severity yellows with
                        | Some FoulAnalysis.CardDecision.Yellow ->
                            let side = actx.Def.ClubSide
                            let clubId = if side = HomeClub then ctx.Home.Id else ctx.Away.Id
                            RefereeApplicator.apply subTick (IssueYellow(defP, clubId)) ctx state |> ignore
                            []
                        | Some FoulAnalysis.CardDecision.Red ->
                            let side = actx.Def.ClubSide
                            let clubId = if side = HomeClub then ctx.Home.Id else ctx.Away.Id
                            RefereeApplicator.apply subTick (IssueRed(defP, clubId)) ctx state |> ignore
                            []
                        | None -> []

                    [ createEvent subTick defP.Id defClubId FoulCommitted ], cardActions
                else
                    let diff = float attScore - float defScore

                    let aX = float attFrame.Physics.PosX[attIdx] * 1.0<meter>
                    let aY = float attFrame.Physics.PosY[attIdx] * 1.0<meter>
                    let dX = float defFrame.Physics.PosX[defIdx] * 1.0<meter>
                    let dY = float defFrame.Physics.PosY[defIdx] * 1.0<meter>

                    if logisticBernoulli diff cfg.DuelSteepness then
                        let nx, ny = PitchMath.jitter bX bY aX aY 0.5 cfg.JitterWin cfg.JitterWin

                        let ballWithJitter =
                            { state.Ball with
                                Position =
                                    { state.Ball.Position with
                                        X = nx
                                        Y = ny } }

                        givePossessionTo actx.Att.ClubSide attP.Id (attP.Position = GK) subTick ballWithJitter state

                        state.Momentum <- Math.Clamp(state.Momentum + cfg.MomentumBonus, -10.0, 10.0)
                        recordDuel actx.Att.ClubSide attIdx defIdx Won state.MatchMemory
                        recordSuccess actx.Att.ClubSide attIdx state.MatchMemory
                        [ createEvent subTick attP.Id attClubId DribbleSuccess ], []
                    elif logisticBernoulli (-diff) cfg.DuelSteepness then
                        let nx, ny = PitchMath.jitter bX bY dX dY 0.5 cfg.JitterRecover cfg.JitterRecover

                        losePossession state

                        state.Ball <-
                            { state.Ball with
                                Position =
                                    { state.Ball.Position with
                                        X = nx
                                        Y = ny } }

                        state.Momentum <- Math.Clamp(state.Momentum - 1.0, -10.0, 10.0)
                        recordDuel actx.Att.ClubSide attIdx defIdx Lost state.MatchMemory
                        [ createEvent subTick attP.Id attClubId DribbleFail ], []
                    else
                        let nx, ny = PitchMath.jitter bX bY bX bY 0.0 cfg.JitterKeep cfg.JitterKeep

                        withBallVelocity
                            ((nx - bX) / 1.0<meter> * cfg.SpeedKeep)
                            ((ny - bY) / 1.0<meter> * cfg.SpeedKeep)
                            cfg.SpeedKeepVz
                            state

                        [ createEvent subTick attP.Id attClubId DribbleKeep ], []
            | _ -> [], []

    let resolveTackle
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (defender: Player)
        : MatchEvent list * RefereeAction list =
        let actx = ActionContext.build ctx state
        let defFrame = actx.Def.OwnFrame
        let attFrame = actx.Att.OwnFrame
        let defRoster = getRoster ctx actx.Def.ClubSide
        let attRoster = getRoster ctx actx.Att.ClubSide

        let defClubId =
            match findIdxByPid defender.Id defFrame defRoster with
            | ValueSome _ ->
                defRoster.Players
                |> Array.tryFind (fun p -> p.Id = defender.Id)
                |> Option.map (fun _ -> defRoster)
                |> Option.map (fun _ -> actx.Def.ClubId)
                |> Option.defaultValue ctx.Away.Id
            | ValueNone ->
                match findIdxByPid defender.Id (getFrame state HomeClub) (getRoster ctx HomeClub) with
                | ValueSome _ -> ctx.Home.Id
                | ValueNone -> ctx.Away.Id

        let defIdx =
            match findIdxByPid defender.Id defFrame defRoster with
            | ValueSome i -> i
            | ValueNone -> -1

        if defIdx < 0 then
            [], []
        else
            let condNorm = normaliseCondition (int defFrame.Condition[defIdx])

            let tCfg = ctx.Config.Tackle
            let dCfg = ctx.Config.Duel

            let defScore =
                ActionMath.evalPerformance
                    PerformanceDefaults.duelPerformanceConfig
                    (normaliseAttr defender.Technical.Tackling)
                    (int defFrame.Condition[defIdx])
                    0
                + actx.Def.Bonus.Tackle

            let bPos = state.Ball.Position

            let mutable bestIdx = ValueNone
            let mutable bestDistSq = Single.MaxValue
            let bX32 = float32 bPos.X
            let bY32 = float32 bPos.Y

            for i = 0 to attFrame.SlotCount - 1 do
                match attFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let player = attRoster.Players[i]

                    if player.Position <> GK then
                        let dx = attFrame.Physics.PosX[i] - bX32
                        let dy = attFrame.Physics.PosY[i] - bY32
                        let dSq = dx * dx + dy * dy

                        if dSq < bestDistSq then
                            bestDistSq <- dSq
                            bestIdx <- ValueSome i
                | _ -> ()

            let attacker =
                match bestIdx with
                | ValueSome idx -> attRoster.Players[idx]
                | ValueNone ->
                    attRoster.Players
                    |> Array.tryFind (fun p -> p.Position <> GK)
                    |> Option.defaultValue (failwith "No active players found in attacker team")

            let attIdx =
                match findIdxByPid attacker.Id attFrame attRoster with
                | ValueSome i -> i
                | ValueNone -> 0

            let attCond = int attFrame.Condition[attIdx]

            let attScore =
                ActionMath.evalPerformance
                    PerformanceDefaults.duelPerformanceConfig
                    (normaliseAttr attacker.Technical.Dribbling)
                    attCond
                    0

            let physVar = physicalVariation (int defFrame.Condition[defIdx])
            let duelScore = (defScore - attScore) * condNorm * physVar

            if logisticBernoulli duelScore ctx.Config.Tackle.TackleSteepness then
                let aggressionNorm = normaliseAttr defender.Mental.Aggression
                let positioningNorm = normaliseAttr defender.Mental.Positioning

                let baseFoulRate =
                    ctx.Config.SetPiece.FoulBaseRate + aggressionNorm * tCfg.AggressionWeight
                    - positioningNorm * tCfg.PositioningReduction
                    |> abs

                let adjustedFoulRate = baseFoulRate * (1.0 - actx.Def.Bonus.CardReduc)
                let foulChance = betaSample adjustedFoulRate tCfg.FoulShapeBeta

                if bernoulli foulChance then
                    state.Ball <- { state.Ball with Control = Free }

                    adjustMomentum actx.Att.AttackDir (-tCfg.FoulMomentum) state

                    let reckless = bernoulli (aggressionNorm * 0.3)
                    let excessiveForce = bernoulli (aggressionNorm * 0.15)
                    let ballPos = state.Ball.Position

                    let foulCtx =
                        FoulAnalysis.assess
                            ballPos.X
                            ballPos.Y
                            actx.Att.AttackDir
                            actx.Att.ClubSide
                            defender.Mental.Aggression
                            reckless
                            excessiveForce
                            attFrame
                            attRoster
                            defFrame
                            defRoster

                    let severity = FoulAnalysis.classifySeverity foulCtx

                    let yellows =
                        getYellows state actx.Def.ClubSide
                        |> Map.tryFind defender.Id
                        |> Option.defaultValue 0

                    let cardActions =
                        match FoulAnalysis.decideCard severity yellows with
                        | Some FoulAnalysis.CardDecision.Yellow ->
                            let clubId = actx.Def.ClubId

                            RefereeApplicator.apply subTick (IssueYellow(defender, clubId)) ctx state
                            |> ignore

                            []
                        | Some FoulAnalysis.CardDecision.Red ->
                            let clubId = actx.Def.ClubId
                            RefereeApplicator.apply subTick (IssueRed(defender, clubId)) ctx state |> ignore
                            []
                        | None -> []

                    [ createEvent subTick defender.Id defClubId FoulCommitted ], cardActions
                else
                    adjustMomentum actx.Att.AttackDir tCfg.SuccessMomentum state
                    [ createEvent subTick defender.Id defClubId TackleSuccess ], []
            else
                adjustMomentum actx.Att.AttackDir (-tCfg.FailMomentum) state
                clearOffsideSnapshot state
                [ createEvent subTick defender.Id defClubId TackleFail ], []
