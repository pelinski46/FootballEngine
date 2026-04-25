namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open FootballEngine.PhysicsContract

module ShotAction =

    let private finishingBonusForPosition (cfg: ShotConfig) =
        function
        | ST | AMC | AML | AMR -> cfg.FinishingBonusAM
        | MC -> cfg.FinishingBonusMC
        | _ -> cfg.FinishingBonusOther

    let private calcShotPower
        (cfg: ShotConfig)
        (player: Player)
        (condition: int)
        (composure: float)
        (urgency: float)
        (dist: float<meter>)
        (homeComposure: float)
        =
        let bonus = finishingBonusForPosition cfg player.Position
        let condFactor = sqrt (float condition / cfg.CondFactorDivisor)
        let basePower = float player.Technical.Finishing / cfg.BasePowerDivisor

        basePower
        * condFactor
        * (1.0 + composure * cfg.ComposureMultiplier)
        * (1.0 + urgency * cfg.UrgencyMultiplier)
        * bonus

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let sc = ctx.Config.Shot
        let attClubId = actx.Att.ClubId
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

        let bX = state.Ball.Position.X
        let bY = state.Ball.Position.Y

        let inChance = actx.Zone = AttackingZone || actx.Zone = MidfieldZone

        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> []
        | ValueSome shooterIdx ->
            let shooter = attRoster.Players[shooterIdx]
            let shooterCond = int attFrame.Condition[shooterIdx]
            let shooterProfile = attRoster.Profiles[shooterIdx]

            if not inChance then
                [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
            else
                let quality =
                    let distToGoal = PhysicsContract.distToGoal bX actx.Att.AttackDir
                    let distNorm = PhysicsContract.clamp (distToGoal / sc.NormalisationDistance) 0.0 1.0
                    let positionBonus =
                        shooterProfile.Directness * sc.PositionDirectnessWeight
                        + shooterProfile.AttackingDepth * sc.PositionDepthWeight
                        + shooterProfile.CreativityWeight * sc.PositionCreativityWeight
                    (1.0 - distNorm) * sc.DistNormWeight + positionBonus * sc.PositionBonusWeight

                if quality < sc.QualityGate then
                    [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
                else
                    let attTactics = getTactics state actx.Att.ClubSide
                    let attInstructions = getInstructions state actx.Att.ClubSide
                    let tacticsCfg = tacticsConfig attTactics attInstructions

                    let composure =
                        pressureMultiplier attClubId ctx state * tacticsCfg.UrgencyMultiplier

                    let u = matchUrgency attClubId ctx state clock * tacticsCfg.UrgencyMultiplier

                    let dist =
                        PhysicsContract.distToGoal bX actx.Att.AttackDir
                        * sc.DistanceToGoalMultiplier

                    let homeComposure = actx.Att.Bonus.ShotCompos
                    let finishing = calcShotPower sc shooter shooterCond composure u dist homeComposure
                    let dirSign = PhysicsContract.forwardX actx.Att.AttackDir

                    let finishingNorm =
                        Math.Clamp(
                            PhysicsContract.normaliseAttr shooter.Technical.Finishing,
                            sc.FinishingMin,
                            sc.FinishingMax
                        )

                    let speed = ActionMath.shotSpeed shooter.Technical.Finishing
                    let angleSpread = sc.AngleSpreadBase * (1.0 - finishingNorm)
                    let angle = normalSample 0.0 angleSpread
                    let speedMag = float speed
                    let vx = dirSign * speedMag * Math.Cos(angle)
                    let vy = speedMag * Math.Sin(angle)

                    let vz =
                        abs (normalSample (float sc.VzBase) sc.VzVariance)
                        * 1.0<meter / second>

                    let spin =
                        { Top =
                            -(PhysicsContract.normaliseAttr shooter.Technical.Finishing)
                            * sc.SpinTopMultiplier
                            * 1.0<radianPerSecond>
                          Side = 0.0<radianPerSecond> }

                    let gkIdx =
                        let mutable idx = -1
                        for i = 0 to defFrame.SlotCount - 1 do
                            match defFrame.Occupancy[i] with
                            | OccupancyKind.Active _ when defRoster.Players[i].Position = GK -> idx <- i
                            | _ -> ()
                        idx

                    let gk = if gkIdx >= 0 then Some defRoster.Players[gkIdx] else None

                    let onTarget =
                        let distToGoal = PhysicsContract.distToGoal bX actx.Att.AttackDir
                        let distPenalty = sc.OnTargetDistMaxPenalty * (1.0 - Math.Exp(-float distToGoal / sc.OnTargetDistDecayRate))
                        let onTargetProb = sc.OnTargetBase + finishingNorm * sc.OnTargetMultiplier - distPenalty
                        bernoulli (Math.Clamp(onTargetProb, 0.05, 0.95))

                    let gkSaves =
                        match gk with
                        | Some g ->
                            let gkCond = if gkIdx >= 0 then int defFrame.Condition[gkIdx] else 50
                            let savePower =
                                ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig (PhysicsContract.normaliseAttr g.Goalkeeping.Reflexes) gkCond g.Morale * sc.GkReflexesStatMult
                                + ActionMath.evalPerformance PerformanceDefaults.technicalPerformanceConfig (PhysicsContract.normaliseAttr g.Goalkeeping.OneOnOne) gkCond g.Morale * sc.GkOneOnOneStatMult

                            let adjustedSave = savePower
                            onTarget && ActionMath.engineBernoulli (Probability.from (adjustedSave / (adjustedSave + finishing + sc.SaveDenominatorOffset)))
                        | None -> false

                    state.Ball <-
                        { state.Ball with
                            Position =
                                { state.Ball.Position with
                                    Vx = vx * 1.0<meter / second>
                                    Vy = vy * 1.0<meter / second>
                                    Vz = vz
                                    X = bX }
                            Spin = spin
                            LastTouchBy = Some shooter.Id }

                    SimStateOps.adjustMomentum actx.Att.AttackDir ctx.Config.Duel.MomentumBonus state
                    clearOffsideSnapshot state

                    let gkClubId = actx.Def.ClubId

                    state.Ball <-
                        { state.Ball with
                            Possession = InFlight(actx.Att.ClubSide, shooter.Id) }

                    match gk with
                    | Some g when gkSaves ->
                        [ createEvent subTick shooter.Id attClubId ShotBlocked
                          createEvent subTick g.Id gkClubId Save ]
                    | _ when not onTarget -> [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
                    | _ -> [ createEvent subTick shooter.Id attClubId ShotOnTarget ]
