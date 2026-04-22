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
        | ST
        | AMC
        | AML
        | AMR -> cfg.FinishingBonusAM
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
        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots

        let bX = state.Ball.Position.X
        let bY = state.Ball.Position.Y

        let inChance = actx.Zone = AttackingZone || actx.Zone = MidfieldZone

        match MatchSpatial.nearestActiveSlot attSlots bX bY with
        | ValueNone -> []
        | ValueSome shooterSlot ->
            let shooter = shooterSlot.Player

            if not inChance then
                [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
            else

                let shooter, shooterCond, shooterProfile =
                    shooterSlot.Player, shooterSlot.Condition, shooterSlot.Profile

                let defPlayers = playersArray defSlots

                let quality =
                    let distToGoal = PhysicsContract.distToGoal bX actx.Att.AttackDir

                    let distNorm =
                        PhysicsContract.clamp (distToGoal / sc.NormalisationDistance) 0.0 1.0

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

                    let speed = PhysicsContract.shotSpeed shooter.Technical.Finishing
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

                    let gk = defPlayers |> Array.tryFind (fun p -> p.Position = GK)

                    let onTarget =
                        bernoulli (
                            sc.OnTargetBase
                            + finishingNorm * sc.OnTargetMultiplier
                        )

                    let gkSaves =
                        match gk with
                        | Some g ->
                            let savePower =
                                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale sc.GkReflexesStatMult
                                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale sc.GkOneOnOneStatMult

                            let adjustedSave = savePower
                            onTarget && bernoulli (adjustedSave / (adjustedSave + finishing + sc.SaveDenominatorOffset))
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
                    | _ -> [ createEvent subTick shooter.Id attClubId Goal ]
