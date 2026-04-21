namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas
open FootballEngine.PhysicsContract

module ShotAction =

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
        (dist: float<meter>)
        (homeComposure: float)
        =
        let bonus = finishingBonusForPosition player.Position
        let condFactor = sqrt (float condition / 100.0)
        let basePower = float player.Technical.Finishing / 20.0

        basePower
        * condFactor
        * (1.0 + composure * 0.1)
        * (1.0 + urgency * 0.05)
        * bonus

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

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
                    let distToGoal = PhysicsContract.distToGoal bX actx.Dir

                    let distNorm =
                        PhysicsContract.clamp (distToGoal / BalanceConfig.ShotNormalisationDistance) 0.0 1.0

                    let positionBonus =
                        shooterProfile.Directness * 0.3
                        + shooterProfile.AttackingDepth * 0.2
                        + shooterProfile.CreativityWeight * 0.1

                    (1.0 - distNorm) * 0.7 + positionBonus * 0.3


                if quality < BalanceConfig.ShotQualityGate then
                    [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
                else
                    let attTactics = getTactics state actx.AttSide
                    let attInstructions = getInstructions state actx.AttSide
                    let tacticsCfg = tacticsConfig attTactics attInstructions

                    let composure =
                        pressureMultiplier attClubId ctx state * tacticsCfg.UrgencyMultiplier

                    let u = matchUrgency attClubId ctx state clock * tacticsCfg.UrgencyMultiplier

                    let dist =
                        PhysicsContract.distToGoal bX actx.Dir
                        * BalanceConfig.ShotDistanceToGoalMultiplier

                    let homeComposure = actx.AttBonus.ShotCompos
                    let finishing = calcShotPower shooter shooterCond composure u dist homeComposure
                    let dirSign = PhysicsContract.forwardX actx.Dir

                    let finishingNorm =
                        Math.Clamp(
                            PhysicsContract.normaliseAttr shooter.Technical.Finishing,
                            BalanceConfig.ShotFinishingMin,
                            BalanceConfig.ShotFinishingMax
                        )

                    let speed = PhysicsContract.shotSpeed shooter.Technical.Finishing
                    let angleSpread = BalanceConfig.ShotAngleSpreadBase * (1.0 - finishingNorm)
                    let angle = normalSample 0.0 angleSpread
                    let speedMag = float speed
                    let vx = dirSign * speedMag * Math.Cos(angle)
                    let vy = speedMag * Math.Sin(angle)

                    let vz =
                        abs (normalSample (float BalanceConfig.ShotVzBase) BalanceConfig.ShotVzVariance)
                        * 1.0<meter / second>

                    let spin =
                        { Top =
                            -(PhysicsContract.normaliseAttr shooter.Technical.Finishing)
                            * 0.4
                            * 1.0<radianPerSecond>
                          Side = 0.0<radianPerSecond> }

                    let gk = defPlayers |> Array.tryFind (fun p -> p.Position = GK)

                    let onTarget =
                        bernoulli (
                            BalanceConfig.ShotOnTargetBase
                            + finishingNorm * BalanceConfig.ShotOnTargetMultiplier
                        )

                    let gkSaves =
                        match gk with
                        | Some g ->
                            let savePower =
                                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5

                            let adjustedSave = savePower + BalanceConfig.GkSaveBonus
                            onTarget && bernoulli (adjustedSave / (adjustedSave + finishing + 1.0))
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

                    SimStateOps.adjustMomentum actx.Dir BalanceConfig.DuelMomentumBonus state
                    clearOffsideSnapshot state

                    let gkClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

                    state.Ball <-
                        { state.Ball with
                            Possession = InFlight(actx.AttSide, shooter.Id) }

                    match gk with
                    | Some g when gkSaves ->
                        [ createEvent subTick shooter.Id attClubId ShotBlocked
                          createEvent subTick g.Id gkClubId Save ]
                    | _ when not onTarget -> [ createEvent subTick shooter.Id attClubId ShotOffTarget ]
                    | _ -> [ createEvent subTick shooter.Id attClubId Goal ]
