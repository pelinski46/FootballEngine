namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchCalc

module ShotAction =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

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
        (dist: float)
        (homeComposure: float)
        =
        let bonus = finishingBonusForPosition player.Position

        effectiveStat player.Technical.Finishing condition player.Morale (bonus * 0.8)
        + effectiveStat player.Mental.Composure condition player.Morale (1.5 * composure * urgency + homeComposure)
        + effectiveStat player.Technical.LongShots condition player.Morale 1.0
        + effectiveStat player.Physical.Pace condition player.Morale 0.5
        - dist

    let resolve (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s
        let bX = s.Ball.Position.X

        let inChance = ctx.Zone = AttackingZone || ctx.Zone = MidfieldZone

        if not inChance then
            s, []
        else
            let ballPt = bX, s.Ball.Position.Y

            let shooterIdx =
                attSide.Positions
                |> Array.mapi (fun i _ -> i)
                |> Array.minBy (fun i ->
                    let dx = attSide.Positions[i].X - fst ballPt
                    let dy = attSide.Positions[i].Y - snd ballPt
                    dx * dx + dy * dy)

            let shooter = attSide.Players[shooterIdx]
            let shooterCond = attSide.Conditions[shooterIdx]

            let quality =
                let distToGoal = AttackDir.distToGoal bX ctx.Dir
                let distNorm = Math.Clamp(distToGoal / 30.0, 0.0, 1.0)

                let positionBonus =
                    match shooter.Position with
                    | ST
                    | AMC -> 0.3
                    | AML
                    | AMR
                    | MC -> 0.2
                    | _ -> 0.0

                (1.0 - distNorm) * 0.7 + positionBonus * 0.3

            if quality < BalanceConfig.ShotQualityGate then
                s, []
            else
                let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
                let composure = pressureMultiplier attClubId s * tacticsCfg.UrgencyMultiplier
                let u = matchUrgency attClubId s * tacticsCfg.UrgencyMultiplier

                let dist =
                    AttackDir.distToGoal bX ctx.Dir * BalanceConfig.ShotDistanceToGoalMultiplier

                let homeComposure = ctx.AttBonus.ShotCompos

                let finishing = calcShotPower shooter shooterCond composure u dist homeComposure
                let dirSign = AttackDir.forwardX ctx.Dir

                let finishingNorm =
                    Math.Clamp(finishing / 300.0, BalanceConfig.ShotFinishingMin, BalanceConfig.ShotFinishingMax)

                let speed =
                    BalanceConfig.ShotBaseSpeed + finishingNorm * BalanceConfig.ShotSpeedMultiplier

                let angleSpread = BalanceConfig.ShotAngleSpreadBase * (1.0 - finishingNorm)
                let angle = normalSample 0.0 angleSpread
                let vx = dirSign * speed * Math.Cos(angle)
                let vy = speed * Math.Sin(angle)
                let vz = abs (normalSample BalanceConfig.ShotVzBase BalanceConfig.ShotVzVariance)

                let spin =
                    { Top = -(float shooter.Technical.Finishing / 20.0) * 0.4
                      Side = 0.0 }

                let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

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

                let s' =
                    { s with
                        Ball =
                            { s.Ball with
                                Position =
                                    { s.Ball.Position with
                                        Vx = vx
                                        Vy = vy
                                        Vz = vz
                                        X = bX }
                                Spin = spin
                                LastTouchBy = Some shooter.Id }
                        Momentum =
                            Math.Clamp(
                                s.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.DuelMomentumBonus,
                                -10.0,
                                10.0
                            ) }
                    |> clearOffsideSnapshot

                let gkClubId = ClubSide.toClubId (ClubSide.flip ctx.AttSide) s

                let events =
                    match gk with
                    | Some g when gkSaves ->
                        [ event second shooter.Id attClubId ShotBlocked
                          event second g.Id gkClubId Save ]
                    | _ when not onTarget -> [ event second shooter.Id attClubId ShotOffTarget ]
                    | _ -> []

                s', events
