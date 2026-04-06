namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchFormulas

module ShotAction =

    let private event subTick playerId clubId t =
        { SubTick = subTick
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

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let defSlots =
            if actx.DefSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let bX = state.Ball.Position.X

        let inChance = actx.Zone = AttackingZone || actx.Zone = MidfieldZone

        if not inChance then
            []
        else
            let mutable shooterIdx = 0
            let mutable shooterDistSq = System.Double.MaxValue

            for i = 0 to attSlots.Length - 1 do
                match attSlots[i] with
                | PlayerSlot.Active s ->
                    let dx = s.Pos.X - bX
                    let dy = s.Pos.Y - state.Ball.Position.Y
                    let dSq = dx * dx + dy * dy

                    if dSq < shooterDistSq then
                        shooterDistSq <- dSq
                        shooterIdx <- i
                | _ -> ()

            let shooter, shooterCond =
                match attSlots[shooterIdx] with
                | PlayerSlot.Active s -> s.Player, s.Condition
                | _ -> Unchecked.defaultof<Player>, 0

            let defPlayers =
                Array.init defSlots.Length (fun i ->
                    match defSlots[i] with
                    | PlayerSlot.Active s -> s.Player
                    | _ -> Unchecked.defaultof<Player>)

            let quality =
                let distToGoal = AttackDir.distToGoal bX actx.Dir

                let distNorm =
                    Math.Clamp(distToGoal / BalanceConfig.ShotNormalisationDistance, 0.0, 1.0)

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
                []
            else
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

                let composure =
                    pressureMultiplier attClubId ctx state * tacticsCfg.UrgencyMultiplier

                let u = matchUrgency attClubId ctx state * tacticsCfg.UrgencyMultiplier

                let dist =
                    AttackDir.distToGoal bX actx.Dir * BalanceConfig.ShotDistanceToGoalMultiplier

                let homeComposure = actx.AttBonus.ShotCompos
                let finishing = calcShotPower shooter shooterCond composure u dist homeComposure
                let dirSign = AttackDir.forwardX actx.Dir

                let finishingNorm =
                    Math.Clamp(
                        PhysicsContract.normaliseAttr shooter.Technical.Finishing,
                        BalanceConfig.ShotFinishingMin,
                        BalanceConfig.ShotFinishingMax
                    )

                let speed = PhysicsContract.shotSpeed shooter.Technical.Finishing

                let angleSpread = BalanceConfig.ShotAngleSpreadBase * (1.0 - finishingNorm)
                let angle = normalSample 0.0 angleSpread
                let vx = dirSign * speed * Math.Cos(angle)
                let vy = speed * Math.Sin(angle)
                let vz = abs (normalSample BalanceConfig.ShotVzBase BalanceConfig.ShotVzVariance)

                let spin =
                    { Top = -(PhysicsContract.normaliseAttr shooter.Technical.Finishing) * 0.4
                      Side = 0.0 }

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
                                Vx = vx
                                Vy = vy
                                Vz = vz
                                X = bX }
                        Spin = spin
                        LastTouchBy = Some shooter.Id }

                adjustMomentum actx.Dir BalanceConfig.DuelMomentumBonus state
                clearOffsideSnapshot state

                let gkClubId = if actx.DefSide = HomeClub then ctx.Home.Id else ctx.Away.Id

                match gk with
                | Some g when gkSaves ->
                    [ event subTick shooter.Id attClubId ShotBlocked
                      event subTick g.Id gkClubId Save ]
                | _ when not onTarget -> [ event subTick shooter.Id attClubId ShotOffTarget ]
                | _ -> []
