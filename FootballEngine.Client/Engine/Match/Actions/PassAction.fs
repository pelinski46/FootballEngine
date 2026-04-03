namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchSpatial

module PassAction =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let private ballTowards (targetX: float) (targetY: float) (speed: float) (vz: float) (s: MatchState) =
        let bX = s.Ball.Position.X
        let bY = s.Ball.Position.Y
        let dx = targetX - bX
        let dy = targetY - bY
        let dist = sqrt (dx * dx + dy * dy)

        if dist < 0.01 then
            s |> withBallVelocity 0.0 0.0 vz
        else
            s |> withBallVelocity (dx / dist * speed) (dy / dist * speed) vz

    let resolve (second: int) (s: MatchState) (target: Player) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s
        let clubId = attClubId

        match attSide.Players |> Array.tryFindIndex (fun p -> p.Id = target.Id) with
        | None -> s, []
        | Some targetIdx ->
            let targetSp = attSide.Positions[targetIdx]
            let offside = isOffside target targetSp.X s ctx.Dir

            if offside then
                s
                |> flipPossessionAndClearOffside ctx.AttSide
                |> fun s'' ->
                    { s'' with
                        Momentum =
                            Math.Clamp(
                                s''.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.PassOffsideMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second target.Id clubId (PassIncomplete target.Id) ]

            else
                let passer =
                    attSide.Players
                    |> Array.tryFind (fun p -> p.Id = (s.Ball.LastTouchBy |> Option.defaultValue -1))
                    |> Option.defaultValue attSide.Players[0]

                let passerCond =
                    attSide.Conditions |> Array.tryFind (fun _ -> true) |> Option.defaultValue 70

                let condition = float passerCond / 100.0

                let passMean =
                    BalanceConfig.PassBaseMean
                    + float passer.Technical.Passing / 100.0 * BalanceConfig.PassTechnicalWeight
                    + float passer.Mental.Vision / 100.0 * BalanceConfig.PassVisionWeight
                    + ctx.AttBonus.PassAcc

                let successChance =
                    betaSample
                        passMean
                        (BalanceConfig.PassSuccessShapeAlpha
                         + condition * BalanceConfig.PassSuccessConditionMultiplier)

                if bernoulli successChance then
                    let snapshot = snapshotAtPass passer target s ctx.Dir

                    let s' =
                        s
                        |> ballTowards targetSp.X targetSp.Y BalanceConfig.PassSpeed BalanceConfig.PassVz
                        |> fun s'' ->
                            { s'' with
                                Momentum =
                                    Math.Clamp(
                                        s''.Momentum + AttackDir.momentumDelta ctx.Dir BalanceConfig.PassSuccessMomentum,
                                        -10.0,
                                        10.0
                                    )
                                PendingOffsideSnapshot = Some snapshot
                                Ball =
                                    { s''.Ball with
                                        Spin = { Top = 0.0; Side = 0.0 } } }

                    s', [ event second passer.Id clubId (PassCompleted(passer.Id, target.Id)) ]
                else
                    s
                    |> flipPossessionAndClearOffside ctx.AttSide
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.PassFailMomentum,
                                    -10.0,
                                    10.0
                                ) },
                        [ event second passer.Id clubId (PassIncomplete passer.Id) ]


    let resolveLong (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let clubId = attClubId

        let bX, bY = s.Ball.Position.X, s.Ball.Position.Y

        let passerIdx =
            attSide.Positions
            |> Array.mapi (fun i _ -> i)
            |> Array.minBy (fun i ->
                let dx = attSide.Positions[i].X - bX
                let dy = attSide.Positions[i].Y - bY
                dx * dx + dy * dy)

        let passer = attSide.Players[passerIdx]
        let passerCond = attSide.Conditions[passerIdx]
        let condition = float passerCond / 100.0

        let longMean =
            BalanceConfig.LongBallBaseMean
            + float passer.Technical.LongShots / 100.0 * BalanceConfig.LongBallLongShotsWeight
            + float passer.Technical.Passing / 100.0 * BalanceConfig.LongBallPassingWeight
            + float passer.Mental.Vision / 100.0 * BalanceConfig.LongBallVisionWeight
            + ctx.AttBonus.SetPlay

        let successChance =
            betaSample
                longMean
                (BalanceConfig.LongBallSuccessShapeAlpha
                 + condition * BalanceConfig.LongBallSuccessConditionMultiplier)

        let forwards =
            teamRoster attSide
            |> Array.filter (fun (p, _, _) ->
                p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

        if bernoulli successChance && forwards.Length > 0 then
            let target, targetSp, _ = forwards[0]
            let offside = isOffside target targetSp.X s ctx.Dir

            if offside then
                s
                |> flipPossessionAndClearOffside ctx.AttSide
                |> fun s'' ->
                    { s'' with
                        Momentum =
                            Math.Clamp(
                                s''.Momentum
                                - AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallOffsideMomentum,
                                -10.0,
                                10.0
                            ) },
                    [ event second passer.Id clubId (LongBall false) ]

            else
                let snapshot = snapshotAtPass passer target s ctx.Dir

                let targetIdx =
                    attSide |> teamRoster |> Array.findIndex (fun (p, _, _) -> p.Id = target.Id)

                let s' =
                    s
                    |> ballTowards targetSp.X targetSp.Y BalanceConfig.LongBallSpeed BalanceConfig.LongBallVz
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum
                                    + AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallSuccessMomentum,
                                    -10.0,
                                    10.0
                                )
                            PendingOffsideSnapshot = Some snapshot }

                s', [ event second passer.Id clubId (LongBall true) ]
        else
            s
            |> flipPossessionAndClearOffside ctx.AttSide
            |> fun s'' ->
                { s'' with
                    Momentum =
                        Math.Clamp(
                            s''.Momentum
                            - AttackDir.momentumDelta ctx.Dir BalanceConfig.LongBallFailMomentum,
                            -10.0,
                            10.0
                        ) },
                [ event second passer.Id clubId (LongBall false) ]
