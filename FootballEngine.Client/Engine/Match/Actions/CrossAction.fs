namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchSpatial

module CrossAction =

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

    let resolve (second: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let defClubId = ClubSide.toClubId ctx.DefSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s
        let clubId = attClubId

        let bX, bY = ballXY s

        let crosserIdx =
            attSide.Positions
            |> Array.mapi (fun i _ -> i)
            |> Array.minBy (fun i ->
                let dx = attSide.Positions[i].X - bX
                let dy = attSide.Positions[i].Y - bY
                dx * dx + dy * dy)

        let crosser = attSide.Players[crosserIdx]
        let crosserCond = attSide.Conditions[crosserIdx]
        let condition = float crosserCond / 100.0

        let crossMean =
            BalanceConfig.CrossBaseMean
            + float crosser.Technical.Crossing / 100.0 * BalanceConfig.CrossCrossingWeight
            + float crosser.Technical.Passing / 100.0 * BalanceConfig.CrossPassingWeight
            + ctx.AttBonus.SetPlay

        let successChance =
            betaSample
                crossMean
                (BalanceConfig.CrossSuccessShapeAlpha
                 + condition * BalanceConfig.CrossSuccessConditionMultiplier)

        let targets =
            teamRoster attSide
            |> Array.filter (fun (p, _, _) ->
                p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)
            |> Array.sortBy (fun (_, sp, _) ->
                let defDist =
                    outfieldRoster defSide
                    |> Array.map (fun (_, dSp, _) ->
                        let dx = dSp.X - sp.X
                        let dy = dSp.Y - sp.Y
                        dx * dx + dy * dy)
                    |> Array.min

                -(defDist))

        if targets.Length > 0 && bernoulli successChance then
            let target, targetSp, _ = targets[0]
            let gk = defSide.Players |> Array.tryFind (fun p -> p.Position = GK)

            let gkSkill =
                gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

            let headerScore =
                float (target.Physical.Strength + target.Technical.Heading) / 200.0
                * physicalVariation crosserCond

            let gkScore = gkSkill / 150.0

            let nearDefs =
                outfieldRoster defSide
                |> Array.sumBy (fun (p, _, _) -> float p.Mental.Positioning / 200.0)
                |> fun v -> v / float (max 1 (outfieldRoster defSide).Length)

            let spin =
                { Top = -(float crosser.Technical.Crossing / 20.0) * 0.2
                  Side = (float crosser.Technical.Crossing / 20.0) * 0.8 }

            let targetIdx =
                attSide |> teamRoster |> Array.findIndex (fun (p, _, _) -> p.Id = target.Id)

            if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                let s1, goalEvents = awardGoal ctx.AttSide (Some target.Id) second s
                s1, (event second crosser.Id clubId (CrossAttempt true)) :: goalEvents
            else
                let blockPos =
                    nearestOutfield defSide targetSp.X targetSp.Y
                    |> Option.map (fun (_, dSp) -> dSp.X, dSp.Y)
                    |> Option.defaultValue (if ctx.Dir = LeftToRight then (20.0, 50.0) else (80.0, 50.0))

                let bx, by = blockPos

                let s' =
                    s
                    |> ballTowards bx by BalanceConfig.CrossSpeed BalanceConfig.CrossVz
                    |> flipPossessionAndClearOffside ctx.AttSide
                    |> fun s'' ->
                        { s'' with
                            Momentum =
                                Math.Clamp(
                                    s''.Momentum - AttackDir.momentumDelta ctx.Dir BalanceConfig.CrossFailMomentum,
                                    -10.0,
                                    10.0
                                )
                            Ball = { s''.Ball with Spin = spin } }

                s',
                [ event second crosser.Id clubId (CrossAttempt true)
                  event second target.Id clubId ShotBlocked ]

        else
            let targetX = if ctx.Dir = LeftToRight then 85.0 else 15.0

            let s' =
                s
                |> ballTowards targetX 50.0 15.0 2.0
                |> flipPossessionAndClearOffside ctx.AttSide

            s', [ event second crosser.Id clubId (CrossAttempt false) ]
