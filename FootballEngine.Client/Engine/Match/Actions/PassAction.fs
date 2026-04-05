namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchSpatial

module PassAction =

    // Phase 0: Second -> SubTick
    let private event subTick playerId clubId t =
        { SubTick = subTick
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

    // Phase 5: JIT — target position resolved at execution time
    // Phase 3: normaliseAttr for passer attributes
    let resolve (subTick: int) (s: MatchState) (target: Player) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let clubId = attClubId

        match attSide.Players |> Array.tryFindIndex (fun p -> p.Id = target.Id) with
        | None -> s, []
        | Some targetIdx ->
            // JIT: use CURRENT position at execution time
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
                    [ event subTick target.Id clubId (PassIncomplete target.Id) ]
            else
                let passer =
                    attSide.Players
                    |> Array.tryFind (fun p -> p.Id = (s.Ball.LastTouchBy |> Option.defaultValue -1))
                    |> Option.defaultValue attSide.Players[0]

                let passerCond =
                    attSide.Players
                    |> Array.tryFindIndex (fun p -> p.Id = passer.Id)
                    |> Option.map (fun i -> attSide.Conditions[i])
                    |> Option.defaultValue 70

                let condNorm = PhysicsContract.normaliseCondition passerCond

                // Phase 3: normaliseAttr instead of /100.0
                let passMean =
                    BalanceConfig.PassBaseMean
                    + PhysicsContract.normaliseAttr passer.Technical.Passing
                      * BalanceConfig.PassTechnicalWeight
                    + PhysicsContract.normaliseAttr passer.Mental.Vision
                      * BalanceConfig.PassVisionWeight
                    + ctx.AttBonus.PassAcc

                let successChance =
                    betaSample
                        passMean
                        (BalanceConfig.PassSuccessShapeAlpha
                         + condNorm * BalanceConfig.PassSuccessConditionMultiplier)

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

                    s', [ event subTick passer.Id clubId (PassCompleted(passer.Id, target.Id)) ]
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
                        [ event subTick passer.Id clubId (PassIncomplete passer.Id) ]

    // Phase 3: normaliseAttr for long ball attributes
    // Phase 5: JIT — target resolved from current positions
    let resolveLong (subTick: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let clubId = attClubId

        let bX, bY = s.Ball.Position.X, s.Ball.Position.Y

        let mutable passerIdx = 0
        let mutable passerDistSq = System.Double.MaxValue
        for i = 0 to attSide.Positions.Length - 1 do
            let dx = attSide.Positions[i].X - bX
            let dy = attSide.Positions[i].Y - bY
            let dSq = dx * dx + dy * dy
            if dSq < passerDistSq then
                passerDistSq <- dSq
                passerIdx <- i

        let passer = attSide.Players[passerIdx]
        let passerCond = attSide.Conditions[passerIdx]
        let condNorm = PhysicsContract.normaliseCondition passerCond

        // Phase 3: normaliseAttr
        let longMean =
            BalanceConfig.LongBallBaseMean
            + PhysicsContract.normaliseAttr passer.Technical.LongShots
              * BalanceConfig.LongBallLongShotsWeight
            + PhysicsContract.normaliseAttr passer.Technical.Passing
              * BalanceConfig.LongBallPassingWeight
            + PhysicsContract.normaliseAttr passer.Mental.Vision
              * BalanceConfig.LongBallVisionWeight
            + ctx.AttBonus.SetPlay

        let successChance =
            betaSample
                longMean
                (BalanceConfig.LongBallSuccessShapeAlpha
                 + condNorm * BalanceConfig.LongBallSuccessConditionMultiplier)

        // JIT: current positions of forwards
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
                    [ event subTick passer.Id clubId (LongBall false) ]
            else
                let snapshot = snapshotAtPass passer target s ctx.Dir

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

                s', [ event subTick passer.Id clubId (LongBall true) ]
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
                [ event subTick passer.Id clubId (LongBall false) ]
