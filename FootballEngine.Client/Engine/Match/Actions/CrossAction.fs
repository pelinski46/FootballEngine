namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open MatchSpatial

module CrossAction =

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
        if dist < 0.01 then s |> withBallVelocity 0.0 0.0 vz
        else s |> withBallVelocity (dx / dist * speed) (dy / dist * speed) vz

    // Phase 3: normaliseAttr for crosser attributes
    // Phase 5: JIT — all positions resolved at execution time
    let resolve (subTick: int) (s: MatchState) : MatchState * MatchEvent list =
        let ctx = ActionContext.build s
        let attClubId = ClubSide.toClubId ctx.AttSide s
        let attSide = side (ClubSide.toClubId ctx.AttSide s) s
        let defSide = side (ClubSide.toClubId ctx.DefSide s) s
        let clubId = attClubId

        let bX, bY = ballXY s

        // JIT: current nearest player to ball
        let mutable crosserIdx = 0
        let mutable crosserDistSq = System.Double.MaxValue
        for i = 0 to attSide.Positions.Length - 1 do
            let dx = attSide.Positions[i].X - bX
            let dy = attSide.Positions[i].Y - bY
            let dSq = dx * dx + dy * dy
            if dSq < crosserDistSq then
                crosserDistSq <- dSq
                crosserIdx <- i

        let crosser = attSide.Players[crosserIdx]
        let crosserCond = attSide.Conditions[crosserIdx]
        let condNorm = PhysicsContract.normaliseCondition crosserCond

        // Phase 3: normaliseAttr
        let crossMean =
            BalanceConfig.CrossBaseMean
            + PhysicsContract.normaliseAttr crosser.Technical.Crossing * BalanceConfig.CrossCrossingWeight
            + PhysicsContract.normaliseAttr crosser.Technical.Passing * BalanceConfig.CrossPassingWeight
            + ctx.AttBonus.SetPlay

        let successChance =
            betaSample
                crossMean
                (BalanceConfig.CrossSuccessShapeAlpha + condNorm * BalanceConfig.CrossSuccessConditionMultiplier)

        // JIT: current positions of targets and defenders
        let defOutfield = outfieldRoster defSide

        let targets =
            teamRoster attSide
            |> Array.filter (fun (p, _, _) ->
                p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)
            |> Array.sortBy (fun (_, sp, _) ->
                let defDist =
                    defOutfield
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
                PhysicsContract.normaliseAttr (target.Physical.Strength + target.Technical.Heading |> min 20)
                * physicalVariation crosserCond

            let gkScore = gkSkill / 150.0

            let nearDefs =
                defOutfield
                |> Array.sumBy (fun (p, _, _) -> PhysicsContract.normaliseAttr p.Mental.Positioning)
                |> fun v -> v / float (max 1 defOutfield.Length)

            let spin =
                { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.2
                  Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.8 }

            if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                let s1, goalEvents = awardGoal ctx.AttSide (Some target.Id) subTick s
                s1, (event subTick crosser.Id clubId (CrossAttempt true)) :: goalEvents
            else
                let blockPos =
                    nearestOutfield defSide targetSp.X targetSp.Y
                    |> Option.map (fun (_, dSp) -> dSp.X, dSp.Y)
                    |> Option.defaultValue (
                        if ctx.Dir = LeftToRight then
                            (PhysicsContract.PenaltyAreaDepth, PhysicsContract.PitchWidth / 2.0)
                        else
                            (PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth, PhysicsContract.PitchWidth / 2.0))

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
                                    10.0)
                            Ball = { s''.Ball with Spin = spin } }

                s',
                [ event subTick crosser.Id clubId (CrossAttempt true)
                  event subTick target.Id clubId ShotBlocked ]
        else
            // Phase 2: target positions in metres
            let targetX =
                if ctx.Dir = LeftToRight then
                    PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                else
                    PhysicsContract.PenaltyAreaDepth

            let s' =
                s
                |> ballTowards targetX (PhysicsContract.PitchWidth / 2.0) 15.0 2.0
                |> flipPossessionAndClearOffside ctx.AttSide

            s', [ event subTick crosser.Id clubId (CrossAttempt false) ]
