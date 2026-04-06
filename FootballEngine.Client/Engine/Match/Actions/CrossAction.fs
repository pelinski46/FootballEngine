namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial

module CrossAction =

    let private event subTick playerId clubId t =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let private ballTowards (targetX: float) (targetY: float) (speed: float) (vz: float) (state: SimState) =
        let bX = state.Ball.Position.X
        let bY = state.Ball.Position.Y
        let dx = targetX - bX
        let dy = targetY - bY
        let dist = sqrt (dx * dx + dy * dy)

        if dist < 0.01 then
            withBallVelocity 0.0 0.0 vz state
        else
            withBallVelocity (dx / dist * speed) (dy / dist * speed) vz state

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

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let mutable crosserIdx = 0
        let mutable crosserDistSq = System.Double.MaxValue

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - bX
                let dy = s.Pos.Y - bY
                let dSq = dx * dx + dy * dy

                if dSq < crosserDistSq then
                    crosserDistSq <- dSq
                    crosserIdx <- i
            | _ -> ()

        let crosser, crosserCond =
            match attSlots[crosserIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition
            | _ -> Unchecked.defaultof<Player>, 0

        let condNorm = PhysicsContract.normaliseCondition crosserCond

        let crossMean =
            BalanceConfig.CrossBaseMean
            + PhysicsContract.normaliseAttr crosser.Technical.Crossing
              * BalanceConfig.CrossCrossingWeight
            + PhysicsContract.normaliseAttr crosser.Technical.Passing
              * BalanceConfig.CrossPassingWeight
            + actx.AttBonus.SetPlay

        let successChance =
            betaSample
                crossMean
                (BalanceConfig.CrossSuccessShapeAlpha
                 + condNorm * BalanceConfig.CrossSuccessConditionMultiplier)

        let defOutfield =
            defSlots
            |> Array.mapi (fun i slot ->
                match slot with
                | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                | _ -> None)
            |> Array.choose id

        let targets =
            attSlots
            |> Array.mapi (fun i slot ->
                match slot with
                | PlayerSlot.Active s when
                    s.Player.Position = ST
                    || s.Player.Position = AML
                    || s.Player.Position = AMR
                    || s.Player.Position = AMC
                    ->
                    Some(s.Player, s.Pos)
                | _ -> None)
            |> Array.choose id
            |> Array.sortBy (fun (_, sp) ->
                let defDist =
                    defOutfield
                    |> Array.map (fun (_, dSp) ->
                        let dx = dSp.X - sp.X
                        let dy = dSp.Y - sp.Y
                        dx * dx + dy * dy)
                    |> Array.min

                -(defDist))

        if targets.Length > 0 && bernoulli successChance then
            let target, targetSp = targets[0]

            let gk =
                defSlots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Position = GK -> Some s.Player
                    | _ -> None)

            let gkSkill =
                gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

            let headerScore =
                PhysicsContract.normaliseAttr (min 20 (target.Physical.Strength + target.Technical.Heading))
                * physicalVariation crosserCond

            let gkScore = gkSkill / 150.0

            let nearDefs =
                defOutfield
                |> Array.sumBy (fun (p, _) -> PhysicsContract.normaliseAttr p.Mental.Positioning)
                |> fun v -> v / float (max 1 defOutfield.Length)

            let spin =
                { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.2
                  Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.8 }

            if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                let goalEvents = awardGoal actx.AttSide (Some target.Id) subTick ctx state
                (event subTick crosser.Id attClubId (CrossAttempt true)) :: goalEvents
            else
                let blockPos =
                    defOutfield
                    |> Array.map (fun (_, dSp) ->
                        let dx = dSp.X - targetSp.X
                        let dy = dSp.Y - targetSp.Y
                        dx * dx + dy * dy, (dSp.X, dSp.Y))
                    |> Array.minBy fst
                    |> snd

                let bx, by = blockPos

                ballTowards bx by BalanceConfig.CrossSpeed BalanceConfig.CrossVz state
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.CrossFailMomentum) state
                state.Ball <- { state.Ball with Spin = spin }

                [ event subTick crosser.Id attClubId (CrossAttempt true)
                  event subTick target.Id attClubId ShotBlocked ]
        else
            let targetX =
                if actx.Dir = LeftToRight then
                    PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                else
                    PhysicsContract.PenaltyAreaDepth

            ballTowards targetX (PhysicsContract.PitchWidth / 2.0) 15.0 2.0 state
            flipPossession state

            [ event subTick crosser.Id attClubId (CrossAttempt false) ]
