namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module CrossAction =

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attSlots = getSlots state actx.AttSide
        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y
        let crosserIdx = nearestIdxToBall attSlots bX bY
        let crosser =
            match attSlots[crosserIdx] with
            | PlayerSlot.Active s -> s.Player
            | _ -> Unchecked.defaultof<Player>

        state.Ball <-
            { state.Ball with
                Possession = InFlight (state.AttackingClub, crosser.Id) }

        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots = getSlots state actx.AttSide
        let defSlots = getSlots state actx.DefSide

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let crosserIdx = nearestIdxToBall attSlots bX bY

        let crosser, crosserCond, crosserPos =
            match attSlots[crosserIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition, s.Pos
            | _ -> Unchecked.defaultof<Player>, 0, kickOffSpatial

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
            |> Array.map (function
                | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                | _ -> None)
            |> Array.choose id

        let targets =
            attSlots
            |> Array.map (function
                | PlayerSlot.Active s when s.Profile.AerialThreat > 0.4 || s.Profile.AttackingDepth > 0.5 ->
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
                { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.2<radianPerSecond>
                  Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * 0.8<radianPerSecond> }

            if logisticBernoulli (headerScore - gkScore - nearDefs) 3.0 then
                let goalEvents = awardGoal actx.AttSide (Some target.Id) subTick ctx state
                (createEvent subTick crosser.Id attClubId (CrossAttempt true)) :: goalEvents
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

                ballTowards crosserPos.X crosserPos.Y bx by BalanceConfig.CrossSpeed BalanceConfig.CrossVz state

                let defClub = ClubSide.flip actx.AttSide

                state.Ball <-
                    { state.Ball with
                        Possession = Loose
                        Spin = spin }

                clearOffsideSnapshot state

                adjustMomentum actx.Dir (-BalanceConfig.CrossFailMomentum) state

                [ createEvent subTick crosser.Id attClubId (CrossAttempt true) ]
        else
            let targetX =
                if actx.Dir = LeftToRight then
                    PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                else
                    PhysicsContract.PenaltyAreaDepth

            let defClub = ClubSide.flip actx.AttSide

            ballTowards crosserPos.X crosserPos.Y targetX (PhysicsContract.PitchWidth / 2.0) 15.0<meter/second> 2.0<meter/second> state

            state.Ball <-
                { state.Ball with
                    Possession = Loose }
            clearOffsideSnapshot state

            [ createEvent subTick crosser.Id attClubId (CrossAttempt false) ]
