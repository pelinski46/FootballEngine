namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module CrossAction =

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build ctx state
        let cc = ctx.Config.Cross
        let attSlots = actx.Att.OwnSlots
        let defSlots = actx.Def.OwnSlots
        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y
        match MatchSpatial.nearestActiveSlot attSlots bX bY with
        | ValueNone -> []
        | ValueSome crosserSlot ->
            let crosser = crosserSlot.Player
            let crosserCond = crosserSlot.Condition
            let crosserPos = crosserSlot.Pos
            state.Ball <-
                { state.Ball with
                    Possession = InFlight (state.AttackingSide, crosser.Id) }

            let attClubId = actx.Att.ClubId
            let condNorm = PhysicsContract.normaliseCondition crosserCond

            let crossMean =
                cc.BaseMean
                + PhysicsContract.normaliseAttr crosser.Technical.Crossing
                  * cc.CrossingWeight
                + PhysicsContract.normaliseAttr crosser.Technical.Passing
                  * cc.PassingWeight
                + actx.Att.Bonus.SetPlay

            let successChance =
                betaSample
                    crossMean
                    (cc.SuccessShapeAlpha
                     + condNorm * cc.SuccessConditionMultiplier)

            let defOutfield =
                defSlots
                |> Array.map (function
                    | PlayerSlot.Active s when s.Player.Position <> GK -> Some(s.Player, s.Pos)
                    | _ -> None)
                |> Array.choose id

            let targets =
                attSlots
                |> Array.map (function
                    | PlayerSlot.Active s when s.Profile.AerialThreat > cc.AerialThreatThreshold || s.Profile.AttackingDepth > cc.AttackingDepthThreshold ->
                        Some(s.Player, s.Pos)
                    | _ -> None)
                |> Array.choose id
                |> Array.sortBy (fun (_, sp) ->
                    let defDist =
                        if defOutfield.Length = 0 then 999.0<meterSquared>
                        else
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
                    gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue cc.GkSkillDefault

                let headerScore =
                    PhysicsContract.normaliseAttr (min 20 (target.Physical.Strength + target.Technical.Heading))
                    * physicalVariation crosserCond

                let gkScore = gkSkill / cc.GkSkillDivisor

                let nearDefs =
                    defOutfield
                    |> Array.sumBy (fun (p, _) -> PhysicsContract.normaliseAttr p.Mental.Positioning)
                    |> fun v -> v / float (max 1 defOutfield.Length)

                let spin =
                    { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinTopMult * 1.0<radianPerSecond>
                      Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinSideMult * 1.0<radianPerSecond> }

                if logisticBernoulli (headerScore - gkScore - nearDefs) cc.HeaderLogisticSteepness then
                    [ createEvent subTick crosser.Id attClubId (CrossAttempt true)
                      createEvent subTick target.Id attClubId Goal ]
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

                    ballTowards crosserPos.X crosserPos.Y bx by cc.Speed cc.Vz state

                    let defClub = ClubSide.flip actx.Att.ClubSide

                    state.Ball <-
                        { state.Ball with
                            Possession = Contest(defClub)
                            Spin = spin }

                    clearOffsideSnapshot state

                    adjustMomentum actx.Att.AttackDir (-cc.FailMomentum) state

                    [ createEvent subTick crosser.Id attClubId (CrossAttempt true) ]
            else
                let targetX =
                    if actx.Att.AttackDir = LeftToRight then
                        PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
                    else
                        PhysicsContract.PenaltyAreaDepth

                let defClub = ClubSide.flip actx.Att.ClubSide

                ballTowards crosserPos.X crosserPos.Y targetX (PhysicsContract.PitchWidth / 2.0) cc.FallbackSpeed cc.FallbackVz state

                state.Ball <-
                    { state.Ball with
                        Possession = Contest(defClub) }
                clearOffsideSnapshot state

                [ createEvent subTick crosser.Id attClubId (CrossAttempt false) ]
