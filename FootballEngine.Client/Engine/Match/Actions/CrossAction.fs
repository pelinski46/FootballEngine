namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial
open FootballEngine.PhysicsContract

module CrossAction =

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : ActionResult =
        let actx = ActionContext.build ctx state
        let cc = ctx.Config.Cross
        let attFrame = actx.Att.OwnFrame
        let defFrame = actx.Def.OwnFrame
        let attRoster = SimStateOps.getRoster ctx actx.Att.ClubSide
        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y
        match SimStateOps.nearestActiveSlotInFrame attFrame bX bY with
        | ValueNone -> ActionResult.empty
        | ValueSome crosserIdx ->
            let crosser = attRoster.Players[crosserIdx]
            let crosserCond = int attFrame.Condition[crosserIdx]
            let crosserPos = { X = float attFrame.PosX[crosserIdx] * 1.0<meter>; Y = float attFrame.PosY[crosserIdx] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
            let attClubId = actx.Att.ClubId
            let condNorm = PhysicsContract.normaliseCondition crosserCond

            let crossMean =
                cc.BaseMean
                + PhysicsContract.normaliseAttr crosser.Technical.Crossing * cc.CrossingWeight
                + PhysicsContract.normaliseAttr crosser.Technical.Passing * cc.PassingWeight
                + actx.Att.Bonus.SetPlay

            let quality = betaSample crossMean (cc.SuccessShapeAlpha + condNorm * cc.SuccessConditionMultiplier)

            let defRoster = SimStateOps.getRoster ctx actx.Def.ClubSide

            let defOutfield =
                [| for i = 0 to defFrame.SlotCount - 1 do
                     match defFrame.Occupancy[i] with
                     | OccupancyKind.Active _ when defRoster.Players[i].Position <> GK ->
                         let sp = { X = float defFrame.PosX[i] * 1.0<meter>; Y = float defFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                         yield (defRoster.Players[i], sp)
                     | _ -> () |]

            let targets =
                [| for i = 0 to attFrame.SlotCount - 1 do
                     match attFrame.Occupancy[i] with
                     | OccupancyKind.Active _ ->
                         let profile = attRoster.Profiles[i]
                         if profile.AerialThreat > cc.AerialThreatThreshold || profile.AttackingDepth > cc.AttackingDepthThreshold then
                             let sp = { X = float attFrame.PosX[i] * 1.0<meter>; Y = float attFrame.PosY[i] * 1.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                             let defDist =
                                 if defOutfield.Length = 0 then 999.0<meterSquared>
                                 else
                                     defOutfield
                                     |> Array.map (fun (_, dSp) ->
                                         let dx = dSp.X - sp.X
                                         let dy = dSp.Y - sp.Y
                                         dx * dx + dy * dy)
                                     |> Array.min
                             yield (attRoster.Players[i], sp, -defDist)
                     | _ -> () |]
                |> Array.sortBy (fun (_, _, d) -> d)
                |> Array.map (fun (p, sp, _) -> p, sp)

            if targets.Length = 0 then
                let targetX = if actx.Att.AttackDir = LeftToRight then PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth else PhysicsContract.PenaltyAreaDepth
                let defClub = ClubSide.flip actx.Att.ClubSide
                ballTowards crosserPos.X crosserPos.Y targetX (PhysicsContract.PitchWidth / 2.0) cc.FallbackSpeed cc.FallbackVz state
                state.Ball <- { state.Ball with Possession = InFlight }
                clearOffsideSnapshot state
                ActionResult.ofEvents [ createEvent subTick crosser.Id attClubId (CrossLaunched(crosser.Id, crosser.Id)) ]
            else
                let target, targetSp = targets[0]

                let accuracyNoise = 0.15 * (1.0 - quality)

                let targetX = targetSp.X + normalSample 0.0 accuracyNoise * 1.0<meter> |> fun x -> PhysicsContract.clamp x 0.0<meter> PhysicsContract.PitchLength
                let targetY = targetSp.Y + normalSample 0.0 accuracyNoise * 1.0<meter> |> fun y -> PhysicsContract.clamp y 0.0<meter> PhysicsContract.PitchWidth

                let dist = sqrt ((targetX - crosserPos.X) * (targetX - crosserPos.X) + (targetY - crosserPos.Y) * (targetY - crosserPos.Y))
                let flightTime = if cc.Speed > 0.0<meter/second> then dist / cc.Speed else 1.0<second>
                let arrivalSubTick = subTick + int (float (flightTime / 1.0<second>) * float clock.SubTicksPerSecond)

                let spin = { Top = -(PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinTopMult * 1.0<radianPerSecond>; Side = (PhysicsContract.normaliseAttr crosser.Technical.Crossing) * cc.SpinSideMult * 1.0<radianPerSecond> }

                let trajectory = {
                    OriginX = crosserPos.X
                    OriginY = crosserPos.Y
                    TargetX = targetX
                    TargetY = targetY
                    LaunchSubTick = subTick
                    EstimatedArrivalSubTick = arrivalSubTick
                    KickerId = crosser.Id
                    PeakHeight = cc.Vz * cc.Vz / (2.0 * 9.80665<meter/second^2>)
                    ActionKind = BallActionKind.Cross(crosser.Id, target.Id, quality)
                }

                ballTowards crosserPos.X crosserPos.Y targetX targetY cc.Speed cc.Vz state

                state.Ball <-
                    { state.Ball with
                        Possession = InFlight
                        Spin = spin
                        LastTouchBy = Some crosser.Id
                        Trajectory = Some trajectory }

                clearOffsideSnapshot state
                ActionResult.ofEvents [ createEvent subTick crosser.Id attClubId (CrossLaunched(crosser.Id, target.Id)) ]
