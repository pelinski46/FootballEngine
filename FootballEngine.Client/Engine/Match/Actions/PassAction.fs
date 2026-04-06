namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open MatchSpatial

module PassAction =

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

    let resolve (subTick: int) (ctx: MatchContext) (state: SimState) (target: Player) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let mutable targetIdx = -1
        let mutable targetSp = kickOffSpatial

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = target.Id ->
                targetIdx <- i
                targetSp <- s.Pos
            | _ -> ()

        if targetIdx < 0 then
            []
        else
            let offside = isOffside target targetSp.X ctx state actx.Dir

            if offside then
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.PassOffsideMomentum) state
                [ event subTick target.Id attClubId (PassIncomplete target.Id) ]
            else
                let mutable passerIdx = 0
                let mutable passerFound = false

                for i = 0 to attSlots.Length - 1 do
                    match attSlots[i] with
                    | PlayerSlot.Active s when state.Ball.LastTouchBy = Some s.Player.Id ->
                        passerIdx <- i
                        passerFound <- true
                    | _ -> ()

                let passer, passerCond =
                    if passerFound then
                        match attSlots[passerIdx] with
                        | PlayerSlot.Active s -> s.Player, s.Condition
                        | _ -> Unchecked.defaultof<Player>, 70
                    else
                        match attSlots[0] with
                        | PlayerSlot.Active s -> s.Player, s.Condition
                        | _ -> Unchecked.defaultof<Player>, 70

                let condNorm = PhysicsContract.normaliseCondition passerCond

                let passMean =
                    BalanceConfig.PassBaseMean
                    + PhysicsContract.normaliseAttr passer.Technical.Passing
                      * BalanceConfig.PassTechnicalWeight
                    + PhysicsContract.normaliseAttr passer.Mental.Vision
                      * BalanceConfig.PassVisionWeight
                    + actx.AttBonus.PassAcc

                let successChance =
                    betaSample
                        passMean
                        (BalanceConfig.PassSuccessShapeAlpha
                         + condNorm * BalanceConfig.PassSuccessConditionMultiplier)

                if bernoulli successChance then
                    let snapshot = snapshotAtPass passer target ctx state actx.Dir

                    ballTowards targetSp.X targetSp.Y BalanceConfig.PassSpeed BalanceConfig.PassVz state
                    adjustMomentum actx.Dir BalanceConfig.PassSuccessMomentum state
                    state.PendingOffsideSnapshot <- Some snapshot

                    state.Ball <-
                        { state.Ball with
                            Spin = { Top = 0.0; Side = 0.0 } }

                    [ event subTick passer.Id attClubId (PassCompleted(passer.Id, target.Id)) ]
                else
                    flipPossession state
                    adjustMomentum actx.Dir (-BalanceConfig.PassFailMomentum) state
                    [ event subTick passer.Id attClubId (PassIncomplete passer.Id) ]

    let resolveLong (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        let actx = ActionContext.build state
        let attClubId = if actx.AttSide = HomeClub then ctx.Home.Id else ctx.Away.Id

        let attSlots =
            if actx.AttSide = HomeClub then
                state.HomeSlots
            else
                state.AwaySlots

        let bX, bY = state.Ball.Position.X, state.Ball.Position.Y

        let mutable passerIdx = 0
        let mutable passerDistSq = System.Double.MaxValue

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - bX
                let dy = s.Pos.Y - bY
                let dSq = dx * dx + dy * dy

                if dSq < passerDistSq then
                    passerDistSq <- dSq
                    passerIdx <- i
            | _ -> ()

        let passer, passerCond =
            match attSlots[passerIdx] with
            | PlayerSlot.Active s -> s.Player, s.Condition
            | _ -> Unchecked.defaultof<Player>, 0

        let condNorm = PhysicsContract.normaliseCondition passerCond

        let longMean =
            BalanceConfig.LongBallBaseMean
            + PhysicsContract.normaliseAttr passer.Technical.LongShots
              * BalanceConfig.LongBallLongShotsWeight
            + PhysicsContract.normaliseAttr passer.Technical.Passing
              * BalanceConfig.LongBallPassingWeight
            + PhysicsContract.normaliseAttr passer.Mental.Vision
              * BalanceConfig.LongBallVisionWeight
            + actx.AttBonus.SetPlay

        let successChance =
            betaSample
                longMean
                (BalanceConfig.LongBallSuccessShapeAlpha
                 + condNorm * BalanceConfig.LongBallSuccessConditionMultiplier)

        let forwards =
            attSlots
            |> Array.mapi (fun i slot ->
                match slot with
                | PlayerSlot.Active s -> Some(s.Player, s.Pos)
                | _ -> None)
            |> Array.choose id
            |> Array.filter (fun (p, _) -> p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

        if bernoulli successChance && forwards.Length > 0 then
            let target, targetSp = forwards[0]
            let offside = isOffside target targetSp.X ctx state actx.Dir

            if offside then
                flipPossession state
                adjustMomentum actx.Dir (-BalanceConfig.LongBallOffsideMomentum) state
                [ event subTick passer.Id attClubId (LongBall false) ]
            else
                let snapshot = snapshotAtPass passer target ctx state actx.Dir

                ballTowards targetSp.X targetSp.Y BalanceConfig.LongBallSpeed BalanceConfig.LongBallVz state
                adjustMomentum actx.Dir BalanceConfig.LongBallSuccessMomentum state
                state.PendingOffsideSnapshot <- Some snapshot

                [ event subTick passer.Id attClubId (LongBall true) ]
        else
            flipPossession state
            adjustMomentum actx.Dir (-BalanceConfig.LongBallFailMomentum) state
            [ event subTick passer.Id attClubId (LongBall false) ]
