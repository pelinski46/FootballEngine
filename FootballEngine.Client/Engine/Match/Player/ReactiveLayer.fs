namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

type ReactiveIntent =
    | PressBall of tx: float32 * ty: float32
    | TackleAttempt of oppSlotIdx: int
    | InterceptLane of tx: float32 * ty: float32
    | NoReaction

module ReactiveLayer =

    let private reactiveRadius = 25.0<meter>
    let private tackleRadius = 3.0<meter>
    let private pressRadius = 12.0<meter>
    let private coverRadius = 20.0<meter>
    let private laneCutRadiusSq = 9.0f

    let evaluateReactiveIntent
        (meIdx: int)
        (ownFrame: TeamFrame)
        (oppFrame: TeamFrame)
        (ballCarrierOppIdx: int)
        (bcx: float32)
        (bcy: float32)
        (aggression: float)
        : ReactiveIntent =

        if ballCarrierOppIdx < 0 then NoReaction
        else
            let myX = ownFrame.PosX[meIdx]
            let myY = ownFrame.PosY[meIdx]
            let dx = float myX - float bcx
            let dy = float myY - float bcy
            let dist = sqrt (dx * dx + dy * dy) * 1.0<meter>

            if dist > reactiveRadius then NoReaction
            elif dist < tackleRadius then
                TackleAttempt ballCarrierOppIdx
            elif dist < pressRadius then
                PressBall(bcx, bcy)
            else
                let mutable bestTargetX = bcx
                let mutable bestTargetY = bcy
                let mutable bestScore = System.Single.MaxValue
                for j = 0 to oppFrame.SlotCount - 1 do
                    if j <> ballCarrierOppIdx then
                        let active = oppFrame.Occupancy[j]
                        match active with
                        | OccupancyKind.Active _ ->
                            let odx = oppFrame.PosX[j] - bcx
                            let ody = oppFrame.PosY[j] - bcy
                            let oDist = sqrt (odx * odx + ody * ody)
                            if oDist > 3.0f && oDist < 30.0f then
                                let laneDistSq = MatchSpatial.pointToLineDistSq myX myY bcx bcy oppFrame.PosX[j] oppFrame.PosY[j]
                                if laneDistSq < laneCutRadiusSq && laneDistSq < bestScore then
                                    bestScore <- laneDistSq
                                    bestTargetX <- oppFrame.PosX[j]
                                    bestTargetY <- oppFrame.PosY[j]
                        | _ -> ()
                if bestScore < laneCutRadiusSq then
                    InterceptLane(bestTargetX, bestTargetY)
                else
                    NoReaction
