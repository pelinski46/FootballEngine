namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PlayerSteering
open SimStateOps
open FootballEngine.PhysicsContract
open SimulationClock

module MovementEngine =

    let private applyReactiveOverrides
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (currentSubTick: int)
        =
        let oppSide = ClubSide.flip clubSide
        let ownFrame = getFrame state clubSide
        let oppFrame = getFrame state oppSide
        let roster = getRoster ctx clubSide

        let ballCarrierOppIdx =
            let cFrame = if clubSide = HomeClub then state.HomeCognitiveFrame else state.AwayCognitiveFrame
            cFrame.BallCarrierOppIdx

        if ballCarrierOppIdx < 0s then ()
        else
            let bcx = oppFrame.PosX[int ballCarrierOppIdx]
            let bcy = oppFrame.PosY[int ballCarrierOppIdx]

            for i = 0 to ownFrame.SlotCount - 1 do
                match ownFrame.Occupancy[i] with
                | OccupancyKind.Sidelined _ -> ()
                | OccupancyKind.Active _ ->
                    if ownFrame.IntentKind[i] <> IntentKind.ExecuteRun then
                        let aggression =
                            if i < roster.Players.Length then
                                float roster.Players[i].Mental.Aggression / 20.0
                            else 0.5

                        match ReactiveLayer.evaluateReactiveIntent i ownFrame oppFrame (int ballCarrierOppIdx) bcx bcy aggression with
                        | TackleAttempt oppSlot ->
                            FrameMutate.setIntent ownFrame i IntentKind.PressBall
                                oppFrame.PosX[oppSlot] oppFrame.PosY[oppSlot] oppSlot
                        | PressBall(tx, ty) ->
                            FrameMutate.setIntent ownFrame i IntentKind.PressBall tx ty 0
                        | InterceptLane(tx, ty) ->
                            FrameMutate.setIntent ownFrame i IntentKind.CoverSpace tx ty 0
                        | NoReaction -> ()
                | _ -> ()

    let updatePhysics
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (dt: float<second>)
        =
        let frame = getFrame state clubSide
        let roster = getRoster ctx clubSide
        let pcfg = state.Config.Physics

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Sidelined _ -> ()
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]
                let condition = int frame.Condition[i]

                let targetX = frame.IntentTargetX[i]
                let targetY = frame.IntentTargetY[i]

                let hasBall =
                    match state.Ball.Possession with
                    | Owned(_, pid) -> pid = roster.Players[i].Id
                    | _ -> false

                let chasingBall =
                    match frame.IntentKind[i] with
                    | IntentKind.PressBall | IntentKind.RecoverBall -> true
                    | _ -> false

                let myX = float frame.PosX[i] * 1.0<meter>
                let myY = float frame.PosY[i] * 1.0<meter>
                let myVx = float frame.VelX[i] * 1.0<meter/second>
                let myVy = float frame.VelY[i] * 1.0<meter/second>

                let current = {
                    X = myX; Y = myY; Z = 0.0<meter>
                    Vx = myVx; Vy = myVy; Vz = 0.0<meter/second>
                }

                let newPos =
                    PlayerPhysics.steerSoA
                        pcfg player condition current i
                        frame.PosX frame.PosY frame.SlotCount
                        (targetX, targetY)
                        state.Ball.Position hasBall chasingBall dt

                FrameMutate.setPos frame i newPos.X newPos.Y
                FrameMutate.setVel frame i newPos.Vx newPos.Vy

    let refreshCache
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        =
        let frame = getFrame state clubSide
        let roster = getRoster ctx clubSide

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Sidelined _ -> ()
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]
                let condition = int frame.Condition[i]

                let exec =
                    ActionMath.calcMovementExecution
                        player.Physical.Agility
                        player.Physical.Balance
                        player.Physical.Acceleration
                        condition

                let targetX = frame.IntentTargetX[i]
                let targetY = frame.IntentTargetY[i]

                FrameMutate.setCachedTarget frame i
                    (float targetX * 1.0<meter>)
                    (float targetY * 1.0<meter>)
                FrameMutate.setCachedExecution frame i (float exec)

    let updateTeamSide
        (currentSubTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (dt: float<second>)
        (steeringRate: int)
        (cognitiveRate: int)
        =
        let smoothing = 0.92
        state.BallXSmooth <- smoothing * state.BallXSmooth + (1.0 - smoothing) * state.Ball.Position.X

        applyReactiveOverrides ctx state clubSide currentSubTick
        updatePhysics ctx state clubSide dt
        refreshCache ctx state clubSide
