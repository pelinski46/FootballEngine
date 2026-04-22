namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PlayerSteering
open SimStateOps
open FootballEngine.PhysicsContract
open SimulationClock

module MovementEngine =

    let private updateIntents
        (currentSubTick: int)
        (cognitiveRate: int)
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        =
        let slots = getSlots state clubSide
        let emergent = getEmergentState state clubSide
        let basePositions = getBasePositions state clubSide
        let dir = attackDirFor clubSide state
        let ballXSmooth = state.BallXSmooth
        let phase = phaseFromBallZone dir ballXSmooth

        let tacticsCfg =
            tacticsConfig (getTactics state clubSide) (getInstructions state clubSide)

        let shapeTargets =
            ShapeEngine.computeShapeTargets basePositions dir phase ballXSmooth tacticsCfg

        let team = SimStateOps.buildTeamPerspective clubSide ctx state

        let perceptionsAndScores =
            slots
            |> Array.mapi (fun i slot ->
                match slot with
                | PlayerSlot.Active s ->
                    let actx =
                        AgentContext.build
                            s.Player
                            s.Profile
                            i
                            team
                            s.MovementIntent
                            s.IntentLockExpiry
                            state
                            defaultClock
                            state.Config.Decision
                            state.Config.BuildUp

                    let scores = MovementScorer.computeAll actx emergent
                    Some(i, s, scores, actx)
                | _ -> None)

        for item in perceptionsAndScores do
            match item with
            | Some(i, s, scores, actx) ->
                let intent = MovementScorer.pickIntent currentSubTick scores actx
                let lockDuration = cognitiveRate * 2

                let newExpiry =
                    match actx.PreviousIntent, intent with
                    | Some prev, newIntent when prev.GetType() <> newIntent.GetType() -> currentSubTick + lockDuration
                    | _ -> s.IntentLockExpiry

                let finalIntent =
                    match intent with
                    | MaintainShape _ when i < shapeTargets.Length ->
                        let tx, ty = shapeTargets[i]
                        Some(MaintainShape(SimStateOps.defaultSpatial tx ty))
                    | other -> Some other

                slots[i] <-
                    PlayerSlot.Active
                        { s with
                            MovementIntent = finalIntent
                            IntentLockExpiry = newExpiry }
            | None -> ()

    let updatePhysics (currentSubTick: int) (state: SimState) (clubSide: ClubSide) (dt: float<second>) =
        let slots = getSlots state clubSide
        let pcfg = state.Config.Physics

        let allPositions =
            slots
            |> Array.map (function
                | PlayerSlot.Active s -> s.Pos
                | PlayerSlot.Sidelined _ ->
                    { X = 0.0<meter>
                      Y = 0.0<meter>
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> })

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Sidelined _ -> ()
            | PlayerSlot.Active s ->
                let target =
                    match s.MovementIntent with
                    | Some(MaintainShape sp) -> (sp.X, sp.Y)
                    | Some(MarkMan(_, pos)) -> (pos.X, pos.Y)
                    | Some(PressBall ballPos) -> (ballPos.X, ballPos.Y)
                    | Some(SupportAttack sp) -> (sp.X, sp.Y)
                    | Some(RecoverBall ballPos) -> (ballPos.X, ballPos.Y)
                    | Some(CoverSpace sp) -> (sp.X, sp.Y)
                    | Some(ExecuteRun run) ->
                        let t = RunAssignment.progress currentSubTick run
                        let x, y = RunAssignment.evaluateTrajectory t run.Trajectory
                        (x, y)
                    | None -> s.CachedTarget

                let hasBall =
                    match state.Ball.Possession with
                    | Owned(_, pid) -> pid = s.Player.Id
                    | _ -> false

                let chasingBall =
                    match s.MovementIntent with
                    | Some(PressBall _)
                    | Some(RecoverBall _) -> true
                    | _ -> false

                let newPos =
                    PlayerPhysics.steer
                        pcfg
                        s.Player
                        s.Condition
                        s.Pos
                        i
                        allPositions
                        target
                        state.Ball.Position
                        hasBall
                        chasingBall
                        dt

                slots[i] <- PlayerSlot.Active { s with Pos = newPos }

    let refreshCache (currentSubTick: int) (state: SimState) (clubSide: ClubSide) =
        let slots = getSlots state clubSide

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let exec =
                    ActionMath.calcMovementExecution
                        s.Player.Physical.Agility
                        s.Player.Physical.Balance
                        s.Player.Physical.Acceleration
                        s.Condition

                let target =
                    match s.MovementIntent with
                    | Some(MaintainShape sp) -> sp.XY
                    | Some(MarkMan(_, pos)) -> pos.XY
                    | Some(PressBall ballPos) -> ballPos.XY
                    | Some(SupportAttack sp) -> sp.XY
                    | Some(RecoverBall ballPos) -> ballPos.XY
                    | Some(CoverSpace sp) -> sp.XY
                    | Some(ExecuteRun _) -> s.CachedTarget
                    | None -> s.CachedTarget

                slots[i] <-
                    PlayerSlot.Active
                        { s with
                            CachedTarget = target
                            CachedExecution = float exec }
            | _ -> ()

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

        let shouldEvaluate =
            currentSubTick % cognitiveRate = 0
            || getSlots state clubSide
               |> Array.exists (function
                   | PlayerSlot.Active s -> s.MovementIntent.IsNone
                   | _ -> false)

        if shouldEvaluate then
            updateIntents currentSubTick cognitiveRate ctx state clubSide

        updatePhysics currentSubTick state clubSide dt
        refreshCache currentSubTick state clubSide
