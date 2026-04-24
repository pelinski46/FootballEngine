namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps
open FootballEngine.Movement

module BatchDecision =

    let processTeam
        (currentSubTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (clubSide: ClubSide)
        (cFrame: CognitiveFrame)
        : unit =
        let team = SimStateOps.buildTeamPerspective clubSide ctx state
        let frame = team.OwnFrame
        let roster = team.OwnRoster
        let emergent = getEmergentState state clubSide
        let basePositions = getBasePositions state clubSide
        let dir = team.AttackDir
        let ballXSmooth = state.BallXSmooth
        let phase = phaseFromBallZone dir ballXSmooth

        let tacticsCfg =
            tacticsConfig (getTactics state clubSide) (getInstructions state clubSide)

        ShapeEngine.computeShapeTargets basePositions dir phase ballXSmooth tacticsCfg frame.IntentTargetX frame.IntentTargetY

        let teamIntent = TeamIntentModule.build clubSide ctx state cFrame emergent

        SimStateOps.setCognitiveFrame state clubSide cFrame
        SimStateOps.setTeamIntent state clubSide teamIntent

        let ballDistances =
            if teamIntent.PressTrigger then
                let bx = float cFrame.BallX
                let by = float cFrame.BallY
                Array.init frame.SlotCount (fun i ->
                    match frame.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let dx = float frame.PosX[i] - bx
                        let dy = float frame.PosY[i] - by
                        dx * dx + dy * dy
                    | _ -> System.Double.MaxValue)
            else
                Array.empty

        let pressCount =
            if teamIntent.PressTrigger then
                int (3.0 + tacticsCfg.PressingIntensity * 3.0)
            else
                0

        let pressers =
            if pressCount > 0 then
                ballDistances
                |> Array.mapi (fun i d -> i, d)
                |> Array.filter (fun (_, d) -> d < System.Double.MaxValue)
                |> Array.sortBy snd
                |> Array.take (min pressCount frame.SlotCount)
                |> Array.map fst
                |> Set.ofArray
            else
                Set.empty

        let isSetPiece =
            match state.Ball.Possession with
            | Possession.SetPiece _ -> true
            | _ -> false

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | OccupancyKind.Sidelined _ -> ()
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]
                let profile = roster.Profiles[i]

                let fallbackPos =
                    SimStateOps.defaultSpatial
                        (float frame.PosX[i] * 1.0<meter>)
                        (float frame.PosY[i] * 1.0<meter>)

                let previousIntent =
                    IntentFrame.toMovementIntent
                        frame.IntentKind[i]
                        frame.IntentTargetX[i]
                        frame.IntentTargetY[i]
                        frame.IntentTargetPid[i]
                        fallbackPos

                let myX = float frame.PosX[i] * 1.0<meter>
                let myY = float frame.PosY[i] * 1.0<meter>
                let myVx = float frame.VelX[i] * 1.0<meter/second>
                let myVy = float frame.VelY[i] * 1.0<meter/second>

                let visibilityMask =
                    if isSetPiece then None
                    else
                        Some (Perception.computeVisibilityMask
                            i
                            { X = myX; Y = myY; Z = 0.0<meter>; Vx = myVx; Vy = myVy; Vz = 0.0<meter/second> }
                            myVx myVy
                            frame.IntentKind[i]
                            frame.IntentTargetX[i]
                            frame.IntentTargetY[i]
                            player.Mental.Vision
                            player.Mental.Positioning
                            (player.Position = GK)
                            state.Ball.Position
                            frame
                            team.OppFrame
                            ctx.Config.Perception)

                let actx =
                    AgentContext.build
                        player profile i team teamIntent previousIntent 0
                        state clock state.Config.Decision state.Config.BuildUp
                        (Some cFrame)
                        visibilityMask

                let movementScores = MovementScorer.computeAll actx emergent

                let adjustedScores =
                    if teamIntent.PressTrigger then
                        let isPresser = Set.contains i pressers
                        if isPresser then
                            { movementScores with PressBall = movementScores.PressBall * 1.5 }
                        else
                            { movementScores with CoverSpace = movementScores.CoverSpace * 1.3 }
                    else
                        movementScores

                let intent = MovementScorer.pickIntent currentSubTick adjustedScores actx

                let finalIntent =
                    match intent with
                    | MaintainShape _ ->
                        let tx = float frame.IntentTargetX[i] * 1.0<meter>
                        let ty = float frame.IntentTargetY[i] * 1.0<meter>
                        MaintainShape(SimStateOps.defaultSpatial tx ty)
                    | other -> other

                let kind, tx, ty, tpid = IntentFrame.fromMovementIntent finalIntent
                FrameMutate.setIntent frame i kind tx ty tpid
