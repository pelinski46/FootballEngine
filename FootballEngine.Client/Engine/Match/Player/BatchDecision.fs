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

        let pressersArray =
            if pressCount > 0 && teamIntent.PressTrigger then
                let n = frame.SlotCount
                let indices = Array.create n -1
                let mutable count = 0
                for i = 0 to n - 1 do
                    match frame.Occupancy[i] with
                    | OccupancyKind.Active _ when ballDistances[i] < System.Double.MaxValue ->
                        indices[count] <- i
                        count <- count + 1
                    | _ -> ()
                if count > 1 && count > pressCount then
                    let rec quickSelect lo hi k =
                        if lo >= hi then ()
                        else
                            let pivotIdx = indices[lo + (hi - lo) / 2]
                            let pivotDist = ballDistances[pivotIdx]
                            let mutable m = lo
                            let mutable n = hi
                            while m <= n do
                                while ballDistances[indices[m]] < pivotDist do m <- m + 1
                                while ballDistances[indices[n]] > pivotDist do n <- n - 1
                                if m <= n then
                                    let tmp = indices[m]
                                    indices[m] <- indices[n]
                                    indices[n] <- tmp
                                    m <- m + 1
                                    n <- n - 1
                                if m <= k && n >= k then
                                    if n < k then
                                        m <- k + 1
                                    elif m > k then
                                        n <- k - 1
                    quickSelect 0 (count - 1) (pressCount - 1)
                indices
            else
                Array.empty

        let isPresserIdx (i: int) =
            if pressCount > 0 && teamIntent.PressTrigger then
                let mutable result = false
                let mutable j = 0
                while j < pressCount && pressersArray[j] >= 0 && not result do
                    if pressersArray[j] = i then result <- true
                    else j <- j + 1
                result
            else false

        let isSetPiece =
            match state.Ball.Possession with
            | Possession.SetPiece _ -> true
            | _ -> false

        let cognitiveInterval = clock.CognitiveRate

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
                    ValueSome (IntentFrame.toMovementIntent
                        frame.IntentKind[i]
                        frame.IntentTargetX[i]
                        frame.IntentTargetY[i]
                        frame.IntentTargetPid[i]
                        fallbackPos)

                let myX = float frame.PosX[i] * 1.0<meter>
                let myY = float frame.PosY[i] * 1.0<meter>
                let myVx = float frame.VelX[i] * 1.0<meter/second>
                let myVy = float frame.VelY[i] * 1.0<meter/second>

                let visibilityMask =
                    if isSetPiece then ValueNone
                    else
                        ValueSome (Perception.computeVisibilityMask
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

                let influence =
                    if clubSide = HomeClub then state.HomeInfluenceFrame
                    else state.AwayInfluenceFrame

                let actx =
                    AgentContext.build
                        player profile i team teamIntent previousIntent frame.IntentLockExpiry[i]
                        state clock ctx state.Config.Decision state.Config.BuildUp
                        (Some cFrame)
                        visibilityMask
                        influence

                let movementScores = MovementScorer.computeAll actx emergent

                let adjustedScores =
                    if teamIntent.PressTrigger then
                        let isPresser = isPresserIdx i
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

                let lockDuration =
                    let baseDuration =
                        match kind with
                        | IntentKind.MaintainShape -> cognitiveInterval * 2
                        | IntentKind.CoverSpace -> cognitiveInterval * 2
                        | IntentKind.MarkMan -> cognitiveInterval
                        | IntentKind.SupportAttack -> cognitiveInterval
                        | IntentKind.PressBall -> cognitiveInterval / 2
                        | IntentKind.RecoverBall -> cognitiveInterval / 2
                        | IntentKind.ExecuteRun -> cognitiveInterval
                        | IntentKind.Idle -> cognitiveInterval

                    let scoreMargin =
                        let topScore =
                            [| adjustedScores.MaintainShape; adjustedScores.MarkMan
                               adjustedScores.PressBall; adjustedScores.CoverSpace
                               adjustedScores.SupportAttack; adjustedScores.RecoverBall |]
                            |> Array.max
                        let secondScore =
                            [| adjustedScores.MaintainShape; adjustedScores.MarkMan
                               adjustedScores.PressBall; adjustedScores.CoverSpace
                               adjustedScores.SupportAttack; adjustedScores.RecoverBall |]
                            |> Array.sortDescending |> Array.item 1
                        let margin = topScore - secondScore
                        if margin > 0.5 then 1.5
                        elif margin > 0.3 then 1.2
                        elif margin > 0.15 then 1.0
                        else 0.7

                    int (float baseDuration * scoreMargin)

                frame.IntentLockExpiry[i] <- currentSubTick + lockDuration
