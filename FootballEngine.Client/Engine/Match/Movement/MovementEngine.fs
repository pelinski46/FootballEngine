namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchStateOps
open FootballEngine.Movement.AdaptiveTactics
open FootballEngine.Movement.CognitiveLayer
open FootballEngine.PlayerSteering


type TeamMovementState =
    { MentalStates: MentalState[]
      ActiveRuns: RunAssignment list
      DirectiveMap: Directive list[]
      Chemistry: ChemistryGraph
      EmergentState: EmergentState
      AdaptiveState: AdaptiveState
      LastCognitiveSecond: int
      LastShapeSecond: int
      LastAdaptiveSecond: int }

module MovementEngine =
    let initMovementState playerCount =
        { MentalStates = Array.zeroCreate playerCount
          ActiveRuns = []
          DirectiveMap = Array.create playerCount []
          Chemistry = ChemistryGraph.init playerCount
          EmergentState = EmergentLoops.initial
          AdaptiveState = AdaptiveTactics.initial
          LastCognitiveSecond = 0
          LastShapeSecond = 0
          LastAdaptiveSecond = 0 }

    let private computeMarkingTargets (mySide: TeamSide) (oppSide: TeamSide) =
        let oppOutfield =
            oppSide.Players
            |> Array.mapi (fun i p -> i, p)
            |> Array.filter (fun (_, p) -> p.Position <> GK)
            |> Array.sortBy (fun (_, p) -> p.Position)

        mySide.Players
        |> Array.mapi (fun i p ->
            if p.Position = GK then
                None
            else
                let oppIdx = i % max 1 oppOutfield.Length
                Some(oppIdx, oppOutfield[oppIdx]))

    let updateTeamSide
        simSecond
        (teamState: TeamMovementState)
        (mySide: TeamSide)
        (oppSide: TeamSide)
        (ball: BallPhysicsState)
        (dir: AttackDir)
        (momentum: float)
        (dt: float)
        : TeamMovementState * TeamSide =

        let phase = phaseFromBallZone dir ball.Position.X

        let shapeTargets =
            if simSecond - teamState.LastShapeSecond >= ShapeEngine.ShapeInterval then
                ShapeEngine.computeShapeTargets
                    mySide.BasePositions
                    dir
                    phase
                    ball.Position.X
                    (tacticsConfig mySide.Tactics mySide.Instructions)
            else
                mySide.Positions |> Array.map (fun sp -> sp.X, sp.Y)

        let activeRuns =
            teamState.ActiveRuns |> List.filter (RunAssignment.isActive simSecond)

        let markingTargets = computeMarkingTargets mySide oppSide

        let emergentModifiers = EmergentLoops.toDirectiveModifiers teamState.EmergentState

        let newPositions =
            mySide.Players
            |> Array.mapi (fun i p ->
                let currentPos = mySide.Positions[i]
                let cond = mySide.Conditions[i]

                let shapeTarget = shapeTargets[i]

                let directives =
                    let baseDirectives =
                        [ Directive.create Shape (fst shapeTarget) (snd shapeTarget) 0.4 0.5 (simSecond + 30) "shape" ]

                    let runDirectives =
                        activeRuns
                        |> List.tryFind (fun r -> r.PlayerId = p.Id)
                        |> Option.map (fun r ->
                            let t = RunAssignment.progress simSecond r
                            let rx, ry = RunAssignment.evaluateTrajectory t r.Trajectory
                            [ Directive.create Run rx ry r.Intensity 0.8 (r.StartSecond + r.DurationSeconds) "run" ])
                        |> Option.defaultValue []

                    let markDirectives =
                        match markingTargets[i] with
                        | Some(oppIdx, _) ->
                            let oppPos = oppSide.Positions[oppIdx]
                            [ Directive.create MarkMan oppPos.X oppPos.Y 0.6 0.5 (simSecond + 20) "marking" ]
                        | None -> []

                    let playerDirectives = teamState.DirectiveMap[i]

                    baseDirectives @ runDirectives @ markDirectives @ playerDirectives

                let finalTargetX, finalTargetY =
                    Directive.composeDirectives simSecond directives emergentModifiers

                let driftX, driftY = OrganicDrift.compute p.Position p.Id simSecond

                let adjustedTargetX = finalTargetX + driftX * 0.15
                let adjustedTargetY = finalTargetY + driftY * 0.15

                let teammates = mySide.Positions |> Array.filter (fun sp -> sp <> currentPos)



                let closestIdx =
                    mySide.Positions
                    |> Array.mapi (fun i sp ->
                        let dx = sp.X - ball.Position.X
                        let dy = sp.Y - ball.Position.Y
                        i, dx * dx + dy * dy)
                    |> Array.minBy snd
                    |> fst

                let hasBall = i = closestIdx

                PlayerPhysics.steer
                    p
                    cond
                    currentPos
                    (adjustedTargetX, adjustedTargetY)
                    teammates
                    ball.Position
                    hasBall
                    dt)

        let newState =
            { teamState with
                ActiveRuns = activeRuns
                LastShapeSecond =
                    if simSecond - teamState.LastShapeSecond >= ShapeEngine.ShapeInterval then
                        simSecond
                    else
                        teamState.LastShapeSecond }

        let updatedSide = { mySide with Positions = newPositions }
        (newState, updatedSide)

    let updateCognitive
        simSecond
        (teamState: TeamMovementState)
        (mySide: TeamSide)
        (oppSide: TeamSide)
        ballX
        ballY
        (dir: AttackDir)
        (momentum: float)
        (reversedEvents: MatchEvent list)
        : TeamMovementState =

        if simSecond - teamState.LastCognitiveSecond < CognitiveInterval then
            teamState
        else
            let newMentalStates = Array.zeroCreate mySide.Players.Length
            let newDirectiveMap = Array.create mySide.Players.Length []
            let newRuns = ResizeArray<RunAssignment>()

            mySide.Players
            |> Array.iteri (fun i p ->
                let mental =
                    if obj.ReferenceEquals(teamState.MentalStates[i], null) then
                        MentalState.initial p
                    else
                        teamState.MentalStates[i]

                let update =
                    CognitiveLayer.evaluate
                        simSecond
                        p
                        i
                        mySide
                        oppSide
                        ballX
                        ballY
                        dir
                        momentum
                        mental
                        teamState.Chemistry
                        teamState.DirectiveMap[i]

                match update.MentalStateDelta with
                | Some ms -> newMentalStates[i] <- ms
                | None -> newMentalStates[i] <- mental

                let removedKinds = Set.ofList update.RemovedKinds

                let filtered =
                    teamState.DirectiveMap[i]
                    |> List.filter (fun d ->
                        not (Directive.expired simSecond d) && not (Set.contains d.Kind removedKinds))

                newDirectiveMap[i] <- filtered @ update.NewDirectives

                update.RunAssignments |> List.iter newRuns.Add

            )

            let emergentEvents = EventWindow.recentEvents 120 reversedEvents
            let shortPassRate = EventWindow.shortPassSuccessRate emergentEvents
            let pressRate = EventWindow.pressSuccessRate emergentEvents
            let flankRate = EventWindow.flankSuccessRate dir emergentEvents

            let updatedEmergent =
                teamState.EmergentState |> EmergentLoops.updateCompactness <| shortPassRate
                |> EmergentLoops.updatePressing
                <| pressRate
                |> EmergentLoops.updateWingPlay
                <| flankRate

            { teamState with
                MentalStates = newMentalStates
                DirectiveMap = newDirectiveMap
                ActiveRuns = teamState.ActiveRuns @ (newRuns |> Seq.toList)
                EmergentState = updatedEmergent
                LastCognitiveSecond = simSecond }

    let updateAdaptive simSecond teamState dir reversedEvents : TeamMovementState =

        if simSecond - teamState.LastAdaptiveSecond < AdaptiveCheckInterval then
            teamState
        else
            let events = EventWindow.recentEvents 300 reversedEvents

            let updatedState =
                [ LeftFlank; RightFlank; Central; AttackPattern.LongBall; ShortPass ]
                |> List.fold
                    (fun st pattern ->
                        let record = EventWindow.patternResults pattern events

                        let result =
                            if record.Successes > 0 then
                                SuccessfulXG record.TotalXG
                            else
                                LostPossession

                        AdaptiveTactics.recordAttempt pattern result st)
                    teamState.AdaptiveState

            { teamState with
                AdaptiveState = updatedState
                LastAdaptiveSecond = simSecond }

    let recordPattern pattern result teamState =
        { teamState with
            AdaptiveState = AdaptiveTactics.recordAttempt pattern result teamState.AdaptiveState }
