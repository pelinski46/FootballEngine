namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchStateOps
open FootballEngine.PlayerSteering

module MovementEngine =

    let private isHome (clubSide: ClubSide) = clubSide = HomeClub

    let private getMySide (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then s.HomeSide else s.AwaySide

    let private getOppSide (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then s.AwaySide else s.HomeSide

    let private getMentalStates (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeMentalStates
        else
            s.AwayMentalStates

    let private getDirectives (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeDirectives
        else
            s.AwayDirectives

    let private getActiveRuns (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeActiveRuns
        else
            s.AwayActiveRuns

    let private getChemistry (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then s.HomeChemistry else s.AwayChemistry

    let private getEmergentState (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeEmergentState
        else
            s.AwayEmergentState

    let private getAdaptiveState (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeAdaptiveState
        else
            s.AwayAdaptiveState

    let private getLastCognitiveSubTick (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeLastCognitiveSubTick
        else
            s.AwayLastCognitiveSubTick

    let private getLastShapeSubTick (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeLastShapeSubTick
        else
            s.AwayLastShapeSubTick

    let private getLastAdaptiveSubTick (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeLastAdaptiveSubTick
        else
            s.AwayLastAdaptiveSubTick

    let private getLastMarkingSubTick (s: MatchState) (clubSide: ClubSide) =
        if isHome clubSide then
            s.HomeLastMarkingSubTick
        else
            s.AwayLastMarkingSubTick

    let private withMySide (s: MatchState) (clubSide: ClubSide) (ts: TeamSide) =
        if isHome clubSide then
            { s with HomeSide = ts }
        else
            { s with AwaySide = ts }

    let private withMovementFields
        (s: MatchState)
        (clubSide: ClubSide)
        (mentalStates: MentalState[])
        (directives: Directive[][])
        (activeRuns: RunAssignment list)
        (chemistry: ChemistryGraph)
        (emergentState: EmergentState)
        (adaptiveState: AdaptiveState)
        (lastCogSubTick: int)
        (lastShapeSubTick: int)
        (lastMarkingSubTick: int)
        (lastAdaptiveSubTick: int)
        : MatchState =
        if isHome clubSide then
            { s with
                HomeMentalStates = mentalStates
                HomeDirectives = directives
                HomeActiveRuns = activeRuns
                HomeChemistry = chemistry
                HomeEmergentState = emergentState
                HomeAdaptiveState = adaptiveState
                HomeLastCognitiveSubTick = lastCogSubTick
                HomeLastShapeSubTick = lastShapeSubTick
                HomeLastMarkingSubTick = lastMarkingSubTick
                HomeLastAdaptiveSubTick = lastAdaptiveSubTick }
        else
            { s with
                AwayMentalStates = mentalStates
                AwayDirectives = directives
                AwayActiveRuns = activeRuns
                AwayChemistry = chemistry
                AwayEmergentState = emergentState
                AwayAdaptiveState = adaptiveState
                AwayLastCognitiveSubTick = lastCogSubTick
                AwayLastShapeSubTick = lastShapeSubTick
                AwayLastMarkingSubTick = lastMarkingSubTick
                AwayLastAdaptiveSubTick = lastAdaptiveSubTick }

    let private computeMarkingTargets (mySide: TeamSide) (oppSide: TeamSide) : Option<int * Player>[] =
        let oppOutfield = Array.zeroCreate<Player> oppSide.Players.Length
        let mutable oppCount = 0

        for i = 0 to oppSide.Players.Length - 1 do
            if oppSide.Players[i].Position <> GK then
                oppOutfield[oppCount] <- oppSide.Players[i]
                oppCount <- oppCount + 1

        for i = 1 to oppCount - 1 do
            let tmp = oppOutfield[i]
            let mutable j = i

            while j > 0 && oppOutfield[j - 1].Position > tmp.Position do
                oppOutfield[j] <- oppOutfield[j - 1]
                j <- j - 1

            oppOutfield[j] <- tmp

        let result = Array.zeroCreate<Option<int * Player>> mySide.Players.Length
        let mutable myOutfieldCount = 0

        for i = 0 to mySide.Players.Length - 1 do
            if mySide.Players[i].Position = GK then
                result[i] <- None
            else
                let n = max 1 oppCount
                let idx = myOutfieldCount % n
                result[i] <- Some(idx, oppOutfield[idx])
                myOutfieldCount <- myOutfieldCount + 1

        result

    // Acumula peso/posición de un array de directives sobre mutables existentes, sin alocar.
    let inline foldDirectives
        currentSubTick
        (modifiers: DirectiveModifiers)
        (tw: float byref)
        (sx: float byref)
        (sy: float byref)
        (ds: Directive[])
        =
        for i = 0 to ds.Length - 1 do
            let d = ds[i]
            if not (Directive.expired currentSubTick d) && d.Weight > 0.0 then
                let w =
                    match d.Kind with
                    | Shape -> d.Weight * modifiers.Shape
                    | Run -> d.Weight * modifiers.Run
                    | MarkMan -> d.Weight * modifiers.MarkMan
                    | MarkZone -> d.Weight * modifiers.MarkZone
                    | Press -> d.Weight * modifiers.Press
                    | Cover -> d.Weight * modifiers.Cover
                    | Support -> d.Weight * modifiers.Support
                    | Flank -> d.Weight * modifiers.Flank
                    | Compact -> d.Weight * modifiers.Compact
                    | Spread -> d.Weight * modifiers.Spread
                    | ThirdMan -> d.Weight * modifiers.ThirdMan

                tw <- tw + w
                sx <- sx + d.TargetX * w
                sy <- sy + d.TargetY * w

    let inline foldSingleDirective
        currentSubTick
        (modifiers: DirectiveModifiers)
        (tw: float byref)
        (sx: float byref)
        (sy: float byref)
        (d: Directive)
        =
        if not (Directive.expired currentSubTick d) && d.Weight > 0.0 then
            let w =
                match d.Kind with
                | Shape -> d.Weight * modifiers.Shape
                | Run -> d.Weight * modifiers.Run
                | MarkMan -> d.Weight * modifiers.MarkMan
                | MarkZone -> d.Weight * modifiers.MarkZone
                | Press -> d.Weight * modifiers.Press
                | Cover -> d.Weight * modifiers.Cover
                | Support -> d.Weight * modifiers.Support
                | Flank -> d.Weight * modifiers.Flank
                | Compact -> d.Weight * modifiers.Compact
                | Spread -> d.Weight * modifiers.Spread
                | ThirdMan -> d.Weight * modifiers.ThirdMan

            tw <- tw + w
            sx <- d.TargetX * w + sx
            sy <- d.TargetY * w + sy

    // Filtra runs activos sin alocar si todos están activos (caso común).
    let private filterActiveRuns currentSubTick (runs: RunAssignment list) =
        let mutable allActive = true
        let mutable node = runs

        while allActive && not (List.isEmpty node) do
            if not (RunAssignment.isActive currentSubTick node.Head) then
                allActive <- false

            node <- node.Tail

        if allActive then
            runs
        else
            runs |> List.filter (RunAssignment.isActive currentSubTick)

    let updateTeamSide (currentSubTick: int) (s: MatchState) (clubSide: ClubSide) (dt: float) : MatchState =
        let mySide = getMySide s clubSide
        let oppSide = getOppSide s clubSide
        let dir = AttackDir.ofClubSide (if isHome clubSide then HomeClub else AwayClub)

        let lastShapeSubTick = getLastShapeSubTick s clubSide
        let shapeNeedsUpdate = currentSubTick - lastShapeSubTick >= 600

        let lastMarkingSubTick = getLastMarkingSubTick s clubSide
        let markingNeedsUpdate = currentSubTick - lastMarkingSubTick >= PhysicsContract.MarkingIntervalSubTicks

        // Gate steering to SteeringIntervalSubTicks — skip entirely if not time
        if currentSubTick % PhysicsContract.SteeringIntervalSubTicks <> 0 then
            s
        else
            let activeRuns = filterActiveRuns currentSubTick (getActiveRuns s clubSide)

            let phase = phaseFromBallZone dir s.Ball.Position.X

            let shapeTargets =
                if shapeNeedsUpdate then
                    ShapeEngine.computeShapeTargets
                        mySide.BasePositions
                        dir
                        phase
                        s.Ball.Position.X
                        (tacticsConfig mySide.Tactics mySide.Instructions)
                else
                    let n = mySide.Positions.Length
                    let arr = Array.zeroCreate<float * float> n

                    for i = 0 to n - 1 do
                        let sp = mySide.Positions[i]
                        arr[i] <- (sp.X, sp.Y)

                    arr

            let markingTargets =
                if markingNeedsUpdate then
                    computeMarkingTargets mySide oppSide
                else
                    // Reuse current positions as marking targets (no reassignment)
                    Array.init mySide.Players.Length (fun i ->
                        if mySide.Players[i].Position <> GK then
                            let mutable bestIdx = 0
                            let mutable bestDistSq = System.Double.MaxValue
                            for j = 0 to oppSide.Players.Length - 1 do
                                if oppSide.Players[j].Position <> GK then
                                    let dx = mySide.Positions[i].X - oppSide.Positions[j].X
                                    let dy = mySide.Positions[i].Y - oppSide.Positions[j].Y
                                    let dSq = dx * dx + dy * dy
                                    if dSq < bestDistSq then
                                        bestDistSq <- dSq
                                        bestIdx <- j
                            Some(bestIdx, oppSide.Players[bestIdx])
                        else
                            None
                    )

            let emergentState = getEmergentState s clubSide
            let emergentModifiers = EmergentLoops.toDirectiveModifiers emergentState
            let directives = getDirectives s clubSide
            let ballX = s.Ball.Position.X
            let ballY = s.Ball.Position.Y

            // Pre-compute closest-to-ball index once per tick (same for all players).
            let mutable closestIdx = 0
            let mutable closestDistSq = System.Double.MaxValue

            for j = 0 to mySide.Positions.Length - 1 do
                let sp = mySide.Positions[j]
                let dx = sp.X - ballX
                let dy = sp.Y - ballY
                let dSq = dx * dx + dy * dy

                if dSq < closestDistSq then
                    closestDistSq <- dSq
                    closestIdx <- j

            let newPositions = Array.copy mySide.Positions

            for i = 0 to mySide.Players.Length - 1 do
                let p = mySide.Players[i]
                let currentPos = mySide.Positions[i]
                let cond = mySide.Conditions[i]
                let stX, stY = shapeTargets[i]

                let mutable tw = 0.0
                let mutable sx = 0.0
                let mutable sy = 0.0

                // base — 1 directive, 0 allocs
                foldSingleDirective
                    currentSubTick
                    emergentModifiers
                    &tw
                    &sx
                    &sy
                    (Directive.create Shape stX stY 0.4 0.5 (currentSubTick + 120) "shape")

                // run — 0 o 1 directive, 0 allocs
                let mutable node = activeRuns

                while not (List.isEmpty node) do
                    let r = node.Head

                    if r.PlayerId = p.Id then
                        let t = RunAssignment.progress currentSubTick r
                        let rx, ry = RunAssignment.evaluateTrajectory t r.Trajectory

                        foldSingleDirective
                            currentSubTick
                            emergentModifiers
                            &tw
                            &sx
                            &sy
                            (Directive.create Run rx ry r.Intensity 0.8 (r.StartSubTick + r.DurationSubTicks) "run")

                    node <- node.Tail

                // marking — 0 o 1 directive, 0 allocs
                match markingTargets[i] with
                | Some(oppIdx, _) ->
                    let oppPos = oppSide.Positions[oppIdx]

                    foldSingleDirective
                        currentSubTick
                        emergentModifiers
                        &tw
                        &sx
                        &sy
                        (Directive.create MarkMan oppPos.X oppPos.Y 0.6 0.5 (currentSubTick + 80) "marking")
                | None -> ()

                // existing directives
                foldDirectives currentSubTick emergentModifiers &tw &sx &sy directives[i]

                let finalTargetX, finalTargetY =
                    if tw = 0.0 then (52.5, 34.0) else (sx / tw, sy / tw)

                let driftX, driftY = OrganicDrift.compute p.Position p.Id currentSubTick
                let adjustedTargetX = finalTargetX + driftX * 0.15
                let adjustedTargetY = finalTargetY + driftY * 0.15

                newPositions[i] <-
                    PlayerPhysics.steer
                        p
                        cond
                        currentPos
                        i
                        mySide.Positions
                        (adjustedTargetX, adjustedTargetY)
                        s.Ball.Position
                        (i = closestIdx)
                        dt

            let newLastShapeSubTick =
                if shapeNeedsUpdate then
                    currentSubTick
                else
                    lastShapeSubTick

            let newLastMarkingSubTick =
                if markingNeedsUpdate then
                    currentSubTick
                else
                    lastMarkingSubTick

            let s2 = withMySide s clubSide { mySide with Positions = newPositions }

            if isHome clubSide then
                { s2 with
                    HomeActiveRuns = activeRuns
                    HomeLastShapeSubTick = newLastShapeSubTick
                    HomeLastMarkingSubTick = newLastMarkingSubTick }
            else
                { s2 with
                    AwayActiveRuns = activeRuns
                    AwayLastShapeSubTick = newLastShapeSubTick
                    AwayLastMarkingSubTick = newLastMarkingSubTick }

    let updateCognitive
        (currentSubTick: int)
        (s: MatchState)
        (clubSide: ClubSide)
        (events: ResizeArray<MatchEvent>)
        : MatchState =

        let lastCogSubTick = getLastCognitiveSubTick s clubSide

        if currentSubTick - lastCogSubTick < PhysicsContract.CognitiveIntervalSubTicks then
            s
        else
            let mySide = getMySide s clubSide
            let oppSide = getOppSide s clubSide
            let dir = AttackDir.ofClubSide (if isHome clubSide then HomeClub else AwayClub)
            let momentum = if isHome clubSide then s.Momentum else -s.Momentum
            let mentalStates = getMentalStates s clubSide
            let directives = getDirectives s clubSide
            let chemistry = getChemistry s clubSide
            let emergentState = getEmergentState s clubSide

            let newMentalStates = Array.zeroCreate mySide.Players.Length
            let newDirectiveMap = Array.zeroCreate<Directive[]> mySide.Players.Length
            let newRuns = System.Collections.Generic.List<RunAssignment>()

            mySide.Players
            |> Array.iteri (fun i p ->
                let mental =
                    if i < mentalStates.Length && not (obj.ReferenceEquals(box mentalStates[i], null)) then
                        mentalStates[i]
                    else
                        MentalState.initial p

                let dq = FatiguePipeline.decisionQuality p mySide.Conditions[i]

                let update =
                    CognitiveLayer.evaluate
                        currentSubTick
                        p
                        i
                        mySide
                        oppSide
                        s.Ball.Position.X
                        s.Ball.Position.Y
                        dir
                        momentum
                        mental
                        chemistry
                        (if i < directives.Length && not (isNull directives[i]) then directives[i] else Array.empty)

                newMentalStates[i] <-
                    match update.MentalStateDelta with
                    | Some ms -> ms
                    | None -> mental

                let removedKinds = update.RemovedKinds
                let existing = if i < directives.Length then directives[i] else null

                let buf = System.Collections.Generic.List<Directive>(
                    (if isNull existing then 0 else existing.Length) + update.NewDirectives.Length)

                if not (isNull existing) then
                    for d in existing do
                        if
                            not (Directive.expired currentSubTick d)
                            && not (List.contains d.Kind removedKinds)
                        then
                            buf.Add(d)

                for d in update.NewDirectives do
                    buf.Add({ d with Urgency = d.Urgency * dq })

                newDirectiveMap[i] <- buf.ToArray()
                update.RunAssignments |> List.iter newRuns.Add)

            let matchMinute = PhysicsContract.subTicksToSeconds currentSubTick / 60.0

            let isPressing =
                mySide.Tactics = TeamTactics.Pressing
                || mySide.Instructions |> Option.exists (fun instr -> instr.PressingIntensity >= 4)

            let degradedConditions = Array.copy mySide.Conditions

            for i = 0 to mySide.Players.Length - 1 do
                let player = mySide.Players[i]
                let currentCond = mySide.Conditions[i]

                if player.Position <> GK then
                    degradedConditions[i] <- FatiguePipeline.degradeCondition player currentCond isPressing matchMinute

            let updatedMySide =
                { mySide with
                    Conditions = degradedConditions }

            let rates, emergentEvents = EventWindow.computeRates 4800 events
            let shortPassRate = rates.ShortPassRate
            let pressRate = rates.PressRate
            let flankRate = rates.FlankRate

            let updatedEmergent =
                emergentState |> EmergentLoops.updateCompactness <| shortPassRate
                |> EmergentLoops.updatePressing
                <| pressRate
                |> EmergentLoops.updateWingPlay
                <| flankRate

            let existingRuns = getActiveRuns s clubSide

            let allRuns =
                if newRuns.Count = 0 then
                    existingRuns
                else
                    let mutable acc = existingRuns

                    for i = newRuns.Count - 1 downto 0 do
                        acc <- newRuns[i] :: acc

                    acc

            withMovementFields
                s
                clubSide
                newMentalStates
                newDirectiveMap
                allRuns
                chemistry
                updatedEmergent
                (getAdaptiveState s clubSide)
                currentSubTick
                (getLastShapeSubTick s clubSide)
                (getLastMarkingSubTick s clubSide)
                (getLastAdaptiveSubTick s clubSide)
            |> fun st ->
                if isHome clubSide then
                    { st with HomeSide = updatedMySide }
                else
                    { st with AwaySide = updatedMySide }

    let updateAdaptive
        (currentSubTick: int)
        (s: MatchState)
        (clubSide: ClubSide)
        (events: ResizeArray<MatchEvent>)
        : MatchState =

        let lastAdaptiveSubTick = getLastAdaptiveSubTick s clubSide

        if currentSubTick - lastAdaptiveSubTick < PhysicsContract.AdaptiveIntervalSubTicks then
            s
        else
            let recentEvts = EventWindow.recentEvents 12000 events

            let updatedAdaptive =
                [ LeftFlank; RightFlank; Central; AttackPattern.LongBall; ShortPass ]
                |> List.fold
                    (fun st pattern ->
                        let record = EventWindow.patternResults pattern recentEvts

                        let result =
                            if record.Successes > 0 then
                                SuccessfulXG record.TotalXG
                            else
                                LostPossession

                        AdaptiveTactics.recordAttempt pattern result st)
                    (getAdaptiveState s clubSide)

            if isHome clubSide then
                { s with
                    HomeAdaptiveState = updatedAdaptive
                    HomeLastAdaptiveSubTick = currentSubTick }
            else
                { s with
                    AwayAdaptiveState = updatedAdaptive
                    AwayLastAdaptiveSubTick = currentSubTick }
