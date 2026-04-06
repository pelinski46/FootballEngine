namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open SimStateOps
open FootballEngine.PlayerSteering

module MovementEngine =

    let private isHome (clubSide: ClubSide) = clubSide = HomeClub

    let private getSlots (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then state.HomeSlots else state.AwaySlots

    let private getActiveRuns (state: SimState) (clubSide: ClubSide) : RunAssignment list =
        if isHome clubSide then
            state.HomeActiveRuns
        else
            state.AwayActiveRuns

    let private getChemistry (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeChemistry
        else
            state.AwayChemistry

    let private getEmergentState (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeEmergentState
        else
            state.AwayEmergentState

    let private getAdaptiveState (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeAdaptiveState
        else
            state.AwayAdaptiveState

    let private getLastCognitiveSubTick (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeLastCognitiveSubTick
        else
            state.AwayLastCognitiveSubTick

    let private getLastShapeSubTick (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeLastShapeSubTick
        else
            state.AwayLastShapeSubTick

    let private getLastAdaptiveSubTick (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeLastAdaptiveSubTick
        else
            state.AwayLastAdaptiveSubTick

    let private getLastMarkingSubTick (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeLastMarkingSubTick
        else
            state.AwayLastMarkingSubTick

    let private getTactics (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeTactics
        else
            state.AwayTactics

    let private getInstructions (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeInstructions
        else
            state.AwayInstructions

    let private getBasePositions (state: SimState) (clubSide: ClubSide) =
        if clubSide = HomeClub then
            state.HomeBasePositions
        else
            state.AwayBasePositions

    let private getSubsUsed (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeSubsUsed
        else
            state.AwaySubsUsed

    let private getSidelined (state: SimState) (clubSide: ClubSide) =
        if isHome clubSide then
            state.HomeSidelined
        else
            state.AwaySidelined

    let private computeMarkingTargets
        (myPlayers: Player[])
        (myPositions: Spatial[])
        (oppPlayers: Player[])
        (oppPositions: Spatial[])
        : Option<int * Player>[] =
        let oppOutfield = Array.zeroCreate<Player> oppPlayers.Length
        let mutable oppCount = 0

        for i = 0 to oppPlayers.Length - 1 do
            if oppPlayers[i].Position <> GK then
                oppOutfield[oppCount] <- oppPlayers[i]
                oppCount <- oppCount + 1

        for i = 1 to oppCount - 1 do
            let tmp = oppOutfield[i]
            let mutable j = i

            while j > 0 && oppOutfield[j - 1].Position > tmp.Position do
                oppOutfield[j] <- oppOutfield[j - 1]
                j <- j - 1

            oppOutfield[j] <- tmp

        let result = Array.zeroCreate<Option<int * Player>> myPlayers.Length
        let mutable myOutfieldCount = 0

        for i = 0 to myPlayers.Length - 1 do
            if myPlayers[i].Position = GK then
                result[i] <- None
            else
                let n = max 1 oppCount
                let idx = myOutfieldCount % n
                result[i] <- Some(idx, oppOutfield[idx])
                myOutfieldCount <- myOutfieldCount + 1

        result

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

    let updateTeamSide (currentSubTick: int) (ctx: MatchContext) (state: SimState) (clubSide: ClubSide) (dt: float) =
        let slots = getSlots state clubSide
        let oppSlots = getSlots state (ClubSide.flip clubSide)
        let dir = attackDirFor clubSide state

        let lastShapeSubTick = getLastShapeSubTick state clubSide
        let shapeNeedsUpdate = currentSubTick - lastShapeSubTick >= 600

        let lastMarkingSubTick = getLastMarkingSubTick state clubSide

        let markingNeedsUpdate =
            currentSubTick - lastMarkingSubTick >= PhysicsContract.MarkingIntervalSubTicks

        if currentSubTick % PhysicsContract.SteeringIntervalSubTicks <> 0 then
            ()
        else
            let activeRuns = filterActiveRuns currentSubTick (getActiveRuns state clubSide)

            let phase = phaseFromBallZone dir state.Ball.Position.X

            let shapeTargets =
                if shapeNeedsUpdate then
                    ShapeEngine.computeShapeTargets
                        (getBasePositions state clubSide)
                        dir
                        phase
                        state.Ball.Position.X
                        (tacticsConfig (getTactics state clubSide) (getInstructions state clubSide))
                else
                    let n = slots.Length
                    let arr = Array.zeroCreate<float * float> n
                    let basePos = getBasePositions state clubSide

                    for i = 0 to n - 1 do
                        match slots[i] with
                        | PlayerSlot.Active _ -> arr[i] <- (basePos[i].X, basePos[i].Y)
                        | Sidelined _ -> arr[i] <- (52.5, 34.0)

                    arr

            let markingTargets =
                if markingNeedsUpdate then
                    let myPlayers =
                        Array.init slots.Length (fun i ->
                            match slots[i] with
                            | PlayerSlot.Active s -> s.Player
                            | Sidelined _ -> Unchecked.defaultof<Player>)

                    let myPositions =
                        Array.init slots.Length (fun i ->
                            match slots[i] with
                            | PlayerSlot.Active s -> s.Pos
                            | Sidelined _ -> kickOffSpatial)

                    let oppPlayers =
                        Array.init oppSlots.Length (fun i ->
                            match oppSlots[i] with
                            | PlayerSlot.Active s -> s.Player
                            | Sidelined _ -> Unchecked.defaultof<Player>)

                    let oppPositions =
                        Array.init oppSlots.Length (fun i ->
                            match oppSlots[i] with
                            | PlayerSlot.Active s -> s.Pos
                            | Sidelined _ -> kickOffSpatial)

                    computeMarkingTargets myPlayers myPositions oppPlayers oppPositions
                else
                    Array.init slots.Length (fun i ->
                        match slots[i] with
                        | PlayerSlot.Active s ->
                            if s.Player.Position <> GK then
                                let mutable bestIdx = 0
                                let mutable bestDistSq = System.Double.MaxValue

                                for j = 0 to oppSlots.Length - 1 do
                                    match oppSlots[j] with
                                    | PlayerSlot.Active os when os.Player.Position <> GK ->
                                        let dx = s.Pos.X - os.Pos.X
                                        let dy = s.Pos.Y - os.Pos.Y
                                        let dSq = dx * dx + dy * dy

                                        if dSq < bestDistSq then
                                            bestDistSq <- dSq
                                            bestIdx <- j
                                    | _ -> ()

                                Some(
                                    bestIdx,
                                    match oppSlots[bestIdx] with
                                    | PlayerSlot.Active os -> os.Player
                                    | _ -> Unchecked.defaultof<Player>
                                )
                            else
                                None
                        | Sidelined _ -> None)

            let emergentState = getEmergentState state clubSide
            let emergentModifiers = EmergentLoops.toDirectiveModifiers emergentState
            let ballX = state.Ball.Position.X
            let ballY = state.Ball.Position.Y

            let mutable closestIdx = 0
            let mutable closestDistSq = System.Double.MaxValue

            for j = 0 to slots.Length - 1 do
                match slots[j] with
                | PlayerSlot.Active s ->
                    let sp = s.Pos
                    let dx = sp.X - ballX
                    let dy = sp.Y - ballY
                    let dSq = dx * dx + dy * dy

                    if dSq < closestDistSq then
                        closestDistSq <- dSq
                        closestIdx <- j
                | Sidelined _ -> ()

            let currentPositions =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> s.Pos
                    | Sidelined _ -> kickOffSpatial)

            for i = 0 to slots.Length - 1 do
                match slots[i] with
                | Sidelined _ -> ()
                | PlayerSlot.Active s ->
                    let p = s.Player
                    let currentPos = s.Pos
                    let cond = s.Condition
                    let stX, stY = shapeTargets[i]

                    let mutable tw = 0.0
                    let mutable sx = 0.0
                    let mutable sy = 0.0

                    foldSingleDirective
                        currentSubTick
                        emergentModifiers
                        &tw
                        &sx
                        &sy
                        (Directive.create Shape stX stY 0.4 0.5 (currentSubTick + 120) "shape")

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

                    match markingTargets[i] with
                    | Some(oppIdx, _) ->
                        let oppPos =
                            match oppSlots[oppIdx] with
                            | PlayerSlot.Active os -> os.Pos
                            | _ -> kickOffSpatial

                        foldSingleDirective
                            currentSubTick
                            emergentModifiers
                            &tw
                            &sx
                            &sy
                            (Directive.create MarkMan oppPos.X oppPos.Y 0.6 0.5 (currentSubTick + 80) "marking")
                    | None -> ()

                    if not (isNull s.Directives) then
                        foldDirectives currentSubTick emergentModifiers &tw &sx &sy s.Directives

                    if i = closestIdx then
                        foldSingleDirective
                            currentSubTick
                            emergentModifiers
                            &tw
                            &sx
                            &sy
                            (Directive.create Run ballX ballY 4.0 1.0 (currentSubTick + 40) "chase")

                    let finalTargetX, finalTargetY =
                        if tw = 0.0 then (52.5, 34.0) else (sx / tw, sy / tw)

                    let driftX, driftY = OrganicDrift.compute p.Position p.Id currentSubTick
                    let adjustedTargetX = finalTargetX + driftX * 0.15
                    let adjustedTargetY = finalTargetY + driftY * 0.15

                    let newPos =
                        PlayerPhysics.steer
                            p
                            cond
                            currentPos
                            i
                            currentPositions
                            (adjustedTargetX, adjustedTargetY)
                            state.Ball.Position
                            (i = closestIdx)
                            dt

                    slots[i] <- PlayerSlot.Active { s with Pos = newPos }

            if isHome clubSide then
                state.HomeActiveRuns <- activeRuns

                state.HomeLastShapeSubTick <-
                    if shapeNeedsUpdate then
                        currentSubTick
                    else
                        lastShapeSubTick

                state.HomeLastMarkingSubTick <-
                    if markingNeedsUpdate then
                        currentSubTick
                    else
                        lastMarkingSubTick
            else
                state.AwayActiveRuns <- activeRuns

                state.AwayLastShapeSubTick <-
                    if shapeNeedsUpdate then
                        currentSubTick
                    else
                        lastShapeSubTick

                state.AwayLastMarkingSubTick <-
                    if markingNeedsUpdate then
                        currentSubTick
                    else
                        lastMarkingSubTick

    let updateCognitive
        (currentSubTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (events: ResizeArray<MatchEvent>)
        =

        let lastCogSubTick = getLastCognitiveSubTick state clubSide

        if currentSubTick - lastCogSubTick < PhysicsContract.CognitiveIntervalSubTicks then
            ()
        else
            let slots = getSlots state clubSide
            let oppSlots = getSlots state (ClubSide.flip clubSide)
            let dir = attackDirFor clubSide state
            let momentum = if isHome clubSide then state.Momentum else -state.Momentum

            let players =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> s.Player
                    | Sidelined _ -> Unchecked.defaultof<Player>)

            let positions =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> s.Pos
                    | Sidelined _ -> kickOffSpatial)

            let conditions =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> s.Condition
                    | Sidelined _ -> 0)

            let mentalStates =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> s.Mental
                    | Sidelined _ -> MentalState.initial players[i])

            let directives =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | PlayerSlot.Active s -> if isNull s.Directives then Array.empty else s.Directives
                    | Sidelined _ -> Array.empty)

            let oppPlayers =
                Array.init oppSlots.Length (fun i ->
                    match oppSlots[i] with
                    | PlayerSlot.Active s -> s.Player
                    | Sidelined _ -> Unchecked.defaultof<Player>)

            let oppPositions =
                Array.init oppSlots.Length (fun i ->
                    match oppSlots[i] with
                    | PlayerSlot.Active s -> s.Pos
                    | Sidelined _ -> kickOffSpatial)

            let chemistry = getChemistry state clubSide
            let emergentState = getEmergentState state clubSide

            let newMentalStates = Array.zeroCreate slots.Length
            let newDirectiveMap = Array.zeroCreate<Directive[]> slots.Length
            let newRuns = System.Collections.Generic.List<RunAssignment>()
            let basePositions = getBasePositions state clubSide

            for i = 0 to slots.Length - 1 do
                match slots[i] with
                | Sidelined _ -> ()
                | PlayerSlot.Active s ->
                    let p = s.Player
                    let mental = mentalStates[i]
                    let dq = FatiguePipeline.decisionQuality p conditions[i]

                    let update =
                        CognitiveLayer.evaluate
                            currentSubTick
                            p
                            i
                            players
                            positions
                            conditions
                            oppPlayers
                            oppPositions
                            state.Ball.Position.X
                            state.Ball.Position.Y
                            dir
                            momentum
                            mental
                            chemistry
                            directives[i]
                            basePositions[i]

                    newMentalStates[i] <-
                        match update.MentalStateDelta with
                        | Some ms -> ms
                        | None -> mental

                    let removedKinds = update.RemovedKinds
                    let existing = directives[i]

                    let buf =
                        System.Collections.Generic.List<Directive>(existing.Length + update.NewDirectives.Length)

                    for d in existing do
                        if
                            not (Directive.expired currentSubTick d)
                            && not (List.contains d.Kind removedKinds)
                        then
                            buf.Add(d)

                    for d in update.NewDirectives do
                        buf.Add({ d with Urgency = d.Urgency * dq })

                    newDirectiveMap[i] <- if buf.Count = 0 then Array.empty else buf.ToArray()

                    update.RunAssignments |> List.iter newRuns.Add

            let matchMinute = PhysicsContract.subTicksToSeconds currentSubTick / 60.0

            let isPressing =
                getTactics state clubSide = TeamTactics.Pressing
                || getInstructions state clubSide
                   |> Option.exists (fun instr -> instr.PressingIntensity >= 4)

            let degradedConditions =
                Array.init slots.Length (fun i ->
                    match slots[i] with
                    | Sidelined _ -> 0
                    | PlayerSlot.Active s ->
                        let player = s.Player
                        let currentCond = s.Condition

                        if player.Position <> GK then
                            FatiguePipeline.degradeCondition player currentCond isPressing matchMinute
                        else
                            currentCond)

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

            let existingRuns = getActiveRuns state clubSide

            let allRuns =
                if newRuns.Count = 0 then
                    existingRuns
                else
                    let mutable acc = existingRuns

                    for i = newRuns.Count - 1 downto 0 do
                        acc <- newRuns[i] :: acc

                    acc

            for i = 0 to slots.Length - 1 do
                match slots[i] with
                | Sidelined _ -> ()
                | PlayerSlot.Active s ->
                    slots[i] <-
                        PlayerSlot.Active
                            { s with
                                Mental = newMentalStates[i]
                                Directives = newDirectiveMap[i]
                                Condition = degradedConditions[i] }

            if isHome clubSide then
                state.HomeActiveRuns <- allRuns
                state.HomeEmergentState <- updatedEmergent
                state.HomeLastCognitiveSubTick <- currentSubTick
            else
                state.AwayActiveRuns <- allRuns
                state.AwayEmergentState <- updatedEmergent
                state.AwayLastCognitiveSubTick <- currentSubTick

    let updateAdaptive
        (currentSubTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (events: ResizeArray<MatchEvent>)
        =

        let lastAdaptiveSubTick = getLastAdaptiveSubTick state clubSide

        if currentSubTick - lastAdaptiveSubTick < PhysicsContract.AdaptiveIntervalSubTicks then
            ()
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
                    (getAdaptiveState state clubSide)

            if isHome clubSide then
                state.HomeAdaptiveState <- updatedAdaptive
                state.HomeLastAdaptiveSubTick <- currentSubTick
            else
                state.AwayAdaptiveState <- updatedAdaptive
                state.AwayLastAdaptiveSubTick <- currentSubTick
