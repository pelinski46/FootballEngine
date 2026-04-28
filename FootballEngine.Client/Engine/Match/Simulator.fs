namespace FootballEngine

open FootballEngine.Domain
open SimStateOps
open FootballEngine.Movement
open FootballEngine.PhysicsContract
open SchedulingTypes
open SimulationClock

module MatchSimulator =

    let private bothSides = [| HomeClub; AwayClub |]

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private generateInitialTicks
        (ctx: MatchContext)
        (clock: SimulationClock)
        (timeline: Timeline)
        (seqStart: int64)
        : int64 =
        let mutable seq = seqStart
        let ft = fullTime clock
        let ht = halfTime clock

        timeline.Insert(0, KickOffTick, TickPriority.SetPiece, seq)
        seq <- seq + 1L

        timeline.Insert(ht, HalfTimeTick, TickPriority.MatchControl, seq)
        seq <- seq + 1L

        timeline.Insert(ft, FullTimeTick, TickPriority.MatchControl, seq)
        seq <- seq + 1L

        for idx, min in List.indexed [ 60; 75; 85 ] do
            let st = secondsToSubTicks clock (float min * 60.0)
            timeline.Insert(st, SubstitutionTick ctx.Home.Id, TickPriority.Manager, seq)
            seq <- seq + 1L
            timeline.Insert(st, SubstitutionTick ctx.Away.Id, TickPriority.Manager, seq)
            seq <- seq + 1L

        seq

    let private runLoop
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        (takeSnapshot: SimState -> int -> unit)
        : SimState * MatchEvent list =
        let ft = fullTime clock
        let ht = halfTime clock
        let stoppageBuffer = clock.SubTicksPerSecond * 60 * 10
        let maxTick = ft + stoppageBuffer
        let timeline = Timeline(maxTick)
        let schedule = PhaseSchedule.build clock maxTick
        let timelineBuffer = ResizeArray<TickKind * TickPriority * int64>(16)

        let mutable seqCounter = generateInitialTicks ctx clock timeline 0L
        let mutable playState = LivePlay
        let events = ResizeArray<MatchEvent>()
        let mutable pending = TickOrchestrator.empty
        let mutable effectiveFullTime = ft
        let mutable matchEnded = false
        let varState = VARState()
        let mutable momentum = DynamicMomentum.initial
        let mutable advantageState = NoAdvantage

        let addEvent (e: MatchEvent) =
            events.Add e
            state.MatchEvents.Add e

            if state.MatchEvents.Count > 1024 then
                state.MatchEvents.RemoveRange(0, 512)

        let scheduleNextTick (nt: PendingTick) =
            match TickOrchestrator.trySchedule nt.SubTick nt.Kind nt.Priority seqCounter pending with
            | ValueSome(seq, newPending) ->
                timeline.Insert(nt.SubTick, nt.Kind, nt.Priority, seq)
                seqCounter <- seq + 1L
                pending <- newPending
            | ValueNone -> ()

        let mutable subTick = 0

        while subTick <= effectiveFullTime && not matchEnded do
            state.SubTick <- subTick

            let phases =
                if subTick < schedule.Phases.Length then
                    schedule.Phases[subTick]
                else
                    { Physics = subTick % clock.PhysicsRate = 0
                      Cognitive = subTick % clock.CognitiveRate = 0
                      Steering = subTick % clock.SteeringRate = 0
                      Referee = subTick % clock.PhysicsRate = 0
                      Action = subTick % clock.ActionRate = 0
                      SetPiece = false
                      Adaptive = subTick % clock.AdaptiveRate = 0 }

            if phases.Cognitive then
                // Compute influence frames once (needs both home and away frames)
                let homeInfluence = InfluenceFrame.compute state.Home.Frame state.Away.Frame
                let awayInfluence = InfluenceFrame.compute state.Away.Frame state.Home.Frame
                state.HomeInfluenceFrame <- homeInfluence
                state.AwayInfluenceFrame <- awayInfluence

                for clubSide in bothSides do
                    let cFrame = CognitiveFrameModule.build ctx state clubSide
                    BatchDecision.processTeam subTick ctx state clock clubSide cFrame

            if phases.Physics then
                let tick =
                    { SubTick = subTick
                      Priority = TickPriority.Physics
                      SequenceId = 0L
                      Kind = PhysicsTick }

                let ballResult = BallAgent.agent tick ctx state clock

                if phases.Steering then
                    let dtPlayer = SimulationClock.dtPlayer clock

                    MovementEngine.updateTeamSide
                        subTick
                        ctx
                        state
                        HomeClub
                        dtPlayer
                        clock.SteeringRate
                        clock.CognitiveRate

                    MovementEngine.updateTeamSide
                        subTick
                        ctx
                        state
                        AwayClub
                        dtPlayer
                        clock.SteeringRate
                        clock.CognitiveRate

                match ballResult.NextTick with
                | Some nt when nt.Kind <> PhysicsTick -> scheduleNextTick nt
                | _ -> ()

                match ballResult.Transition with
                | Some ts -> playState <- ts
                | None -> ()

                ballResult.Events |> List.iter addEvent

            if phases.Referee then
                let tick =
                    { SubTick = subTick
                      Priority = TickPriority.Referee
                      SequenceId = 0L
                      Kind = RefereeTick }

                let refResult = RefereeAgent.agent tick ctx state clock

                match refResult.NextTick with
                | Some nt -> scheduleNextTick nt
                | None -> ()

                match refResult.Transition with
                | Some ts -> playState <- ts
                | None -> ()

                refResult.Actions
                |> List.collect (fun a -> RefereeApplicator.apply subTick a ctx state)
                |> List.iter addEvent

                if not varState.IsChecking then
                    match refResult.Actions with
                    | ConfirmGoal(scoringClub, scorerId, isOwnGoal) :: _ ->
                        match VARDetector.detectGoalCheck scoringClub scorerId isOwnGoal subTick with
                        | Some incident ->
                            state.StoppageTime.Add(subTick, StoppageReason.VARReviewDelay) |> ignore
                            let duration = VARReview.reviewDuration subTick
                            varState.AddReview(incident, subTick, duration)
                        | None -> ()
                    | _ -> ()

            match advantageState with
            | AdvantagePlaying(foulSubTick, fouledTeam, _) ->
                if AdvantageEngine.shouldCallBack foulSubTick subTick state.Ball.Possession fouledTeam then
                    state.Ball <-
                        { state.Ball with
                            Possession = Possession.SetPiece(fouledTeam, SetPieceKind.FreeKick) }

                    advantageState <- AdvantageCalledBack foulSubTick
            | _ -> ()

            if varState.IsChecking then
                match varState.CurrentReview with
                | Some(Reviewing(incident, startSubTick, durationSubTicks)) ->
                    if subTick >= startSubTick + durationSubTicks then
                        let decision = VARReview.evaluate state incident
                        varState.CompleteReview decision

                        let varEvents =
                            match decision with
                            | Overturn -> VARApplicator.applyOverturn subTick incident ctx state
                            | CheckComplete -> VARApplicator.applyCheckComplete subTick incident ctx state
                            | _ -> []

                        varEvents |> List.iter addEvent
                        varState.Clear()
                | _ -> varState.Clear()

            if phases.Action then
                match playState with
                | LivePlay ->
                    let hasCachedIntentHome = (SimStateOps.getTeamIntent state HomeClub).IsSome
                    let hasCachedIntentAway = (SimStateOps.getTeamIntent state AwayClub).IsSome

                    if hasCachedIntentHome && hasCachedIntentAway then
                        let actionResult = ActionResolver.run subTick ctx state clock
                        actionResult.Events |> List.iter addEvent
                | _ -> ()

            if phases.Adaptive then
                for clubSide in bothSides do
                    let stats = SimStateOps.getMatchStats state clubSide
                    let emergent = SimStateOps.getEmergentState state clubSide

                    let shortPassRate =
                        if stats.PassAttempts > 0 then
                            float stats.PassSuccesses / float stats.PassAttempts
                        else
                            0.5

                    let pressRate =
                        if stats.PressAttempts > 0 then
                            float stats.PressSuccesses / float stats.PressAttempts
                        else
                            0.5

                    let flankRate =
                        if stats.FlankAttempts > 0 then
                            float stats.FlankSuccesses / float stats.FlankAttempts
                        else
                            0.5

                    let frame = SimStateOps.getFrame state clubSide
                    let mutable totalCondition = 0
                    let mutable activeCount = 0

                    for i = 0 to frame.SlotCount - 1 do
                        match frame.Occupancy[i] with
                        | OccupancyKind.Active _ ->
                            totalCondition <- totalCondition + int frame.Condition[i]
                            activeCount <- activeCount + 1
                        | _ -> ()

                    let avgCondition =
                        if activeCount > 0 then
                            float totalCondition / float activeCount
                        else
                            50.0

                    let updated =
                        emergent
                        |> EmergentLoops.updateCompactness shortPassRate
                        |> EmergentLoops.updatePressing pressRate
                        |> EmergentLoops.updateWingPlay flankRate
                        |> EmergentLoops.updateFatigueSpiral avgCondition 0

                    SimStateOps.setEmergentState state clubSide updated

                    let recent = EventWindow.recentEvents 1200 state.MatchEvents
                    let adaptiveState = SimStateOps.getAdaptiveState state clubSide

                    let updatedRecords =
                        adaptiveState.Records
                        |> Array.map (fun r -> EventWindow.patternResults r.Pattern recent)

                    SimStateOps.setAdaptiveState
                        state
                        clubSide
                        { AdaptiveTactics.initial with
                            Records = updatedRecords }

                    SimStateOps.resetAdaptiveStats state clubSide

            if subTick - state.LastMemoryDecaySubTick >= MatchMemory.DecayIntervalSubTicks then
                MatchMemory.decay state.MatchMemory
                state.LastMemoryDecaySubTick <- subTick

            match playState with
            | Stopped _
            | PlayState.SetPiece _ ->
                timeline.CancelTicks (function
                    | PhysicsTick -> false
                    | RefereeTick -> false
                    | HalfTimeTick -> false
                    | FullTimeTick -> false
                    | MatchEndTick -> false
                    | _ -> true)
            | _ -> ()

            timeline.DequeueAllInto(timelineBuffer, subTick)

            for (kind, _priority, seqId) in timelineBuffer do
                let tick =
                    { SubTick = subTick
                      Priority = TickPriority.Physics
                      SequenceId = seqId
                      Kind = kind }

                let result =
                    match kind with
                    | SetPieceTick _
                    | KickOffTick -> SetPieceAgent.agent tick ctx state clock
                    | HalfTimeTick ->
                        state.HomeAttackDir <- RightToLeft

                        state.HomeCognitiveFrame <- CognitiveFrameDefaults.empty
                        state.AwayCognitiveFrame <- CognitiveFrameDefaults.empty
                        state.HomeTeamIntent <- TeamIntentDefaults.empty
                        state.AwayTeamIntent <- TeamIntentDefaults.empty

                        let mirrorFrame (frame: TeamFrame) : unit =
                            for i = 0 to frame.SlotCount - 1 do
                                match frame.Occupancy[i] with
                                | OccupancyKind.Active _ ->
                                    let x = float frame.PosX[i] * 1.0<meter>
                                    let y = float frame.PosY[i] * 1.0<meter>

                                    let mirrored =
                                        MatchSpatial.mirrorSpatial
                                            { X = x
                                              Y = y
                                              Z = 0.0<meter>
                                              Vx = 0.0<meter / second>
                                              Vy = 0.0<meter / second>
                                              Vz = 0.0<meter / second> }

                                    FrameMutate.setPos frame i mirrored.X mirrored.Y
                                | _ -> ()

                        mirrorFrame state.Home.Frame
                        mirrorFrame state.Away.Frame

                        state.HomeBasePositions <- Array.map MatchSpatial.mirrorSpatial state.HomeBasePositions
                        state.AwayBasePositions <- Array.map MatchSpatial.mirrorSpatial state.AwayBasePositions

                        let halfAdded = state.StoppageTime.DecideHalfTime()
                        let addedSubTicks = halfAdded * clock.SubTicksPerSecond

                        if ht + addedSubTicks > effectiveFullTime then
                            effectiveFullTime <- ht + addedSubTicks

                        { NextTick = None
                          Events = []
                          Transition = Some(SetPiece SetPieceKind.KickOff) }
                    | FullTimeTick
                    | MatchEndTick ->
                        if not state.StoppageTime.FullTimeDecided then
                            let fullAdded = state.StoppageTime.DecideFullTime()
                            let addedSubTicks = fullAdded * clock.SubTicksPerSecond

                            if ft + addedSubTicks > effectiveFullTime then
                                effectiveFullTime <- ft + addedSubTicks

                            if subTick >= effectiveFullTime then
                                matchEnded <- true

                        { NextTick = None
                          Events = []
                          Transition = None }
                    | InjuryTick _ ->
                        let refResult = RefereeAgent.agent tick ctx state clock

                        refResult.Actions
                        |> List.collect (fun a -> RefereeApplicator.apply subTick a ctx state)
                        |> List.iter addEvent

                        { NextTick = refResult.NextTick
                          Events = []
                          Transition = refResult.Transition }
                    | ResumePlayTick ->
                        { NextTick = None
                          Events = []
                          Transition = Some LivePlay }
                    | RefereeTick ->
                        { NextTick = None
                          Events = []
                          Transition = None }
                    | SubstitutionTick _
                    | ManagerReactionTick _
                    | ManagerTick _ -> ManagerAgent.agent tick ctx state clock
                    | PhysicsTick
                    | CognitiveTick ->
                        { NextTick = None
                          Events = []
                          Transition = None }

                match result.NextTick with
                | Some nt -> scheduleNextTick nt
                | None -> ()

                match result.Transition with
                | Some ts -> playState <- ts
                | None -> ()

                result.Events |> List.iter addEvent

                for e in result.Events do
                    match e.ClubId with
                    | cid when cid = ctx.Home.Id -> momentum <- DynamicMomentum.update momentum subTick HomeClub
                    | cid when cid = ctx.Away.Id -> momentum <- DynamicMomentum.update momentum subTick AwayClub
                    | _ -> ()

            takeSnapshot state subTick
            subTick <- subTick + 1

        state, events |> Seq.toList

    let private isKeyEvent (e: MatchEvent) : bool =
        match e.Type with
        | MatchEventType.Goal
        | MatchEventType.Assist
        | MatchEventType.YellowCard
        | MatchEventType.RedCard
        | MatchEventType.Injury _
        | MatchEventType.Save -> true
        | _ -> false

    let runLoopFast (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : SimState * MatchEvent list =
        let finalState, allEvents = runLoop ctx state clock (fun _ _ -> ())
        let keyEvents = allEvents |> List.filter isKeyEvent
        finalState, keyEvents

    let runLoopFull (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchReplay =
        let snapshots = System.Collections.Generic.List<SimSnapshot>()
        let snapshotInterval = clock.SubTicksPerSecond / 8
        let mutable lastSnapshotAt = 0

        let takeSnapshot st subTick =
            if subTick >= lastSnapshotAt + snapshotInterval then
                snapshots.Add(SnapshotBuilder.take st)
                lastSnapshotAt <- subTick + snapshotInterval

        let finalState, events = runLoop ctx state clock takeSnapshot

        { Context = ctx
          Final = finalState
          Events = events
          Snapshots = snapshots.ToArray() }

    open FsToolkit.ErrorHandling

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    let private toCoords slotX slotY (clubSide: ClubSide) : float<meter> * float<meter> =
        match clubSide with
        | HomeClub -> (1.0 - slotY) * PitchLength, slotX * PitchWidth
        | AwayClub -> slotY * PitchLength, slotX * PitchWidth

    let private kickOffPosition (x: float) (y: float) (clubSide: ClubSide) : float<meter> * float<meter> =
        let tacticalX, tacticalY = toCoords x y clubSide

        let clampedX =
            match clubSide with
            | HomeClub -> PhysicsContract.clamp tacticalX 2.0<meter> (HalfwayLineX - 1.0<meter>)
            | AwayClub -> PhysicsContract.clamp tacticalX (HalfwayLineX + 1.0<meter>) (PitchLength - 2.0<meter>)

        clampedX, tacticalY

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (clubSide: ClubSide)
        : Result<(Player * float<meter> * float<meter>)[] * TeamTactics * TacticalInstructions option, SimulationError> =
        match headCoach.Attributes.Coaching.Lineup with
        | None -> Error(MissingLineup club.Name)
        | Some lu ->
            let slots =
                lu.Slots
                |> List.choose (fun s ->
                    s.PlayerId
                    |> Option.bind (fun pid ->
                        players
                        |> Map.tryFind pid
                        |> Option.map (fun p ->
                            let x, y = toCoords s.X s.Y clubSide
                            p, x, y)))
                |> Array.ofList

            Ok(slots, lu.Tactics, lu.Instructions)

    let private validateLineups (home: Club) homeData (away: Club) awayData : Result<unit, SimulationError> =
        if home.Id = away.Id then
            Error(SameClub home.Name)
        elif homeData |> Array.length <> 11 then
            Error(IncompleteLineup(home.Name, homeData |> Array.length))
        elif awayData |> Array.length <> 11 then
            Error(IncompleteLineup(away.Name, awayData |> Array.length))
        else
            Ok()

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float<meter> * float<meter>>) : Spatial[] =
        players
        |> Array.map (fun p ->
            match Map.tryFind p.Id posMap with
            | Some(x, y) -> defaultSpatial x y
            | None -> failwithf $"Position missing for player %s{p.Name} (%A{p.Id})")

    let private initSimState
        home
        homeCoach
        hp
        (hPosMap: Map<PlayerId, float<meter> * float<meter>>)
        homeTactics
        homeInstructions
        away
        awayCoach
        ap
        (aPosMap: Map<PlayerId, float<meter> * float<meter>>)
        awayTactics
        awayInstructions
        isKnockout
        profileMap
        =

        let hPos =
            hp
            |> Array.mapi (fun i (p: Player) ->
                let tx, ty =
                    hPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let prof = profileMap |> Map.tryFind p.Id |> Option.defaultValue (Player.profile p)

                let kx =
                    match p.Position with
                    | ST
                    | AMC ->
                        let allowedDepth = (float prof.AttackingDepth) * 5.0<meter>
                        let threshold = HalfwayLineX - 5.0<meter> + allowedDepth

                        if tx > threshold then
                            HalfwayLineX - 1.0<meter> + allowedDepth * 0.5
                        else
                            PhysicsContract.clamp tx 2.0<meter> (HalfwayLineX - 2.0<meter>)
                    | _ -> PhysicsContract.clamp tx 2.0<meter> (HalfwayLineX - 2.0<meter>)

                defaultSpatial kx ty)

        let aPos =
            ap
            |> Array.map (fun (p: Player) ->
                let tx, ty =
                    aPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let kx =
                    PhysicsContract.clamp tx (HalfwayLineX + 4.0<meter>) (PitchLength - 2.0<meter>)

                defaultSpatial kx ty)

        let defaultInstr = TacticalInstructions.defaultInstructions
        let hCount = hp |> Array.length
        let aCount = ap |> Array.length

        let homeRoster = PlayerRoster.build hp
        let awayRoster = PlayerRoster.build ap
        let homeFrame = TeamFrame.init homeRoster hPos
        let awayFrame = TeamFrame.init awayRoster aPos

        let ctx =
            { Home = home
              Away = away
              HomeCoach = homeCoach
              AwayCoach = awayCoach
              HomePlayers = hp
              AwayPlayers = ap
              HomeBasePositions = hPos
              AwayBasePositions = aPos
              HomeChemistry = ChemistryGraph.init hCount
              AwayChemistry = ChemistryGraph.init aCount
              IsKnockoutMatch = isKnockout
              Config = BalanceCalibrator.getConfig ()
              HomeRoster = homeRoster
              AwayRoster = awayRoster }

        let state = SimState()
        state.SubTick <- 0
        state.HomeScore <- 0
        state.AwayScore <- 0
        state.Config <- BalanceCalibrator.getConfig ()

        state.Ball <-
            { defaultBall with
                Possession = Possession.SetPiece(HomeClub, SetPieceKind.KickOff) }

        state.Momentum <- 0.0
        state.HomeBasePositions <- ctx.HomeBasePositions
        state.AwayBasePositions <- ctx.AwayBasePositions
        state.HomeAttackDir <- LeftToRight
        state.BallXSmooth <- 52.5<meter>

        let homeTeam = TeamSimState.empty ()
        homeTeam.Frame <- homeFrame
        homeTeam.Tactics <- homeTactics
        homeTeam.Instructions <- homeInstructions |> Option.orElse (Some defaultInstr)
        state.Home <- homeTeam

        let awayTeam = TeamSimState.empty ()
        awayTeam.Frame <- awayFrame
        awayTeam.Tactics <- awayTactics
        awayTeam.Instructions <- awayInstructions |> Option.orElse (Some defaultInstr)
        state.Away <- awayTeam

        ctx, state

    let private simulatePenaltyShootout
        (ctx: MatchContext)
        (state: SimState)
        (players: Map<PlayerId, Player>)
        (clock: SimulationClock)
        =
        result {
            let homePlayers =
                ctx.Home.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let awayPlayers =
                ctx.Away.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let rec simulateKicks homeKicks awayKicks kickNum =
                let homeKicker = homePlayers |> List.item ((kickNum - 1) % homePlayers.Length)
                let awayKicker = awayPlayers |> List.item ((kickNum - 1) % awayPlayers.Length)

                let homeSubTick = fullTime clock + kickNum * clock.SubTicksPerSecond * 2
                let awaySubTick = homeSubTick + clock.SubTicksPerSecond

                let prevHomeScore = state.HomeScore

                let homeResult =
                    SetPlayAction.resolvePenalty homeSubTick ctx state homeKicker HomeClub clock

                match homeResult.PendingGoal with
                | Some pg ->
                    let scorerId, isOwnGoal = GoalDetector.scorer pg.ScoringClub state.Ball ctx state

                    RefereeApplicator.apply homeSubTick (ConfirmGoal(pg.ScoringClub, scorerId, isOwnGoal)) ctx state
                    |> ignore
                | None -> ()

                let homeScored = state.HomeScore > prevHomeScore

                let prevAwayScore = state.AwayScore

                let awayResult =
                    SetPlayAction.resolvePenalty awaySubTick ctx state awayKicker AwayClub clock

                match awayResult.PendingGoal with
                | Some pg ->
                    let scorerId, isOwnGoal = GoalDetector.scorer pg.ScoringClub state.Ball ctx state

                    RefereeApplicator.apply awaySubTick (ConfirmGoal(pg.ScoringClub, scorerId, isOwnGoal)) ctx state
                    |> ignore
                | None -> ()

                let awayScored = state.AwayScore > prevAwayScore

                let newHomeKicks = (homeKicker.Id, homeScored, kickNum) :: homeKicks
                let newAwayKicks = (awayKicker.Id, awayScored, kickNum) :: awayKicks
                let homeGoals = newHomeKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)
                let awayGoals = newAwayKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)

                if kickNum > 5 && homeGoals <> awayGoals then
                    { HomeKicks = List.rev newHomeKicks
                      AwayKicks = List.rev newAwayKicks
                      CurrentKick = kickNum
                      IsComplete = true }
                elif kickNum > 5 then
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                elif kickNum <= 5 then
                    let remaining = 5 - kickNum

                    if homeGoals > awayGoals + remaining || awayGoals > homeGoals + remaining then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    elif kickNum = 5 then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    else
                        simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                else
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)

            let shootout = simulateKicks [] [] 1

            let baseSubTick = fullTime clock

            let events =
                [ for pid, scored, k in shootout.HomeKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = ctx.Home.Id
                        Type = PenaltyAwarded scored
                        Context = EventContext.empty }
                  for pid, scored, k in shootout.AwayKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = ctx.Away.Id
                        Type = PenaltyAwarded scored
                        Context = EventContext.empty } ]

            return
                { Context = ctx
                  Final = state
                  Events = events
                  Snapshots = [||] }
        }

    let setup
        home
        away
        players
        staff
        isKnockout
        profileMap
        : Result<MatchContext * SimState * Player list * Player list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players HomeClub
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players AwayClub
            do! validateLineups home homeData away awayData

            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)

            let hPosMap = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
            let aPosMap = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray

            let ctx, state =
                initSimState
                    home
                    homeCoach
                    hp
                    hPosMap
                    homeTactics
                    homeInstructions
                    away
                    awayCoach
                    ap
                    aPosMap
                    awayTactics
                    awayInstructions
                    isKnockout
                    profileMap

            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            return ctx, state, homeSquad, awaySquad
        }

    let trySimulateMatch
        home
        away
        players
        staff
        profileMap
        : Result<int * int * MatchEvent list * SimState, SimulationError> =
        result {
            let validationErrors = ConfigValidation.validateAll (BalanceCalibrator.getConfig ())

            if not validationErrors.IsEmpty then
                let msg = validationErrors |> String.concat "\n"
                return failwithf $"BalanceConfig validation failed:\n%s{msg}"

            let! ctx, state, _, _ = setup home away players staff false profileMap
            let final, events = runLoopFast ctx state defaultClock
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff profileMap : Result<MatchReplay, SimulationError> =
        result {
            let validationErrors = ConfigValidation.validateAll (BalanceCalibrator.getConfig ())

            if not validationErrors.IsEmpty then
                let msg = validationErrors |> String.concat "\n"
                return failwithf $"BalanceConfig validation failed:\n%s{msg}"

            let! ctx, state, _, _ = setup home away players staff false profileMap
            return runLoopFull ctx state defaultClock
        }

    let trySimulateMatchKnockout home away players staff profileMap : Result<MatchReplay * bool, SimulationError> =
        result {
            let validationErrors = ConfigValidation.validateAll (BalanceCalibrator.getConfig ())

            if not validationErrors.IsEmpty then
                let msg = validationErrors |> String.concat "\n"
                return failwithf $"BalanceConfig validation failed:\n%s{msg}"

            let! ctx, state, _, _ = setup home away players staff true profileMap
            let replay = runLoopFull ctx state defaultClock

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootout = simulatePenaltyShootout ctx replay.Final players defaultClock
                return shootout, true
            else
                return replay, false
        }

    let buildInitState home away players staff profileMap =
        match setup home away players staff false profileMap with
        | Ok(ctx, state, homeSquad, awaySquad) -> Some(ctx, state, homeSquad, awaySquad)
        | Error _ -> None
