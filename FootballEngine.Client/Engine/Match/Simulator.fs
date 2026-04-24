namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SetPlayAction
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

    let private defaultIntent : PlayerIntent =
        { Movement = MovementIntent.MaintainShape { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
          Action = None
          Context = NormalPlay
          Urgency = 0.0
          Confidence = 0.5 }

    let private generateInitialTicks (ctx: MatchContext) (clock: SimulationClock) (timeline: Timeline) (seqStart: int64) : int64 =
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
        let timeline = Timeline(ft)
        let schedule = PhaseSchedule.build clock ft

        let mutable seqCounter = generateInitialTicks ctx clock timeline 0L
        let mutable playState = LivePlay
        let events = ResizeArray<MatchEvent>()
        let mutable running = true

        for subTick = 0 to ft do
            if not running then
                ()
            else
                state.SubTick <- subTick
                takeSnapshot state subTick

                let phases = schedule.Phases[subTick]

                if phases.Physics then
                    let tick = { SubTick = subTick; Priority = TickPriority.Physics; SequenceId = 0L; Kind = PhysicsTick }
                    let ballResult = BallAgent.agent tick ctx state clock

                    if subTick % clock.SteeringRate = 0 then
                        let dtPlayer = SimulationClock.dtPlayer clock
                        MovementEngine.updateTeamSide subTick ctx state HomeClub dtPlayer clock.SteeringRate clock.CognitiveRate
                        MovementEngine.updateTeamSide subTick ctx state AwayClub dtPlayer clock.SteeringRate clock.CognitiveRate

                    match ballResult.NextTick with
                    | Some nt when nt.Kind <> PhysicsTick ->
                        timeline.Insert(nt.SubTick, nt.Kind, nt.Priority, seqCounter)
                        seqCounter <- seqCounter + 1L
                    | _ -> ()

                    match ballResult.Transition with
                    | Some ts -> playState <- ts
                    | None -> ()

                    ballResult.Events |> List.iter events.Add

                if phases.Referee then
                    let tick = { SubTick = subTick; Priority = TickPriority.Referee; SequenceId = 0L; Kind = RefereeTick }
                    let refResult = RefereeAgent.agent tick ctx state clock

                    match refResult.NextTick with
                    | Some nt ->
                        timeline.Insert(nt.SubTick, nt.Kind, nt.Priority, seqCounter)
                        seqCounter <- seqCounter + 1L
                    | None -> ()

                    match refResult.Transition with
                    | Some ts -> playState <- ts
                    | None -> ()

                    refResult.Events |> List.iter events.Add

                if phases.Cognitive then
                    match playState with
                    | LivePlay ->
                        for clubSide in bothSides do
                            let cFrame = CognitiveFrameModule.build ctx state clubSide
                            BatchDecision.processTeam subTick ctx state clock clubSide cFrame

                        let actionEvents = ActionResolver.run subTick ctx state clock
                        actionEvents |> List.iter events.Add
                    | _ -> ()

                if phases.Adaptive then
                    for clubSide in bothSides do
                        let stats = SimStateOps.getMatchStats state clubSide
                        let emergent = SimStateOps.getEmergentState state clubSide

                        let shortPassRate =
                            if stats.PassAttempts > 0 then float stats.PassSuccesses / float stats.PassAttempts
                            else 0.5
                        let pressRate =
                            if stats.PressAttempts > 0 then float stats.PressSuccesses / float stats.PressAttempts
                            else 0.5
                        let flankRate =
                            if stats.FlankAttempts > 0 then float stats.FlankSuccesses / float stats.FlankAttempts
                            else 0.5

                        let frame = SimStateOps.getFrame state clubSide
                        let roster = SimStateOps.getRoster ctx clubSide
                        let mutable totalCondition = 0
                        let mutable activeCount = 0
                        for i = 0 to frame.SlotCount - 1 do
                            match frame.Occupancy[i] with
                            | OccupancyKind.Active _ ->
                                totalCondition <- totalCondition + int frame.Condition[i]
                                activeCount <- activeCount + 1
                            | _ -> ()
                        let avgCondition = if activeCount > 0 then float totalCondition / float activeCount else 50.0

                        let updated =
                            emergent
                            |> EmergentLoops.updateCompactness shortPassRate
                            |> EmergentLoops.updatePressing pressRate
                            |> EmergentLoops.updateWingPlay flankRate
                            |> EmergentLoops.updateFatigueSpiral avgCondition 0

                        SimStateOps.setEmergentState state clubSide updated
                        SimStateOps.resetAdaptiveStats state clubSide

                let timelineTicks = timeline.DequeueAllAtCurrent()

                for (kind, _priority, seqId) in timelineTicks do
                    let tick = { SubTick = subTick; Priority = TickPriority.Physics; SequenceId = seqId; Kind = kind }

                    let result =
                        match kind with
                        | SetPieceTick _
                        | KickOffTick -> SetPieceAgent.agent tick ctx state clock
                        | HalfTimeTick
                        | FullTimeTick
                        | MatchEndTick
                        | InjuryTick _
                        | ResumePlayTick
                        | RefereeTick -> RefereeAgent.agent tick ctx state clock
                        | SubstitutionTick _
                        | ManagerReactionTick _
                        | ManagerTick _ -> ManagerAgent.agent tick ctx state clock
                        | PhysicsTick
                        | CognitiveTick ->
                            { Intent = defaultIntent; NextTick = None; Events = []; Transition = None }

                    match result.NextTick with
                    | Some nt ->
                        timeline.Insert(nt.SubTick, nt.Kind, nt.Priority, seqCounter)
                        seqCounter <- seqCounter + 1L
                    | None -> ()

                    match result.Transition with
                    | Some ts -> playState <- ts
                    | None -> ()

                    result.Events |> List.iter events.Add

                match playState with
                | Stopped _ | PlayState.SetPiece _ ->
                    timeline.CancelTicks (function
                        | PhysicsTick -> false
                        | RefereeTick -> false
                        | HalfTimeTick -> false
                        | FullTimeTick -> false
                        | MatchEndTick -> false
                        | _ -> true)
                | _ -> ()

                if subTick >= ft then
                    running <- false

        state, events |> Seq.toList

    let runLoopFast (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : SimState * MatchEvent list =
        runLoop ctx state clock (fun _ _ -> ())

    let runLoopFull (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : MatchReplay =
        let snapshots = System.Collections.Generic.List<SimSnapshot>()
        let snapshotInterval = clock.SubTicksPerSecond
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
            | None -> failwithf "Position missing for player %s (%A)" p.Name p.Id)

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
              Config = BalanceConfig.defaultConfig
              HomeRoster = homeRoster
              AwayRoster = awayRoster }

        let state = SimState()
        state.SubTick <- 0
        state.HomeScore <- 0
        state.AwayScore <- 0
        state.Config <- BalanceConfig.defaultConfig

        state.Ball <-
            { defaultBall with
                Possession = Possession.SetPiece(HomeClub, SetPieceKind.KickOff) }

        state.Momentum <- 0.0
        state.HomeBasePositions <- ctx.HomeBasePositions
        state.AwayBasePositions <- ctx.AwayBasePositions
        state.HomeAttackDir <- LeftToRight
        state.BallXSmooth <- 52.5<meter>

        state.Home <-
            { TeamSimState.empty with
                Frame = homeFrame
                Tactics = homeTactics
                Instructions = homeInstructions |> Option.orElse (Some defaultInstr) }

        state.Away <-
            { TeamSimState.empty with
                Frame = awayFrame
                Tactics = awayTactics
                Instructions = awayInstructions |> Option.orElse (Some defaultInstr) }

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

                let prevHomeScore = state.HomeScore
                resolvePenalty ctx state homeKicker HomeClub kickNum clock |> ignore
                let homeScored = state.HomeScore > prevHomeScore

                let prevAwayScore = state.AwayScore
                resolvePenalty ctx state awayKicker AwayClub kickNum clock |> ignore
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
                        Type = PenaltyAwarded scored }
                  for pid, scored, k in shootout.AwayKicks ->
                      { SubTick = baseSubTick + k
                        PlayerId = pid
                        ClubId = ctx.Away.Id
                        Type = PenaltyAwarded scored } ]

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
            let! ctx, state, _, _ = setup home away players staff false profileMap
            let final, events = runLoopFast ctx state defaultClock
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff profileMap : Result<MatchReplay, SimulationError> =
        result {
            let! ctx, state, _, _ = setup home away players staff false profileMap
            return runLoopFull ctx state defaultClock
        }

    let trySimulateMatchKnockout home away players staff profileMap : Result<MatchReplay * bool, SimulationError> =
        result {
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
