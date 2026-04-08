namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SetPlayAction
open SimStateOps
open FootballEngine.Movement
open FootballEngine.PhysicsContract
open FootballEngine.Stats
open SchedulingTypes

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private generateInitialTicks (ctx: MatchContext) : ScheduledTick list =
        let sub min = secondsToSubTicks (float min * 60.0)

        let subs =
            [ for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub min
                    Priority = TickPriority.Manager
                    SequenceId = int64 idx
                    Kind = SubstitutionTick ctx.Home.Id }
              for idx, min in List.indexed [ 60; 75; 85 ] ->
                  { SubTick = sub min
                    Priority = TickPriority.Manager
                    SequenceId = int64 (idx + 3)
                    Kind = SubstitutionTick ctx.Away.Id } ]

        [ yield
              { SubTick = PhysicsIntervalSubTicks
                Priority = TickPriority.Physics
                SequenceId = 0L
                Kind = PhysicsTick }
          yield
              { SubTick = secondsToSubTicks 12.0
                Priority = TickPriority.Manager
                SequenceId = 1L
                Kind = CognitiveTick }
          yield
              { SubTick = secondsToSubTicks 60.0
                Priority = TickPriority.Manager
                SequenceId = 2L
                Kind = AdaptiveTick }
          yield! subs
          yield
              { SubTick = HalfTimeSubTick
                Priority = TickPriority.MatchControl
                SequenceId = 10L
                Kind = HalfTimeTick }
          yield
              { SubTick = FullTimeSubTick
                Priority = TickPriority.MatchControl
                SequenceId = 11L
                Kind = FullTimeTick }
          yield
              { SubTick = 0
                Priority = TickPriority.SetPiece
                SequenceId = -1L
                Kind = KickOffTick } ]

    let private spawnCognitiveTick (tick: ScheduledTick) =
        { SubTick = tick.SubTick + CognitiveIntervalSubTicks
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = CognitiveTick }

    let private spawnAdaptiveTick (tick: ScheduledTick) =
        { SubTick = tick.SubTick + AdaptiveIntervalSubTicks
          Priority = TickPriority.Manager
          SequenceId = 0L
          Kind = AdaptiveTick }

    let private runTick
        homeId
        homeSquad
        awaySquad
        (tick: ScheduledTick)
        (ctx: MatchContext)
        (state: SimState)
        : AgentOutput =
        let dispatchAgent (tick: ScheduledTick) : Agent =
            match tick.Kind with
            | PhysicsTick -> BallAgent.agent
            | DuelTick _
            | PlayerActionTick _
            | CognitiveTick
            | AdaptiveTick
            | PossessionChangeTick _ -> PlayerAgent.agent
            | FreeKickTick _
            | CornerTick _
            | ThrowInTick _
            | PenaltyTick _
            | GoalKickTick
            | KickOffTick -> SetPieceAgent.agent
            | InjuryTick _
            | ResumePlayTick
            | HalfTimeTick
            | FullTimeTick
            | MatchEndTick
            | ExtraTimeTick _ -> RefereeAgent.agent
            | SubstitutionTick _
            | ManagerReactionTick _ -> ManagerAgent.agent

        (dispatchAgent tick) homeId homeSquad awaySquad tick ctx state

    let runLoopDes homeId homeSquad awaySquad (ctx: MatchContext) (state: SimState) (saveSnapshots: bool) =
        let scheduler = TickScheduler(MaxMatchSubTicks)
        generateInitialTicks ctx |> List.iter scheduler.Insert

        let snapshots =
            if saveSnapshots then
                Some(System.Collections.Generic.List<SimSnapshot>())
            else
                None

        let initialState =
            { Context = ctx
              State = state
              Events = ResizeArray()
              PlayState = SetPiece KickOff
              Snapshots = snapshots
              MatchEndScheduled = false
              SequenceCounter = scheduler.Count |> int64 }

        let firstDuelInterval =
            normalInt
                (float BalanceConfig.duelNextDelay.MeanST)
                (float BalanceConfig.duelNextDelay.StdST)
                BalanceConfig.duelNextDelay.MinST
                BalanceConfig.duelNextDelay.MaxST

        scheduler.Insert
            { SubTick = firstDuelInterval
              Priority = TickPriority.Duel
              SequenceId = -1L
              Kind = DuelTick 0 }

        let snapshotInterval = SubTicksPerSecond


        let rec loop (ls: LoopState) (scheduler: TickScheduler) lastSnapshotAt =
            match scheduler.Dequeue() with
            | ValueNone -> ls.State, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

            | ValueSome tick ->
                ls.State.SubTick <- tick.SubTick

                match tick.Kind with
                | FullTimeTick ->
                    ls.State.SubTick <- tick.SubTick
                    ls.State, ls.Events |> Seq.toList, ls.Snapshots |> Option.map _.ToArray()

                | _ ->
                    let shouldProcess =
                        match ls.PlayState, tick.Kind with
                        | (Stopped _ | PlayState.SetPiece _), DuelTick _ -> false
                        | (Stopped _ | PlayState.SetPiece _), PlayerActionTick _ -> false
                        | _ -> true

                    if not shouldProcess then
                        loop ls scheduler lastSnapshotAt
                    else
                        let result = runTick homeId homeSquad awaySquad tick ls.Context ls.State

                        let autoSpawned =
                            match tick.Kind with
                            | CognitiveTick -> [ spawnCognitiveTick tick ]
                            | AdaptiveTick -> [ spawnAdaptiveTick tick ]
                            | _ -> []

                        match tick.Kind with
                        | PhysicsTick ->
                            if tick.SubTick % SteeringIntervalSubTicks = 0 then
                                MovementEngine.updateTeamSide tick.SubTick ls.Context ls.State HomeClub DtPlayer
                                MovementEngine.updateTeamSide tick.SubTick ls.Context ls.State AwayClub DtPlayer

                            let dir = SimStateOps.attackDirFor ls.State.AttackingClub ls.State
                            ls.State.CurrentPhase <- SimStateOps.phaseFromBallZone dir ls.State.Ball.Position.X

                        | CognitiveTick ->
                            MovementEngine.updateCognitive tick.SubTick ls.Context ls.State HomeClub ls.Events
                            MovementEngine.updateCognitive tick.SubTick ls.Context ls.State AwayClub ls.Events

                        | AdaptiveTick ->
                            MovementEngine.updateAdaptive tick.SubTick ls.Context ls.State HomeClub ls.Events
                            MovementEngine.updateAdaptive tick.SubTick ls.Context ls.State AwayClub ls.Events


                        | _ -> ()

                        let newLastSnapshotAt =
                            match ls.Snapshots with
                            | Some snaps when tick.SubTick >= lastSnapshotAt + snapshotInterval ->
                                snaps.Add
                                    { SubTick = ls.State.SubTick
                                      HomePositions =
                                        ls.State.HomeSlots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Pos
                                            | Sidelined _ -> kickOffSpatial)
                                      AwayPositions =
                                        ls.State.AwaySlots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Pos
                                            | Sidelined _ -> kickOffSpatial)
                                      BallX = ls.State.Ball.Position.X
                                      BallY = ls.State.Ball.Position.Y
                                      BallVx = ls.State.Ball.Position.Vx
                                      BallVy = ls.State.Ball.Position.Vy
                                      BallControlledBy = ls.State.Ball.ControlledBy
                                      HomeScore = ls.State.HomeScore
                                      AwayScore = ls.State.AwayScore
                                      HomeConditions =
                                        ls.State.HomeSlots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Condition
                                            | Sidelined _ -> 0)
                                      AwayConditions =
                                        ls.State.AwaySlots
                                        |> Array.map (function
                                            | PlayerSlot.Active s -> s.Condition
                                            | Sidelined _ -> 0)
                                      HomeSidelined = ls.State.HomeSidelined
                                      AwaySidelined = ls.State.AwaySidelined
                                      AttackingClub = ls.State.AttackingClub
                                      HomeAttackDir = ls.State.HomeAttackDir
                                      Momentum = ls.State.Momentum }

                                tick.SubTick + snapshotInterval
                            | _ -> lastSnapshotAt

                        let allSpawned =
                            match autoSpawned with
                            | [] -> result.Spawned
                            | _ -> autoSpawned @ result.Spawned

                        let stampedTicks, newCounter =
                            allSpawned
                            |> List.mapFold (fun seq t -> { t with SequenceId = seq }, seq + 1L) ls.SequenceCounter

                        stampedTicks |> List.iter scheduler.Insert

                        let newPlayState = result.Transition |> Option.defaultValue ls.PlayState

                        result.Events |> List.iter ls.Events.Add

                        loop
                            { ls with
                                PlayState = newPlayState
                                SequenceCounter = newCounter }
                            scheduler
                            newLastSnapshotAt

        loop initialState scheduler 0

    let runLoopFastDes homeId homeSquad awaySquad ctx state =
        let s, evs, _ = runLoopDes homeId homeSquad awaySquad ctx state false
        s, evs

    let private runLoopFullDes homeId homeSquad awaySquad ctx state =
        match runLoopDes homeId homeSquad awaySquad ctx state true with
        | s, evs, Some snaps -> s, evs, snaps
        | s, evs, None -> s, evs, [||]

    open FsToolkit.ErrorHandling

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    let private toCoords slotX slotY (clubSide: ClubSide) =
        match clubSide with
        | HomeClub -> (1.0 - slotY) * PitchLength, slotX * PitchWidth
        | AwayClub -> slotY * PitchLength, slotX * PitchWidth

    let private kickOffPosition (x: float) (y: float) (clubSide: ClubSide) =
        let tacticalX, tacticalY = toCoords x y clubSide

        let clampedX =
            match clubSide with
            | HomeClub -> System.Math.Clamp(tacticalX, 2.0, HalfwayLineX - 1.0)
            | AwayClub -> System.Math.Clamp(tacticalX, HalfwayLineX + 1.0, PitchLength - 2.0)

        clampedX, tacticalY

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (clubSide: ClubSide)
        : Result<(Player * float * float)[] * TeamTactics * TacticalInstructions option, SimulationError> =
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

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float * float>) : Spatial[] =
        players
        |> Array.map (fun p ->
            let x, y =
                posMap
                |> Map.tryFind p.Id
                |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

            defaultSpatial x y)

    let private initSimState
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
        =

        let hPos =
            hp
            |> Array.map (fun (p: Player) ->
                let tx, ty =
                    hPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let kx =
                    match p.Position with
                    | ST
                    | AMC when tx > HalfwayLineX - 5.0 -> HalfwayLineX - 1.0
                    | _ -> System.Math.Clamp(tx, 2.0, HalfwayLineX - 2.0)

                defaultSpatial kx ty)

        let aPos =
            ap
            |> Array.map (fun (p: Player) ->
                let tx, ty =
                    aPosMap
                    |> Map.tryFind p.Id
                    |> Option.defaultValue (HalfwayLineX, PitchWidth / 2.0)

                let kx = System.Math.Clamp(tx, HalfwayLineX + 4.0, PitchLength - 2.0)
                defaultSpatial kx ty)

        let defaultInstr = TacticalInstructions.defaultInstructions
        let hCount = hp |> Array.length
        let aCount = ap |> Array.length

        let ctx =
            { Home = home
              Away = away
              HomeCoach = homeCoach
              AwayCoach = awayCoach
              HomePlayers = hp
              AwayPlayers = ap
              HomeBasePositions = hPos
              AwayBasePositions = aPos
              IsKnockoutMatch = isKnockout }

        let state = SimState()
        state.SubTick <- 0
        state.HomeScore <- 0
        state.AwayScore <- 0
        state.Ball <- defaultBall
        state.AttackingClub <- HomeClub
        state.Momentum <- 0.0
        state.HomeBasePositions <- ctx.HomeBasePositions
        state.AwayBasePositions <- ctx.AwayBasePositions
        state.HomeAttackDir <- LeftToRight

        state.HomeSlots <-
            Array.init hCount (fun i ->
                let p = hp[i]

                PlayerSlot.Active
                    { Player = p
                      Pos = hPos[i]
                      Condition = p.Condition
                      Mental = MentalState.initial p
                      Directives = Array.empty })

        state.HomeSidelined <- Map.empty
        state.HomeYellows <- Map.empty
        state.HomeSubsUsed <- 0
        state.HomeInstructions <- homeInstructions |> Option.orElse (Some defaultInstr)

        state.AwaySlots <-
            Array.init aCount (fun i ->
                let p = ap[i]

                PlayerSlot.Active
                    { Player = p
                      Pos = aPos[i]
                      Condition = p.Condition
                      Mental = MentalState.initial p
                      Directives = Array.empty })

        state.AwaySidelined <- Map.empty
        state.AwayYellows <- Map.empty
        state.AwaySubsUsed <- 0
        state.AwayInstructions <- awayInstructions |> Option.orElse (Some defaultInstr)
        state.HomeActiveRuns <- []
        state.HomeChemistry <- ChemistryGraph.init hCount
        state.HomeEmergentState <- EmergentState.initial
        state.HomeAdaptiveState <- AdaptiveTactics.initial
        state.HomeLastCognitiveSubTick <- 0
        state.HomeLastShapeSubTick <- 0
        state.HomeLastMarkingSubTick <- 0
        state.HomeLastAdaptiveSubTick <- 0
        state.AwayActiveRuns <- []
        state.AwayChemistry <- ChemistryGraph.init aCount
        state.AwayEmergentState <- EmergentState.initial
        state.AwayAdaptiveState <- AdaptiveTactics.initial
        state.AwayLastCognitiveSubTick <- 0
        state.AwayLastShapeSubTick <- 0
        state.AwayLastMarkingSubTick <- 0
        state.AwayLastAdaptiveSubTick <- 0

        ctx, state

    let private simulatePenaltyShootout (ctx: MatchContext) (state: SimState) (players: Map<PlayerId, Player>) =
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



                resolvePenalty (FullTimeSubTick + kickNum) ctx state homeKicker HomeClub kickNum
                |> ignore

                let homeScored = state.HomeScore > state.HomeScore


                let prevAwayScore = state.AwayScore

                resolvePenalty (FullTimeSubTick + kickNum) ctx state awayKicker AwayClub kickNum
                |> ignore

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

            let baseSubTick = secondsToSubTicks (95.0 * 60.0)

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

            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            return ctx, state, homeSquad, awaySquad
        }

    let trySimulateMatch home away players staff : Result<int * int * MatchEvent list * SimState, SimulationError> =
        result {
            let! ctx, state, homeSquad, awaySquad = setup home away players staff false
            let final, events = runLoopFastDes home.Id homeSquad awaySquad ctx state
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull home away players staff : Result<MatchReplay, SimulationError> =
        result {
            let! ctx, state, homeSquad, awaySquad = setup home away players staff false
            let final, events, snapshots = runLoopFullDes home.Id homeSquad awaySquad ctx state

            return
                { Context = ctx
                  Final = final
                  Events = events
                  Snapshots = snapshots }
        }

    let trySimulateMatchKnockout home away players staff : Result<MatchReplay * bool, SimulationError> =
        result {
            let! ctx, state, homeSquad, awaySquad = setup home away players staff true
            let final, events, snapshots = runLoopFullDes home.Id homeSquad awaySquad ctx state

            let replay =
                { Context = ctx
                  Final = final
                  Events = events
                  Snapshots = snapshots }

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootout = simulatePenaltyShootout ctx replay.Final players
                return shootout, true
            else
                return replay, false
        }

    let buildInitState home away players staff =
        match setup home away players staff false with
        | Ok(ctx, state, homeSquad, awaySquad) -> Some(ctx, state, homeSquad, awaySquad)
        | Error _ -> None
