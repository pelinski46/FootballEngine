namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Movement.TeamDirectiveOps
open FootballEngine.PhysicsContract
open FootballEngine.Movement
open SimStateOps

module BatchDecisionSupport =

    let computeSupportPositions
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballControl: BallControl)
        (phase: MatchPhase)
        (tactics: TacticsConfig)
        (desiredWidth: float)
        (basePositions: Spatial[])
        : Spatial[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let result = Array.zeroCreate<Spatial> n

        let ballCarrierId =
            match ballControl with
            | Controlled(_, pid)
            | Receiving(_, pid, _) -> Some pid
            | _ -> None

        for i = 0 to n - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let px = basePositions[i].X
                let py = basePositions[i].Y

                let isCarrier =
                    match ballCarrierId with
                    | Some pid -> team.OwnRoster.Players[i].Id = pid
                    | None -> false

                if isCarrier then
                    result[i] <- defaultSpatial px py
                else
                    let forwardX = if team.AttackDir = LeftToRight then 1.0 else -1.0
                    let baseX = px + 8.0<meter> * forwardX
                    let centerY = 34.0<meter>
                    let widthOffset = (py - centerY) * desiredWidth
                    let targetX = clamp baseX 2.0<meter> 98.0<meter>
                    let targetY = clamp (centerY + widthOffset) 2.0<meter> 98.0<meter>
                    result[i] <- defaultSpatial targetX targetY
            | _ -> result[i] <- defaultSpatial 52.5<meter> 34.0<meter>

        result

    let pickRunner
        (team: TeamPerspective)
        (ballPos: Spatial)
        (ballControl: BallControl)
        (emergent: EmergentState)
        (influence: InfluenceTypes.InfluenceFrame)
        : PlayerId option * RunType option * Spatial option =
        let frame = team.OwnFrame
        let roster = team.OwnRoster

        let ballCarrierId =
            match ballControl with
            | Controlled(_, pid)
            | Receiving(_, pid, _) -> Some pid
            | _ -> None

        let dir = team.AttackDir
        let forwardX = if dir = LeftToRight then 1.0 else -1.0

        let mutable bestCellIdx = 0
        let mutable bestCellScore = -1.0

        for cell = 0 to InfluenceTypes.GridSize - 1 do
            let s =
                InfluenceTypes.scoreCellRaw cell influence (float32 ballPos.X) (float32 ballPos.Y) dir

            if s > bestCellScore then
                bestCellScore <- s
                bestCellIdx <- cell

        let bestCellTarget =
            let cx, cy = InfluenceTypes.cellToCenter bestCellIdx

            defaultSpatial
                (clamp (float cx * 1.0<meter>) 2.0<meter> 98.0<meter>)
                (clamp (float cy * 1.0<meter>) 2.0<meter> 98.0<meter>)

        let mutable bestPlayerId: PlayerId option = None
        let mutable bestRunType: RunType option = None
        let mutable bestScore = -1.0

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = roster.Players[i]

                if ballCarrierId <> Some player.Id then
                    let px = float frame.Physics.PosX[i] * 1.0<meter>
                    let isAdvanced = (px - 52.5<meter>) * forwardX > 5.0<meter>
                    let workRate = float player.Mental.WorkRate / 20.0
                    let positioning = float player.Mental.Positioning / 20.0
                    let score = workRate * 0.3 + positioning * 0.4 + (if isAdvanced then 0.5 else 0.0)

                    if score > bestScore then
                        bestScore <- score
                        bestPlayerId <- Some player.Id

                        bestRunType <-
                            Some(
                                match player.Position with
                                | ST
                                | AMC -> DeepRun
                                | AML
                                | ML
                                | WBL -> DriftWide
                                | AMR
                                | MR
                                | WBR -> DriftWide
                                | MC -> CheckToBall
                                | DL
                                | DR -> OverlapRun
                                | _ -> DeepRun
                            )
            | _ -> ()

        bestPlayerId, bestRunType, Some bestCellTarget

    let computeDefensiveShape
        (team: TeamPerspective)
        (cFrame: CognitiveFrame)
        (currentSubTick: int)
        (transitionExpiry: int)
        : DefensiveRole[] =
        let frame = team.OwnFrame
        let n = frame.SlotCount
        let assignments = Array.create n DefensiveRole.Marker

        if cFrame.BallCarrierOppIdx < 0s then
            assignments
        else
            let bcIdx = int cFrame.BallCarrierOppIdx
            let bcX32 = team.OppFrame.Physics.PosX[bcIdx]
            let bcY32 = team.OppFrame.Physics.PosY[bcIdx]

            let mutable nearestIdx = -1
            let mutable nearestDistSq = System.Single.MaxValue

            for i = 0 to n - 1 do
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let dx = frame.Physics.PosX[i] - bcX32
                    let dy = frame.Physics.PosY[i] - bcY32
                    let dSq = dx * dx + dy * dy

                    if dSq < nearestDistSq then
                        nearestDistSq <- dSq
                        nearestIdx <- i
                | _ -> ()

            if nearestIdx >= 0 then
                assignments[nearestIdx] <- DefensiveRole.FirstDefender
                let maxCover = if currentSubTick < transitionExpiry then 2 else 3
                let mutable coverCount = 0

                for i = 0 to n - 1 do
                    if i <> nearestIdx && coverCount < maxCover then
                        match frame.Physics.Occupancy[i] with
                        | OccupancyKind.Active _ ->
                            if team.OwnRoster.Players[i].Position <> GK then
                                assignments[i] <- DefensiveRole.Cover
                                coverCount <- coverCount + 1
                        | _ -> ()

            assignments


// ============================================================
// BatchDecision
//
// Single pipeline per team per tick:
//   1. resolveDirective  — qué está haciendo el equipo AHORA
//   2. assignSlotRoles   — qué hace cada slot según el directive
//   3. per-player loop   — intent sesgado por su rol, no por magic multipliers
//
// Reemplaza el processTeam anterior que mezclaba las tres fases
// y decidía el pressing con `score * 1.5`.
// ============================================================

// ── Rol individual derivado del DirectiveKind ───────────────

type SlotRole =
    | PressFirst // primer presionador, máxima urgencia
    | PressSupport // cierra línea de pase, no persigue
    | HoldShape // mantiene posición táctica
    | AnchorDefense // no sube bajo ningún concepto
    | SupportBuild // ofrece opción de pase, corto
    | MakeRunForward // corre al espacio en profundidad
    | FreeRole // perfil propio del jugador manda

module SlotRole =

    // Escala cuánto sube/baja el score de PressBall según el rol.
    // La decisión final sigue siendo del softmax en PlayerDecision —
    // esto solo inclina la balanza, no la fuerza.
    let pressBallBias (role: SlotRole) : float =
        match role with
        | PressFirst -> 1.8
        | PressSupport -> 0.6
        | HoldShape -> 0.2
        | AnchorDefense -> 0.0
        | SupportBuild -> 0.3
        | MakeRunForward -> 0.1
        | FreeRole -> 1.0

    let supportAttackBias (role: SlotRole) : float =
        match role with
        | MakeRunForward -> 1.6
        | SupportBuild -> 1.3
        | FreeRole -> 1.0
        | PressFirst -> 0.2
        | PressSupport -> 0.3
        | HoldShape -> 0.7
        | AnchorDefense -> 0.0

    let coverSpaceBias (role: SlotRole) : float =
        match role with
        | AnchorDefense -> 1.5
        | HoldShape -> 1.2
        | PressSupport -> 1.0
        | FreeRole -> 1.0
        | SupportBuild -> 0.8
        | PressFirst -> 0.4
        | MakeRunForward -> 0.2

// ── Paso 1: resolución dinámica del DirectiveKind ───────────

module DirectiveResolver =

    // Cuántos ticks hay que sostener un directive antes de poder cambiarlo.
    // Evita la oscilación frame a frame que hacía parecer bots a los equipos.
    [<Literal>]
    let private MinCommitmentTicks = 90 // ~3s a 30 subticks/s

    type private Situation =
        | JustWonBall
        | JustLostBall
        | OpponentBuilding
        | Dominating
        | Chasing
        | Balanced

    let private classifySituation (state: SimState) (clubSide: ClubSide) (pressingCoordination: float) : Situation =
        let scoreDiff =
            if clubSide = HomeClub then
                state.HomeScore - state.AwayScore
            else
                state.AwayScore - state.HomeScore

        let justChanged = state.PossessionHistory.LastChangeTick > state.SubTick - 30

        let weHaveBall =
            match state.Ball.Control with
            | Controlled(side, _)
            | Receiving(side, _, _) -> side = clubSide
            | _ -> false

        if justChanged && weHaveBall then JustWonBall
        elif justChanged && not weHaveBall then JustLostBall
        elif not weHaveBall && scoreDiff < 0 then Chasing
        elif weHaveBall && scoreDiff > 0 then Dominating
        elif not weHaveBall then OpponentBuilding
        else Balanced

    // Función pura: situación + tácticas del manager → DirectiveKind.
    // El manager sigue mandando su intención (Attacking, Pressing, etc.)
    // pero la situación puede sobreescribirla puntualmente.
    //
    // pressingCoordination viene de ChemistryTracker.calculateCohesion,
    // calculado desde ctx.HomeChemistry / ctx.AwayChemistry.
    let resolve
        (state: SimState)
        (clubSide: ClubSide)
        (pressingCoordination: float)
        (currentDirective: TeamDirective)
        : DirectiveKind =
        let situation = classifySituation state clubSide pressingCoordination

        // Si el directive actual es reciente, no cambiar todavía
        let isCommitted = state.SubTick - currentDirective.ActiveSince < MinCommitmentTicks

        if isCommitted then
            currentDirective.Kind
        else
            let managerIntent = kindFromTactics (getTactics state clubSide)

            match situation with
            | JustWonBall -> CounterReady
            | JustLostBall -> PressingBlock
            | Chasing -> DirectAttack
            | Dominating ->
                match managerIntent with
                | DefensiveBlock -> DefensiveBlock
                | _ -> Structured
            | OpponentBuilding ->
                if pressingCoordination > 0.65 then
                    PressingBlock
                else
                    DefensiveBlock
            | Balanced -> managerIntent

// ── Paso 2: asignación de roles por slot ────────────────────

module SlotRoleAssigner =

    // Cuántos presionadores hay que enviar según la intensidad configurada.
    let private pressCount (tacticsCfg: TacticsConfig) : int =
        int (2.0 + tacticsCfg.PressingIntensity * 3.0) |> max 2 |> min 5

    // Distancia al balón de cada slot activo, para elegir presionadores.
    let private ballDistances (frame: TeamFrame) (bx: float32) (by: float32) : float32[] =
        Array.init frame.SlotCount (fun i ->
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let dx = frame.Physics.PosX[i] - bx
                let dy = frame.Physics.PosY[i] - by
                dx * dx + dy * dy
            | _ -> System.Single.MaxValue)

    let assign
        (frame: TeamFrame)
        (roster: PlayerRoster)
        (kind: DirectiveKind)
        (tacticsCfg: TacticsConfig)
        (bx: float32)
        (by: float32)
        : SlotRole[] =
        let n = frame.SlotCount
        let roles = Array.create n FreeRole

        match kind with

        | PressingBlock ->
            let dists = ballDistances frame bx by
            let nPress = pressCount tacticsCfg

            // Índices activos ordenados por distancia al balón
            let sortedByDist =
                Array.init n id
                |> Array.filter (fun i ->
                    match frame.Physics.Occupancy[i] with
                    | OccupancyKind.Active _ -> true
                    | _ -> false)
                |> Array.sortBy (fun i -> dists[i])

            for rank = 0 to sortedByDist.Length - 1 do
                let i = sortedByDist[rank]
                let pos = roster.Players[i].Position

                roles[i] <-
                    if pos = GK then AnchorDefense
                    elif pos = DC && rank > nPress then HoldShape
                    elif rank = 0 then PressFirst
                    elif rank < nPress then PressSupport
                    else HoldShape

        | CounterReady ->
            for i = 0 to n - 1 do
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let pos = roster.Players[i].Position

                    roles[i] <-
                        match pos with
                        | GK
                        | DC -> AnchorDefense
                        | DL
                        | DR
                        | WBL
                        | WBR -> HoldShape
                        | DM -> SupportBuild
                        | MC -> SupportBuild
                        | ML
                        | MR
                        | AMC
                        | AML
                        | AMR -> MakeRunForward
                        | ST -> MakeRunForward
                | _ -> ()

        | DirectAttack ->
            for i = 0 to n - 1 do
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let pos = roster.Players[i].Position

                    roles[i] <-
                        match pos with
                        | GK
                        | DC -> AnchorDefense
                        | DL
                        | DR
                        | WBL
                        | WBR -> SupportBuild
                        | DM
                        | MC -> SupportBuild
                        | _ -> MakeRunForward
                | _ -> ()

        | DefensiveBlock ->
            for i = 0 to n - 1 do
                match frame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let pos = roster.Players[i].Position

                    roles[i] <-
                        match pos with
                        | GK -> AnchorDefense
                        | DC
                        | DL
                        | DR -> AnchorDefense
                        | WBL
                        | WBR -> HoldShape
                        | DM -> HoldShape
                        | _ -> HoldShape
                | _ -> ()

        | Structured ->
            // FreeRole por defecto: el perfil del jugador manda
            ()

        roles

// ── Paso 3: ajuste de scores por rol (reemplaza magic multipliers) ──

module RoleBiasedScores =

    let apply (role: SlotRole) (scores: MovementScores) : MovementScores =
        { scores with
            PressBall = scores.PressBall * SlotRole.pressBallBias role
            SupportAttack = scores.SupportAttack * SlotRole.supportAttackBias role
            CoverSpace = scores.CoverSpace * SlotRole.coverSpaceBias role }

// ── Pipeline principal ───────────────────────────────────────

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

        // ── 1. Resolver DirectiveKind dinámicamente ──────────
        let currentDirective =
            SimStateOps.getDirective state clubSide
            |> TeamDirectiveOps.currentDirective
            |> Option.defaultValue (TeamDirectiveOps.empty currentSubTick)

        let chemistry =
            if clubSide = HomeClub then
                ctx.HomeChemistry
            else
                ctx.AwayChemistry

        let cohesion =
            ChemistryTracker.calculateCohesion chemistry.Familiarity roster.Players.Length

        let resolvedKind =
            DirectiveResolver.resolve state clubSide cohesion.PressingCoordination currentDirective

        // ── 2. Computar shape, support positions, runner ─────
        let shapeTargetX = Array.zeroCreate<float32> frame.SlotCount
        let shapeTargetY = Array.zeroCreate<float32> frame.SlotCount
        ShapeEngine.computeShapeTargets basePositions dir phase ballXSmooth tacticsCfg shapeTargetX shapeTargetY

        let ballPos = state.Ball.Position

        let supportPositions =
            BatchDecisionSupport.computeSupportPositions
                team
                ballPos
                state.Ball.Control
                phase
                tacticsCfg
                tacticsCfg.Width
                basePositions

        let influence =
            if clubSide = HomeClub then
                state.HomeInfluenceFrame
            else
                state.AwayInfluenceFrame

        let runnerId, runType, runTarget =
            BatchDecisionSupport.pickRunner team ballPos state.Ball.Control emergent influence

        let defAssignments =
            BatchDecisionSupport.computeDefensiveShape
                team
                cFrame
                currentSubTick
                (getTeam state clubSide).TransitionPressExpiry

        // Escribir support positions y defensive roles
        for i = 0 to frame.SlotCount - 1 do
            if i < supportPositions.Length then
                frame.SupportPositionX[i] <- float32 supportPositions[i].X
                frame.SupportPositionY[i] <- float32 supportPositions[i].Y

            if i < defAssignments.Length then
                frame.DefensiveRole[i] <- byte defAssignments[i]

        // Actualizar el directive en el estado con el kind resuelto
        let newDirective =
            { currentDirective with
                Kind = resolvedKind
                Params = SimStateOps.defaultParams tacticsCfg emergent
                TargetRunner = runnerId
                RunType = runType
                RunTarget = runTarget
                ActiveSince =
                    if resolvedKind <> currentDirective.Kind then
                        currentSubTick
                    else
                        currentDirective.ActiveSince }

        setDirective state clubSide (TeamDirectiveState.Active newDirective)
        SimStateOps.setCognitiveFrame state clubSide cFrame

        // ── 3. Asignar roles por slot ─────────────────────────
        let bx = float32 ballPos.X
        let by = float32 ballPos.Y
        let slotRoles = SlotRoleAssigner.assign frame roster resolvedKind tacticsCfg bx by

        // ── 4. Set piece override ─────────────────────────────
        let isSetPiece =
            match state.Flow with
            | RestartDelay _ -> true
            | _ -> false

        let setPiecePositions =
            if isSetPiece then
                SetPiecePositioning.computePositions ctx state clubSide
            else
                Array.empty

        if isSetPiece then
            SetPiecePositioning.assignPositions ctx setPiecePositions clubSide

        let possessionHistory = state.PossessionHistory

        // ── 5. Loop por jugador: intent sesgado por rol ───────
        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Sidelined _ -> ()
            | OccupancyKind.Active rosterIdx ->

                if not (IntentPhase.shouldRecalculate frame.Intent i currentSubTick possessionHistory) then
                    ()
                else
                    let player = roster.Players[rosterIdx]
                    let profile = roster.Profiles[rosterIdx]

                    let previousIntent =
                        match frame.Intent.Kind[i] with
                        | IntentKind.Idle -> ValueNone
                        | kind ->
                            let tx = float frame.Intent.TargetX[i] * 1.0<meter>
                            let ty = float frame.Intent.TargetY[i] * 1.0<meter>

                            ValueSome(
                                IntentFrame.toMovementIntent
                                    kind
                                    frame.Intent.TargetX[i]
                                    frame.Intent.TargetY[i]
                                    frame.Intent.TargetPid[i]
                                    (SimStateOps.defaultSpatial tx ty)
                            )

                    let myX = float frame.Physics.PosX[i] * 1.0<meter>
                    let myY = float frame.Physics.PosY[i] * 1.0<meter>
                    let myVx = float frame.Physics.VelX[i] * 1.0<meter / second>
                    let myVy = float frame.Physics.VelY[i] * 1.0<meter / second>

                    let visibilityMask =
                        if isSetPiece then
                            ValueNone
                        else
                            ValueSome(
                                Perception.computeVisibilityMask
                                    i
                                    { X = myX
                                      Y = myY
                                      Z = 0.0<meter>
                                      Vx = myVx
                                      Vy = myVy
                                      Vz = 0.0<meter / second> }
                                    myVx
                                    myVy
                                    frame.Intent.Kind[i]
                                    frame.Intent.TargetX[i]
                                    frame.Intent.TargetY[i]
                                    player.Mental.Vision
                                    player.Mental.Positioning
                                    (player.Position = GK)
                                    state.Ball.Position
                                    frame
                                    team.OppFrame
                                    ctx.Config.Perception
                            )

                    let actx =
                        AgentContext.build
                            player
                            profile
                            i
                            team
                            previousIntent
                            state
                            clock
                            ctx
                            state.Config.Decision
                            state.Config.BuildUp
                            (Some cFrame)
                            visibilityMask
                            influence

                    let rawScores = MovementScorer.computeAll actx emergent
                    // Aquí está el cambio clave: rol sesga, no magic multipliers
                    let biasedScores = RoleBiasedScores.apply slotRoles[i] rawScores

                    let intent = MovementScorer.pickIntent currentSubTick biasedScores actx

                    let myPos = SimStateOps.defaultSpatial myX myY

                    let mutable finalIntent =
                        match intent with
                        | MaintainShape _ ->
                            let tx = float shapeTargetX[i] * 1.0<meter>
                            let ty = float shapeTargetY[i] * 1.0<meter>
                            MaintainShape(SimStateOps.defaultSpatial tx ty)
                        | other -> other

                    if isSetPiece then
                        let pos =
                            if i < setPiecePositions.Length then
                                setPiecePositions[i]
                            else
                                myPos

                        finalIntent <- MoveToSetPiecePos pos

                    let kind, tx, ty, tpid = IntentFrame.fromMovementIntent finalIntent
                    FrameMutate.setIntent frame.Intent i kind tx ty tpid

                    let dur, trigger = IntentPhase.duration clock finalIntent
                    FrameMutate.commitIntent frame.Intent i currentSubTick dur trigger

                    match finalIntent with
                    | ExecuteRun run ->
                        let teamState = SimStateOps.getTeam state clubSide

                        teamState.ActiveRuns <-
                            run
                            :: (teamState.ActiveRuns |> List.filter (fun r -> r.PlayerId <> run.PlayerId))
                    | _ -> ()
