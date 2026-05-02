namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Domain.TacticalInstructions

open FootballEngine.PhysicsContract
open FootballEngine.Movement
open FootballEngine.SimulationClock

module SimStateOps =

    [<Struct>]
    type TacticsConfig =
        { PressureDistance: float
          UrgencyMultiplier: float
          ForwardPush: float
          DefensiveDrop: float
          PressingIntensity: float
          Width: float
          Tempo: float
          Directness: float
          PressTriggerZone: PitchZone
          DefensiveShape: float }

    let private baseTacticsConfig =
        function
        | TeamTactics.Balanced ->
            { PressureDistance = 0.0
              UrgencyMultiplier = 1.0
              ForwardPush = 0.0
              DefensiveDrop = 0.0
              PressingIntensity = 1.0
              Width = 0.5
              Tempo = 0.5
              Directness = 0.5
              PressTriggerZone = MidfieldZone
              DefensiveShape = 0.5 }
        | TeamTactics.Attacking ->
            { PressureDistance = 8.0
              UrgencyMultiplier = 1.15
              ForwardPush = 10.0
              DefensiveDrop = -5.0
              PressingIntensity = 1.2
              Width = 0.6
              Tempo = 0.6
              Directness = 0.4
              PressTriggerZone = AttackingZone
              DefensiveShape = 0.4 }
        | TeamTactics.Defensive ->
            { PressureDistance = -10.0
              UrgencyMultiplier = 0.9
              ForwardPush = -5.0
              DefensiveDrop = 8.0
              PressingIntensity = 0.7
              Width = 0.4
              Tempo = 0.3
              Directness = 0.6
              PressTriggerZone = DefensiveZone
              DefensiveShape = 0.6 }
        | TeamTactics.Pressing ->
            { PressureDistance = 12.0
              UrgencyMultiplier = 1.1
              ForwardPush = 8.0
              DefensiveDrop = -3.0
              PressingIntensity = 1.5
              Width = 0.5
              Tempo = 0.7
              Directness = 0.5
              PressTriggerZone = AttackingZone
              DefensiveShape = 0.5 }
        | TeamTactics.Counter ->
            { PressureDistance = -6.0
              UrgencyMultiplier = 1.2
              ForwardPush = -8.0
              DefensiveDrop = 6.0
              PressingIntensity = 0.8
              Width = 0.3
              Tempo = 0.8
              Directness = 0.8
              PressTriggerZone = MidfieldZone
              DefensiveShape = 0.4 }

    let defaultPressParams (tactics: TacticsConfig) (emergent: EmergentState) =
        { Intensity = tactics.PressingIntensity * emergent.PressingIntensity
          TriggerZone = tactics.PressTriggerZone
          MinPresserCount = int (3.0 + tactics.PressingIntensity * 3.0) }

    let defaultShapeParams (tactics: TacticsConfig) (emergent: EmergentState) =
        { Width = tactics.Width * 0.5 + emergent.WingPlayPreference * 0.5
          DefensiveLineHeight = tactics.DefensiveDrop
          Compactness = emergent.CompactnessLevel }

    let defaultTransitionParams (tactics: TacticsConfig) (emergent: EmergentState) =
        { Tempo = tactics.Tempo * 0.6 + emergent.TempoLevel * 0.4
          DirectnessThreshold = tactics.Directness
          CounterTrigger = false
          WingBias       = 0.0
          DirectnessBias = 0.0 }

    let defaultParams (tactics: TacticsConfig) (emergent: EmergentState) : DirectiveParams =
        { Press = defaultPressParams tactics emergent
          Shape = defaultShapeParams tactics emergent
          Transition = defaultTransitionParams tactics emergent }

    let ofBallX (x: float<meter>) (dir: AttackDir) : PitchZone =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> PitchLength - x

        if effectiveX < 30.0<meter> then DefensiveZone
        elif effectiveX <= 70.0<meter> then MidfieldZone
        else AttackingZone

    let tacticsConfig (teamTactics: TeamTactics) (instructions: TacticalInstructions option) =
        let baseCfg = baseTacticsConfig teamTactics
        let instr = instructions |> Option.defaultValue defaultInstructions
        let mentalityMod = float (instr.Mentality - 2) * 0.08
        let defensiveLineMod = float (instr.DefensiveLine - 2) * 3.0
        let pressingMod = float (instr.PressingIntensity - 2) * 0.15

        let pressTriggerZone =
            match instr.PressTriggerZone with
            | 0 -> DefensiveZone
            | 2 -> AttackingZone
            | _ -> MidfieldZone

        { PressureDistance = baseCfg.PressureDistance + defensiveLineMod
          UrgencyMultiplier = baseCfg.UrgencyMultiplier * (1.0 + mentalityMod)
          ForwardPush = baseCfg.ForwardPush + mentalityMod * 5.0 + defensiveLineMod * 0.5
          DefensiveDrop = baseCfg.DefensiveDrop - mentalityMod * 5.0 - defensiveLineMod * 0.5
          PressingIntensity = baseCfg.PressingIntensity * (1.0 + pressingMod)
          Width = baseCfg.Width * 0.5 + float instr.Width / 4.0 * 0.5
          Tempo = baseCfg.Tempo * 0.5 + float instr.Tempo / 4.0 * 0.5
          Directness = baseCfg.Directness * 0.5 + float instr.Directness / 4.0 * 0.5
          PressTriggerZone = pressTriggerZone
          DefensiveShape = baseCfg.DefensiveShape * 0.5 + float instr.DefensiveShape / 4.0 * 0.5 }

    let defaultSpatial (x: float<meter>) (y: float<meter>) : Spatial =
        { X = x
          Y = y
          Z = 0.0<meter>
          Vx = 0.0<meter / second>
          Vy = 0.0<meter / second>
          Vz = 0.0<meter / second> }

    let kickOffSpatial = defaultSpatial HalfwayLineX (PitchWidth / 2.0)

    let getTeam (state: SimState) (side: ClubSide) =
        if side = HomeClub then state.Home else state.Away

    let getTeamByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        if clubId = ctx.Home.Id then state.Home else state.Away

    let getRoster (ctx: MatchContext) (side: ClubSide) : PlayerRoster =
        if side = HomeClub then ctx.HomeRoster else ctx.AwayRoster

    let findIdxByPid (pid: PlayerId) (frame: TeamFrame) (roster: PlayerRoster) : int voption =
        let mutable bestIdx = ValueNone

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active rosterIdx when roster.Players[rosterIdx].Id = pid -> bestIdx <- ValueSome i
            | _ -> ()

        bestIdx

    let tryGetPlayerFromFrame (frame: TeamFrame) (roster: PlayerRoster) (idx: int) : Player option =
        match frame.Physics.Occupancy[idx] with
        | OccupancyKind.Active rosterIdx -> Some roster.Players[rosterIdx]
        | _ -> None

    let tryFindPlayerByPidInFrame (frame: TeamFrame) (roster: PlayerRoster) (pid: PlayerId) : Player option =
        match findIdxByPid pid frame roster with
        | ValueSome idx -> tryGetPlayerFromFrame frame roster idx
        | ValueNone -> None

    let playerOnSide (ctx: MatchContext) (state: SimState) (side: ClubSide) (pid: PlayerId) : bool =
        let frame = (getTeam state side).Frame
        let roster = getRoster ctx side
        findIdxByPid pid frame roster |> ValueOption.isSome

    let updateTeamByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (f: TeamSimState -> TeamSimState) =
        if clubId = ctx.Home.Id then
            state.Home <- f state.Home
        else
            state.Away <- f state.Away

    let setTeam (state: SimState) (side: ClubSide) (team: TeamSimState) =
        if side = HomeClub then
            state.Home <- team
        else
            state.Away <- team

    let updateTeam (state: SimState) (side: ClubSide) (f: TeamSimState -> TeamSimState) =
        if side = HomeClub then
            state.Home <- f state.Home
        else
            state.Away <- f state.Away

    let getSidelined (state: SimState) (side: ClubSide) = (getTeam state side).Sidelined

    let setSidelined (state: SimState) (side: ClubSide) (m: Map<PlayerId, PlayerOut>) =
        (getTeam state side).Sidelined <- m

    let getYellows (state: SimState) (side: ClubSide) = (getTeam state side).Yellows

    let setYellows (state: SimState) (side: ClubSide) (m: Map<PlayerId, int>) = (getTeam state side).Yellows <- m

    let getSubsUsed (state: SimState) (side: ClubSide) = (getTeam state side).SubsUsed

    let setSubsUsed (state: SimState) (side: ClubSide) (n: int) = (getTeam state side).SubsUsed <- n

    let getTactics (state: SimState) (side: ClubSide) = (getTeam state side).Tactics

    let setTactics (state: SimState) (side: ClubSide) (tac: TeamTactics) = (getTeam state side).Tactics <- tac

    let getInstructions (state: SimState) (side: ClubSide) = (getTeam state side).Instructions

    let setInstructions (state: SimState) (side: ClubSide) (i: TacticalInstructions option) =
        (getTeam state side).Instructions <- i

    let getBasePositions (state: SimState) (side: ClubSide) =
        if side = HomeClub then
            state.HomeBasePositions
        else
            state.AwayBasePositions

    let getActiveRuns (state: SimState) (side: ClubSide) = (getTeam state side).ActiveRuns

    let setActiveRuns (state: SimState) (side: ClubSide) (runs: RunAssignment list) =
        (getTeam state side).ActiveRuns <- runs

    let getChemistry (ctx: MatchContext) (side: ClubSide) =
        if side = HomeClub then
            ctx.HomeChemistry
        else
            ctx.AwayChemistry

    let getEmergentState (state: SimState) (side: ClubSide) = (getTeam state side).EmergentState

    let setEmergentState (state: SimState) (side: ClubSide) (s: EmergentState) = (getTeam state side).EmergentState <- s

    let getAdaptiveState (state: SimState) (side: ClubSide) = (getTeam state side).AdaptiveState

    let setAdaptiveState (state: SimState) (side: ClubSide) (s: AdaptiveState) = (getTeam state side).AdaptiveState <- s

    let getMatchStats (state: SimState) (side: ClubSide) = (getTeam state side).MatchStats

    let setMatchStats (state: SimState) (side: ClubSide) (s: MatchStats) = (getTeam state side).MatchStats <- s

    let updateMatchStats (state: SimState) (side: ClubSide) (f: MatchStats -> MatchStats) =
        let team = getTeam state side
        team.MatchStats <- f team.MatchStats

    let resetAdaptiveStats (state: SimState) (side: ClubSide) =
        (getTeam state side).MatchStats <- MatchStats.empty

    let getTacticsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Tactics

    let getInstructionsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Instructions

    let setTacticsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (tac: TeamTactics) =
        (getTeamByClubId clubId ctx state).Tactics <- tac

    let getSidelinedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Sidelined

    let setSidelinedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (m: Map<PlayerId, PlayerOut>) =
        (getTeamByClubId clubId ctx state).Sidelined <- m

    let getSubsUsedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).SubsUsed

    let setSubsUsedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (n: int) =
        (getTeamByClubId clubId ctx state).SubsUsed <- n

    let activePlayersFromFrame (frame: TeamFrame) (roster: PlayerRoster) : Player[] =
        Array.init frame.SlotCount (fun i ->
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active rosterIdx -> roster.Players[rosterIdx]
            | _ -> roster.Players[0])

    let defaultBall =
        { Position = kickOffSpatial
          Spin = Spin.zero
          Control = Free
          LastTouchBy = None
          PendingOffsideSnapshot = None
          StationarySinceSubTick = None
          GKHoldSinceSubTick = None
          PlayerHoldSinceSubTick = None
          Trajectory = None }

    let resetBallForKickOff (receivingClub: ClubSide) (state: SimState) =
        state.LastAttackingClub <- receivingClub

        state.Ball <-
            { state.Ball with
                Position = kickOffSpatial
                Spin = Spin.zero
                Control = Free
                PendingOffsideSnapshot = None
                StationarySinceSubTick = None
                GKHoldSinceSubTick = None
                PlayerHoldSinceSubTick = None
                Trajectory = None }

    let clearOffsideSnapshot (state: SimState) =
        state.Ball <-
            { state.Ball with
                PendingOffsideSnapshot = None }

    let hasPossession (state: SimState) (pid: PlayerId) : bool =
        match state.Ball.Control with
        | Controlled(_, p) | Receiving(_, p, _) -> p = pid
        | _ -> false

    let losePossession (state: SimState) =
        match state.Ball.Control with
        | Controlled(side, _) | Receiving(side, _, _) -> (getTeam state side).ActiveRuns <- []
        | _ -> ()

        state.Ball <-
            { state.Ball with
                Control = Free
                PendingOffsideSnapshot = None
                GKHoldSinceSubTick = None
                PlayerHoldSinceSubTick = None
                Trajectory = None }

    let givePossessionTo
        (club: ClubSide)
        (pid: PlayerId)
        (isGk: bool)
        (subTick: int)
        (ballBase: BallPhysicsState)
        (state: SimState)
        =
        match state.Ball.Control with
        | Controlled(losingClub, _) | Receiving(losingClub, _, _) when losingClub <> club ->
            (getTeam state losingClub).ActiveRuns <- []
        | _ -> ()

        state.LastAttackingClub <- club

        state.Ball <-
            { ballBase with
                Control =
                    if isGk then Controlled(club, pid)
                    else Receiving(club, pid, subTick)
                LastTouchBy              = Some pid
                PendingOffsideSnapshot   = None
                GKHoldSinceSubTick       = if isGk then Some subTick else None
                PlayerHoldSinceSubTick   = if isGk then None else Some subTick
                Trajectory               = None
                Position                 = ballBase.Position }

    let adjustMomentum (dir: AttackDir) (delta: float) (state: SimState) =
        state.Momentum <- clampFloat (state.Momentum + momentumDelta dir delta) -10.0 10.0

    let goalDiff (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        if clubId = ctx.Home.Id then
            state.HomeScore - state.AwayScore
        else
            state.AwayScore - state.HomeScore

    let pressureMultiplier (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        1.1 - float (max -2 (min 2 (goalDiff clubId ctx state))) * 0.25

    let matchUrgency (clubId: ClubId) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : float =
        let elapsedSeconds = subTicksToSeconds clock state.SubTick
        let late = elapsedSeconds > 60.0 * 60.0

        let ts =
            tacticsConfig (getTacticsByClubId clubId ctx state) (getInstructionsByClubId clubId ctx state)

        match goalDiff clubId ctx state, late with
        | d, true when d < 0 -> 1.35 * ts.UrgencyMultiplier
        | d, false when d < 0 -> 1.15 * ts.UrgencyMultiplier
        | d, _ when d > 0 -> 0.85 * ts.UrgencyMultiplier
        | _, true -> 1.10 * ts.UrgencyMultiplier
        | _ -> 1.00 * ts.UrgencyMultiplier

    let phaseFromBallZone (dir: AttackDir) (x: float<meter>) =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> PitchLength - x

        let third = PitchLength / 3.0

        if effectiveX < third then BuildUp
        elif effectiveX < third * 2.0 then Midfield
        else Attack

    let attackDirFor (clubSide: ClubSide) (state: SimState) =
        match clubSide with
        | HomeClub -> state.HomeAttackDir
        | AwayClub ->
            match state.HomeAttackDir with
            | LeftToRight -> RightToLeft
            | RightToLeft -> LeftToRight

    let currentPhase (state: SimState) =
        let dir = attackDirFor state.AttackingSide state
        phaseFromBallZone dir state.Ball.Position.X

    let clubSideOf (ctx: MatchContext) (state: SimState) (pid: PlayerId) : ClubSide option =
        let homeFrame = state.Home.Frame
        let homeRoster = getRoster ctx HomeClub
        let mutable found = false

        for i = 0 to homeFrame.SlotCount - 1 do
            match homeFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active rosterIdx when homeRoster.Players[rosterIdx].Id = pid -> found <- true
            | _ -> ()

        if found then
            Some HomeClub
        else
            let awayFrame = state.Away.Frame
            let awayRoster = getRoster ctx AwayClub

            for i = 0 to awayFrame.SlotCount - 1 do
                match awayFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active rosterIdx when awayRoster.Players[rosterIdx].Id = pid -> found <- true
                | _ -> ()

            if found then Some AwayClub else None

    let findActivePlayer (ctx: MatchContext) (state: SimState) (pid: PlayerId) : Player option =
        match clubSideOf ctx state pid with
        | Some HomeClub -> tryFindPlayerByPidInFrame state.Home.Frame (getRoster ctx HomeClub) pid
        | Some AwayClub -> tryFindPlayerByPidInFrame state.Away.Frame (getRoster ctx AwayClub) pid
        | None -> None

    let activeRunsFilter currentSubTick (runs: RunAssignment list) =
        runs |> List.filter (RunAssignment.isActive currentSubTick)

    let createEvent subTick playerId clubId t : MatchEvent =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t
          Context = EventContext.empty }

    let createEventAt subTick playerId clubId t (pos: Spatial) : MatchEvent =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t
          Context = EventContext.at (float pos.X) (float pos.Y) }

    let buildTeamPerspective (clubSide: ClubSide) (ctx: MatchContext) (state: SimState) : TeamPerspective =
        let clubId = if clubSide = HomeClub then ctx.Home.Id else ctx.Away.Id
        let dir = attackDirFor clubSide state
        let bonus = HomeBonus.build clubSide state.Config.HomeAdvantage
        let ownFrame = (getTeam state clubSide).Frame
        let oppFrame = (getTeam state (ClubSide.flip clubSide)).Frame
        let ownRoster = getRoster ctx clubSide
        let oppRoster = getRoster ctx (ClubSide.flip clubSide)

        { ClubSide = clubSide
          ClubId = clubId
          AttackDir = dir
          OwnFrame = ownFrame
          OppFrame = oppFrame
          OwnRoster = ownRoster
          OppRoster = oppRoster
          Bonus = bonus }

    let getFrame (state: SimState) (side: ClubSide) : TeamFrame = (getTeam state side).Frame

    let nearestActiveSlotInFrame (frame: TeamFrame) (x: float<meter>) (y: float<meter>) : int voption =
        let mutable bestIdx = ValueNone
        let mutable bestDistSq = System.Single.MaxValue
        let x32 = float32 x
        let y32 = float32 y

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let dx = frame.Physics.PosX[i] - x32
                let dy = frame.Physics.PosY[i] - y32
                let d = dx * dx + dy * dy

                if d < bestDistSq then
                    bestDistSq <- d
                    bestIdx <- ValueSome i
            | _ -> ()

        bestIdx

    let getCognitiveFrame (state: SimState) (side: ClubSide) : CognitiveFrame option =
        let cf =
            if side = HomeClub then
                state.HomeCognitiveFrame
            else
                state.AwayCognitiveFrame

        if cf.SlotCount > 0 then Some cf else None

    let setCognitiveFrame (state: SimState) (side: ClubSide) (cf: CognitiveFrame) =
        if side = HomeClub then
            state.HomeCognitiveFrame <- cf
        else
            state.AwayCognitiveFrame <- cf

    let getDirective (state: SimState) (side: ClubSide) : FootballEngine.Movement.TeamDirectiveState =
        (getTeam state side).Directive

    let setDirective (state: SimState) (side: ClubSide) (d: FootballEngine.Movement.TeamDirectiveState) =
        (getTeam state side).Directive <- d

    let suspendDirective (state: SimState) (side: ClubSide) =
        let team = getTeam state side
        team.Directive <- FootballEngine.Movement.TeamDirectiveOps.suspend team.Directive

    let resumeDirective (state: SimState) (side: ClubSide) =
        let team = getTeam state side
        team.Directive <- FootballEngine.Movement.TeamDirectiveOps.resume team.Directive

    let expireReceiving (subTick: int) (graceTicks: int) (state: SimState) =
        match state.Ball.Control with
        | Receiving(club, pid, since) when subTick - since >= graceTicks ->
            state.Ball <- { state.Ball with Control = Controlled(club, pid) }
        | _ -> ()
