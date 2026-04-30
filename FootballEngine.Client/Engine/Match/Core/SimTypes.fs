namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Domain.TacticalInstructions
open SimulationClock


type RunType =
    | DeepRun
    | OverlapRun
    | UnderlapRun
    | DiagonalRun
    | CheckToBall
    | DriftWide
    | ThirdManRun
    | FalseNineDrop
    | WingBackSurge

type RunTrigger =
    | TeammateHasBall
    | TeammateStartedDribble
    | SpaceDetected
    | TacticalInstruction
    | SetPieceRoutine
    | CounterAttack

type RunTrajectory =
    | Linear of float<meter> * float<meter> * float<meter> * float<meter>
    | Waypoints of (float<meter> * float<meter>)[]


type RunAssignment =
    { PlayerId: PlayerId
      RunType: RunType
      Trigger: RunTrigger
      Trajectory: RunTrajectory
      StartSubTick: int
      DurationSubTicks: int
      Intensity: float
      Priority: int }

module RunAssignment =
    let isActive currentSubTick (r: RunAssignment) =
        currentSubTick >= r.StartSubTick
        && currentSubTick < r.StartSubTick + r.DurationSubTicks

    let progress currentSubTick (r: RunAssignment) =
        if not (isActive currentSubTick r) then
            0.0
        else
            min 1.0 (float (currentSubTick - r.StartSubTick) / float r.DurationSubTicks)

    let evaluateTrajectory t trajectory =
        match trajectory with
        | Linear(sx, sy, ex, ey) -> (sx + (ex - sx) * t, sy + (ey - sy) * t)
        | Waypoints pts ->
            if Array.isEmpty pts then
                (52.5<meter>, 34.0<meter>)
            elif pts.Length = 1 then
                pts[0]
            else
                let st = t * float (pts.Length - 1)
                let idx = int st
                let frac = st - float idx
                let i = min idx (pts.Length - 2)
                let ax, ay = pts[i]
                let bx, by = pts[i + 1]
                (ax + (bx - ax) * frac, ay + (by - ay) * frac)

    let create
        (runType: RunType)
        (startX: float<meter>)
        (startY: float<meter>)
        (targetX: float<meter>)
        (targetY: float<meter>)
        (playerId: PlayerId)
        (currentSubTick: int)
        (durationSubTicks: int)
        : RunAssignment =
        let trajectory =
            match runType with
            | CheckToBall ->
                Waypoints
                    [| startX, startY
                       (startX + targetX) / 2.0, (startY + targetY) / 2.0
                       targetX, targetY |]
            | _ -> Linear(startX, startY, targetX, targetY)

        { PlayerId = playerId
          RunType = runType
          Trigger = TeammateHasBall
          Trajectory = trajectory
          StartSubTick = currentSubTick
          DurationSubTicks = durationSubTicks
          Intensity = 0.8
          Priority = 1 }

[<Struct>]
type DefensiveRole =
    | FirstDefender
    | Cover
    | Marker

[<Struct>]
type MentalState =
    { ComposureLevel: float
      ConfidenceLevel: float
      FocusLevel: float
      AggressionLevel: float
      RiskTolerance: float }

module MentalState =
    let initial (player: Player) =
        { ComposureLevel = PhysicsContract.normaliseAttr player.Mental.Composure
          ConfidenceLevel = PhysicsContract.normaliseCondition player.Morale
          FocusLevel = PhysicsContract.normaliseAttr player.Mental.Concentration
          AggressionLevel = PhysicsContract.normaliseAttr player.Mental.Aggression
          RiskTolerance = 0.5 }

type ChemistryGraph =
    { Familiarity: float[,]
      Leadership: float[]
      PlayerCount: int }

module ChemistryGraph =
    let init playerCount =
        { Familiarity = Array2D.create playerCount playerCount 0.5
          Leadership = Array.zeroCreate playerCount
          PlayerCount = playerCount }

type AttackPattern =
    | LeftFlank
    | RightFlank
    | Central
    | LongBall
    | ShortPass

type PatternResult =
    | SuccessfulXG of float
    | LostPossession
    | StillInProgress

type PatternRecord =
    { Pattern: AttackPattern
      Attempts: int
      Successes: int
      TotalXG: float }

type AdaptiveState = { Records: PatternRecord[] }

module AdaptiveTactics =
    let initial =
        { Records =
            [| { Pattern = LeftFlank
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = RightFlank
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = Central
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = LongBall
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = ShortPass
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 } |] }

[<Struct>]
type EmergentState =
    { CompactnessLevel: float
      PressingIntensity: float
      WingPlayPreference: float
      TempoLevel: float
      RiskAppetite: float }

module EmergentState =
    let initial =
        { CompactnessLevel = 0.5
          PressingIntensity = 0.5
          WingPlayPreference = 0.5
          TempoLevel = 0.5
          RiskAppetite = 0.5 }

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

[<Struct>]
type Spatial =
    { X: float<meter>
      Y: float<meter>
      Z: float<meter>
      Vx: float<meter / second>
      Vy: float<meter / second>
      Vz: float<meter / second> }

    member this.XY = this.X, this.Y
    member this.XYZ = this.X, this.Y, this.Z
    member this.Vel = this.Vx, this.Vy, this.Vz

    member this.DistSqTo(other: Spatial) =
        let dx = this.X - other.X
        let dy = this.Y - other.Y
        let dz = this.Z - other.Z
        dx * dx + dy * dy + dz * dz

    member this.DistTo(other: Spatial) = sqrt (this.DistSqTo(other))

    member this.DistSqTo2D(other: Spatial) =
        let dx = this.X - other.X
        let dy = this.Y - other.Y
        dx * dx + dy * dy

    member this.DistTo2D(other: Spatial) = sqrt (this.DistSqTo2D(other))
    member this.VelMagSq = this.Vx * this.Vx + this.Vy * this.Vy + this.Vz * this.Vz
    member this.VelMag = sqrt this.VelMagSq

type MovementIntent =
    | MaintainShape of target: Spatial
    | MarkMan of targetPlayerId: PlayerId * targetPos: Spatial
    | PressBall of ballPredPos: Spatial
    | ExecuteRun of assignment: RunAssignment
    | CoverSpace of target: Spatial
    | SupportAttack of target: Spatial
    | RecoverBall of ballPredPos: Spatial
    | MoveToSetPiecePos of target: Spatial

// ============================================================
// FASE 1: Phantom types para índices de slot
// ============================================================

type HomeSlotIndex = HomeSlotIndex of int
type AwaySlotIndex = AwaySlotIndex of int

module SlotIndex =
    let home i = HomeSlotIndex i
    let away i = AwaySlotIndex i
    let unboxHome (HomeSlotIndex i) = i
    let unboxAway (AwaySlotIndex i) = i

// ============================================================
// FASE 1: OccupancyKind — reemplaza PlayerSlot en el hot path
// ============================================================

type OccupancyKind =
    | Active of int
    | Sidelined of PlayerOut


type IntentKind =
    | Idle = 0uy
    | MaintainShape = 1uy
    | MarkMan = 2uy
    | PressBall = 3uy
    | ExecuteRun = 4uy
    | CoverSpace = 5uy
    | SupportAttack = 6uy
    | RecoverBall = 7uy
    | MoveToSetPiecePos = 8uy


type TeamFrame() =
    member val Occupancy: OccupancyKind[] = Array.empty with get, set
    member val PosX: float32[] = Array.empty with get, set
    member val PosY: float32[] = Array.empty with get, set
    member val VelX: float32[] = Array.empty with get, set
    member val VelY: float32[] = Array.empty with get, set
    member val Condition: byte[] = Array.empty with get, set
    member val IntentKind: IntentKind[] = Array.empty with get, set
    member val IntentTargetX: float32[] = Array.empty with get, set
    member val IntentTargetY: float32[] = Array.empty with get, set
    member val IntentTargetPid: int[] = Array.empty with get, set
    member val IntentLockExpiry: int[] = Array.empty with get, set
    member val CachedTargetX: float32[] = Array.empty with get, set
    member val CachedTargetY: float32[] = Array.empty with get, set
    member val CachedExecution: float32[] = Array.empty with get, set
    member val ComposureLevel: float32[] = Array.empty with get, set
    member val ConfidenceLevel: float32[] = Array.empty with get, set
    member val AggressionLevel: float32[] = Array.empty with get, set
    member val FocusLevel: float32[] = Array.empty with get, set
    member val RiskTolerance: float32[] = Array.empty with get, set
    member val LastCognitiveSubTick: int = 0 with get, set
    member val LastShapeSubTick: int = 0 with get, set
    member val LastMarkingSubTick: int = 0 with get, set
    member val LastAdaptiveSubTick: int = 0 with get, set
    member val ActiveCount: int = 0 with get, set
    member val SlotCount: int = 0 with get, set

// ============================================================
// FASE 1: PlayerRoster — datos inmutables del equipo
// ============================================================

type PlayerRoster =
    { Players: Player[]
      Profiles: BehavioralProfile[]
      SlotCount: int }

module PlayerRoster =
    let build (players: Player[]) : PlayerRoster =
        let profiles = players |> Array.map Player.profile

        { Players = players
          Profiles = profiles
          SlotCount = players.Length }

module TeamFrame =
    let init (roster: PlayerRoster) (basePositions: Spatial[]) : TeamFrame =
        let n = roster.SlotCount
        let frame = TeamFrame()
        frame.Occupancy <- Array.init n (fun i -> OccupancyKind.Active i)
        frame.PosX <- Array.init n (fun i -> float32 basePositions[i].X)
        frame.PosY <- Array.init n (fun i -> float32 basePositions[i].Y)
        frame.VelX <- Array.zeroCreate n
        frame.VelY <- Array.zeroCreate n
        frame.Condition <- Array.init n (fun i -> byte roster.Players[i].Condition)
        frame.IntentKind <- Array.create n IntentKind.Idle
        frame.IntentTargetX <- Array.zeroCreate n
        frame.IntentTargetY <- Array.zeroCreate n
        frame.IntentTargetPid <- Array.zeroCreate n
        frame.IntentLockExpiry <- Array.zeroCreate n
        frame.CachedTargetX <- Array.init n (fun i -> float32 basePositions[i].X)
        frame.CachedTargetY <- Array.init n (fun i -> float32 basePositions[i].Y)
        frame.CachedExecution <- Array.create n 1.0f
        frame.ComposureLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ComposureLevel)
        frame.ConfidenceLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ConfidenceLevel)
        frame.AggressionLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).AggressionLevel)
        frame.FocusLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).FocusLevel)
        frame.RiskTolerance <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).RiskTolerance)
        frame.LastCognitiveSubTick <- 0
        frame.LastShapeSubTick <- 0
        frame.LastMarkingSubTick <- 0
        frame.LastAdaptiveSubTick <- 0
        frame.ActiveCount <- n
        frame.SlotCount <- n
        frame



[<Struct>]
type Spin =
    { Top: float<radianPerSecond>
      Side: float<radianPerSecond> }

module Spin =
    let zero =
        { Top = 0.0<radianPerSecond>
          Side = 0.0<radianPerSecond> }

[<Struct>]
type SetPieceKind =
    | KickOff
    | ThrowIn
    | Corner
    | GoalKick
    | FreeKick
    | Penalty

type RestartCause =
    | AfterGoal
    | AfterFoul
    | AfterBallOut
    | AfterInjury
    | AfterVAR
    | InitialKickOff

type RestartPlan =
    { Kind: SetPieceKind
      Team: ClubSide
      Cause: RestartCause
      RemainingTicks: int }

type GoalPauseState =
    { ScoringTeam: ClubSide
      ScorerId: PlayerId option
      IsOwnGoal: bool
      RemainingTicks: int
      VARRequested: bool }

type InjuryPauseState =
    { PlayerId: PlayerId
      Team: ClubSide
      Severity: int
      RemainingTicks: int
      CanContinue: bool option }

type VARPhase =
    | CheckingIncident
    | ReviewingAngles
    | RefereeToMonitor
    | DecisionReady

type VARFlowState =
    { Incident: VARReviewableIncident
      Phase: VARPhase
      RemainingTicks: int
      TotalTicks: int }

type MatchFlow =
    | Live
    | GoalPause of GoalPauseState
    | VARReview of VARFlowState
    | InjuryPause of InjuryPauseState
    | RestartDelay of RestartPlan
    | HalfTimePause of remainingTicks: int
    | FullTimeReview
    | MatchEnded

type Possession =
    | Loose
    | Owned of ClubSide * PlayerId
    | InFlight
    | Contest of ClubSide
    | Transition of ClubSide
    | SetPiece of ClubSide * SetPieceKind

type ArrivalWinner =
    | IntendedTarget of player: Player * club: ClubSide
    | Intercepted of player: Player * club: ClubSide
    | Contested
    | NoOneInRange

type ArrivalContext =
    { BallPos: Spatial
      TargetId: PlayerId
      Quality: float
      HomeFrame: TeamFrame
      AwayFrame: TeamFrame
      HomeRoster: PlayerRoster
      AwayRoster: PlayerRoster
      PhysicsCfg: PhysicsConfig }

type BallActionKind =
    | Pass of passerId: PlayerId * targetId: PlayerId * quality: float
    | Shot of shooterId: PlayerId * quality: float
    | Cross of crosserId: PlayerId * targetId: PlayerId * quality: float
    | LongBall of passerId: PlayerId * targetId: PlayerId * quality: float
    | Clearance of playerId: PlayerId
    | Deflection of playerId: PlayerId
    | FreeBall

type OffsideSnapshot =
    { PasserId: PlayerId
      ReceiverId: PlayerId
      ReceiverXAtPass: float<meter>
      SecondLastDefenderX: float<meter>
      BallXAtPass: float<meter>
      Dir: AttackDir }

type BallTrajectory =
    { OriginX: float<meter>
      OriginY: float<meter>
      TargetX: float<meter>
      TargetY: float<meter>
      LaunchSubTick: int
      EstimatedArrivalSubTick: int
      KickerId: PlayerId
      PeakHeight: float<meter>
      ActionKind: BallActionKind }

type BallPhysicsState =
    { Position: Spatial
      Spin: Spin
      Possession: Possession
      LastTouchBy: PlayerId option
      PendingOffsideSnapshot: OffsideSnapshot option
      StationarySinceSubTick: int option
      GKHoldSinceSubTick: int option
      PlayerHoldSinceSubTick: int option
      Trajectory: BallTrajectory option }

type PenaltyShootout =
    { HomeKicks: (PlayerId * bool * int) list
      AwayKicks: (PlayerId * bool * int) list
      CurrentKick: int
      IsComplete: bool }

type MatchContext =
    { Home: Club
      Away: Club
      HomeCoach: Staff
      AwayCoach: Staff
      HomePlayers: Player[]
      AwayPlayers: Player[]
      HomeBasePositions: Spatial[]
      AwayBasePositions: Spatial[]
      HomeChemistry: ChemistryGraph
      AwayChemistry: ChemistryGraph
      IsKnockoutMatch: bool
      Config: BalanceConfig
      HomeRoster: PlayerRoster
      AwayRoster: PlayerRoster }

type MatchStats =
    { PassAttempts: int
      PassSuccesses: int
      PressAttempts: int
      PressSuccesses: int
      FlankAttempts: int
      FlankSuccesses: int }

module MatchStats =
    let empty =
        { PassAttempts = 0
          PassSuccesses = 0
          PressAttempts = 0
          PressSuccesses = 0
          FlankAttempts = 0
          FlankSuccesses = 0 }

type TeamSimState() =
    member val Frame: TeamFrame = TeamFrame() with get, set
    member val Sidelined: Map<PlayerId, PlayerOut> = Map.empty with get, set
    member val Yellows: Map<PlayerId, int> = Map.empty with get, set
    member val SubsUsed: int = 0 with get, set
    member val Tactics: TeamTactics = TeamTactics.Balanced with get, set
    member val Instructions: TacticalInstructions option = None with get, set
    member val ActiveRuns: RunAssignment list = [] with get, set
    member val EmergentState: EmergentState = EmergentState.initial with get, set
    member val AdaptiveState: AdaptiveState = AdaptiveTactics.initial with get, set
    member val MatchStats: MatchStats = MatchStats.empty with get, set
    member val LastCognitiveSubTick: int = 0 with get, set
    member val LastShapeSubTick: int = 0 with get, set
    member val LastMarkingSubTick: int = 0 with get, set
    member val LastAdaptiveSubTick: int = 0 with get, set
    member val TransitionPressExpiry: int = 0 with get, set

module TeamSimState =
    let empty () =
        let ts = TeamSimState()
        ts.Frame <- TeamFrame()
        ts

[<Struct>]
type CognitiveFrame =
    { NearestTeammateIdx: int16[]
      NearestTeammateDistSq: float32[]
      NearestOpponentIdx: int16[]
      NearestOpponentDistSq: float32[]
      BestPassTargetIdx: int16[]
      BestPassTargetPos: Spatial voption[]
      BallX: float32
      BallY: float32
      BallZone: PitchZone
      Phase: MatchPhase
      PressureOnPlayer: float32[]
      SlotCount: int
      BallCarrierOppIdx: int16 }

type BuildUpSide =
    | LeftFlank
    | RightFlank
    | Central
    | Balanced

[<Struct>]
type TeamIntent =
    { BuildUpSide: BuildUpSide
      PressTrigger: bool
      PressTriggerZone: PitchZone option
      TargetRunner: PlayerId option
      RunType: RunType option
      RunTarget: Spatial option
      SupportPositions: Spatial[]
      DesiredWidth: float
      Tempo: float
      DefensiveAssignments: DefensiveRole[] }

module CognitiveFrameDefaults =
    let empty =
        { NearestTeammateIdx = Array.empty
          NearestTeammateDistSq = Array.empty
          NearestOpponentIdx = Array.empty
          NearestOpponentDistSq = Array.empty
          BestPassTargetIdx = Array.empty
          BestPassTargetPos = Array.empty
          BallX = 0.0f
          BallY = 0.0f
          BallZone = MidfieldZone
          Phase = BuildUp
          PressureOnPlayer = Array.empty
          SlotCount = 0
          BallCarrierOppIdx = -1s }

[<Struct>]
type CognitiveFrameBuffers =
    { NearestTeammateIdx: int16[]
      NearestTeammateDistSq: float32[]
      NearestOpponentIdx: int16[]
      NearestOpponentDistSq: float32[]
      BestPassTargetIdx: int16[]
      BestPassTargetPos: Spatial voption[]
      PressureOnPlayer: float32[] }

module CognitiveFrameBuffers =
    let create n =
        { NearestTeammateIdx = Array.zeroCreate<int16> n
          NearestTeammateDistSq = Array.create<float32> n System.Single.MaxValue
          NearestOpponentIdx = Array.zeroCreate<int16> n
          NearestOpponentDistSq = Array.create<float32> n System.Single.MaxValue
          BestPassTargetIdx = Array.create<int16> n -1s
          BestPassTargetPos = Array.create<Spatial voption> n ValueNone
          PressureOnPlayer = Array.create<float32> n System.Single.MaxValue }

[<Struct>]
type VisibilityMask =
    { CanSeeTeammates: bool[]
      CanSeeOpponents: bool[]
      CanSeeBall: bool
      VisibleTeammateCount: int
      VisibleOpponentCount: int }

module VisibilityMask =
    let empty ownCount oppCount =
        { CanSeeTeammates = Array.create ownCount true
          CanSeeOpponents = Array.create oppCount true
          CanSeeBall = true
          VisibleTeammateCount = ownCount
          VisibleOpponentCount = oppCount }

module TeamIntentDefaults =
    let empty =
        { BuildUpSide = Balanced
          PressTrigger = false
          PressTriggerZone = None
          TargetRunner = None
          RunType = None
          RunTarget = None
          SupportPositions = Array.empty
          DesiredWidth = 0.5
          Tempo = 0.5
          DefensiveAssignments = Array.empty }

type SubstitutionRequest =
    { ClubId: ClubId
      OutPlayerId: PlayerId
      InPlayerId: PlayerId
      RequestedSubTick: int
      CommandId: int64 option }

type SimState() =
    member val SubTick = 0 with get, set
    member val HomeScore = 0 with get, set
    member val AwayScore = 0 with get, set

    member val Flow: MatchFlow =
        RestartDelay
            { Kind = KickOff
              Team = HomeClub
              Cause = InitialKickOff
              RemainingTicks = 0 } with get, set

    member val Config = BalanceConfig.defaultConfig with get, set
    member val MatchMemory: MatchMemory = MatchMemory.empty with get, set
    member val MatchEvents: ResizeArray<MatchEvent> = ResizeArray<MatchEvent>(512) with get, set
    member val LastMemoryDecaySubTick = 0 with get, set
    member val HalfTimeHandled = false with get, set
    member val EffectiveFullTimeSubTick = 0 with get, set
    member val FullTimeHandled = false with get, set

    member val Ball =
        { Position =
            { X = 52.5<meter>
              Y = 34.0<meter>
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }
          Spin =
            { Top = 0.0<radianPerSecond>
              Side = 0.0<radianPerSecond> }
          LastTouchBy = None
          Possession = SetPiece(HomeClub, KickOff)
          PendingOffsideSnapshot = None
          StationarySinceSubTick = None
          GKHoldSinceSubTick = None
          PlayerHoldSinceSubTick = None
          Trajectory = None } with get, set

    member val LastAttackingClub = HomeClub with get, set

    member this.AttackingClub =
        match this.Ball.Possession with
        | Owned(club, _) -> Some club
        | InFlight -> None
        | SetPiece(club, _) -> Some club
        | Contest(club) -> Some club
        | Transition(club) -> Some club
        | Loose -> None

    member this.AttackingSide =
        this.AttackingClub |> Option.defaultValue this.LastAttackingClub

    member val HomeAttackDir = LeftToRight with get, set
    member val Momentum = 0.0 with get, set
    member val BallXSmooth = 52.5<meter> with get, set

    member this.PendingOffsideSnapshot = this.Ball.PendingOffsideSnapshot

    member val HomeBasePositions = Array.empty<Spatial> with get, set
    member val AwayBasePositions = Array.empty<Spatial> with get, set
    member val Home = TeamSimState.empty () with get, set
    member val Away = TeamSimState.empty () with get, set

    member val HomeCognitiveFrame = CognitiveFrameDefaults.empty with get, set
    member val AwayCognitiveFrame = CognitiveFrameDefaults.empty with get, set
    member val HomeTeamIntent = TeamIntentDefaults.empty with get, set
    member val AwayTeamIntent = TeamIntentDefaults.empty with get, set

    member val HomeInfluenceFrame = InfluenceTypes.emptyInfluenceFrame () with get, set
    member val AwayInfluenceFrame = InfluenceTypes.emptyInfluenceFrame () with get, set

    member val HomeCFrameBuffers: CognitiveFrameBuffers option = None with get, set
    member val AwayCFrameBuffers: CognitiveFrameBuffers option = None with get, set

    member val StoppageTime: StoppageTimeTracker = StoppageTimeTracker() with get, set
    member val HomePendingSubstitutions: SubstitutionRequest list = [] with get, set
    member val AwayPendingSubstitutions: SubstitutionRequest list = [] with get, set

[<Struct>]
type TeamPerspective =
    { ClubSide: ClubSide
      ClubId: ClubId
      AttackDir: AttackDir
      OwnFrame: TeamFrame
      OppFrame: TeamFrame
      OwnRoster: PlayerRoster
      OppRoster: PlayerRoster
      Bonus: HomeBonus }

type AgentContextParams =
    { MeIdx: int
      Roster: PlayerRoster
      OwnFrame: TeamFrame
      OppFrame: TeamFrame
      State: SimState
      Ctx: MatchContext
      Clock: SimulationClock
      Decision: DecisionConfig
      BuildUp: BuildUpConfig
      PreviousIntent: MovementIntent option
      IntentLockExpiry: int }

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



    let ofBallX (x: float<meter>) (dir: AttackDir) : PitchZone =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> PhysicsContract.PitchLength - x

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

    let kickOffSpatial =
        defaultSpatial PhysicsContract.HalfwayLineX (PhysicsContract.PitchWidth / 2.0)

    let getTeam (state: SimState) (side: ClubSide) =
        if side = HomeClub then state.Home else state.Away

    let getTeamByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        if clubId = ctx.Home.Id then state.Home else state.Away

    let getRoster (ctx: MatchContext) (side: ClubSide) : PlayerRoster =
        if side = HomeClub then ctx.HomeRoster else ctx.AwayRoster

    let findIdxByPid (pid: PlayerId) (frame: TeamFrame) (roster: PlayerRoster) : int voption =
        let mutable bestIdx = ValueNone

        for i = 0 to frame.SlotCount - 1 do
            match frame.Occupancy[i] with
            | Active rosterIdx when roster.Players[rosterIdx].Id = pid -> bestIdx <- ValueSome i
            | _ -> ()

        bestIdx

    let tryGetPlayerFromFrame (frame: TeamFrame) (roster: PlayerRoster) (idx: int) : Player option =
        match frame.Occupancy[idx] with
        | Active rosterIdx -> Some roster.Players[rosterIdx]
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

    let getLastCognitiveSubTick (state: SimState) (side: ClubSide) =
        (getTeam state side).LastCognitiveSubTick

    let setLastCognitiveSubTick (state: SimState) (side: ClubSide) (t: int) =
        (getTeam state side).LastCognitiveSubTick <- t

    let getLastShapeSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastShapeSubTick

    let setLastShapeSubTick (state: SimState) (side: ClubSide) (t: int) =
        (getTeam state side).LastShapeSubTick <- t

    let setCachedTarget (state: SimState) (side: ClubSide) (idx: int) (x: float<meter>) (y: float<meter>) =
        let frame = (getTeam state side).Frame
        frame.CachedTargetX[idx] <- float32 x
        frame.CachedTargetY[idx] <- float32 y

    let getLastMarkingSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastMarkingSubTick

    let setLastMarkingSubTick (state: SimState) (side: ClubSide) (t: int) =
        (getTeam state side).LastMarkingSubTick <- t

    let getLastAdaptiveSubTick (state: SimState) (side: ClubSide) =
        (getTeam state side).LastAdaptiveSubTick

    let setLastAdaptiveSubTick (state: SimState) (side: ClubSide) (t: int) =
        (getTeam state side).LastAdaptiveSubTick <- t

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
            match frame.Occupancy[i] with
            | Active rosterIdx -> roster.Players[rosterIdx]
            | _ -> roster.Players[0])

    let defaultBall =
        { Position = kickOffSpatial
          Spin = Spin.zero
          Possession = SetPiece(HomeClub, KickOff)
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
                Possession = SetPiece(receivingClub, SetPieceKind.KickOff)
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
        match state.Ball.Possession with
        | Owned(_, p) -> p = pid
        | _ -> false

    let losePossession (state: SimState) =
        match state.Ball.Possession with
        | Owned(side, _) -> (getTeam state side).ActiveRuns <- []
        | _ -> ()

        state.Ball <-
            { state.Ball with
                Possession = Loose
                PendingOffsideSnapshot = None
                GKHoldSinceSubTick = None
                PlayerHoldSinceSubTick = None
                Trajectory = None }

    let givePossessionTo (club: ClubSide) (pid: PlayerId) (isGk: bool) (subTick: int) (ballBase: BallPhysicsState) (state: SimState) =
        match state.Ball.Possession with
        | Owned(losingClub, _) when losingClub <> club -> (getTeam state losingClub).ActiveRuns <- []
        | _ -> ()

        state.LastAttackingClub <- club

        state.Ball <-
            { ballBase with
                Possession = Owned(club, pid)
                LastTouchBy = Some pid
                PendingOffsideSnapshot = None
                GKHoldSinceSubTick = if isGk then Some subTick else None
                PlayerHoldSinceSubTick = if isGk then None else Some subTick
                Trajectory = None
                Position =
                    { ballBase.Position with
                        Vx = 0.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = 0.0<meter / second> } }

    let adjustMomentum (dir: AttackDir) (delta: float) (state: SimState) =
        state.Momentum <- PhysicsContract.clampFloat (state.Momentum + momentumDelta dir delta) -10.0 10.0





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
            | RightToLeft -> PhysicsContract.PitchLength - x

        let third = PhysicsContract.PitchLength / 3.0

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
            match homeFrame.Occupancy[i] with
            | Active rosterIdx when homeRoster.Players[rosterIdx].Id = pid -> found <- true
            | _ -> ()

        if found then
            Some HomeClub
        else
            let awayFrame = state.Away.Frame
            let awayRoster = getRoster ctx AwayClub

            for i = 0 to awayFrame.SlotCount - 1 do
                match awayFrame.Occupancy[i] with
                | Active rosterIdx when awayRoster.Players[rosterIdx].Id = pid -> found <- true
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
            match frame.Occupancy[i] with
            | Active _ ->
                let dx = frame.PosX[i] - x32
                let dy = frame.PosY[i] - y32
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

    let getTeamIntent (state: SimState) (side: ClubSide) : TeamIntent option =
        let ti =
            if side = HomeClub then
                state.HomeTeamIntent
            else
                state.AwayTeamIntent

        if ti.SupportPositions.Length > 0 then Some ti else None

    let setTeamIntent (state: SimState) (side: ClubSide) (ti: TeamIntent) =
        if side = HomeClub then
            state.HomeTeamIntent <- ti
        else
            state.AwayTeamIntent <- ti
