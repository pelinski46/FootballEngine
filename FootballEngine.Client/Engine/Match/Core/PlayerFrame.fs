namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract


type DefensiveRole =
    | FirstDefender = 0uy
    | Cover = 1uy
    | Marker = 2uy

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

type OccupancyKind =
    | Active of int
    | Sidelined of PlayerOut

type HomeSlotIndex = HomeSlotIndex of int
type AwaySlotIndex = AwaySlotIndex of int

module SlotIndex =
    let home i = HomeSlotIndex i
    let away i = AwaySlotIndex i
    let unboxHome (HomeSlotIndex i) = i
    let unboxAway (AwaySlotIndex i) = i

[<Struct>]
type MentalState =
    { ComposureLevel: float
      ConfidenceLevel: float
      FocusLevel: float
      AggressionLevel: float
      RiskTolerance: float }

module MentalState =
    let initial (player: Player) =
        { ComposureLevel = normaliseAttr player.Mental.Composure
          ConfidenceLevel = normaliseCondition player.Morale
          FocusLevel = normaliseAttr player.Mental.Concentration
          AggressionLevel = normaliseAttr player.Mental.Aggression
          RiskTolerance = 0.5 }

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

type PhysicsFrame =
    { PosX: float32[]
      PosY: float32[]
      VelX: float32[]
      VelY: float32[]
      Occupancy: OccupancyKind[]
      SlotCount: int
      ActiveCount: int }

module PhysicsFrame =
    let init (n: int) (basePositions: Spatial[]) : PhysicsFrame =
        { PosX = Array.init n (fun i -> float32 basePositions[i].X)
          PosY = Array.init n (fun i -> float32 basePositions[i].Y)
          VelX = Array.zeroCreate n
          VelY = Array.zeroCreate n
          Occupancy = Array.init n (fun i -> OccupancyKind.Active i)
          SlotCount = n
          ActiveCount = n }

type IntentDataFrame =
    { Kind: IntentKind[]
      TargetX: float32[]
      TargetY: float32[]
      TargetPid: int[]
      Phase: byte[]
      CommittedUntil: int[]
      CommittedAt: int[]
      ExitTrigger: byte[] }

module IntentFrameOps =
    let init (n: int) : IntentDataFrame =
        { Kind = Array.create n IntentKind.Idle
          TargetX = Array.zeroCreate n
          TargetY = Array.zeroCreate n
          TargetPid = Array.zeroCreate n
          Phase = Array.create n IntentPhaseTypes.Deciding
          CommittedUntil = Array.zeroCreate n
          CommittedAt = Array.zeroCreate n
          ExitTrigger = Array.create n (byte IntentPhaseTypes.ExitTrigger.None) }

type CognitiveOutputFrame =
    { ShapeTargetX: float32[]
      ShapeTargetY: float32[]
      SupportPosX: float32[]
      SupportPosY: float32[]
      DefensiveRole: byte[] }

module CognitiveOutputFrame =
    let init (n: int) : CognitiveOutputFrame =
        { ShapeTargetX = Array.zeroCreate n
          ShapeTargetY = Array.zeroCreate n
          SupportPosX = Array.zeroCreate n
          SupportPosY = Array.zeroCreate n
          DefensiveRole = Array.create n (byte DefensiveRole.Marker) }

type ConditionFrame =
    { Condition: byte[]
      CachedTargetX: float32[]
      CachedTargetY: float32[]
      CachedExecution: float32[] }

module ConditionFrame =
    let init (n: int) (roster: PlayerRoster) (basePositions: Spatial[]) : ConditionFrame =
        { Condition = Array.init n (fun i -> byte roster.Players[i].Condition)
          CachedTargetX = Array.init n (fun i -> float32 basePositions[i].X)
          CachedTargetY = Array.init n (fun i -> float32 basePositions[i].Y)
          CachedExecution = Array.create n 1.0f }

type MentalFrame =
    { Composure: float32[]
      Confidence: float32[]
      Focus: float32[]
      Aggression: float32[]
      RiskTolerance: float32[] }

module MentalFrame =
    let init (n: int) (roster: PlayerRoster) : MentalFrame =
        { Composure = Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ComposureLevel)
          Confidence = Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ConfidenceLevel)
          Focus = Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).FocusLevel)
          Aggression = Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).AggressionLevel)
          RiskTolerance = Array.create n 0.5f }

type TeamFrame() =
    member val Physics: PhysicsFrame = Unchecked.defaultof<_> with get, set
    member val Intent: IntentDataFrame = Unchecked.defaultof<_> with get, set
    member val Cognitive: CognitiveOutputFrame = Unchecked.defaultof<_> with get, set
    member val Cond: ConditionFrame = Unchecked.defaultof<_> with get, set
    member val Mental: MentalFrame = Unchecked.defaultof<_> with get, set

    member this.SlotCount = this.Physics.SlotCount

    member val Condition: byte[] = Array.empty with get, set
    member val SupportPositionX: float32[] = Array.empty with get, set
    member val SupportPositionY: float32[] = Array.empty with get, set
    member val DefensiveRole: byte[] = Array.empty with get, set
    member val CachedTargetX: float32[] = Array.empty with get, set
    member val CachedTargetY: float32[] = Array.empty with get, set
    member val CachedExecution: float32[] = Array.empty with get, set
    member val ComposureLevel: float32[] = Array.empty with get, set
    member val ConfidenceLevel: float32[] = Array.empty with get, set
    member val AggressionLevel: float32[] = Array.empty with get, set
    member val FocusLevel: float32[] = Array.empty with get, set
    member val RiskTolerance: float32[] = Array.empty with get, set

module TeamFrame =
    let init (roster: PlayerRoster) (basePositions: Spatial[]) : TeamFrame =
        let n = roster.SlotCount
        let frame = TeamFrame()
        frame.Physics <- PhysicsFrame.init n basePositions
        frame.Intent <- IntentFrameOps.init n
        frame.Cognitive <- CognitiveOutputFrame.init n
        frame.Cond <- ConditionFrame.init n roster basePositions
        frame.Mental <- MentalFrame.init n roster
        frame.Condition <- Array.init n (fun i -> byte roster.Players[i].Condition)
        frame.SupportPositionX <- Array.zeroCreate n
        frame.SupportPositionY <- Array.zeroCreate n
        frame.DefensiveRole <- Array.create n (byte DefensiveRole.Marker)
        frame.CachedTargetX <- Array.init n (fun i -> float32 basePositions[i].X)
        frame.CachedTargetY <- Array.init n (fun i -> float32 basePositions[i].Y)
        frame.CachedExecution <- Array.create n 1.0f
        frame.ComposureLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ComposureLevel)
        frame.ConfidenceLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).ConfidenceLevel)
        frame.AggressionLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).AggressionLevel)
        frame.FocusLevel <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).FocusLevel)
        frame.RiskTolerance <- Array.init n (fun i -> float32 (MentalState.initial roster.Players[i]).RiskTolerance)
        frame
