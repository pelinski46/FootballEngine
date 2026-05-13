namespace FootballEngine

open FootballEngine.Domain

open FootballEngine.Types
open FootballEngine.Types.InfluenceTypes
open FootballEngine.Types.PhysicsContract


type FrameWrite =

    | SetPosition of slotIdx: int * x: float<meter> * y: float<meter>
    | SetVelocity of slotIdx: int * vx: float<meter / second> * vy: float<meter / second>
    | SetCondition of slotIdx: int * value: float32

    | SetIntent of slotIdx: int * kind: IntentKind * tx: float32 * ty: float32 * pid: int
    | CommitIntent of slotIdx: int * until: int * trigger: byte

    | SetSlotRole of slotIdx: int * role: SlotRole
    | SetCollectiveIntent of slotIdx: int * intent: CollectiveIntent
    | SetSupportPos of slotIdx: int * x: float32 * y: float32
    | SetDefensiveRole of slotIdx: int * role: DefensiveRole

    | SetMentalState of
        slotIdx: int *
        composure: float *
        confidence: float *
        aggression: float *
        focus: float *
        riskTolerance: float

and SystemOutput =

    | HomeFrame of FrameWrite
    | AwayFrame of FrameWrite

    | BallUpdate of BallPhysicsState
    | FlowChange of MatchFlow
    | ScoreGoal of club: ClubSide * scorerId: PlayerId option * isOwnGoal: bool

    | EmergentUpdate of club: ClubSide * state: EmergentState
    | AdaptiveUpdate of club: ClubSide * state: AdaptiveState
    | DirectiveUpdate of club: ClubSide * directive: TeamDirectiveState
    | MemoryWrite of club: ClubSide * slotIdx: int * write: MemoryWrite

    | RegisterRun of club: ClubSide * run: RunAssignment
    | ExpireRun of club: ClubSide * playerId: PlayerId

    | Emit of MatchEvent
    | EmitSemantic of SemanticEvent

    | PossessionHistoryUpdate of PossessionHistoryDelta

    | InfluenceFrameUpdate of club: ClubSide * frame: InfluenceFrame
    | CognitiveFrameUpdate of club: ClubSide * frame: CognitiveFrame

    | BallXSmoothUpdate of value: float<meter>

    | MomentumUpdate of delta: float

    | StoppageTimeAdd of subTick: int * reason: StoppageReason
    | SidelinedWrite of club: ClubSide * playerId: PlayerId * status: PlayerOut
    | YellowsWrite of club: ClubSide * playerId: PlayerId * count: int
    | LastAttackingClubSet of club: ClubSide
    | ScoreGoalAdjust of club: ClubSide * delta: int
    | MatchStatIncrement of club: ClubSide * field: StatField * delta: int

and StatField = | PassAttempts

and MemoryWrite =
    | PassFailure
    | PassSuccess
    | DuelResult of won: bool * opponentSlot: int

and [<Struct>] PossessionHistoryDelta =
    { PossessionChanged: bool
      BallInFlight: bool
      SetPieceAwarded: bool
      ReceivedByPlayer: PlayerId option }
