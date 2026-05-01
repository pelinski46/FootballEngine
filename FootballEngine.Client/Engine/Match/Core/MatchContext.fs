namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Movement

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
    member val TransitionPressExpiry: int = 0 with get, set

    member val Directive: TeamDirectiveState =
        TeamDirectiveState.Active(FootballEngine.Movement.TeamDirectiveOps.empty 0) with get, set


module TeamSimState =
    let empty () =
        let ts = TeamSimState()
        ts.Frame <- TeamFrame()
        ts

type BuildUpSide =
    | LeftFlank
    | RightFlank
    | Central
    | Balanced

type SubstitutionRequest =
    { ClubId: ClubId
      OutPlayerId: PlayerId
      InPlayerId: PlayerId
      RequestedSubTick: int
      CommandId: int64 option }

type PenaltyShootout =
    { HomeKicks: (PlayerId * bool * int) list
      AwayKicks: (PlayerId * bool * int) list
      CurrentKick: int
      IsComplete: bool }

type SimState() =
    member val SubTick = 0 with get, set
    member val HomeScore = 0 with get, set
    member val AwayScore = 0 with get, set

    member val Flow: MatchFlow =
        RestartDelay
            { Kind = SetPieceKind.KickOff
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

    member val Ball: BallPhysicsState =
        { Position =
            { X = 52.5<meter>
              Y = 34.0<meter>
              Z = 0.0<meter>
              Vx = 0.0<meter / second>
              Vy = 0.0<meter / second>
              Vz = 0.0<meter / second> }
          Spin = Spin.zero
          LastTouchBy = None
          Possession = Possession.SetPiece(HomeClub, SetPieceKind.KickOff)
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
        | Possession.SetPiece(club, _) -> Some club
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
    member val PossessionHistory: PossessionHistory = PossessionHistory.empty with get, set

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
