namespace FootballEngine.Types

open FootballEngine.Domain
open FootballEngine.Types.PhysicsContract

type ActionContext =
    { Att: TeamPerspective
      Def: TeamPerspective
      Zone: PitchZone
      Momentum: float }

[<Struct>]
type AgentContext =
    { MeIdx: int
      Me: Player
      Profile: BehavioralProfile
      MentalState: MentalState
      MyCondition: int
      MyPos: Spatial
      BallState: BallPhysicsState
      Team: TeamPerspective
      TeamHasBall: bool
      Phase: MatchPhase
      Zone: PitchZone
      NearestTeammateIdx: int voption
      NearestOpponentIdx: int voption
      BestPassTargetIdx: int voption
      BestPassTargetPos: Spatial voption
      BallCarrierOppIdx: int16
      DistToGoal: float<meter>
      GoalDiff: int
      Minute: int
      Urgency: float
      Tactics: TacticsConfig
      Decision: DecisionConfig
      BuildUp: BuildUpConfig
      Dribble: DribbleConfig
      DirectiveKind: DirectiveKind
      DirectiveParams: DirectiveParams
      TargetRunner: PlayerId option
      RunType: RunType option
      RunTarget: Spatial option
      PreviousIntent: MovementIntent voption
      VisibilityMask: VisibilityMask voption
      CurrentSubTick: int
      TransitionPressExpiry: int
      Influence: InfluenceTypes.InfluenceFrame }
