namespace FootballEngine

open FootballEngine.PhysicsContract
open FootballEngine.SimulationClock

// ============================================================================
// Balance Config Records
// ============================================================================

type DuelConfig = {
    WinProbabilityBase: float
    RecoverProbabilityBase: float
    MomentumBonus: float
    JitterWin: float
    JitterRecover: float
    JitterKeep: float
    SpeedKeep: float<meter / second>
    SpeedKeepVz: float<meter / second>
    AttackerDribblingWeight: float
    AttackerAgilityWeight: float
    AttackerBalanceWeight: float
    DefenderTacklingWeight: float
    DefenderStrengthWeight: float
    DefenderPositionWeight: float
    FatigueThreshold: int
    FatigueDecay: float
}

type ShotConfig = {
    QualityGate: float
    AngleSpreadBase: float
    VzBase: float<meter / second>
    VzVariance: float
    OnTargetBase: float
    OnTargetMultiplier: float
    NormalisationDistance: float<meter>
    DistanceToGoalMultiplier: float
    FinishingMin: float
    FinishingMax: float
    FinishingBonusST: float
    FinishingBonusAM: float
    FinishingBonusMC: float
    FinishingBonusOther: float
    CondFactorDivisor: float
    ComposureMultiplier: float
    UrgencyMultiplier: float
    BasePowerDivisor: float
    SpinTopMultiplier: float
    PositionDirectnessWeight: float
    PositionDepthWeight: float
    PositionCreativityWeight: float
    DistNormWeight: float
    PositionBonusWeight: float
    HeavyTouchDivisor: float
    HeavyTouchMultiplier: float
    JitterStdDev: float
    GkReflexesStatMult: float
    GkOneOnOneStatMult: float
    SaveDenominatorOffset: float
}

type PassConfig = {
    BaseMean: float
    TechnicalWeight: float
    VisionWeight: float
    SuccessShapeAlpha: float
    SuccessConditionMultiplier: float
    OffsideMomentum: float
    SuccessMomentum: float
    FailMomentum: float
    DeflectBaseRate: float
    MisplacedBaseRate: float
    InterceptBaseRate: float
    InterceptionRadius: float<meter>
    PressureDistance: float<meter>
    DeflectPressureMultiplier: float
    InterceptPaceWeight: float
    InterceptPositioningWeight: float
    ScrambleJitter: float<meter>
    Speed: float<meter / second>
    Vz: float<meter / second>
    InterceptDistFactorWeight: float
    InterceptPositioningContrib: float
    InterceptVisionContrib: float
    CreativityWeight: float
    DirectnessWeight: float
    MeanMin: float
    MeanMax: float
    DefaultNearestDefDist: float<meter>
    DefaultTackling: float
    HeavyTouchDivisor: float
    HeavyTouchMultiplier: float
    JitterStdDev: float
    DeflectedSpeedMult: float
    DeflectedVzMult: float
    InterceptProbMax: float
    MisplacedSpeedMult: float
    LongBallBaseMean: float
    LongBallLongShotsWeight: float
    LongBallPassingWeight: float
    LongBallVisionWeight: float
    LongBallSuccessShapeAlpha: float
    LongBallSuccessConditionMultiplier: float
    LongBallOffsideMomentum: float
    LongBallSuccessMomentum: float
    LongBallFailMomentum: float
    LongBallSpeed: float<meter / second>
    LongBallVz: float<meter / second>
    LongBallDeflectMult: float
    LongBallInterceptMult: float
    LongBallPressureContrib: float
    ForwardDepthThreshold: float
    ForwardCreativityThreshold: float
    LongBallScrambleJitterMult: float
}

type CrossConfig = {
    BaseMean: float
    CrossingWeight: float
    PassingWeight: float
    SuccessShapeAlpha: float
    SuccessConditionMultiplier: float
    FailMomentum: float
    Speed: float<meter / second>
    Vz: float<meter / second>
    AerialThreatThreshold: float
    AttackingDepthThreshold: float
    GkSkillDefault: float
    GkSkillDivisor: float
    SpinTopMult: float
    SpinSideMult: float
    HeaderLogisticSteepness: float
    FallbackSpeed: float<meter / second>
    FallbackVz: float<meter / second>
}

type DribbleConfig = {
    TechnicalWeight: float
    AgilityWeight: float
    BalanceWeight: float
    ForwardDistance: float
    SuccessMomentum: float
    FailMomentum: float
    CrossProbability: float
    PassProbability: float
    ShotProbability: float
}

type TackleConfig = {
    TechnicalWeight: float
    PositioningWeight: float
    StrengthWeight: float
    AggressionWeight: float
    PositioningReduction: float
    FoulShapeBeta: float
    FoulMomentum: float
    SuccessMomentum: float
    FailMomentum: float
}

type SetPieceConfig = {
    FreeKickTargetX: float<meter>
    FreeKickSpeed: float<meter / second>
    FreeKickVz: float<meter / second>
    FreeKickSavePowerThreshold: float
    FreeKickSaveVariance: float
    FreeKickSpinTopMult: float
    FreeKickSpinSideMult: float
    CornerBoxXThreshold: float<meter>
    CornerDefenderBoxThreshold: float<meter>
    CornerSecondPhaseProbability: float
    CornerKeepPossessionProbability: float
    CornerSpeed: float<meter / second>
    CornerVz: float<meter / second>
    CornerDensityBase: float
    CornerDensityPenalty: float
    CornerLogisticSteepness: float
    CornerDefScoreDefault: float
    ThrowInSpeed: float<meter / second>
    ThrowInVz: float<meter / second>
    ThrowInMomentum: float
    PenaltySkillMultiplier: float
    PenaltyMoraleMultiplier: float
    PenaltyPressureMultiplier: float
    PenaltyComposureNoise: float
    PenaltyLogisticBase: float
    PenaltyGkReflexesMult: float
    PenaltyGkHandlingMult: float
    PenaltySkillDivisor: float
    PenaltyCondDivisor: float
    PenaltyMoraleBase: float
    PenaltyMoraleDivisor: float
    PenaltyGkSkillDivisor: float
    PostShotClearProbability: float
    ClearSpeed: float<meter / second>
    ClearVz: float<meter / second>
    ClearYStdDev: float
    GoalKickFallbackDistHome: float<meter>
    GoalKickFallbackDistAway: float<meter>
    KickOffPartnerOffsetX: float<meter>
    KickOffPartnerOffsetY: float<meter>
    FoulBaseRate: float
    CornerOnFailedCross: float
}

type HomeAdvantageConfig = {
    Strength: float
    DuelAttackBonus: float
    DuelDefenseBonus: float
    ShotComposureBonus: float
    PassAccuracyBonus: float
    DribbleBonus: float
    SetPlayAccuracyBonus: float
    TackleBonus: float
    FreeKickComposure: float
    PenaltyBonus: float
    CardReduction: float
    FatigueReduction: float
}

type PhysicsConfig = {
    Gravity: float<meter / second^2>
    AirDrag: float
    GroundRestitution: float
    GroundFriction: float
    PostRestitution: float
    SpinDecay: float
    MagnusCoeff: float
    ContactRadius: float<meter>
    PlayerMaxForce: float<meter / second^2>
    PlayerMassBase: float
    PlayerMassWeightCoeff: float
    PlayerMassStrengthCoeff: float
    SteeringSlowRadius: float<meter>
    SteeringFleeRadius: float<meter>
    SteeringAlignmentWeight: float
    CohesionWeight: float
    TurnConstraintAgilityCoeff: float
    TurnConstraintBaseLimit: float
    MoveSpeedMax: float<meter / second>
    MoveSpeedMin: float<meter / second>
    SeparationMinDistance: float<meter>
    BallContestSeparationRadius: float<meter>
    SeparationStrength: float
    SeparationAgilityMultiplier: float
    JitterBase: float
    JitterAgilityMultiplier: float
    BallStopThreshold: float
    AirborneCheckThreshold: float<meter>
    AxisStopThreshold: float
    AirborneRestitutionBase: float
    AirborneRestitutionCoeff: float
    AirborneRestitutionFloor: float
    GroundRestitutionBase: float
    GroundRestitutionCoeff: float
    GroundRestitutionFloor: float
    ChaserProximity: float<meter>
    AirborneThreshold: float<meter>
}

type TimingConfig = {
    DuelChainDelay: TickDelay
    DuelNextDelay: TickDelay
    ShotDelay: TickDelay
    FoulDelay: TickDelay
    GoalDelay: TickDelay
    KickOffDelay: TickDelay
    CornerDelay: TickDelay
    FreeKickDelay: TickDelay
    ThrowInDelay: TickDelay
    InjuryDelay: TickDelay
    ManagerReactDelay: TickDelay
    SubsDelay: TickDelay
    StuckBallDelay: int
}

type MatchVolumeConfig = {
    MaxChainLength: int
    TargetDuelTicksPerMatch: int
    TargetShotsPerMatch: float
    TargetDribblesPerMatch: float
    TargetPassesPerMatch: float
    TargetCrossesPerMatch: float
    TargetLongBallsPerMatch: float
}

type ManagerConfig = {
    FatigueReactionThreshold: int
    SustainedMomentumSubTicks: int
    MomentumThreshold: float
    FatigueCheckSubTicks: int
    ConditionThresholdLosing: int
    ConditionThresholdDrawing: int
    ConditionThresholdWinning: int
    SubWindowMinutes: int[]
}

type BuildUpConfig = {
    PassSuccessBonus: float
    DribblePenalty: float
    LongBallPenalty: float
    GKDistributionBonus: float
    DCPassingBonus: float
}

type DecisionConfig = {
    ShootFinishingWeight: float
    ShootLongShotsWeight: float
    ShootComposureWeight: float
    ShootDistNormWeight: float
    ShootDistNormDivisor: float
    ShootPosDirectnessWeight: float
    ShootPosDepthWeight: float
    ShootSTBonus: float
    ShootDistPenaltyDivisor: float
    ShootDistPenaltyMax: float
    PassPassingWeight: float
    PassVisionWeight: float
    PassTargetBonus: float
    PassAttackPhasePenalty: float
    DribbleZoneBonusAttacking: float
    DribbleZoneBonusMidfield: float
    DribbleAttackPhaseBonus: float
    CrossCrossingWeight: float
    CrossLateralTendencyWeight: float
    CrossLateralTendencyBase: float
    CrossZoneBonus: float
    LongBallPassingWeight: float
    LongBallVisionWeight: float
    LongBallPressDistBase: float
    LongBallPressMin: float
    LongBallPressMax: float
    LongBallPressNoOpponent: float
    LongBallAttackPhaseBonus: float
    CreativityWeight: float
    DirectnessWeight: float
}

type BalanceConfig = {
    Duel: DuelConfig
    Shot: ShotConfig
    Pass: PassConfig
    Cross: CrossConfig
    Dribble: DribbleConfig
    Tackle: TackleConfig
    SetPiece: SetPieceConfig
    HomeAdvantage: HomeAdvantageConfig
    Physics: PhysicsConfig
    Timing: TimingConfig
    MatchVolume: MatchVolumeConfig
    Manager: ManagerConfig
    BuildUp: BuildUpConfig
    Decision: DecisionConfig
}

// ============================================================================
// Default config value
// ============================================================================

module BalanceConfig =

    let private clock = defaultClock

    let defaultConfig : BalanceConfig = {
        Duel = {
            WinProbabilityBase = 3.0
            RecoverProbabilityBase = 5.0
            MomentumBonus = 0.50
            JitterWin = 8.0
            JitterRecover = 2.0
            JitterKeep = 2.5
            SpeedKeep = 3.0<meter / second>
            SpeedKeepVz = 0.20<meter / second>
            AttackerDribblingWeight = 0.50
            AttackerAgilityWeight = 0.30
            AttackerBalanceWeight = 0.20
            DefenderTacklingWeight = 0.50
            DefenderStrengthWeight = 0.30
            DefenderPositionWeight = 0.20
            FatigueThreshold = 50
            FatigueDecay = 0.04
        }
        Shot = {
            QualityGate = 0.10
            AngleSpreadBase = 0.30
            VzBase = PhysicsContract.LongBallVz
            VzVariance = 1.5
            OnTargetBase = 0.25
            OnTargetMultiplier = 0.30
            NormalisationDistance = 30.0<meter>
            DistanceToGoalMultiplier = 0.15
            FinishingMin = 0.20
            FinishingMax = 1.00
            FinishingBonusST = 2.0
            FinishingBonusAM = 2.0
            FinishingBonusMC = 1.2
            FinishingBonusOther = 0.5
            CondFactorDivisor = 100.0
            ComposureMultiplier = 0.1
            UrgencyMultiplier = 0.05
            BasePowerDivisor = 20.0
            SpinTopMultiplier = 0.4
            PositionDirectnessWeight = 0.3
            PositionDepthWeight = 0.2
            PositionCreativityWeight = 0.1
            DistNormWeight = 0.7
            PositionBonusWeight = 0.3
            HeavyTouchDivisor = 20.0
            HeavyTouchMultiplier = 0.25
            JitterStdDev = 2.0
            GkReflexesStatMult = 2.5
            GkOneOnOneStatMult = 3.5
            SaveDenominatorOffset = 1.0
        }
        Pass = {
            BaseMean = 0.60
            TechnicalWeight = 0.15
            VisionWeight = 0.15
            SuccessShapeAlpha = 10.0
            SuccessConditionMultiplier = 6.0
            OffsideMomentum = 0.30
            SuccessMomentum = 0.30
            FailMomentum = 0.50
            DeflectBaseRate = 0.06
            MisplacedBaseRate = 0.03
            InterceptBaseRate = 0.05
            InterceptionRadius = 5.0<meter>
            PressureDistance = 8.0<meter>
            DeflectPressureMultiplier = 0.12
            InterceptPaceWeight = 0.35
            InterceptPositioningWeight = 0.45
            ScrambleJitter = 3.0<meter>
            Speed = PhysicsContract.PassSpeed
            Vz = PhysicsContract.PassVz
            InterceptDistFactorWeight = 0.3
            InterceptPositioningContrib = 0.15
            InterceptVisionContrib = 0.10
            CreativityWeight = 0.06
            DirectnessWeight = 0.06
            MeanMin = 0.01
            MeanMax = 0.99
            DefaultNearestDefDist = 10.0<meter>
            DefaultTackling = 0.5
            HeavyTouchDivisor = 20.0
            HeavyTouchMultiplier = 0.25
            JitterStdDev = 2.0
            DeflectedSpeedMult = 0.6
            DeflectedVzMult = 0.5
            InterceptProbMax = 0.8
            MisplacedSpeedMult = 0.7
            LongBallBaseMean = 0.40
            LongBallLongShotsWeight = 0.20
            LongBallPassingWeight = 0.20
            LongBallVisionWeight = 0.15
            LongBallSuccessShapeAlpha = 5.0
            LongBallSuccessConditionMultiplier = 10.0
            LongBallOffsideMomentum = 0.40
            LongBallSuccessMomentum = 0.50
            LongBallFailMomentum = 0.40
            LongBallSpeed = PhysicsContract.LongBallSpeed
            LongBallVz = PhysicsContract.LongBallVz
            LongBallDeflectMult = 1.5
            LongBallInterceptMult = 1.5
            LongBallPressureContrib = 0.3
            ForwardDepthThreshold = 0.5
            ForwardCreativityThreshold = 0.4
            LongBallScrambleJitterMult = 2.0
        }
        Cross = {
            BaseMean = 0.50
            CrossingWeight = 0.25
            PassingWeight = 0.10
            SuccessShapeAlpha = 6.0
            SuccessConditionMultiplier = 8.0
            FailMomentum = 0.30
            Speed = PhysicsContract.CrossSpeed
            Vz = PhysicsContract.CrossVz
            AerialThreatThreshold = 0.4
            AttackingDepthThreshold = 0.5
            GkSkillDefault = 50.0
            GkSkillDivisor = 150.0
            SpinTopMult = 0.2
            SpinSideMult = 0.8
            HeaderLogisticSteepness = 3.0
            FallbackSpeed = 15.0<meter / second>
            FallbackVz = 2.0<meter / second>
        }
        Dribble = {
            TechnicalWeight = 0.50
            AgilityWeight = 0.30
            BalanceWeight = 0.20
            ForwardDistance = 5.0
            SuccessMomentum = 0.50
            FailMomentum = 0.60
            CrossProbability = 0.05
            PassProbability = 0.75
            ShotProbability = 0.20
        }
        Tackle = {
            TechnicalWeight = 0.50
            PositioningWeight = 0.30
            StrengthWeight = 0.20
            AggressionWeight = 0.15
            PositioningReduction = 0.10
            FoulShapeBeta = 10.0
            FoulMomentum = 0.30
            SuccessMomentum = 0.80
            FailMomentum = 0.50
        }
        SetPiece = {
            FreeKickTargetX = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth
            FreeKickSpeed = 16.0<meter / second>
            FreeKickVz = 1.50<meter / second>
            FreeKickSavePowerThreshold = 2.0
            FreeKickSaveVariance = 1.5
            FreeKickSpinTopMult = 0.5
            FreeKickSpinSideMult = 0.9
            CornerBoxXThreshold = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth
            CornerDefenderBoxThreshold = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth - 5.0<meter>
            CornerSecondPhaseProbability = 0.35
            CornerKeepPossessionProbability = 0.55
            CornerSpeed = 14.0<meter / second>
            CornerVz = 1.0<meter / second>
            CornerDensityBase = 3.0
            CornerDensityPenalty = 0.05
            CornerLogisticSteepness = 2.5
            CornerDefScoreDefault = 0.5
            ThrowInSpeed = 12.0<meter / second>
            ThrowInVz = 0.50<meter / second>
            ThrowInMomentum = 0.10
            PenaltySkillMultiplier = 0.04
            PenaltyMoraleMultiplier = 0.01
            PenaltyPressureMultiplier = 0.01
            PenaltyComposureNoise = 1.40
            PenaltyLogisticBase = 0.80
            PenaltyGkReflexesMult = 2.5
            PenaltyGkHandlingMult = 2.0
            PenaltySkillDivisor = 20.0
            PenaltyCondDivisor = 200.0
            PenaltyMoraleBase = 0.7
            PenaltyMoraleDivisor = 166.6
            PenaltyGkSkillDivisor = 40.0
            PostShotClearProbability = 0.40
            ClearSpeed = 16.0<meter / second>
            ClearVz = 1.5<meter / second>
            ClearYStdDev = 10.0
            GoalKickFallbackDistHome = 30.0<meter>
            GoalKickFallbackDistAway = 75.0<meter>
            KickOffPartnerOffsetX = -3.0<meter>
            KickOffPartnerOffsetY = 2.0<meter>
            FoulBaseRate = 0.35
            CornerOnFailedCross = 0.85
        }
        HomeAdvantage = {
            Strength = 4.0
            DuelAttackBonus = 16.0
            DuelDefenseBonus = 8.0
            ShotComposureBonus = 16.0
            PassAccuracyBonus = 0.20
            DribbleBonus = 8.0
            SetPlayAccuracyBonus = 0.16
            TackleBonus = 24.0
            FreeKickComposure = 20.0
            PenaltyBonus = 0.16
            CardReduction = 0.80
            FatigueReduction = 0.40
        }
        Physics = {
            Gravity = PhysicsContract.Gravity
            AirDrag = PhysicsContract.BallAirDrag
            GroundRestitution = PhysicsContract.BallGroundRestitution
            GroundFriction = PhysicsContract.BallGroundFriction
            PostRestitution = PhysicsContract.BallPostRestitution
            SpinDecay = PhysicsContract.BallSpinDecay
            MagnusCoeff = PhysicsContract.BallMagnusCoeff
            ContactRadius = PhysicsContract.BallContactRadius
            PlayerMaxForce = 25.0<meter / second^2>
            PlayerMassBase = 70.0
            PlayerMassWeightCoeff = 0.30
            PlayerMassStrengthCoeff = 0.50
            SteeringSlowRadius = PhysicsContract.SteeringSlowRadius
            SteeringFleeRadius = 8.0<meter>
            SteeringAlignmentWeight = 0.30
            CohesionWeight = 0.08
            TurnConstraintAgilityCoeff = PhysicsContract.TurnConstraintAgilityCoeff
            TurnConstraintBaseLimit = PhysicsContract.TurnConstraintBase
            MoveSpeedMax = PhysicsContract.PlayerSpeedMax
            MoveSpeedMin = PhysicsContract.PlayerSpeedMin
            SeparationMinDistance = PhysicsContract.PlayerSeparationRadius
            BallContestSeparationRadius = PhysicsContract.BallContestSeparationRadius
            SeparationStrength = 0.10
            SeparationAgilityMultiplier = 0.15
            JitterBase = 0.30
            JitterAgilityMultiplier = 0.50
            BallStopThreshold = 0.15
            AirborneCheckThreshold = 0.05<meter>
            AxisStopThreshold = 0.5
            AirborneRestitutionBase = 0.35
            AirborneRestitutionCoeff = 0.25
            AirborneRestitutionFloor = 0.008
            GroundRestitutionBase = 0.50
            GroundRestitutionCoeff = 0.20
            GroundRestitutionFloor = 0.005
            ChaserProximity = 1.0<meter>
            AirborneThreshold = 0.3<meter>
        }
        Timing = {
            DuelChainDelay = TickDelay.ofSeconds clock 4.0 1.0 2.0 7.0
            DuelNextDelay = TickDelay.ofSeconds clock 24.0 5.0 12.0 38.0
            ShotDelay = TickDelay.ofSeconds clock 2.0 0.5 1.0 4.0
            FoulDelay = TickDelay.ofSeconds clock 5.0 1.5 3.0 9.0
            GoalDelay = TickDelay.ofSeconds clock 28.0 4.0 20.0 38.0
            KickOffDelay = TickDelay.ofSeconds clock 1.0 0.0 1.0 1.0
            CornerDelay = TickDelay.ofSeconds clock 11.0 2.0 7.0 16.0
            FreeKickDelay = TickDelay.ofSeconds clock 10.0 2.0 6.0 15.0
            ThrowInDelay = TickDelay.ofSeconds clock 5.0 1.0 3.0 8.0
            InjuryDelay = TickDelay.ofSeconds clock 30.0 6.0 20.0 45.0
            ManagerReactDelay = TickDelay.ofSeconds clock 8.0 2.5 5.0 15.0
            SubsDelay = TickDelay.ofSeconds clock 22.0 3.0 14.0 30.0
            StuckBallDelay = secondsToSubTicks clock 5.0
        }
        MatchVolume = {
            MaxChainLength = 6
            TargetDuelTicksPerMatch = 228
            TargetShotsPerMatch = 25.0
            TargetDribblesPerMatch = 25.0
            TargetPassesPerMatch = 200.0
            TargetCrossesPerMatch = 20.0
            TargetLongBallsPerMatch = 40.0
        }
        Manager = {
            FatigueReactionThreshold = 60
            SustainedMomentumSubTicks = secondsToSubTicks clock 600
            MomentumThreshold = -2.0
            FatigueCheckSubTicks = secondsToSubTicks clock 120
            ConditionThresholdLosing = 75
            ConditionThresholdDrawing = 65
            ConditionThresholdWinning = 55
            SubWindowMinutes = [| 60; 75; 85 |]
        }
        BuildUp = {
            PassSuccessBonus = 0.06
            DribblePenalty = 0.12
            LongBallPenalty = 0.08
            GKDistributionBonus = 0.08
            DCPassingBonus = 0.05
        }
        Decision = {
            ShootFinishingWeight = 0.35
            ShootLongShotsWeight = 0.15
            ShootComposureWeight = 0.20
            ShootDistNormWeight = 0.20
            ShootDistNormDivisor = 30.0
            ShootPosDirectnessWeight = 0.10
            ShootPosDepthWeight = 0.08
            ShootSTBonus = 0.20
            ShootDistPenaltyDivisor = 50.0
            ShootDistPenaltyMax = 0.5
            PassPassingWeight = 0.40
            PassVisionWeight = 0.30
            PassTargetBonus = 0.30
            PassAttackPhasePenalty = -0.03
            DribbleZoneBonusAttacking = 0.1
            DribbleZoneBonusMidfield = 0.05
            DribbleAttackPhaseBonus = 0.05
            CrossCrossingWeight = 0.60
            CrossLateralTendencyWeight = 0.60
            CrossLateralTendencyBase = 0.10
            CrossZoneBonus = 0.15
            LongBallPassingWeight = 0.30
            LongBallVisionWeight = 0.20
            LongBallPressDistBase = 10.0
            LongBallPressMin = 0.3
            LongBallPressMax = 1.0
            LongBallPressNoOpponent = 0.7
            LongBallAttackPhaseBonus = 0.05
            CreativityWeight = 0.10
            DirectnessWeight = 0.06
        }
    }
