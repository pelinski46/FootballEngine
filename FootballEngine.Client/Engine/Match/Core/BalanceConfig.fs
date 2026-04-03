module BalanceConfig

type TickDelay = { Mean: float; Std: float; Min: int; Max: int }

let AvgChainLength = 4
let TargetDuelTicksPerMatch = 228

let duelChainDelay    = { Mean = 3.0;  Std = 1.0; Min = 2;  Max = 5  }
let duelNextDelay     = { Mean = 25.0; Std = 5.0; Min = 12; Max = 38 }
let shotDelay         = { Mean = 2.0;  Std = 0.5; Min = 1;  Max = 4  }
let foulDelay         = { Mean = 5.0;  Std = 1.5; Min = 3;  Max = 9  }
let goalDelay         = { Mean = 28.0; Std = 4.0; Min = 20; Max = 38 }
let cornerDelay       = { Mean = 11.0; Std = 2.0; Min = 7;  Max = 16 }
let freeKickDelay     = { Mean = 10.0; Std = 2.0; Min = 6;  Max = 15 }
let throwInDelay      = { Mean = 5.0;  Std = 1.0; Min = 3;  Max = 8  }
let injuryDelay       = { Mean = 30.0; Std = 6.0; Min = 20; Max = 45 }
let managerReactDelay = { Mean = 8.0;  Std = 2.5; Min = 5;  Max = 15 }
let subsDelay         = { Mean = 22.0; Std = 3.0; Min = 14; Max = 30 }

let TargetShotsPerMatch = 25.0
let TargetDribblesPerMatch = 25.0
let TargetPassesPerMatch = 200.0
let TargetCrossesPerMatch = 20.0
let TargetLongBallsPerMatch = 40.0

let GoalDifficulty = 1.9
let ShotQualityGate = 0.45
let FoulBaseRate = 0.35
let CornerOnFailedCross = 0.85
let PostShotClearProbability = 0.40

let HomeAdvantageStrength = 2.5

let HomeDuelAttackBonus = 3.0 * HomeAdvantageStrength
let HomeDuelDefenseBonus = 1.0 * HomeAdvantageStrength
let HomeShotComposureBonus = 3.0 * HomeAdvantageStrength
let HomePassAccuracyBonus = 0.03 * HomeAdvantageStrength
let HomeDribbleBonus = 1.5 * HomeAdvantageStrength
let HomeSetPlayAccuracyBonus = 0.02 * HomeAdvantageStrength
let HomeTackleBonus = 5.0 * HomeAdvantageStrength
let HomeFreeKickComposure = 3.0 * HomeAdvantageStrength
let HomePenaltyBonus = 0.02 * HomeAdvantageStrength
let HomeCardReduction = 0.15 * HomeAdvantageStrength
let HomeFatigueReduction = 0.05 * HomeAdvantageStrength

let GkSaveBonus = (GoalDifficulty - 1.0) * 50.0
let OnTargetBase = 0.30 + (1.0 - ShotQualityGate) * 0.15

// Shot physics constants
let ShotBaseSpeed = 20.0
let ShotSpeedMultiplier = 30.0
let ShotAngleSpreadBase = 0.3
let ShotVzBase = 4.0
let ShotVzVariance = 1.5
let ShotOnTargetBase = 0.5
let ShotOnTargetMultiplier = 0.3
let ShotDistanceToGoalMultiplier = 0.15
let ShotFinishingMin = 0.2
let ShotFinishingMax = 1.0

// Duel constants
let DuelWinProbabilityBase = 0.50
let DuelRecoverProbabilityBase = 0.35
let DuelJitterWin = 10.0
let DuelJitterRecover = 2.0
let DuelJitterKeep = 3.0
let DuelSpeedKeep = 3.0
let DuelSpeedKeepVz = 0.2
let DuelMomentumBonus = 0.5
let DuelMomentumRecover = 1.0
let DuelMoraleBonusMultiplier = 0.05
let DuelReputationBonusMultiplier = 0.002

// Pass constants
let PassBaseMean = 0.82
let PassTechnicalWeight = 0.2
let PassVisionWeight = 0.1
let PassSuccessShapeAlpha = 8.0
let PassSuccessConditionMultiplier = 12.0
let PassForwardBonus = 0.15
let PassLaneClearBonus = 0.2
let PassSpeed = 18.0
let PassVz = 0.3
let PassOffsideMomentum = 0.3
let PassSuccessMomentum = 0.3
let PassFailMomentum = 0.5

// Dribble constants
let DribbleTechnicalWeight = 0.5
let DribbleAgilityWeight = 0.3
let DribbleBalanceWeight = 0.2
let DribbleSpeed = 10.0
let DribbleVz = 0.2
let DribbleForwardDistance = 5.0
let DribbleSuccessMomentum = 0.5
let DribbleFailMomentum = 0.6
let DribbleCrossProbability = 0.10
let DribblePassProbability = 0.60
let DribbleShotProbability = 0.30

// Cross constants
let CrossBaseMean = 0.50
let CrossCrossingWeight = 0.25
let CrossPassingWeight = 0.10
let CrossSuccessShapeAlpha = 6.0
let CrossSuccessConditionMultiplier = 8.0
let CrossSpeed = 20.0
let CrossVz = 3.0
let CrossFailMomentum = 0.3

// Long ball constants
let LongBallBaseMean = 0.40
let LongBallLongShotsWeight = 0.2
let LongBallPassingWeight = 0.2
let LongBallVisionWeight = 0.15
let LongBallSuccessShapeAlpha = 5.0
let LongBallSuccessConditionMultiplier = 10.0
let LongBallSpeed = 22.0
let LongBallVz = 4.0
let LongBallOffsideMomentum = 0.4
let LongBallSuccessMomentum = 0.5
let LongBallFailMomentum = 0.4

// Tackle constants
let TackleTechnicalWeight = 0.5
let TacklePositioningWeight = 0.3
let TackleStrengthWeight = 0.2
let TackleAggressionWeight = 0.15
let TacklePositioningReduction = 0.1
let TackleFoulShapeBeta = 10.0
let TackleFoulMomentum = 0.3
let TackleSuccessMomentum = 0.8
let TackleFailMomentum = 0.5

// Free kick constants
let FreeKickTargetX = 75.0
let FreeKickSpeed = 16.0
let FreeKickVz = 1.5
let FreeKickSavePowerThreshold = 120.0
let FreeKickSaveVariance = 25.0

// Corner constants
let CornerBoxXThreshold = 75.0
let CornerDefenderBoxThreshold = 70.0
let CornerOutsideXThreshold = 75.0
let CornerSecondPhaseProbability = 0.35
let CornerKeepPossessionProbability = 0.55
let CornerSpeed = 14.0
let CornerVz = 1.0

// Throw-in constants
let ThrowInSpeed = 12.0
let ThrowInVz = 0.5
let ThrowInMomentum = 0.1

// Penalty constants
let PenaltySkillMultiplier = 0.04
let PenaltyMoraleMultiplier = 0.01
let PenaltyPressureMultiplier = 0.01
let PenaltyComposureNoise = 1.4
let PenaltyLogisticBase = 0.8

// Ball physics
let BallFriction = 0.98
let BallBounceDamping = 0.5
let BallControlDistanceThreshold = 1.5
let BallControlDampingX = 0.1
let BallControlDampingY = 0.1
let BallControlVx = 0.2
let BallControlVy = 0.2

// Ball physics (v2 — extended)
let BallGravity               = -9.8
let BallAirDrag               = 0.985
let BallGroundRestitution     = 0.55
let BallGroundFriction        = 0.92
let BallPostRestitution       = 0.65
let BallSpinDecay             = 0.98
let BallMagnusCoeff           = 0.0035

// Player physics (v2)
let PlayerContactRadiusFactor = 0.005   // * playerHeight
let PlayerMaxForce            = 8.0
let PlayerMassBase            = 0.8
let PlayerMassWeightCoeff     = 0.005   // kg -> mass contribution
let PlayerMassStrengthCoeff   = 0.003   // strength stat -> mass contribution
let SteeringSlowRadius        = 12.0
let SteeringFleeRadius        = 5.0
let SteeringAlignmentWeight   = 0.3
let TurnConstraintAgilityCoeff = 0.02   // agility reduces max lateral acceleration
let TurnConstraintBaseLimit   = 15.0    // max lateral accel for agility=0

// Duel biomechanics (v2 — attribute opposition)
let DuelAttackerDribblingWeight = 0.50
let DuelAttackerAgilityWeight   = 0.30
let DuelAttackerBalanceWeight   = 0.20
let DuelDefenderTacklingWeight  = 0.50
let DuelDefenderStrengthWeight  = 0.30
let DuelDefenderPositionWeight  = 0.20
let DuelFatigueExponentialThreshold = 50  // condition % where fatigue drops exponentially
let DuelFatigueExponentialDecay = 0.04    // decay rate below threshold
let DuelAttributeSteepness      = 2.5     // logisticBernoulli steepness for attribute diff

// Manager reaction thresholds
let ManagerFatigueReactionThreshold = 60  // condition % that triggers manager reaction
let ManagerSustainedMomentumSeconds = 600 // 10 minutes of negative momentum triggers reaction
let ManagerMomentumThreshold        = -2.0 // momentum level considered "against team"
let ManagerFatigueCheckInterval     = 120 // seconds between fatigue checks outside sub windows

// Player movement
let MoveSpeedBase = 0.45
let MoveSpeedMin = 0.05
let MoveSpeedMax = 0.45
let SeparationMinDistance = 4.0
let SeparationStrength = 0.1
let SeparationAgilityMultiplier = 0.15
let JitterBase = 0.3
let JitterAgilityMultiplier = 0.5
