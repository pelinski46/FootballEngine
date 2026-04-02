module BalanceConfig

let AvgChainLength = 4
let TargetDuelTicksPerMatch = 228

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
let DuelSkillBonusMultiplier = 0.12
let DuelMoraleBonusMultiplier = 0.05
let DuelConditionBonusMultiplier = 3.0
let DuelReputationBonusMultiplier = 0.002
let DuelMomentumBonus = 0.5
let DuelMomentumRecover = 1.0

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

// Player movement
let MoveSpeedBase = 0.45
let MoveSpeedMin = 0.05
let MoveSpeedMax = 0.45
let SeparationMinDistance = 4.0
let SeparationStrength = 0.1
let SeparationAgilityMultiplier = 0.15
let JitterBase = 0.3
let JitterAgilityMultiplier = 0.5
