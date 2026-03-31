module BalanceConfig

let AvgChainLength = 4
let TargetDuelTicksPerMatch = 228

let TargetShotsPerMatch = 25.0
let TargetDribblesPerMatch = 25.0
let TargetPassesPerMatch = 200.0
let TargetCrossesPerMatch = 20.0
let TargetLongBallsPerMatch = 40.0

let GoalDifficulty = 1.6
let ShotQualityGate = 0.45
let FoulBaseRate = 0.35
let CornerOnFailedCross = 0.85
let PostShotClearProbability = 0.40

let HomeAdvantageStrength = 1.0

let HomeDuelAttackBonus = 3.0 * HomeAdvantageStrength
let HomeDuelDefenseBonus = 2.0 * HomeAdvantageStrength
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
