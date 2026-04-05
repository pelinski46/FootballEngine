module BalanceConfig

open FootballEngine

// ============================================================================
// Timing — all delays expressed in SubTicks via PhysicsContract
// ============================================================================

/// Delays are in SubTicks. Use PhysicsContract.secondsToSubTicks to convert.
type TickDelay =
    { MeanST: int   // mean in SubTicks
      StdST:  int   // standard deviation in SubTicks
      MinST:  int
      MaxST:  int }

module TickDelay =
    let ofSeconds mean std min max =
        { MeanST = PhysicsContract.secondsToSubTicks mean
          StdST  = PhysicsContract.secondsToSubTicks std
          MinST  = PhysicsContract.secondsToSubTicks min
          MaxST  = PhysicsContract.secondsToSubTicks max }

// Action chain — how quickly actions follow each other within a sequence
let duelChainDelay  = TickDelay.ofSeconds 3.0  1.0 2.0  5.0
let duelNextDelay   = TickDelay.ofSeconds 24.0 5.0 12.0 38.0
let shotDelay       = TickDelay.ofSeconds 2.0  0.5 1.0  4.0
let foulDelay       = TickDelay.ofSeconds 5.0  1.5 3.0  9.0
let goalDelay       = TickDelay.ofSeconds 28.0 4.0 20.0 38.0
let cornerDelay     = TickDelay.ofSeconds 11.0 2.0 7.0  16.0
let freeKickDelay   = TickDelay.ofSeconds 10.0 2.0 6.0  15.0
let throwInDelay    = TickDelay.ofSeconds 5.0  1.0 3.0  8.0
let injuryDelay     = TickDelay.ofSeconds 30.0 6.0 20.0 45.0
let managerReactDelay = TickDelay.ofSeconds 8.0 2.5 5.0 15.0
let subsDelay       = TickDelay.ofSeconds 22.0 3.0 14.0 30.0

// ============================================================================
// Match volume targets — used for balance calibration only, not physics
// ============================================================================

let AvgChainLength          = 4
let TargetDuelTicksPerMatch = 228
let TargetShotsPerMatch     = 25.0
let TargetDribblesPerMatch  = 25.0
let TargetPassesPerMatch    = 200.0
let TargetCrossesPerMatch   = 20.0
let TargetLongBallsPerMatch = 40.0

// ============================================================================
// Home advantage
// ============================================================================

let HomeAdvantageStrength       = 2.5

let HomeDuelAttackBonus         = 3.0  * HomeAdvantageStrength
let HomeDuelDefenseBonus        = 1.0  * HomeAdvantageStrength
let HomeShotComposureBonus      = 3.0  * HomeAdvantageStrength
let HomePassAccuracyBonus       = 0.03 * HomeAdvantageStrength
let HomeDribbleBonus            = 1.5  * HomeAdvantageStrength
let HomeSetPlayAccuracyBonus    = 0.02 * HomeAdvantageStrength
let HomeTackleBonus             = 5.0  * HomeAdvantageStrength
let HomeFreeKickComposure       = 3.0  * HomeAdvantageStrength
let HomePenaltyBonus            = 0.02 * HomeAdvantageStrength
let HomeCardReduction           = 0.15 * HomeAdvantageStrength
let HomeFatigueReduction        = 0.05 * HomeAdvantageStrength

// ============================================================================
// Goal / shot difficulty
// ============================================================================

let GoalDifficulty              = 1.9
let ShotQualityGate             = 0.45
let FoulBaseRate                = 0.35
let CornerOnFailedCross         = 0.85
let PostShotClearProbability    = 0.40

let GkSaveBonus     = (GoalDifficulty - 1.0) * 50.0
let OnTargetBase    = 0.30 + (1.0 - ShotQualityGate) * 0.15

// ============================================================================
// Shot — velocities reference PhysicsContract
// ============================================================================

/// Angle spread (radians) for a Finishing=1 shot. Decreases linearly with skill.
let ShotAngleSpreadBase         = 0.30
let ShotVzBase                  = PhysicsContract.LongBallVz     // reuse loft constant
let ShotVzVariance              = 1.5
let ShotOnTargetBase            = 0.50
let ShotOnTargetMultiplier      = 0.30

/// Distance at which shot quality normalises to 0. In metres (pitch is 105m long).
let ShotNormalisationDistance   = 30.0
let ShotDistanceToGoalMultiplier = 0.15
let ShotFinishingMin            = 0.20
let ShotFinishingMax            = 1.00

// ============================================================================
// Duel — attribute weights and probability bases
// ============================================================================

let DuelWinProbabilityBase      = 0.50
let DuelRecoverProbabilityBase  = 0.35
let DuelMomentumBonus           = 0.50
let DuelMomentumRecover         = 1.00
let DuelMoraleBonusMultiplier   = 0.05
let DuelReputationBonusMultiplier = 0.002
let DuelAttributeSteepness      = 2.50

// Jitter in metres — how far the ball moves from its current position on each outcome
let DuelJitterWin               = 8.0    // metres — attacker carries it forward
let DuelJitterRecover           = 2.0    // metres — defender pokes it away
let DuelJitterKeep              = 2.5    // metres — loose ball, no clear winner

// Ball speed after a loose-ball duel outcome (m/s)
let DuelSpeedKeep               = 3.0
let DuelSpeedKeepVz             = 0.20

// Attribute weights — all normalised against AttrMax (20) at call sites
let DuelAttackerDribblingWeight = 0.50
let DuelAttackerAgilityWeight   = 0.30
let DuelAttackerBalanceWeight   = 0.20
let DuelDefenderTacklingWeight  = 0.50
let DuelDefenderStrengthWeight  = 0.30
let DuelDefenderPositionWeight  = 0.20

// Fatigue model — exponential drop-off below a condition threshold
let DuelFatigueThreshold        = 50    // condition % below which fatigue accelerates
let DuelFatigueDecay            = 0.04

// ============================================================================
// Pass
// ============================================================================

let PassBaseMean                = 0.82
let PassTechnicalWeight         = 0.20
let PassVisionWeight            = 0.10
let PassSuccessShapeAlpha       = 8.0
let PassSuccessConditionMultiplier = 12.0
let PassForwardBonus            = 0.15
let PassLaneClearBonus          = 0.20
let PassOffsideMomentum         = 0.30
let PassSuccessMomentum         = 0.30
let PassFailMomentum            = 0.50

// Velocities come from PhysicsContract — no local redefinition
let PassSpeed                   = PhysicsContract.PassSpeed
let PassVz                      = PhysicsContract.PassVz

// ============================================================================
// Dribble
// ============================================================================

let DribbleTechnicalWeight      = 0.50
let DribbleAgilityWeight        = 0.30
let DribbleBalanceWeight        = 0.20
let DribbleForwardDistance      = 5.0   // metres the ball moves on a successful dribble
let DribbleSuccessMomentum      = 0.50
let DribbleFailMomentum         = 0.60
let DribbleCrossProbability     = 0.10
let DribblePassProbability      = 0.60
let DribbleShotProbability      = 0.30

// ============================================================================
// Cross
// ============================================================================

let CrossBaseMean               = 0.50
let CrossCrossingWeight         = 0.25
let CrossPassingWeight          = 0.10
let CrossSuccessShapeAlpha      = 6.0
let CrossSuccessConditionMultiplier = 8.0
let CrossFailMomentum           = 0.30

let CrossSpeed                  = PhysicsContract.CrossSpeed
let CrossVz                     = PhysicsContract.CrossVz

// ============================================================================
// Long ball
// ============================================================================

let LongBallBaseMean            = 0.40
let LongBallLongShotsWeight     = 0.20
let LongBallPassingWeight       = 0.20
let LongBallVisionWeight        = 0.15
let LongBallSuccessShapeAlpha   = 5.0
let LongBallSuccessConditionMultiplier = 10.0
let LongBallOffsideMomentum     = 0.40
let LongBallSuccessMomentum     = 0.50
let LongBallFailMomentum        = 0.40

let LongBallSpeed               = PhysicsContract.LongBallSpeed
let LongBallVz                  = PhysicsContract.LongBallVz

// ============================================================================
// Tackle
// ============================================================================

let TackleTechnicalWeight       = 0.50
let TacklePositioningWeight     = 0.30
let TackleStrengthWeight        = 0.20
let TackleAggressionWeight      = 0.15
let TacklePositioningReduction  = 0.10
let TackleFoulShapeBeta         = 10.0
let TackleFoulMomentum          = 0.30
let TackleSuccessMomentum       = 0.80
let TackleFailMomentum          = 0.50

// ============================================================================
// Set pieces — positions in metres, velocities from PhysicsContract
// ============================================================================

// Free kick
let FreeKickTargetX             = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth
let FreeKickSpeed               = 16.0
let FreeKickVz                  = 1.50
let FreeKickSavePowerThreshold  = 120.0
let FreeKickSaveVariance        = 25.0

// Corner
let CornerBoxXThreshold         = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth
let CornerDefenderBoxThreshold  = PhysicsContract.GoalLineHome - PhysicsContract.PenaltyAreaDepth - 5.0
let CornerSecondPhaseProbability    = 0.35
let CornerKeepPossessionProbability = 0.55
let CornerSpeed                 = 14.0
let CornerVz                    = 1.0

// Throw-in
let ThrowInSpeed                = 12.0
let ThrowInVz                   = 0.50
let ThrowInMomentum             = 0.10

// Penalty
let PenaltySkillMultiplier      = 0.04
let PenaltyMoraleMultiplier     = 0.01
let PenaltyPressureMultiplier   = 0.01
let PenaltyComposureNoise       = 1.40
let PenaltyLogisticBase         = 0.80

// ============================================================================
// Ball physics — all from PhysicsContract
// ============================================================================

let BallGravity             = PhysicsContract.Gravity
let BallAirDrag             = PhysicsContract.BallAirDrag
let BallGroundRestitution   = PhysicsContract.BallGroundRestitution
let BallGroundFriction      = PhysicsContract.BallGroundFriction
let BallPostRestitution     = PhysicsContract.BallPostRestitution
let BallSpinDecay           = PhysicsContract.BallSpinDecay
let BallMagnusCoeff         = PhysicsContract.BallMagnusCoeff

// ============================================================================
// Player physics — derived from PhysicsContract, no magic numbers
// ============================================================================

let BallContactRadius       = PhysicsContract.BallContactRadius
let PlayerMaxForce          = 25.0  // N/kg — caps total steering force
let PlayerMassBase          = 70.0  // kg baseline
let PlayerMassWeightCoeff   = 0.30  // kg per kg of player weight (proportion)
let PlayerMassStrengthCoeff = 0.50  // kg per strength point (1–20)

let SteeringSlowRadius      = PhysicsContract.SteeringSlowRadius
let SteeringFleeRadius      = 8.0   // metres — beyond this, flee behaviour turns off
let SteeringAlignmentWeight = 0.30

let TurnConstraintAgilityCoeff = PhysicsContract.TurnConstraintAgilityCoeff
let TurnConstraintBaseLimit    = PhysicsContract.TurnConstraintBase

let MoveSpeedMax            = PhysicsContract.PlayerSpeedMax
let MoveSpeedMin            = PhysicsContract.PlayerSpeedMin
let SeparationMinDistance   = PhysicsContract.PlayerSeparationRadius

let SeparationStrength          = 0.10
let SeparationAgilityMultiplier = 0.15
let JitterBase                  = 0.30  // metres of random noise on player movement
let JitterAgilityMultiplier     = 0.50

// ============================================================================
// Manager / cognitive timing — in SubTicks via PhysicsContract
// ============================================================================

let ManagerFatigueReactionThreshold = 60   // condition % that triggers reaction
let ManagerSustainedMomentumSubTicks =
    PhysicsContract.secondsToSubTicks 600   // 10 min sustained negative momentum
let ManagerMomentumThreshold        = -2.0
let ManagerFatigueCheckSubTicks     =
    PhysicsContract.secondsToSubTicks 120   // check every 2 min
