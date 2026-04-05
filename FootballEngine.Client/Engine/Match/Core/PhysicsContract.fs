namespace FootballEngine

module PhysicsContract =

    [<Literal>]
    let Dt = 0.025

    [<Literal>]
    let SubTicksPerSecond = 40

    [<Literal>]
    let SecondsPerSubTick = 0.025

    let inline secondsToSubTicks (s: float) : int = int (s * float SubTicksPerSecond)


    let inline subTicksToSeconds (t: int) : float = float t * SecondsPerSubTick


    [<Literal>]
    let PitchLength = 105.0


    [<Literal>]
    let PitchWidth = 68.0


    [<Literal>]
    let HalfwayLineX = 52.5


    [<Literal>]
    let GoalLineHome = 105.0


    [<Literal>]
    let GoalLineAway = 0.0


    [<Literal>]
    let PostNearY = 30.34 // 34.0 - 3.66


    [<Literal>]
    let PostFarY = 37.66 // 34.0 + 3.66


    [<Literal>]
    let CrossbarHeight = 2.44


    [<Literal>]
    let PenaltySpotDistance = 11.0


    [<Literal>]
    let PenaltyAreaDepth = 16.5


    [<Literal>]
    let PenaltyAreaHalfWidth = 20.16


    [<Literal>]
    let GoalAreaDepth = 5.5


    let AttrMax = 20.0


    [<Literal>]
    let ConditionMax = 100.0


    let inline normaliseAttr (v: int) : float = float v / AttrMax


    let inline normaliseCondition (v: int) : float = float v / ConditionMax

    [<Literal>]
    let PlayerSpeedMax = 9.5

    [<Literal>]
    let PlayerSpeedMin = 1.2


    let playerMaxSpeed (pace: int) (condition: int) : float =
        let paceNorm = normaliseAttr pace
        let condFactor = sqrt (normaliseCondition condition)
        (PlayerSpeedMin + (PlayerSpeedMax - PlayerSpeedMin) * paceNorm) * condFactor


    [<Literal>]
    let PlayerAccelMax = 6.5

    [<Literal>]
    let PlayerAccelMin = 2.0

    let playerAccel (acceleration: int) (condition: int) : float =
        let aNorm = normaliseAttr acceleration
        let condFactor = normaliseCondition condition
        (PlayerAccelMin + (PlayerAccelMax - PlayerAccelMin) * aNorm) * condFactor

    [<Literal>]
    let PlayerSeparationRadius = 1.2

    [<Literal>]
    let BallContactRadius = 0.35

    [<Literal>]
    let SteeringSlowRadius = 2.5

    [<Literal>]
    let TurnConstraintBase = 0.65


    [<Literal>]
    let TurnConstraintAgilityCoeff = 0.018 // agility=20 → constraint reduced by 0.36


    [<Literal>]
    let Gravity = -9.80665

    [<Literal>]
    let BallAirDrag = 0.985


    [<Literal>]
    let BallGroundRestitution = 0.55

    [<Literal>]
    let BallGroundFriction = 0.92


    [<Literal>]
    let BallPostRestitution = 0.65


    [<Literal>]
    let BallSpinDecay = 0.98


    [<Literal>]
    let BallMagnusCoeff = 0.0035


    [<Literal>]
    let PassSpeed = 16.0

    [<Literal>]
    let PassVz = 0.5


    [<Literal>]
    let LongBallSpeed = 26.0


    [<Literal>]
    let LongBallVz = 5.0

    [<Literal>]
    let CrossSpeed = 20.0


    [<Literal>]
    let CrossVz = 4.0


    [<Literal>]
    let ShotSpeedMax = 34.0

    [<Literal>]
    let ShotSpeedMin = 14.0

    let shotSpeed (finishing: int) : float =
        let fNorm = normaliseAttr finishing
        ShotSpeedMin + (ShotSpeedMax - ShotSpeedMin) * fNorm


    [<Literal>]
    let AttackingThirdThreshold = 35.0 // 105 - 35 = 70m from away goal

    [<Literal>]
    let DefensiveThirdThreshold = 35.0

    [<Literal>]
    let DuelIntervalMeanSeconds = 24.0

    let DuelIntervalMeanSubTicks = secondsToSubTicks DuelIntervalMeanSeconds // 960
    let DuelIntervalStdSubTicks = secondsToSubTicks 5.0 // 200
    let DuelIntervalMinSubTicks = secondsToSubTicks 12.0 // 480
    let DuelIntervalMaxSubTicks = secondsToSubTicks 38.0 // 1520


    let DuelChainDelayMeanSeconds = 3.0
    let DuelChainDelayMeanSubTicks = secondsToSubTicks DuelChainDelayMeanSeconds // 120
    let DuelChainDelayStdSubTicks = secondsToSubTicks 1.0 // 40
    let DuelChainDelayMinSubTicks = secondsToSubTicks 2.0 // 80
    let DuelChainDelayMaxSubTicks = secondsToSubTicks 5.0 // 200


    let SteeringIntervalSubTicks = secondsToSubTicks 0.2 // 8


    let MarkingIntervalSubTicks = secondsToSubTicks 3.0 // 120


    let CognitiveIntervalSubTicks = secondsToSubTicks 2.0 // 80


    let AdaptiveIntervalSubTicks = secondsToSubTicks 60.0 // 2400


    let HalfTimeSubTick = secondsToSubTicks (45.0 * 60.0) // 108,000
    let FullTimeSubTick = secondsToSubTicks (95.0 * 60.0) // 228,000  (90 + 5 stoppage)
    let MaxMatchSubTicks = FullTimeSubTick

    type SimulationMode =
        | Fast
        | Full
