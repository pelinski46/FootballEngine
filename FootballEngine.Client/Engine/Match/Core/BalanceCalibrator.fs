module BalanceCalibrator

open FootballEngine

type ActionThresholds = {
    Shot: float
    Pass: float
    Dribble: float
    Cross: float
    LongBall: float
}

let computeThresholds (config: MatchVolumeConfig) =
    let total =
        config.TargetShotsPerMatch
        + config.TargetDribblesPerMatch
        + config.TargetPassesPerMatch
        + config.TargetCrossesPerMatch
        + config.TargetLongBallsPerMatch

    let frac x = x / total

    { Shot = frac config.TargetShotsPerMatch
      Pass = frac config.TargetPassesPerMatch
      Dribble = frac config.TargetDribblesPerMatch
      Cross = frac config.TargetCrossesPerMatch
      LongBall = frac config.TargetLongBallsPerMatch }

let cumulative (t: ActionThresholds) =
    t.Shot,
    t.Shot + t.Pass,
    t.Shot + t.Pass + t.Dribble,
    t.Shot + t.Pass + t.Dribble + t.Cross
