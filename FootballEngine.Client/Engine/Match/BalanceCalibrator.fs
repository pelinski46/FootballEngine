module BalanceCalibrator

type ActionThresholds = {
    Shot: float
    Pass: float
    Dribble: float
    Cross: float
    LongBall: float
}

let computeThresholds () =
    let total =
        BalanceConfig.TargetShotsPerMatch
        + BalanceConfig.TargetDribblesPerMatch
        + BalanceConfig.TargetPassesPerMatch
        + BalanceConfig.TargetCrossesPerMatch
        + BalanceConfig.TargetLongBallsPerMatch

    let frac x = x / total

    { Shot = frac BalanceConfig.TargetShotsPerMatch
      Pass = frac BalanceConfig.TargetPassesPerMatch
      Dribble = frac BalanceConfig.TargetDribblesPerMatch
      Cross = frac BalanceConfig.TargetCrossesPerMatch
      LongBall = frac BalanceConfig.TargetLongBallsPerMatch }

let cumulative (t: ActionThresholds) =
    t.Shot,
    t.Shot + t.Pass,
    t.Shot + t.Pass + t.Dribble,
    t.Shot + t.Pass + t.Dribble + t.Cross
