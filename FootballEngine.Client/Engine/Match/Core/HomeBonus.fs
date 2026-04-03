namespace FootballEngine

open FootballEngine.Domain

type HomeBonus = {
    AttackDuel   : float
    DefendDuel   : float
    Tackle       : float
    PassAcc      : float
    ShotCompos   : float
    SetPlay      : float
    FreeKick     : float
    Penalty      : float
    CardReduc    : float
    FatigueReduc : float
}

module HomeBonus =
    let build (clubSide: ClubSide) : HomeBonus =
        let isHome = clubSide = HomeClub
        { AttackDuel   = if isHome then BalanceConfig.HomeDuelAttackBonus else 0.0
          DefendDuel   = if isHome then BalanceConfig.HomeDuelDefenseBonus else 0.0
          Tackle       = if isHome then BalanceConfig.HomeTackleBonus else 0.0
          PassAcc      = if isHome then BalanceConfig.HomePassAccuracyBonus else 0.0
          ShotCompos   = if isHome then BalanceConfig.HomeShotComposureBonus else 0.0
          SetPlay      = if isHome then BalanceConfig.HomeSetPlayAccuracyBonus else 0.0
          FreeKick     = if isHome then BalanceConfig.HomeFreeKickComposure else 0.0
          Penalty      = if isHome then BalanceConfig.HomePenaltyBonus else 0.0
          CardReduc    = if isHome then BalanceConfig.HomeCardReduction else 0.0
          FatigueReduc = if isHome then BalanceConfig.HomeFatigueReduction else 0.0 }
