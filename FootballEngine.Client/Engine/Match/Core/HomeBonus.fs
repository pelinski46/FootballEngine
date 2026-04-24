namespace FootballEngine

open FootballEngine.Domain

[<Struct>]
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
    let build (clubSide: ClubSide) (config: HomeAdvantageConfig) : HomeBonus =
        let isHome = clubSide = HomeClub
        { AttackDuel   = if isHome then config.DuelAttackBonus else 0.0
          DefendDuel   = if isHome then config.DuelDefenseBonus else 0.0
          Tackle       = if isHome then config.TackleBonus else 0.0
          PassAcc      = if isHome then config.PassAccuracyBonus else 0.0
          ShotCompos   = if isHome then config.ShotComposureBonus else 0.0
          SetPlay      = if isHome then config.SetPlayAccuracyBonus else 0.0
          FreeKick     = if isHome then config.FreeKickComposure else 0.0
          Penalty      = if isHome then config.PenaltyBonus else 0.0
          CardReduc    = if isHome then config.CardReduction else 0.0
          FatigueReduc = if isHome then config.FatigueReduction else 0.0 }
