namespace FootballEngine.Simulation

open FootballEngine.Domain

type MatchMomentum =
    { HomeMomentum: float
      AwayMomentum: float
      LastEventSubTick: int
      ConsecutiveHomeEvents: int
      ConsecutiveAwayEvents: int }

module DynamicMomentum =

    let initial =
        { HomeMomentum = 0.0
          AwayMomentum = 0.0
          LastEventSubTick = 0
          ConsecutiveHomeEvents = 0
          ConsecutiveAwayEvents = 0 }

    let update (momentum: MatchMomentum) (subTick: int) (eventClub: ClubSide) : MatchMomentum =
        let delta = 0.5
        let decay = 0.02

        let (homeM, awayM, homeC, awayC) =
            match eventClub with
            | HomeClub ->
                (momentum.HomeMomentum + delta, momentum.AwayMomentum - decay, momentum.ConsecutiveHomeEvents + 1, 0)
            | AwayClub ->
                (momentum.HomeMomentum - decay, momentum.AwayMomentum + delta, 0, momentum.ConsecutiveAwayEvents + 1)

        { HomeMomentum = max -10.0 (min 10.0 homeM)
          AwayMomentum = max -10.0 (min 10.0 awayM)
          LastEventSubTick = subTick
          ConsecutiveHomeEvents = homeC
          ConsecutiveAwayEvents = awayC }
