namespace FootballEngine

open FootballEngine.Domain

/// Pre-computed context for a single attacking action
type ActionContext =
    { Dir: AttackDir
      AttSide: ClubSide
      DefSide: ClubSide
      Zone: PitchZone
      AttBonus: HomeBonus
      DefBonus: HomeBonus
      Momentum: float } // already sign-adjusted for the attacking team

module ActionContext =
    let build (s: MatchState) : ActionContext =
        let dir = AttackDir.ofClubSide s.AttackingClub
        let attSide = s.AttackingClub
        let defSide = ClubSide.flip attSide
        let zone = PitchZone.ofBallX s.Ball.Position.X dir

        let momentum = AttackDir.momentumDelta dir s.Momentum

        { Dir = dir
          AttSide = attSide
          DefSide = defSide
          Zone = zone
          AttBonus = HomeBonus.build attSide
          DefBonus = HomeBonus.build defSide
          Momentum = momentum }
