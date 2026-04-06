namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SimStateOps

type ActionContext =
    { Dir: AttackDir
      AttSide: ClubSide
      DefSide: ClubSide
      Zone: PitchZone
      AttBonus: HomeBonus
      DefBonus: HomeBonus
      Momentum: float }

module ActionContext =
    let build (state: SimState) : ActionContext =
        let dir = attackDirFor state.AttackingClub state
        let attSide = state.AttackingClub
        let defSide = ClubSide.flip attSide
        let zone = PitchZone.ofBallX state.Ball.Position.X dir

        let momentum = AttackDir.momentumDelta dir state.Momentum

        { Dir = dir
          AttSide = attSide
          DefSide = defSide
          Zone = zone
          AttBonus = HomeBonus.build attSide
          DefBonus = HomeBonus.build defSide
          Momentum = momentum }
