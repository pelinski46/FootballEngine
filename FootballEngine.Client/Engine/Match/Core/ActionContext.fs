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
        let dir = attackDirFor state.AttackingSide state
        let attSide = state.AttackingSide
        let defSide = ClubSide.flip attSide
        let zone = SimStateOps.ofBallX state.Ball.Position.X dir

        let momentum = PhysicsContract.momentumDelta dir state.Momentum

        { Dir = dir
          AttSide = attSide
          DefSide = defSide
          Zone = zone
          AttBonus = HomeBonus.build attSide
          DefBonus = HomeBonus.build defSide
          Momentum = momentum }
