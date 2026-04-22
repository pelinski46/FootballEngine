namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.SimStateOps

type ActionContext =
    { Att     : TeamPerspective
      Def     : TeamPerspective
      Zone    : PitchZone
      Momentum: float }

module ActionContext =
    let build (ctx: MatchContext) (state: SimState) : ActionContext =
        let attSide = state.AttackingSide
        let att     = SimStateOps.buildTeamPerspective attSide ctx state
        let def     = SimStateOps.buildTeamPerspective (ClubSide.flip attSide) ctx state
        let zone    = SimStateOps.ofBallX state.Ball.Position.X att.AttackDir
        let momentum = PhysicsContract.momentumDelta att.AttackDir state.Momentum
        { Att      = att
          Def      = def
          Zone     = zone
          Momentum = momentum }
