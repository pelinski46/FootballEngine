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
        let att     = buildTeamPerspective attSide ctx state
        let def     = buildTeamPerspective (ClubSide.flip attSide) ctx state
        let zone    = ofBallX state.Ball.Position.X att.AttackDir
        let momentum = PhysicsContract.momentumDelta att.AttackDir state.Momentum
        { Att      = att
          Def      = def
          Zone     = zone
          Momentum = momentum }
