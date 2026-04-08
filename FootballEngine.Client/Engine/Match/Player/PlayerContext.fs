namespace FootballEngine

open FootballEngine.Domain
open SimStateOps
open MatchSpatial

type AgentContext =
    { Me: Player
      MyCondition: int
      MyPos: Spatial
      BallState: BallPhysicsState
      Dir: AttackDir
      Phase: MatchPhase
      Zone: PitchZone
      NearestTeammate: (Player * Spatial) option
      NearestOpponent: (Player * Spatial) option
      BestPassTarget: (Player * Spatial) option
      DistToGoal: float
      GoalDiff: int
      Minute: int
      Urgency: float
      Tactics: TacticsConfig }

module AgentContext =

    let build (me: Player) (meIdx: int) (ctx: MatchContext) (state: SimState) : AgentContext =
        let dir = attackDirFor state.AttackingClub state

        let isHome =
            state.HomeSlots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = me.Id
                | _ -> false)

        let slots = if isHome then state.HomeSlots else state.AwaySlots

        let mutable myPos = kickOffSpatial
        let mutable myCond = 100

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s when s.Player.Id = me.Id ->
                myPos <- s.Pos
                myCond <- s.Condition
            | _ -> ()

        let zone = PitchZone.ofBallX state.Ball.Position.X dir
        let phase = phaseFromBallZone dir state.Ball.Position.X

        let ballState = state.Ball

        let nearestTM =
            findNearestTeammate me ctx state dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let nearestOpp =
            findNearestOpponent me ctx state dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let bestPass =
            findBestPassTarget me ctx state dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let distToGoal = AttackDir.distToGoal myPos.X dir
        let clubId = if isHome then ctx.Home.Id else ctx.Away.Id
        let gd = goalDiff clubId ctx state

        let tacticsCfg =
            tacticsConfig
                (if isHome then state.HomeTactics else state.AwayTactics)
                (if isHome then
                     state.HomeInstructions
                 else
                     state.AwayInstructions)

        { Me = me
          MyCondition = myCond
          MyPos = myPos
          BallState = ballState
          Dir = dir
          Phase = phase
          Zone = zone
          NearestTeammate = nearestTM
          NearestOpponent = nearestOpp
          BestPassTarget = bestPass
          DistToGoal = distToGoal
          GoalDiff = gd
          Minute = int (PhysicsContract.subTicksToSeconds state.SubTick / 60.0)
          Urgency = 1.0
          Tactics = tacticsCfg }
