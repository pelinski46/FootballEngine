namespace FootballEngine

open FootballEngine.Domain
open MatchStateOps
open MatchSpatial

type AgentContext =
    { Me: Player
      MyCondition: int
      MyPos: Spatial
      BallState: BallPhysicsState
      Dir: AttackDir
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

    let build (me: Player) (meIdx: int) (s: MatchState) : AgentContext =
        let dir = AttackDir.ofClubSide s.AttackingClub
        let attSide = ClubSide.teamSide s.AttackingClub s
        let myPos = attSide.Positions[meIdx]
        let myCond = attSide.Conditions[meIdx]
        let zone = PitchZone.ofBallX myPos.X dir

        let ballState =
            { Position = s.Ball.Position
              Spin = s.Ball.Spin
              LastTouchBy = s.Ball.LastTouchBy
              IsInPlay = s.Ball.IsInPlay }

        let nearestTM =
            findNearestTeammate me s dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let nearestOpp =
            findNearestOpponent me s dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let bestPass =
            findBestPassTarget me s dir
            |> Option.map (fun (p, _, xy) ->
                p,
                { X = fst xy
                  Y = snd xy
                  Z = 0.0
                  Vx = 0.0
                  Vy = 0.0
                  Vz = 0.0 })

        let distToGoal = AttackDir.distToGoal myPos.X dir
        let gd = goalDiff (ClubSide.toClubId s.AttackingClub s) s
        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions

        { Me = me
          MyCondition = myCond
          MyPos = myPos
          BallState = ballState
          Dir = dir
          Zone = zone
          NearestTeammate = nearestTM
          NearestOpponent = nearestOpp
          BestPassTarget = bestPass
          DistToGoal = distToGoal
          GoalDiff = gd
          Minute = s.Second / 60
          Urgency = 1.0
          Tactics = tacticsCfg }
