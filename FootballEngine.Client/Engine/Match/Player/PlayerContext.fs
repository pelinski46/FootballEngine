namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

type AgentContext =
    { Me: Player
      Profile: BehavioralProfile
      MyCondition: int
      MyPos: Spatial
      BallState: BallPhysicsState
      Dir: AttackDir
      Phase: MatchPhase
      Zone: PitchZone
      NearestTeammate: (Player * Spatial) option
      NearestOpponent: (Player * Spatial) option
      BestPassTarget: (Player * Spatial) option
      DistToGoal: float<meter>
      GoalDiff: int
      Minute: int
      Urgency: float
      Tactics: TacticsConfig }

module AgentContext =

    let build
        (me: Player)
        (profile: BehavioralProfile)
        (meIdx: int)
        (state: SimState)
        (clock: SimulationClock)
        : AgentContext =
        let ballState = state.Ball
        let bX = ballState.Position.X

        let dir =
            attackDirFor
                (if state.AttackingClub = HomeClub then
                     HomeClub
                 else
                     AwayClub)
                state

        let phase = phaseFromBallZone dir bX
        let zone = ofBallX bX dir

        let gd =
            if dir = LeftToRight then
                state.HomeScore - state.AwayScore
            else
                state.AwayScore - state.HomeScore

        let tacticsCfg =
            tacticsConfig
                (getTactics state (if dir = LeftToRight then HomeClub else AwayClub))
                (getInstructions state (if dir = LeftToRight then HomeClub else AwayClub))

        let nearestTM, nearestOpp, bestPass, distToGoal =
            let attSlots = getSlots state (if dir = LeftToRight then HomeClub else AwayClub)

            let defSlots = getSlots state (if dir = LeftToRight then AwayClub else HomeClub)

            let myPos =
                match attSlots[meIdx] with
                | PlayerSlot.Active s -> s.Pos
                | _ -> defaultSpatial 50.0<meter> 34.0<meter>

            let mutable nearestTM: (Player * Spatial) option = None
            let mutable nearestOpp: (Player * Spatial) option = None
            let mutable minDistTM = PhysicsContract.MaxDistanceSq
            let mutable minDistOpp = PhysicsContract.MaxDistanceSq

            for i = 0 to attSlots.Length - 1 do
                if i <> meIdx then
                    match attSlots[i] with
                    | PlayerSlot.Active s ->
                        let dx = s.Pos.X - myPos.X
                        let dy = s.Pos.Y - myPos.Y
                        let d = dx * dx + dy * dy

                        if d < minDistTM then
                            minDistTM <- d
                            nearestTM <- Some(s.Player, s.Pos)
                    | _ -> ()

            for i = 0 to defSlots.Length - 1 do
                match defSlots[i] with
                | PlayerSlot.Active s ->
                    let dx = s.Pos.X - myPos.X
                    let dy = s.Pos.Y - myPos.Y
                    let d = dx * dx + dy * dy

                    if d < minDistOpp then
                        minDistOpp <- d
                        nearestOpp <- Some(s.Player, s.Pos)
                | _ -> ()

            let goalX =
                if dir = LeftToRight then
                    PhysicsContract.PitchLength
                else
                    0.0<meter>

            let bestPass =
                MatchSpatial.findBestPassTarget me state dir
                |> Option.map (fun (p, _, (x, y)) -> p, SimStateOps.defaultSpatial x y)

            let dist = abs (myPos.X - goalX)

            nearestTM, nearestOpp, bestPass, dist

        { Me = me
          Profile = profile
          MyCondition = me.Condition
          MyPos =
            match state.Home.Slots[meIdx] with
            | PlayerSlot.Active s -> s.Pos
            | _ -> defaultSpatial 50.0<meter> 34.0<meter>
          BallState = ballState
          Dir = dir
          Phase = phase
          Zone = zone
          NearestTeammate = nearestTM
          NearestOpponent = nearestOpp
          BestPassTarget = bestPass
          DistToGoal = distToGoal
          GoalDiff = gd
          Minute = int (SimulationClock.subTicksToSeconds clock state.SubTick / 60.0)
          Urgency = 1.0
          Tactics = tacticsCfg }
