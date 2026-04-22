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
      Team: TeamPerspective
      TeamHasBall: bool
      Phase: MatchPhase
      Zone: PitchZone
      NearestTeammate: (Player * Spatial) option
      NearestOpponent: (Player * Spatial) option
      BestPassTarget: (Player * Spatial) option
      DistToGoal: float<meter>
      GoalDiff: int
      Minute: int
      Urgency: float
      Tactics: TacticsConfig
      Decision: DecisionConfig
      BuildUp: BuildUpConfig
      Dribble: DribbleConfig
      PreviousIntent: MovementIntent option
      IntentLockExpiry: int }

module AgentContext =

    let build
        (me: Player)
        (profile: BehavioralProfile)
        (meIdx: int)
        (team: TeamPerspective)
        (previousIntent: MovementIntent option)
        (intentLockExpiry: int)
        (state: SimState)
        (clock: SimulationClock)
        (decision: DecisionConfig)
        (buildUp: BuildUpConfig)
        : AgentContext =
        let ballState = state.Ball
        let bX = ballState.Position.X

        let dir = team.AttackDir

        let phase = phaseFromBallZone dir bX
        let zone = ofBallX bX dir

        let gd =
            if team.ClubSide = HomeClub then
                state.HomeScore - state.AwayScore
            else
                state.AwayScore - state.HomeScore

        let teamHasBall =
            match ballState.Possession with
            | Possession.Owned(side, _) -> side = team.ClubSide
            | Possession.InFlight(side, _) -> side = team.ClubSide
            | Possession.SetPiece(side, _) -> side = team.ClubSide
            | Possession.Contest(side) -> side = team.ClubSide
            | Possession.Transition(side) -> side = team.ClubSide
            | Possession.Loose -> false

        let tacticsCfg =
            tacticsConfig
                (getTactics state team.ClubSide)
                (getInstructions state team.ClubSide)

        let nearestTM, nearestOpp, bestPass, distToGoal, myPos =
            let ownSlots = team.OwnSlots
            let oppSlots = team.OppSlots

            let myPos =
                match ownSlots[meIdx] with
                | PlayerSlot.Active s -> s.Pos
                | _ -> defaultSpatial 50.0<meter> 34.0<meter>

            let mutable nearestTM: (Player * Spatial) option = None
            let mutable nearestOpp: (Player * Spatial) option = None
            let mutable minDistTM = PhysicsContract.MaxDistanceSq
            let mutable minDistOpp = PhysicsContract.MaxDistanceSq

            for i = 0 to ownSlots.Length - 1 do
                if i <> meIdx then
                    match ownSlots[i] with
                    | PlayerSlot.Active s ->
                        let dx = s.Pos.X - myPos.X
                        let dy = s.Pos.Y - myPos.Y
                        let d = dx * dx + dy * dy

                        if d < minDistTM then
                            minDistTM <- d
                            nearestTM <- Some(s.Player, s.Pos)
                    | _ -> ()

            for i = 0 to oppSlots.Length - 1 do
                match oppSlots[i] with
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

            nearestTM, nearestOpp, bestPass, dist, myPos

        { Me = me
          Profile = profile
          MyCondition = me.Condition
          MyPos = myPos
          BallState = ballState
          Team = team
          TeamHasBall = teamHasBall
          Phase = phase
          Zone = zone
          NearestTeammate = nearestTM
          NearestOpponent = nearestOpp
          BestPassTarget = bestPass
          DistToGoal = distToGoal
          GoalDiff = gd
          Minute = int (SimulationClock.subTicksToSeconds clock state.SubTick / 60.0)
          Urgency = 1.0
          Tactics = tacticsCfg
          Decision = decision
          BuildUp = buildUp
          Dribble = state.Config.Dribble
          PreviousIntent = previousIntent
          IntentLockExpiry = intentLockExpiry }
