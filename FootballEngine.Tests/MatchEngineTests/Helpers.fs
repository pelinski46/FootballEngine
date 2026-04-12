module FootballEngine.Tests.MatchEngineTests.Helpers

open FootballEngine
open FootballEngine.Domain

// ============================================================================
// Default stats blocks (reused by factories)
// ============================================================================

let defaultPhysical =
    { Acceleration = 10
      Pace = 10
      Agility = 10
      Balance = 10
      JumpingReach = 10
      Stamina = 10
      Strength = 10 }

let defaultTechnical =
    { Finishing = 10
      LongShots = 10
      Dribbling = 10
      BallControl = 10
      Passing = 10
      Crossing = 10
      Tackling = 10
      Marking = 10
      Heading = 10
      FreeKick = 10
      Penalty = 10 }

let defaultMental =
    { Aggression = 10
      Composure = 10
      Vision = 10
      Positioning = 10
      Bravery = 10
      WorkRate = 10
      Concentration = 10
      Leadership = 10 }

let defaultGk =
    { Reflexes = 10
      Handling = 10
      Kicking = 10
      OneOnOne = 10
      AerialReach = 10 }

// ============================================================================
// Player factories
// ============================================================================

let makePlayer (id: PlayerId) (pos: Position) (skill: int) : Player =
    { Id = id
      Name = $"P{id}"
      Birthday = System.DateTime(1995, 1, 1)
      Nationality = "AR"
      Position = pos
      PreferredFoot = Right
      Height = 180
      Weight = 75
      Physical = defaultPhysical
      Technical = defaultTechnical
      Mental = defaultMental
      Goalkeeping = defaultGk
      Condition = 100
      MatchFitness = 100
      Morale = 50
      Status = Available
      CurrentSkill = skill
      PotentialSkill = skill
      Reputation = 50
      Affiliation = Contracted(1, { Salary = 1000m; ExpiryYear = 2030 })
      TrainingSchedule =
        { Focus = TrainingAllRound
          Intensity = TrainingLight } }

let makeGk (id: PlayerId) (skill: int) (reflexes: int) (handling: int) : Player =
    makePlayer id GK skill
    |> fun p ->
        { p with
            Goalkeeping =
                { p.Goalkeeping with
                    Reflexes = reflexes
                    Handling = handling } }

let withTechnical (t: TechnicalStats) (p: Player) = { p with Technical = t }
let withMental (m: MentalStats) (p: Player) = { p with Mental = m }
let withPhysical (ph: PhysicalStats) (p: Player) = { p with Physical = ph }

let eliteAttacker id pos =
    makePlayer id pos 20
    |> withTechnical
        { defaultTechnical with
            Finishing = 20
            BallControl = 20 }
    |> withMental { defaultMental with Composure = 20 }

let worstAttacker id pos =
    makePlayer id pos 1
    |> withTechnical
        { defaultTechnical with
            Finishing = 1
            BallControl = 1 }
    |> withMental { defaultMental with Composure = 1 }

let elitePasser id pos =
    makePlayer id pos 20
    |> withTechnical { defaultTechnical with Passing = 20 }
    |> withMental { defaultMental with Vision = 20 }

let worstPasser id pos =
    makePlayer id pos 1
    |> withTechnical { defaultTechnical with Passing = 1 }
    |> withMental { defaultMental with Vision = 1 }

let eliteDribbler id pos =
    makePlayer id pos 20
    |> withTechnical { defaultTechnical with Dribbling = 20 }
    |> withPhysical
        { defaultPhysical with
            Agility = 20
            Balance = 20 }

let worstTackler id pos =
    makePlayer id pos 1
    |> withTechnical { defaultTechnical with Tackling = 1 }
    |> withPhysical { defaultPhysical with Strength = 1 }

let highAggression id pos =
    makePlayer id pos 10
    |> withMental
        { defaultMental with
            Aggression = 20
            Positioning = 1 }

let eliteGk id = makeGk id 20 20 20
let weakGk id = makeGk id 1 1 1

// ============================================================================
// Club stubs
// ============================================================================

let homeClub =
    { Id = 100
      Name = "Home"
      Nationality = "AR"
      Reputation = 50
      PlayerIds = []
      StaffIds = []
      Budget = 1_000_000m
      Morale = 50
      BoardObjective = LeagueObjective MidTable }

let awayClub =
    { Id = 200
      Name = "Away"
      Nationality = "AR"
      Reputation = 50
      PlayerIds = []
      StaffIds = []
      Budget = 1_000_000m
      Morale = 50
      BoardObjective = LeagueObjective MidTable }

// ============================================================================
// State builder
// ============================================================================

let spatialAt x y : Spatial =
    { X = x
      Y = y
      Z = 0.0
      Vx = 0.0
      Vy = 0.0
      Vz = 0.0 }

let buildState
    (homePlayers: Player[])
    (homePos: (float * float)[])
    (awayPlayers: Player[])
    (awayPos: (float * float)[])
    (ballX: float)
    (ballY: float)
    (phase: PossessionPhase)
    : MatchContext * SimState =

    let hSp = homePos |> Array.map (fun (x, y) -> spatialAt x y)
    let aSp = awayPos |> Array.map (fun (x, y) -> spatialAt x y)

    let ctx =
        { Home = homeClub
          Away = awayClub
          HomeCoach = Unchecked.defaultof<Staff>
          AwayCoach = Unchecked.defaultof<Staff>
          HomePlayers = homePlayers
          AwayPlayers = awayPlayers
          HomeBasePositions = hSp
          AwayBasePositions = aSp
          HomeChemistry = ChemistryGraph.init homePlayers.Length
          AwayChemistry = ChemistryGraph.init awayPlayers.Length
          IsKnockoutMatch = false }

    let state = SimState()
    state.SubTick <- 0
    state.HomeScore <- 0
    state.AwayScore <- 0
    state.Momentum <- 0.0
    state.HomeAttackDir <- LeftToRight
    state.HomeBasePositions <- hSp
    state.AwayBasePositions <- aSp

    state.Ball <-
        { Position = spatialAt ballX ballY
          Spin = Spin.zero
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true
          Phase = phase
          PendingOffsideSnapshot = None }

    state.Home <-
        { TeamSimState.empty with
            Slots =
                Array.init homePlayers.Length (fun i ->
                    PlayerSlot.Active
                        { Player = homePlayers[i]
                          Pos = hSp[i]
                          Condition = homePlayers[i].Condition
                          Mental = MentalState.initial homePlayers[i]
                          Directives = Array.empty
                          Profile = Player.profile homePlayers[i] }) }

    state.Away <-
        { TeamSimState.empty with
            Slots =
                Array.init awayPlayers.Length (fun i ->
                    PlayerSlot.Active
                        { Player = awayPlayers[i]
                          Pos = aSp[i]
                          Condition = awayPlayers[i].Condition
                          Mental = MentalState.initial awayPlayers[i]
                          Directives = Array.empty
                          Profile = Player.profile awayPlayers[i] }) }

    ctx, state

let mkSnap () =
    { PasserId = 1
      ReceiverId = 2
      ReceiverXAtPass = 80.0
      SecondLastDefenderX = 75.0
      BallXAtPass = 52.5
      Dir = LeftToRight }

// ============================================================================
// Action resolution helper
// ============================================================================

let resolveAction
    (clubId: ClubId)
    (subTick: int)
    (action: PlayerAction)
    (ctx: MatchContext)
    (state: SimState)
    : MatchEvent list =
    PlayerAgent.resolve clubId subTick action ctx state

// ============================================================================
// Event helpers
// ============================================================================

let hasEventType (t: MatchEventType) (events: MatchEvent list) =
    events |> List.exists (fun e -> e.Type = t)

let countEventType (t: MatchEventType) (events: MatchEvent list) =
    events |> List.filter (fun e -> e.Type = t) |> List.length

let countFreeKicks events =
    events |> List.filter (fun e -> match e.Type with MatchEventType.FreeKick _ -> true | _ -> false) |> List.length

let hasGoal events = hasEventType MatchEventType.Goal events

let hasFoul events =
    hasEventType MatchEventType.FoulCommitted events

let hasSave events = hasEventType MatchEventType.Save events
