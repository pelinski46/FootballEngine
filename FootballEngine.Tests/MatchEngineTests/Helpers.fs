module FootballEngine.Tests.MatchEngineTests.Helpers

open System
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract

// ============================================================================
// Default stats blocks (reused by factories)
// ============================================================================

let defaultPhysical =
    { Acceleration = 10; Pace = 10; Agility = 10; Balance = 10; JumpingReach = 10; Stamina = 10; Strength = 10 }

let defaultTechnical =
    { Finishing = 10; LongShots = 10; Dribbling = 10; BallControl = 10; Passing = 10; Crossing = 10; Tackling = 10; Marking = 10; Heading = 10; FreeKick = 10; Penalty = 10 }

let defaultMental =
    { Aggression = 10; Composure = 10; Vision = 10; Positioning = 10; Bravery = 10; WorkRate = 10; Concentration = 10; Leadership = 10 }

let defaultGk =
    { Reflexes = 10; Handling = 10; Kicking = 10; OneOnOne = 10; AerialReach = 10 }

// ============================================================================
// Player factories
// ============================================================================

let makePlayer (id: PlayerId) (pos: Position) (skill: int) : Player =
    let s = Math.Clamp(skill, 1, 20)
    { Id = id
      Name = $"P{id}"
      Birthday = DateTime(1995, 1, 1)
      Nationality = "AR"
      Position = pos
      PreferredFoot = Right
      Height = 180
      Weight = 75
      Physical = { defaultPhysical with Acceleration = s; Pace = s; Agility = s; Balance = s; Strength = s; Stamina = s }
      Technical = { defaultTechnical with Finishing = s; Dribbling = s; BallControl = s; Passing = s; Tackling = s; Crossing = s; Heading = s }
      Mental = { defaultMental with Aggression = s; Composure = s; Vision = s; Positioning = s; WorkRate = s; Concentration = s }
      Goalkeeping = { defaultGk with Reflexes = s; Handling = s; OneOnOne = s; AerialReach = s }
      Condition = 100
      MatchFitness = 100
      Morale = 50
      Status = Available
      CurrentSkill = skill
      PotentialSkill = skill
      Reputation = 50
      Affiliation = Contracted(1, { Salary = 1000m; ExpiryYear = 2030 })
      TrainingSchedule = { Focus = TrainingAllRound; Intensity = TrainingLight } }

let makeGk (id: PlayerId) (skill: int) (reflexes: int) (handling: int) : Player =
    let p = makePlayer id GK skill
    { p with Goalkeeping = { p.Goalkeeping with Reflexes = reflexes; Handling = handling } }

let withTechnical (t: TechnicalStats) (p: Player) = { p with Technical = t }
let withMental (m: MentalStats) (p: Player) = { p with Mental = m }
let withPhysical (ph: PhysicalStats) (p: Player) = { p with Physical = ph }

let eliteAttacker id pos = makePlayer id pos 20
let worstAttacker id pos = makePlayer id pos 1
let elitePasser id pos = makePlayer id pos 20
let worstPasser id pos = makePlayer id pos 1
let eliteDribbler id pos = makePlayer id pos 20
let worstTackler id pos = makePlayer id pos 1
let highAggression id pos = 
    let p = makePlayer id pos 10
    { p with Mental = { p.Mental with Aggression = 20; Positioning = 1 } }
let eliteGk id = makeGk id 20 20 20
let weakGk id = makeGk id 1 1 1

// ============================================================================
// State builders
// ============================================================================

let spatialAt (x: float) (y: float) : Spatial =
    { X = x * 1.0<meter>; Y = y * 1.0<meter>; Z = 0.0<meter>
      Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

let homeClub = { Id = 100; Name = "Home"; Nationality = "AR"; Reputation = 50; PlayerIds = []; StaffIds = []; Budget = 1_000_000m; Morale = 50; BoardObjective = LeagueObjective MidTable }
let awayClub = { Id = 200; Name = "Away"; Nationality = "AR"; Reputation = 50; PlayerIds = []; StaffIds = []; Budget = 1_000_000m; Morale = 50; BoardObjective = LeagueObjective MidTable }

let buildState (homePlayers: Player[]) (homePos: (float * float)[]) (awayPlayers: Player[]) (awayPos: (float * float)[]) (ballX: float) (ballY: float) (phase: Possession) : MatchContext * SimState =
    let hSp = homePos |> Array.map (fun (x, y) -> spatialAt x y)
    let aSp = awayPos |> Array.map (fun (x, y) -> spatialAt x y)
    let ctx = { Home = homeClub; Away = awayClub; HomeCoach = Unchecked.defaultof<Staff>; AwayCoach = Unchecked.defaultof<Staff>; HomePlayers = homePlayers; AwayPlayers = awayPlayers; HomeBasePositions = hSp; AwayBasePositions = aSp; HomeChemistry = ChemistryGraph.init homePlayers.Length; AwayChemistry = ChemistryGraph.init awayPlayers.Length; IsKnockoutMatch = false; Config = BalanceConfig.defaultConfig }
    let state = SimState()
    state.HomeBasePositions <- hSp; state.AwayBasePositions <- aSp
    state.Ball <- { Position = spatialAt ballX ballY; Spin = Spin.zero; Possession = phase; LastTouchBy = None; PendingOffsideSnapshot = None; StationarySinceSubTick = None }
    state.Home <- { TeamSimState.empty with Slots = Array.init homePlayers.Length (fun i -> PlayerSlot.Active { Player = homePlayers[i]; Pos = hSp[i]; Condition = 100; Mental = MentalState.initial homePlayers[i]; MovementIntent = None; IntentLockExpiry = 0; Profile = Player.profile homePlayers[i]; CachedTarget = (hSp[i].X, hSp[i].Y); CachedExecution = 1.0 }) }
    state.Away <- { TeamSimState.empty with Slots = Array.init awayPlayers.Length (fun i -> PlayerSlot.Active { Player = awayPlayers[i]; Pos = aSp[i]; Condition = 100; Mental = MentalState.initial awayPlayers[i]; MovementIntent = None; IntentLockExpiry = 0; Profile = Player.profile awayPlayers[i]; CachedTarget = (aSp[i].X, aSp[i].Y); CachedExecution = 1.0 }) }
    ctx, state

// ============================================================================
// Event helpers
// ============================================================================

let hasEventType (t: MatchEventType) (events: MatchEvent list) = events |> List.exists (fun e -> e.Type = t)
let countEventType (t: MatchEventType) (events: MatchEvent list) = events |> List.filter (fun e -> e.Type = t) |> List.length
let countFreeKicks events = events |> List.filter (fun e -> match e.Type with MatchEventType.FreeKick _ -> true | _ -> false) |> List.length
let hasGoal events = hasEventType MatchEventType.Goal events
let hasFoul events = hasEventType MatchEventType.FoulCommitted events
let hasSave events = hasEventType MatchEventType.Save events

let mkPhysicsTick subTick = 
    { SchedulingTypes.ScheduledTick.SubTick = subTick
      SchedulingTypes.ScheduledTick.Priority = SchedulingTypes.TickPriority.Physics
      SchedulingTypes.ScheduledTick.SequenceId = 0L
      SchedulingTypes.ScheduledTick.Kind = SchedulingTypes.TickKind.PhysicsTick }

let mkSnap () =
    { PasserId = 1
      ReceiverId = 2
      ReceiverXAtPass = 80.0<meter>
      SecondLastDefenderX = 75.0<meter>
      BallXAtPass = 52.5<meter>
      Dir = LeftToRight }
