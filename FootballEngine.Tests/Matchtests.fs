module FootballEngine.Tests.MatchTests

open System
open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open SimStateOps
open FootballEngine.MatchSpatial
open FootballEngine.PlayerAgent
open FootballEngine.Tests.Helpers

// ============================================================================
// Fixtures
// ============================================================================

let private defaultPhysical =
    { Acceleration = 10
      Pace = 10
      Agility = 10
      Balance = 10
      JumpingReach = 10
      Stamina = 10
      Strength = 10 }

let private defaultTechnical =
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

let private defaultMental =
    { Aggression = 10
      Composure = 10
      Vision = 10
      Positioning = 10
      Bravery = 10
      WorkRate = 10
      Concentration = 10
      Leadership = 10 }

let private defaultGk =
    { Reflexes = 10
      Handling = 10
      Kicking = 10
      OneOnOne = 10
      AerialReach = 10 }

let private weakGk =
    { Reflexes = 1
      Handling = 1
      Kicking = 1
      OneOnOne = 1
      AerialReach = 1 }

let private strongGk =
    { Reflexes = 20
      Handling = 20
      Kicking = 20
      OneOnOne = 20
      AerialReach = 20 }

let private makePlayer id pos skill =
    { Id = id
      Name = $"P{id}"
      Birthday = DateTime(1995, 1, 1)
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

let private withTechnical t (p: Player) = { p with Technical = t }
let private withMental m (p: Player) = { p with Mental = m }
let private withPhysical ph (p: Player) = { p with Physical = ph }
let private withGk gk (p: Player) = { p with Goalkeeping = gk }

let private makeGk id skill gkStats =
    makePlayer id GK skill |> withGk gkStats

let private eliteAttacker id pos =
    makePlayer id pos 20
    |> withTechnical { defaultTechnical with Finishing = 20 }
    |> withMental { defaultMental with Composure = 20 }

let private worstAttacker id pos =
    makePlayer id pos 1
    |> withTechnical { defaultTechnical with Finishing = 1 }
    |> withMental { defaultMental with Composure = 1 }

let private elitePasser id pos =
    makePlayer id pos 20
    |> withTechnical { defaultTechnical with Passing = 20 }
    |> withMental { defaultMental with Vision = 20 }

let private worstPasser id pos =
    makePlayer id pos 1
    |> withTechnical { defaultTechnical with Passing = 1 }
    |> withMental { defaultMental with Vision = 1 }

let private eliteDribbler id pos =
    makePlayer id pos 20
    |> withTechnical { defaultTechnical with Dribbling = 20 }
    |> withPhysical
        { defaultPhysical with
            Agility = 20
            Balance = 20 }

let private worstTackler id pos =
    makePlayer id pos 1
    |> withTechnical { defaultTechnical with Tackling = 1 }
    |> withPhysical { defaultPhysical with Strength = 1 }

let private highAggression id pos =
    makePlayer id pos 10
    |> withMental
        { defaultMental with
            Aggression = 20
            Positioning = 1 }

let private homeClub =
    { Id = 1
      Name = "Home"
      Nationality = "AR"
      Reputation = 50
      PlayerIds = []
      StaffIds = []
      Budget = 1_000_000m
      Morale = 50
      BoardObjective = LeagueObjective MidTable }

let private awayClub =
    { Id = 2
      Name = "Away"
      Nationality = "AR"
      Reputation = 50
      PlayerIds = []
      StaffIds = []
      Budget = 1_000_000m
      Morale = 50
      BoardObjective = LeagueObjective MidTable }

let private defaultInstructions = TacticalInstructions.defaultInstructions

let private spatialAt x y =
    { X = x
      Y = y
      Z = 0.0
      Vx = 0.0
      Vy = 0.0
      Vz = 0.0 }

let private buildState
    (homePlayers: Player[])
    (homePos: (float * float)[])
    (awayPlayers: Player[])
    (awayPos: (float * float)[])
    (ballX: float)
    (ballY: float)
    (attacking: ClubSide)
    =

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
          IsKnockoutMatch = false }

    let state = SimState()

    state.HomeSlots <-
        Array.init homePlayers.Length (fun i ->
            let p = homePlayers[i]

            PlayerSlot.Active
                { Player = p
                  Pos = hSp[i]
                  Condition = p.Condition
                  Mental = MentalState.initial p
                  Directives = Array.empty })

    state.AwaySlots <-
        Array.init awayPlayers.Length (fun i ->
            let p = awayPlayers[i]

            PlayerSlot.Active
                { Player = p
                  Pos = aSp[i]
                  Condition = p.Condition
                  Mental = MentalState.initial p
                  Directives = Array.empty })

    state.HomeSidelined <- Map.empty
    state.AwaySidelined <- Map.empty
    state.HomeYellows <- Map.empty
    state.AwayYellows <- Map.empty
    state.HomeInstructions <- Some defaultInstructions
    state.AwayInstructions <- Some defaultInstructions
    state.HomeActiveRuns <- []
    state.AwayActiveRuns <- []
    state.HomeChemistry <- ChemistryGraph.init homePlayers.Length
    state.AwayChemistry <- ChemistryGraph.init awayPlayers.Length
    state.HomeEmergentState <- EmergentState.initial
    state.AwayEmergentState <- EmergentState.initial
    state.HomeAdaptiveState <- AdaptiveTactics.initial
    state.AwayAdaptiveState <- AdaptiveTactics.initial

    state.Ball <-
        { Position = spatialAt ballX ballY
          Spin = Spin.zero
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true }

    state.AttackingClub <- attacking
    ctx, state

// Helpers
let private resolveAs clubId action (ctx, state) = resolve clubId 1 action ctx state
let private resolveHome = resolveAs homeClub.Id
let private resolveAway = resolveAs awayClub.Id

let private hasEventType t events =
    events |> List.exists (fun e -> e.Type = t)

let private countEventType t events =
    events |> List.filter (fun e -> e.Type = t) |> List.length

let private hasShotEvent events =
    events
    |> List.exists (fun e ->
        match e.Type with
        | Goal
        | ShotOffTarget
        | ShotBlocked
        | Save -> true
        | _ -> false)

let private hasPassEvent events =
    events
    |> List.exists (fun e ->
        match e.Type with
        | PassCompleted _ -> true
        | _ -> false)

let private hasFreeKickEvent events =
    events
    |> List.exists (fun e ->
        match e.Type with
        | FreeKick _ -> true
        | _ -> false)

/// Run `action` N times, return the list of per-run event lists
let private trials n action setup = Array.init n (fun _ -> action setup)

/// Probability estimate over N trials
let private eventRate t n action setup =
    trials n action setup
    |> Array.filter (hasEventType t)
    |> Array.length
    |> fun k -> float k / float n


// ============================================================================
// 1. SimStateOps — pure unit tests
// ============================================================================

let simStateOpsTests =
    testList
        "SimStateOps"
        [

          test "ClubSide.flip is an involution" {
              Expect.equal (ClubSide.flip HomeClub) AwayClub ""
              Expect.equal (ClubSide.flip AwayClub) HomeClub ""
              Expect.equal (ClubSide.flip (ClubSide.flip HomeClub)) HomeClub ""
          }

          test "phaseFromBallZone LeftToRight: x=10→BuildUp, x=50→Midfield, x=80→Attack" {
              Expect.equal (phaseFromBallZone LeftToRight 10.0) BuildUp ""
              Expect.equal (phaseFromBallZone LeftToRight 50.0) Midfield ""
              Expect.equal (phaseFromBallZone LeftToRight 80.0) Attack ""
          }

          test "phaseFromBallZone RightToLeft mirrors LeftToRight" {
              Expect.equal (phaseFromBallZone RightToLeft 95.0) BuildUp ""
              Expect.equal (phaseFromBallZone RightToLeft 20.0) Attack ""
          }

          test "pressureMultiplier: losing > 1.0, winning < 1.0, clamped [0.1, 2.0]" {
              let ctx =
                  { Home = homeClub
                    Away = awayClub
                    HomeCoach = Unchecked.defaultof<_>
                    AwayCoach = Unchecked.defaultof<_>
                    HomePlayers = [||]
                    AwayPlayers = [||]
                    HomeBasePositions = [||]
                    AwayBasePositions = [||]
                    IsKnockoutMatch = false }

              let s02 =
                  let s = SimState() in
                  s.HomeScore <- 0
                  s.AwayScore <- 2
                  s

              let s20 =
                  let s = SimState() in
                  s.HomeScore <- 2
                  s.AwayScore <- 0
                  s

              let s03 =
                  let s = SimState() in
                  s.HomeScore <- 0
                  s.AwayScore <- 3
                  s

              Expect.isTrue (pressureMultiplier homeClub.Id ctx s02 > 1.0) "losing home should increase pressure"
              Expect.isTrue (pressureMultiplier homeClub.Id ctx s20 < 1.0) "winning home should decrease pressure"
              Expect.isTrue (pressureMultiplier awayClub.Id ctx s20 > 1.0) "losing away should increase pressure"
              let losing = pressureMultiplier homeClub.Id ctx s03
              let winning = pressureMultiplier homeClub.Id ctx s20
              Expect.isTrue (losing >= 0.1 && losing <= 2.0) $"losing={losing}"
              Expect.isTrue (winning >= 0.1 && winning <= 2.0) $"winning={winning}"
          }

          test "matchUrgency: 0-0 early ~1.0, 0-0 late > 1.0, losing late >> 1.0, winning < 1.0" {
              let ctx =
                  { Home = homeClub
                    Away = awayClub
                    HomeCoach = Unchecked.defaultof<_>
                    AwayCoach = Unchecked.defaultof<_>
                    HomePlayers = [||]
                    AwayPlayers = [||]
                    HomeBasePositions = [||]
                    AwayBasePositions = [||]
                    IsKnockoutMatch = false }

              let at score1 score2 subTick =
                  let s = SimState()
                  s.HomeScore <- score1
                  s.AwayScore <- score2
                  s.SubTick <- subTick
                  s

              let earlyDraw = matchUrgency homeClub.Id ctx (at 0 0 0)
              let lateDraw = matchUrgency homeClub.Id ctx (at 0 0 160000)
              let lateLoosing = matchUrgency homeClub.Id ctx (at 0 2 160000)
              let winning = matchUrgency homeClub.Id ctx (at 2 0 0)
              Expect.isTrue (earlyDraw >= 0.8 && earlyDraw <= 1.2) $"early draw urgency={earlyDraw}"
              Expect.isTrue (lateDraw > 1.0) $"late draw urgency={lateDraw}"
              Expect.isTrue (lateLoosing > lateDraw) $"late losing={lateLoosing} should be > late draw={lateDraw}"
              Expect.isTrue (winning < 1.0) $"winning urgency={winning}"
          }

          test "adjustMomentum: positive delta increases, clamps to [-10, 10]" {
              let s = SimState()
              s.Momentum <- 0.0
              adjustMomentum LeftToRight 2.0 s
              Expect.isTrue (s.Momentum > 0.0) $"momentum should increase: {s.Momentum}"
              s.Momentum <- 9.0
              adjustMomentum LeftToRight 5.0 s
              Expect.isTrue (s.Momentum <= 10.0) $"momentum should clamp at 10: {s.Momentum}"
              s.Momentum <- -9.0
              adjustMomentum RightToLeft 5.0 s
              Expect.isTrue (s.Momentum >= -10.0) $"momentum should clamp at -10: {s.Momentum}"
          }

          test "goalDiff is antisymmetric" {
              let ctx =
                  { Home = homeClub
                    Away = awayClub
                    HomeCoach = Unchecked.defaultof<_>
                    AwayCoach = Unchecked.defaultof<_>
                    HomePlayers = [||]
                    AwayPlayers = [||]
                    HomeBasePositions = [||]
                    AwayBasePositions = [||]
                    IsKnockoutMatch = false }

              let s = SimState()
              s.HomeScore <- 3
              s.AwayScore <- 1
              let hd = goalDiff homeClub.Id ctx s
              let ad = goalDiff awayClub.Id ctx s
              Expect.equal hd 2 $"home goal diff: {hd}"
              Expect.equal ad -2 $"away goal diff: {ad}"
              Expect.equal (hd + ad) 0 "goal diffs should cancel"
          }

          test "awardGoal HomeClub: increments HomeScore, ball returns to centre, emits Goal when scorer provided" {
              let ctx =
                  { Home = homeClub
                    Away = awayClub
                    HomeCoach = Unchecked.defaultof<_>
                    AwayCoach = Unchecked.defaultof<_>
                    HomePlayers = [||]
                    AwayPlayers = [||]
                    HomeBasePositions = [||]
                    AwayBasePositions = [||]
                    IsKnockoutMatch = false }

              let s = SimState()
              s.HomeScore <- 0
              s.AwayScore <- 0
              let evNoScorer = awardGoal HomeClub None 0 ctx s
              Expect.equal s.HomeScore 1 "home score should be 1"
              Expect.equal s.AwayScore 0 "away score unchanged"
              Expect.equal s.Ball.Position.X PhysicsContract.HalfwayLineX "ball X should be halfway"
              Expect.equal s.Ball.Position.Y (PhysicsContract.PitchWidth / 2.0) "ball Y should be centre"
              Expect.isEmpty evNoScorer "no scorer → no Goal event"

              s.HomeScore <- 0
              s.AwayScore <- 0
              let evWithScorer = awardGoal HomeClub (Some 99) 0 ctx s
              Expect.exists evWithScorer (fun e -> e.Type = Goal) "scorer provided → Goal event expected"
          }

          test "awardGoal AwayClub: increments AwayScore only" {
              let ctx =
                  { Home = homeClub
                    Away = awayClub
                    HomeCoach = Unchecked.defaultof<_>
                    AwayCoach = Unchecked.defaultof<_>
                    HomePlayers = [||]
                    AwayPlayers = [||]
                    HomeBasePositions = [||]
                    AwayBasePositions = [||]
                    IsKnockoutMatch = false }

              let s = SimState()
              s.HomeScore <- 0
              s.AwayScore <- 0
              awardGoal AwayClub None 0 ctx s |> ignore
              Expect.equal s.AwayScore 1 ""
              Expect.equal s.HomeScore 0 ""
          }

          test "resetBallToCenter: position, velocity, ownership all reset" {
              let s = SimState()

              s.Ball <-
                  { Position = spatialAt 80.0 30.0
                    Spin = Spin.zero
                    ControlledBy = Some 1
                    LastTouchBy = Some 1
                    IsInPlay = true }

              resetBallToCenter s
              Expect.equal s.Ball.Position.X PhysicsContract.HalfwayLineX ""
              Expect.equal s.Ball.Position.Y (PhysicsContract.PitchWidth / 2.0) ""
              Expect.equal s.Ball.Position.Vx 0.0 ""
              Expect.equal s.Ball.Position.Vy 0.0 ""
              Expect.equal s.Ball.ControlledBy None ""
              Expect.equal s.Ball.LastTouchBy None ""
          }

          test "subTicksToSeconds: 1200 SubTicks = 30s, FullTimeSubTick = 95 min" {
              Expect.equal (PhysicsContract.subTicksToSeconds 1200) 30.0 ""
              Expect.equal (PhysicsContract.subTicksToSeconds PhysicsContract.FullTimeSubTick) (95.0 * 60.0) ""
          } ]


// ============================================================================
// 2. MatchSpatial — spatial geometry
// ============================================================================

let matchSpatialTests =
    testList
        "MatchSpatial"
        [

          test "teamRoster zips players, positions and conditions" {
              let players = [| makePlayer 1 ST 10; makePlayer 2 MC 10; makePlayer 3 GK 10 |]
              let positions = [| spatialAt 10.0 34.0; spatialAt 52.5 34.0; spatialAt 5.0 34.0 |]
              let conditions = [| 100; 100; 100 |]
              let roster = teamRoster players positions conditions
              Expect.equal roster.Length 3 ""
              let p, sp, c = roster[0]
              Expect.equal p.Id 1 ""
              Expect.equal sp.X 10.0 ""
              Expect.equal c 100 ""
          }

          test "outfieldRoster excludes GK" {
              let players = [| makePlayer 1 GK 10; makePlayer 2 ST 10; makePlayer 3 DC 10 |]
              let positions = [| spatialAt 5.0 34.0; spatialAt 80.0 34.0; spatialAt 20.0 34.0 |]
              let roster = outfieldRoster players positions [| 100; 100; 100 |]
              Expect.equal roster.Length 2 ""
              Expect.isFalse (roster |> Array.exists (fun (p, _, _) -> p.Position = GK)) ""
          }

          test "nearestOutfield returns the geographically closest non-GK player" {
              let players = [| makeGk 1 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 ST 10 |]
              let positions = [| spatialAt 5.0 34.0; spatialAt 30.0 34.0; spatialAt 80.0 34.0 |]
              let result = nearestOutfield players positions 25.0 34.0
              Expect.isSome result ""
              let p, _ = result.Value
              Expect.equal p.Id 2 "should be player 2, not the far ST"
          }

          // --- Offside ---

          test "isOffside: GK is never offside" {
              let gk = makeGk 1 10 defaultGk

              let ctx, state =
                  buildState
                      [| makePlayer 10 GK 10 |]
                      [| (95.0, 34.0) |]
                      [| gk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]
                      [| (5.0, 34.0); (65.0, 30.0); (70.0, 38.0) |]
                      85.0
                      34.0
                      HomeClub

              Expect.isFalse (isOffside gk 85.0 ctx state LeftToRight) "GK should never be offside"
          }

          test "isOffside: player in own half is never offside" {
              let attacker = makePlayer 1 ST 10

              let ctx, state =
                  buildState
                      [| attacker |]
                      [| (30.0, 34.0) |]
                      [| makeGk 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]
                      [| (5.0, 34.0); (60.0, 30.0); (65.0, 38.0) |]
                      30.0
                      34.0
                      HomeClub

              Expect.isFalse (isOffside attacker 30.0 ctx state LeftToRight) "own half → never offside"
          }

          test "isOffside: player ahead of second-last defender IS offside" {
              let attacker = makePlayer 1 ST 10

              let ctx, state =
                  buildState
                      [| attacker |]
                      [| (80.0, 34.0) |]
                      [| makeGk 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]
                      [| (5.0, 34.0); (60.0, 30.0); (65.0, 38.0) |]
                      75.0
                      34.0
                      HomeClub

              Expect.isTrue (isOffside attacker 80.0 ctx state LeftToRight) "ahead of 2nd-last defender → offside"
          }

          test "isOffside: player level with second-last defender is NOT offside" {
              let attacker = makePlayer 1 ST 10

              let ctx, state =
                  buildState
                      [| attacker |]
                      [| (70.0, 34.0) |]
                      [| makeGk 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]
                      [| (5.0, 34.0); (75.0, 30.0); (80.0, 38.0) |]
                      65.0
                      34.0
                      HomeClub

              Expect.isFalse (isOffside attacker 70.0 ctx state LeftToRight) "level with 2nd-last → NOT offside"
          } ]


// ============================================================================
// 3. Action resolution — each action is isolated and tested with extreme inputs
//    so failures pinpoint exactly which action module is broken.
// ============================================================================

// --- 3a. Shot ---

let shotActionTests =
    testList
        "ShotAction"
        [

          testCase "shot always produces a shot-class event"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| makePlayer 1 ST 10 |]
                      [| (90.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      80.0
                      34.0
                      HomeClub

              let events = resolveHome Shoot (ctx, state)
              Expect.isTrue (hasShotEvent events) $"expected a shot event, got: {events}"

          testCase "elite attacker vs weak GK: scores in ≥1 of 50 attempts"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| eliteAttacker 1 ST |]
                      [| (95.0, 34.0) |]
                      [| makeGk 10 1 weakGk |]
                      [| (99.0, 34.0) |]
                      85.0
                      34.0
                      HomeClub

              let goals = eventRate Goal 50 (resolveHome Shoot) (ctx, state)
              Expect.isGreaterThan goals 0.0 $"elite att vs weak GK should score; rate={goals:P0}"

          testCase "elite attacker vs weak GK: goal rate > 30%%"
          <| fun () ->
              // Weak GK + elite att should score often. If this fails, ShotAction math is broken.
              let ctx, state =
                  buildState
                      [| eliteAttacker 1 ST |]
                      [| (95.0, 34.0) |]
                      [| makeGk 10 1 weakGk |]
                      [| (99.0, 34.0) |]
                      85.0
                      34.0
                      HomeClub

              let rate = eventRate Goal 100 (resolveHome Shoot) (ctx, state)
              Expect.isGreaterThan rate 0.30 $"expected >30%% goals, got {rate:P0} — check ShotAction/BalanceConfig"

          testCase "worst attacker vs elite GK: goal rate < 5%%"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| worstAttacker 1 ST |]
                      [| (90.0, 34.0) |]
                      [| makeGk 10 20 strongGk |]
                      [| (99.0, 34.0) |]
                      80.0
                      34.0
                      HomeClub

              let rate = eventRate Goal 100 (resolveHome Shoot) (ctx, state)
              Expect.isLessThan rate 0.05 $"expected <5%% goals, got {rate:P0} — strong GK should dominate"

          testCase "shot from GK zone (x=5) is always off-target (quality gate)"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| makePlayer 1 GK 10 |]
                      [| (5.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      5.0
                      34.0
                      HomeClub

              let hasGoal = eventRate Goal 20 (resolveHome Shoot) (ctx, state)
              Expect.equal hasGoal 0.0 "shot from own GK line should never score (quality gate)"

          testCase "save event only appears alongside shot event"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| makePlayer 1 ST 10 |]
                      [| (90.0, 34.0) |]
                      [| makeGk 10 20 strongGk |]
                      [| (99.0, 34.0) |]
                      80.0
                      34.0
                      HomeClub

              for _ in 1..50 do
                  let events = resolveHome Shoot (ctx, state)

                  if hasEventType Save events then
                      Expect.isTrue (hasShotEvent events) "Save must co-exist with a shot event" ]


// --- 3b. Pass ---

let passActionTests =
    testList
        "PassAction"
        [

          testCase "elite passer to nearby teammate: ≥1 PassCompleted in 30 attempts"
          <| fun () ->
              let passer = elitePasser 1 MC
              let teammate = makePlayer 2 ST 10

              let ctx, state =
                  buildState
                      [| passer; teammate |]
                      [| (52.5, 34.0); (70.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let rate =
                  eventRate (PassCompleted(passer.Id, teammate.Id)) 30 (resolveHome (Pass teammate)) (ctx, state)

              Expect.isGreaterThan rate 0.0 "elite passer should complete at least one pass"

          testCase "elite passer pass completion rate > 70%%"
          <| fun () ->
              let passer = elitePasser 1 MC
              let teammate = makePlayer 2 ST 10

              let ctx, state =
                  buildState
                      [| passer; teammate |]
                      [| (52.5, 34.0); (65.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let completions =
                  Array.init 50 (fun _ -> resolveHome (Pass teammate) (ctx, state))
                  |> Array.filter hasPassEvent
                  |> Array.length

              let rate = float completions / 50.0
              Expect.isGreaterThan rate 0.70 $"elite passer completion={rate:P0}; expected >70%% — check PassAction"

          testCase "worst passer eventually turns over possession (AttackingClub flips)"
          <| fun () ->
              let passer = worstPasser 1 MC
              let teammate = makePlayer 2 ST 10

              let ctx, state =
                  buildState
                      [| passer; teammate |]
                      [| (52.5, 34.0); (70.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let turnovers =
                  Array.init 30 (fun _ ->
                      resolveHome (Pass teammate) (ctx, state) |> ignore
                      state.AttackingClub = AwayClub)
                  |> Array.filter id
                  |> Array.length

              Expect.isGreaterThan turnovers 0 "worst passer should misplace passes and flip possession"

          testCase "pass to non-existent teammate returns empty event list"
          <| fun () ->
              let passer = makePlayer 1 MC 10
              let ghost = makePlayer 99 ST 10 // not in state

              let ctx, state =
                  buildState
                      [| passer |]
                      [| (52.5, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let events = resolveHome (Pass ghost) (ctx, state)
              Expect.isEmpty events "passing to a ghost player should return []" ]


// --- 3c. Duel / Dribble ---

let duelActionTests =
    testList
        "DuelAction"
        [

          testCase "elite dribbler vs worst tackler: DribbleSuccess in ≥1 of 30"
          <| fun () ->
              let attacker = eliteDribbler 1 ST
              let defender = worstTackler 10 DC

              let ctx, state =
                  buildState [| attacker |] [| (70.0, 34.0) |] [| defender |] [| (75.0, 34.0) |] 70.0 34.0 HomeClub

              let successes = eventRate DribbleSuccess 30 (resolveHome Dribble) (ctx, state)
              Expect.isGreaterThan successes 0.0 "elite dribbler vs worst defender should win some duels"

          testCase "elite dribbler win rate > 60%%"
          <| fun () ->
              let attacker = eliteDribbler 1 ST
              let defender = worstTackler 10 DC

              let ctx, state =
                  buildState [| attacker |] [| (70.0, 34.0) |] [| defender |] [| (75.0, 34.0) |] 70.0 34.0 HomeClub

              let rate = eventRate DribbleSuccess 100 (resolveHome Dribble) (ctx, state)

              Expect.isGreaterThan
                  rate
                  0.60
                  $"elite dribbler win rate={rate:P0}; expected >60%% — check DuelAction weights"

          testCase "tackle by high-aggression defender produces FoulCommitted in ≥1 of 50"
          <| fun () ->
              let defender = highAggression 10 DC
              let attacker = makePlayer 1 ST 10

              // Home attacks, Away (defender) tackles
              let ctx, state =
                  buildState [| attacker |] [| (70.0, 34.0) |] [| defender |] [| (71.0, 34.0) |] 70.0 34.0 HomeClub

              let fouls = eventRate FoulCommitted 50 (resolveAway (Tackle defender)) (ctx, state)
              Expect.isGreaterThan fouls 0.0 "high-aggression defender should commit fouls"

          testCase "duel always produces a duel-class event"
          <| fun () ->
              let attacker = makePlayer 1 ST 10

              let ctx, state =
                  buildState
                      [| attacker |]
                      [| (60.0, 34.0) |]
                      [| makePlayer 10 DC 10 |]
                      [| (65.0, 34.0) |]
                      60.0
                      34.0
                      HomeClub

              for _ in 1..30 do
                  let events = resolveHome Dribble (ctx, state)

                  let hasDuelEvent =
                      events
                      |> List.exists (fun e ->
                          match e.Type with
                          | DribbleSuccess
                          | DribbleFail
                          | DribbleKeep
                          | TackleSuccess
                          | TackleFail
                          | FoulCommitted -> true
                          | _ -> false)

                  Expect.isTrue hasDuelEvent $"duel produced no duel event: {events}" ]


// --- 3d. Set pieces ---

let setPieceTests =
    testList
        "SetPieceAction"
        [

          testCase "FreeKick always emits a FreeKick event"
          <| fun () ->
              let kicker =
                  makePlayer 1 MC 16
                  |> withTechnical
                      { defaultTechnical with
                          Finishing = 18
                          LongShots = 18 }
                  |> withMental { defaultMental with Composure = 18 }

              let ctx, state =
                  buildState
                      [| kicker |]
                      [| (65.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      65.0
                      34.0
                      HomeClub

              let events = resolveHome PlayerAction.FreeKick (ctx, state)
              Expect.isTrue (hasFreeKickEvent events) $"FreeKick action must emit FreeKick event, got: {events}"

          testCase "Corner always emits a Corner event"
          <| fun () ->
              let ctx, state =
                  buildState
                      [| makePlayer 1 ST 10 |]
                      [| (95.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      104.0
                      10.0
                      HomeClub

              let events = resolveHome PlayerAction.Corner (ctx, state)
              Expect.isTrue (hasEventType Corner events) $"Corner action must emit Corner event, got: {events}"

          testCase "ThrowIn emits a pass-class event"
          <| fun () ->
              let thrower = makePlayer 1 DL 10
              let teammate = makePlayer 2 MC 10

              let ctx, state =
                  buildState
                      [| thrower; teammate |]
                      [| (5.0, 20.0); (10.0, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      5.0
                      5.0
                      HomeClub

              let events = resolveHome (ThrowIn HomeClub) (ctx, state)
              Expect.isTrue (hasPassEvent events) $"ThrowIn must emit a pass event, got: {events}"

          testCase "Penalty emits PenaltyAwarded event"
          <| fun () ->
              let kicker = makePlayer 1 ST 16

              let ctx, state =
                  buildState
                      [| kicker |]
                      [| (52.5, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let events = resolveHome (Penalty(kicker, HomeClub, 1)) (ctx, state)

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | PenaltyAwarded _ -> true
                      | _ -> false)
                  $"expected PenaltyAwarded, got: {events}"

          testCase "elite free-kick taker scores at least once in 30 direct free kicks"
          <| fun () ->
              let kicker =
                  makePlayer 1 MC 20
                  |> withTechnical
                      { defaultTechnical with
                          Finishing = 20
                          LongShots = 20
                          FreeKick = 20 }
                  |> withMental { defaultMental with Composure = 20 }

              let ctx, state =
                  buildState
                      [| kicker |]
                      [| (68.0, 34.0) |]
                      [| makeGk 10 1 weakGk |]
                      [| (99.0, 34.0) |]
                      68.0
                      34.0
                      HomeClub

              let scored = eventRate Goal 30 (resolveHome PlayerAction.FreeKick) (ctx, state)
              Expect.isGreaterThan scored 0.0 "elite FK taker vs weak GK should score eventually" ]


// ============================================================================
// 4. Full-match structural invariants
// ============================================================================

let structuralInvariantTests =
    testList
        "Structural Invariants"
        [

          test "scores are non-negative and plausible (≤10 each)" {
              let clubs, players, staff = loadClubs ()
              let h, a, _, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              Expect.isTrue (h >= 0 && a >= 0) $"negative score: {h}-{a}"
              Expect.isTrue (h <= 10 && a <= 10) $"implausible score: {h}-{a}"
          }

          test "goal events match reported score" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let hScore, aScore, events, _ = trySimulateMatch home away players staff |> getOk
              let goals = events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length
              Expect.equal hGoals hScore $"home goal events={hGoals} ≠ score={hScore}"
              Expect.equal aGoals aScore $"away goal events={aGoals} ≠ score={aScore}"
          }

          test "all event SubTicks are in [0, FullTimeSubTick]" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let bad =
                  events
                  |> List.filter (fun e -> e.SubTick < 0 || e.SubTick > PhysicsContract.FullTimeSubTick)

              Expect.isEmpty bad $"{bad.Length} events with SubTick out of range: {bad |> List.truncate 3}"
          }

          test "all event ClubIds belong to the two participating clubs" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk

              let bad =
                  events |> List.filter (fun e -> e.ClubId <> home.Id && e.ClubId <> away.Id)

              Expect.isEmpty bad $"{bad.Length} events with unknown ClubId: {bad |> List.truncate 3}"
          }

          test "all event PlayerIds belong to the squads" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk
              let homeIds = home.PlayerIds |> Set.ofList
              let awayIds = away.PlayerIds |> Set.ofList

              let bad =
                  events
                  |> List.filter (fun e -> not (Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))

              Expect.isEmpty bad $"{bad.Length} events with foreign PlayerId: {bad |> List.truncate 3}"
          }

          test "events are ordered chronologically" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let outOfOrder =
                  events |> List.pairwise |> List.filter (fun (a, b) -> b.SubTick < a.SubTick)

              Expect.isEmpty outOfOrder $"{outOfOrder.Length} out-of-order event pairs: {outOfOrder |> List.truncate 3}"
          }

          test "SubstitutionIn and SubstitutionOut counts match" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              let ins = countEventType SubstitutionIn events
              let outs = countEventType SubstitutionOut events
              Expect.equal ins outs $"SubIn={ins} ≠ SubOut={outs}"
          }

          test "at most 3 substitutions per team" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk

              let homeSubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = home.Id)
                  |> List.length

              let awaySubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = away.Id)
                  |> List.length

              Expect.isTrue (homeSubs <= 3 && awaySubs <= 3) $"too many subs: home={homeSubs} away={awaySubs}"
          }

          test "no player receives more than 1 red card" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let reds =
                  events |> List.filter (fun e -> e.Type = RedCard) |> List.countBy _.PlayerId

              let violations = reds |> List.filter (fun (_, n) -> n > 1)
              Expect.isEmpty violations $"players with >1 red card: {violations}"
          }

          test "no player receives more than 2 yellow cards" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              let violations = yellows |> List.filter (fun (_, n) -> n > 2)
              Expect.isEmpty violations $"players with >2 yellows: {violations}"
          }

          test "second yellow triggers red card for same player" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let doubleYellowed =
                  events
                  |> List.filter (fun e -> e.Type = YellowCard)
                  |> List.countBy _.PlayerId
                  |> List.filter (fun (_, n) -> n >= 2)
                  |> List.map fst
                  |> Set.ofList

              let redPlayers =
                  events
                  |> List.filter (fun e -> e.Type = RedCard)
                  |> List.map _.PlayerId
                  |> Set.ofList

              let missing = Set.difference doubleYellowed redPlayers
              Expect.isEmpty missing $"players with 2 yellows but no red: {missing}"
          }

          test "FreeKick events ≤ FoulCommitted events" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              let fouls = countEventType FoulCommitted events

              let fks =
                  events
                  |> List.filter (fun e ->
                      match e.Type with
                      | FreeKick _ -> true
                      | _ -> false)
                  |> List.length

              Expect.isLessThanOrEqual fks fouls $"FreeKick ({fks}) > FoulCommitted ({fouls})"
          }

          test "momentum stays in [-10, 10] at all snapshots" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              let violations =
                  replay.Snapshots
                  |> Array.filter (fun s -> s.Momentum < -10.0 || s.Momentum > 10.0)

              Expect.isEmpty violations $"{violations.Length} snapshots with momentum out of bounds"
          }

          test "all player conditions stay in [0, 100] at all snapshots" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              let bad =
                  replay.Snapshots
                  |> Array.filter (fun s ->
                      s.HomeConditions |> Array.exists (fun c -> c < 0 || c > 100)
                      || s.AwayConditions |> Array.exists (fun c -> c < 0 || c > 100))

              Expect.isEmpty bad $"{bad.Length} snapshots with condition out of [0,100]"
          }

          test "all player positions stay within pitch bounds [0..105] × [0..68]" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              let inBounds (sp: Spatial) =
                  sp.X >= 0.0
                  && sp.X <= PhysicsContract.PitchLength
                  && sp.Y >= 0.0
                  && sp.Y <= PhysicsContract.PitchWidth

              let bad =
                  replay.Snapshots
                  |> Array.filter (fun s ->
                      s.HomePositions |> Array.exists (inBounds >> not)
                      || s.AwayPositions |> Array.exists (inBounds >> not))

              Expect.isEmpty bad $"{bad.Length} snapshots with a player out of bounds"
          } ]


// ============================================================================
// 5. Physics fidelity
// ============================================================================

let physicsTests =
    testList
        "Physics Fidelity"
        [

          test "no player exceeds 10.5 m/s in any snapshot" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              for snap in replay.Snapshots do
                  let checkTeam label (positions: Spatial[]) =
                      for i in 0 .. positions.Length - 1 do
                          let pos = positions[i]
                          let speed = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy)
                          Expect.isLessThanOrEqual speed 10.5 $"{label}[{i}] speed={speed:F2} m/s exceeds 10.5"

                  checkTeam "Home" snap.HomePositions
                  checkTeam "Away" snap.AwayPositions
          }

          test "Pace 20 player is ≥30%% faster than Pace 1 player" {
              let fast = PhysicsContract.playerMaxSpeed 20 100
              let slow = PhysicsContract.playerMaxSpeed 1 100
              Expect.isGreaterThanOrEqual (fast / slow) 1.30 $"pace ratio={fast / slow:F2}; expected ≥1.30"
          }

          test "ball physics step does not accelerate the ball" {
              let ball =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 0.5
                        Vx = 30.0
                        Vy = 5.0
                        Vz = 2.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true }

              let stepped = BallPhysics.update ball

              let origSpeed =
                  sqrt (ball.Position.Vx ** 2.0 + ball.Position.Vy ** 2.0 + ball.Position.Vz ** 2.0)

              let steppedSpeed =
                  sqrt (
                      stepped.Position.Vx ** 2.0
                      + stepped.Position.Vy ** 2.0
                      + stepped.Position.Vz ** 2.0
                  )

              Expect.isLessThanOrEqual
                  steppedSpeed
                  (origSpeed * 1.01)
                  $"ball accelerated: {origSpeed:F1} → {steppedSpeed:F1}"
          }

          test "ball Z=1.5m is above 0.8m (heading/lofted ball threshold)" {
              let ctx, state =
                  buildState
                      [| makePlayer 1 ST 10 |]
                      [| (52.5, 34.0) |]
                      [| makeGk 10 10 defaultGk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              state.Ball <-
                  { state.Ball with
                      Position = { state.Ball.Position with Z = 1.5 } }

              Expect.isGreaterThan state.Ball.Position.Z 0.8 "Z=1.5m should exceed heading threshold 0.8m"
          } ]

// ============================================================================
// 6. Statistical contracts (100 matches)
// ============================================================================

let statisticalTests =
    let n = 100

    let outcomes =
        let clubs, players, staff = loadClubs ()

        Array.Parallel.init n (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length
            trySimulateMatch clubs[hi] clubs[ai] players staff)

    let oks =
        outcomes
        |> Array.choose (function
            | Ok(h, a, ev, _) -> Some(h, a, ev)
            | Error _ -> None)

    let scores = oks |> Array.map (fun (h, a, _) -> h, a)

    // --- Event classifiers ---

    let isShot (e: MatchEvent) =
        match e.Type with
        | Goal
        | ShotBlocked
        | ShotOffTarget
        | Save -> true
        | _ -> false

    let isPass (e: MatchEvent) =
        match e.Type with
        | PassCompleted _
        | PassIncomplete _
        | PassMisplaced _
        | PassIntercepted _
        | PassDeflected _ -> true
        | _ -> false

    let isPassCompleted (e: MatchEvent) =
        match e.Type with
        | PassCompleted _ -> true
        | _ -> false

    let isDrib (e: MatchEvent) =
        match e.Type with
        | DribbleSuccess
        | DribbleFail -> true
        | _ -> false

    let countEv t ev =
        ev |> List.filter (fun e -> e.Type = t) |> List.length

    let avg f =
        oks |> Array.averageBy (fun (_, _, ev) -> float (f ev))

    // --- Diagnostic helpers ---

    // Breaks down shots into Goal/Save/Blocked/OffTarget for failure messages
    let shotBreakdown (events: MatchEvent list) =
        let goals = countEv Goal events
        let saves = countEv Save events
        let blocked = countEv ShotBlocked events
        let offTarget = countEv ShotOffTarget events
        $"goals={goals} saves={saves} blocked={blocked} offTarget={offTarget}"

    // Breaks down passes into completed/incomplete/misplaced/intercepted
    let passBreakdown (events: MatchEvent list) =
        let completed = events |> List.filter isPassCompleted |> List.length

        let incomplete =
            events
            |> List.filter (fun e ->
                match e.Type with
                | PassIncomplete _ -> true
                | _ -> false)
            |> List.length

        let misplaced =
            events
            |> List.filter (fun e ->
                match e.Type with
                | PassMisplaced _ -> true
                | _ -> false)
            |> List.length

        let intercepted =
            events
            |> List.filter (fun e ->
                match e.Type with
                | PassIntercepted _ -> true
                | _ -> false)
            |> List.length

        let completionPct =
            if completed + incomplete + misplaced + intercepted = 0 then
                0.0
            else
                float completed / float (completed + incomplete + misplaced + intercepted)
                * 100.0

        $"completed={completed} incomplete={incomplete} misplaced={misplaced} intercepted={intercepted} completion={completionPct:F1}%%"

    // Worst-case and best-case scores for outlier diagnostics
    let scoreOutliers () =
        scores
        |> Array.sortByDescending (fun (h, a) -> h + a)
        |> Array.truncate 5
        |> Array.map (fun (h, a) -> $"{h}-{a}")
        |> String.concat ", "

    testList
        "Statistical Contracts"
        [

          testCase "all fixtures simulated without error"
          <| fun () ->
              let errors =
                  outcomes
                  |> Array.choose (function
                      | Error e -> Some $"{e}"
                      | Ok _ -> None)

              Expect.isEmpty errors $"{errors.Length} fixture(s) crashed: {errors |> Array.truncate 3}"

          testCase "avg goals/match in [1.5, 4.0]"
          <| fun () ->
              let avgGoals = scores |> Array.averageBy (fun (h, a) -> float (h + a))
              let avgShots = avg (List.filter isShot >> List.length)
              let avgSaves = avg (countEv Save)
              let totalGoals = oks |> Array.sumBy (fun (_, _, ev) -> countEv Goal ev)

              let totalShots =
                  oks |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let convRate =
                  if totalShots = 0 then
                      0.0
                  else
                      float totalGoals / float totalShots * 100.0

              Expect.isTrue
                  (avgGoals >= 1.5 && avgGoals <= 4.0)
                  $"avg goals={avgGoals:F2} | avg shots={avgShots:F1} saves={avgSaves:F1} conversion={convRate:F1}%% | top scores: {scoreOutliers ()} — check GoalDifficulty/GkSaveBonus"

          testCase "no match has a scoreline with either side > 10"
          <| fun () ->
              let outliers = scores |> Array.filter (fun (h, a) -> h > 10 || a > 10)
              let avgGoals = scores |> Array.averageBy (fun (h, a) -> float (h + a))
              let avgShots = avg (List.filter isShot >> List.length)

              Expect.isEmpty
                  outliers
                  $"{outliers.Length} outlier scores: {outliers |> Array.truncate 5} | avg goals={avgGoals:F2} avg shots={avgShots:F1}"

          testCase "avg shots/match in [10, 40]"
          <| fun () ->
              let avgShots = avg (List.filter isShot >> List.length)
              let avgGoals = avg (countEv Goal)
              let avgSaves = avg (countEv Save)
              let avgBlocked = avg (countEv ShotBlocked)
              let avgOffTarget = avg (countEv ShotOffTarget)

              Expect.isTrue
                  (avgShots >= 10.0 && avgShots <= 40.0)
                  $"avg shots={avgShots:F1} | goals={avgGoals:F1} saves={avgSaves:F1} blocked={avgBlocked:F1} offTarget={avgOffTarget:F1} — check ShotQualityGate/TargetShotsPerMatch"

          testCase "avg passes/match in [50, 800]"
          <| fun () ->
              let avgTotal = avg (List.filter isPass >> List.length)
              let avgCompleted = avg (List.filter isPassCompleted >> List.length)

              let avgFailed =
                  avg (fun ev ->
                      ev
                      |> List.filter (fun e ->
                          match e.Type with
                          | PassIncomplete _
                          | PassMisplaced _ -> true
                          | _ -> false)
                      |> List.length)

              let avgInt =
                  avg (fun ev ->
                      ev
                      |> List.filter (fun e ->
                          match e.Type with
                          | PassIntercepted _ -> true
                          | _ -> false)
                      |> List.length)

              let completionPct =
                  if avgTotal = 0.0 then
                      0.0
                  else
                      avgCompleted / avgTotal * 100.0

              Expect.isTrue
                  (avgTotal >= 50.0 && avgTotal <= 800.0)
                  $"avg passes={avgTotal:F1} | completed={avgCompleted:F1} failed={avgFailed:F1} intercepted={avgInt:F1} completion={completionPct:F1}%% — check PassAction"

          testCase "pass completion rate in [55%%, 95%%]"
          <| fun () ->
              let totalAttempts =
                  oks |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isPass |> List.length)

              let totalCompleted =
                  oks
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isPassCompleted |> List.length)

              let rate =
                  if totalAttempts = 0 then
                      0.0
                  else
                      float totalCompleted / float totalAttempts * 100.0

              let sampleBreakdown =
                  oks
                  |> Array.truncate 3
                  |> Array.mapi (fun i (_, _, ev) -> $"match{i + 1}: [{passBreakdown ev}]")
                  |> String.concat " | "

              Expect.isTrue
                  (rate >= 55.0 && rate <= 95.0)
                  $"completion={rate:F1}%% ({totalCompleted}/{totalAttempts}) | {sampleBreakdown} — check PassAction accuracy weights"

          testCase "avg dribbles/match in [15, 100]"
          <| fun () ->
              let avgDribs = avg (List.filter isDrib >> List.length)
              let avgSucceeded = avg (countEv DribbleSuccess)
              let avgFailed = avg (countEv DribbleFail)

              let successRate =
                  if avgDribs = 0.0 then
                      0.0
                  else
                      avgSucceeded / avgDribs * 100.0

              Expect.isTrue
                  (avgDribs >= 15.0 && avgDribs <= 100.0)
                  $"avg dribbles={avgDribs:F1} | succeeded={avgSucceeded:F1} failed={avgFailed:F1} successRate={successRate:F1}%% — check DuelAction"

          testCase "avg corners/match in [3, 20]"
          <| fun () ->
              let avgCorners = avg (countEv Corner)
              let avgShots = avg (List.filter isShot >> List.length)

              Expect.isTrue
                  (avgCorners >= 3.0 && avgCorners <= 20.0)
                  $"avg corners={avgCorners:F2} | avg shots={avgShots:F1} (corners often follow blocked/saved shots)"

          testCase "avg fouls/match in [10, 50]"
          <| fun () ->
              let avgFouls = avg (countEv FoulCommitted)
              let avgDribs = avg (List.filter isDrib >> List.length)

              let avgTackles =
                  avg (fun ev ->
                      ev
                      |> List.filter (fun e ->
                          match e.Type with
                          | TackleSuccess
                          | TackleFail -> true
                          | _ -> false)
                      |> List.length)

              Expect.isTrue
                  (avgFouls >= 10.0 && avgFouls <= 50.0)
                  $"avg fouls={avgFouls:F2} | avg dribbles={avgDribs:F1} tackles={avgTackles:F1} — check FoulBaseRate"

          testCase "shot conversion rate in [3%%, 25%%]"
          <| fun () ->
              let totalShots =
                  oks |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let totalGoals = oks |> Array.sumBy (fun (_, _, ev) -> countEv Goal ev)
              let totalSaves = oks |> Array.sumBy (fun (_, _, ev) -> countEv Save ev)
              let totalBlocked = oks |> Array.sumBy (fun (_, _, ev) -> countEv ShotBlocked ev)
              let totalOff = oks |> Array.sumBy (fun (_, _, ev) -> countEv ShotOffTarget ev)

              let rate =
                  if totalShots = 0 then
                      0.0
                  else
                      float totalGoals / float totalShots * 100.0

              Expect.isTrue
                  (rate >= 3.0 && rate <= 25.0)
                  $"conversion={rate:F1}%% | goals={totalGoals} saves={totalSaves} blocked={totalBlocked} offTarget={totalOff} total={totalShots} — check GkSaveBonus/ShotOnTargetBase"

          testCase "draws are not the most common outcome"
          <| fun () ->
              let total = float scores.Length

              let hwp =
                  scores
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> fun x -> float x / total * 100.0

              let awp =
                  scores
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> fun x -> float x / total * 100.0

              let dp =
                  scores
                  |> Array.filter (fun (h, a) -> h = a)
                  |> Array.length
                  |> fun x -> float x / total * 100.0

              Expect.isTrue
                  (dp < hwp || dp < awp)
                  $"draws={dp:F1}%% dominates | homeWin={hwp:F1}%% awayWin={awp:F1}%% — check HomeAdvantageStrength/GoalVariance"

          testCase "each match produces >0 events"
          <| fun () ->
              let empty = oks |> Array.filter (fun (_, _, ev) -> ev.IsEmpty)
              Expect.isEmpty empty $"{empty.Length} matches produced no events"

          testCase "speed under 150ms/match"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let sw = Diagnostics.Stopwatch.StartNew()

              Array.Parallel.init n (fun i ->
                  let hi = i % clubs.Length
                  let ai = (hi + 1) % clubs.Length
                  trySimulateMatch clubs[hi] clubs[ai] players staff)
              |> ignore

              sw.Stop()
              let msPerGame = sw.Elapsed.TotalMilliseconds / float n
              Expect.isLessThan msPerGame 150.0 $"{msPerGame:F2}ms/match exceeds 150ms budget" ]


// ============================================================================
// 7. Home advantage
// ============================================================================

let homeAdvantageTests =

    let skillMatchedPairs clubs players maxSkillDiff limit =
        clubs
        |> Array.pairwise
        |> Array.filter (fun (a, b) -> abs (Club.averageSkill players a - Club.averageSkill players b) < maxSkillDiff)
        |> Array.truncate limit

    testList
        "Home Advantage"
        [

          testCase "home teams total more goals than away teams across skill-matched fixtures"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let pairs = skillMatchedPairs clubs players 5 100

              let results =
                  pairs
                  |> Array.Parallel.collect (fun (a, b) ->
                      [| trySimulateMatch a b players staff; trySimulateMatch b a players staff |])
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let homeGoals = results |> Array.sumBy fst
              let awayGoals = results |> Array.sumBy snd
              printfn $"[HomeAdv] matches={results.Length} homeGoals={homeGoals} awayGoals={awayGoals}"

              Expect.isGreaterThan
                  homeGoals
                  awayGoals
                  $"home ({homeGoals}) ≤ away ({awayGoals}) — check HomeAdvantageStrength"

          testCase "home win rate > away win rate in skill-matched fixtures"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let pairs = skillMatchedPairs clubs players 5 100

              let results =
                  pairs
                  |> Array.Parallel.collect (fun (a, b) ->
                      [| trySimulateMatch a b players staff; trySimulateMatch b a players staff |])
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let n = float results.Length

              let hwp =
                  results
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              let awp =
                  results
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              printfn $"[HomeAdv] homeWin={hwp:F1}%% awayWin={awp:F1}%%"
              Expect.isGreaterThan hwp awp $"home win ({hwp:F1}%%) ≤ away win ({awp:F1}%%) — check HomeDuelAttackBonus"

          testCase "home team does not commit significantly more fouls than away (≤110%%)"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let pairs = skillMatchedPairs clubs players 5 50

              let homeFouls, awayFouls =
                  pairs
                  |> Array.fold
                      (fun (hF, aF) (home, away) ->
                          match trySimulateMatch home away players staff with
                          | Error _ -> hF, aF
                          | Ok(_, _, events, _) ->
                              let h =
                                  events
                                  |> List.filter (fun e -> e.ClubId = home.Id && e.Type = FoulCommitted)
                                  |> List.length

                              let a =
                                  events
                                  |> List.filter (fun e -> e.ClubId = away.Id && e.Type = FoulCommitted)
                                  |> List.length

                              hF + h, aF + a)
                      (0, 0)

              printfn $"[HomeAdv] homeFouls={homeFouls} awayFouls={awayFouls}"

              Expect.isTrue
                  (homeFouls <= awayFouls * 110 / 100)
                  $"home fouls ({homeFouls}) > 110%% of away ({awayFouls})" ]
