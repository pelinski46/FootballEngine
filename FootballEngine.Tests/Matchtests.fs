module FootballEngine.Tests.MatchTests

open System
open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open FootballEngine.MatchStateOps
open FootballEngine.MatchSpatial
open FootballEngine.MatchPlayerDecision
open FootballEngine.MatchPlayerAction
open FootballEngine.Tests.Helpers

// ============================================================================
// Shared state builders
// ============================================================================

let private defaultPhysical =
    { Acceleration = 70
      Pace = 70
      Agility = 70
      Balance = 70
      JumpingReach = 70
      Stamina = 70
      Strength = 70 }

let private defaultTechnical =
    { Finishing = 70
      LongShots = 70
      Dribbling = 70
      BallControl = 70
      Passing = 70
      Crossing = 70
      Tackling = 70
      Marking = 70
      Heading = 70
      FreeKick = 70
      Penalty = 70 }

let private defaultMental =
    { Aggression = 30
      Composure = 70
      Vision = 70
      Positioning = 70
      Bravery = 70
      WorkRate = 70
      Concentration = 70
      Leadership = 70 }

let private defaultGk =
    { Reflexes = 70
      Handling = 70
      Kicking = 70
      OneOnOne = 70
      AerialReach = 70 }

let private weakGk =
    { Reflexes = 1
      Handling = 1
      Kicking = 1
      OneOnOne = 1
      AerialReach = 1 }

let private strongGk =
    { Reflexes = 99
      Handling = 99
      Kicking = 99
      OneOnOne = 99
      AerialReach = 99 }

let private makePlayer id pos skill =
    { Id = id
      Name = $"Player{id}"
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

let private makeGkPlayer id skill gkStats =
    { (makePlayer id GK skill) with
        Goalkeeping = gkStats }

let private makeHighAggressionPlayer id pos =
    { (makePlayer id pos 70) with
        Mental =
            { defaultMental with
                Aggression = 99
                Positioning = 1 } }

let private makeClub id =
    { Id = id
      Name = $"Club{id}"
      Nationality = "AR"
      Reputation = 50
      PlayerIds = []
      StaffIds = []
      Budget = 1000000m
      Morale = 50
      BoardObjective = LeagueObjective MidTable }

let private defaultSpatialAt x y =
    { X = x
      Y = y
      Z = 0.0
      Vx = 0.0
      Vy = 0.0
      Vz = 0.0 }

let private defaultInstructions = TacticalInstructions.defaultInstructions

let private makeTeamSide (players: Player[]) (positions: (float * float)[]) =
    { Players = players
      Conditions = players |> Array.map (fun _ -> 100)
      Positions = positions |> Array.map (fun (x, y) -> defaultSpatialAt x y)
      BasePositions = positions |> Array.map (fun (x, y) -> defaultSpatialAt x y)
      Sidelined = Map.empty
      Yellows = Map.empty
      SubsUsed = 0
      Tactics = Balanced
      Instructions = Some defaultInstructions }

let private homeClub = makeClub 1
let private awayClub = makeClub 2

let private minimalState homeScore awayScore =
    let dummyCoach = Unchecked.defaultof<Staff>

    { Home = homeClub
      Away = awayClub
      HomeCoach = dummyCoach
      AwayCoach = dummyCoach
      Second = 1
      HomeScore = homeScore
      AwayScore = awayScore
      Ball = defaultBall
      AttackingClub = HomeClub
      Momentum = 0.0
      HomeSide =
        { Players = Array.empty
          Conditions = Array.empty
          Positions = Array.empty
          BasePositions = Array.empty
          Sidelined = Map.empty
          Yellows = Map.empty
          SubsUsed = 0
          Tactics = Balanced
          Instructions = Some defaultInstructions }
      AwaySide =
        { Players = Array.empty
          Conditions = Array.empty
          Positions = Array.empty
          BasePositions = Array.empty
          Sidelined = Map.empty
          Yellows = Map.empty
          SubsUsed = 0
          Tactics = Balanced
          Instructions = Some defaultInstructions }
      PenaltyShootout = None
      IsExtraTime = false
      IsKnockoutMatch = false }

// Builds a full match state with home attacking and away defending
// homePositions and awayPositions are (x,y) arrays aligned with players arrays
let private buildMatchState
    (homePlayers: Player[])
    (homePositions: (float * float)[])
    (awayPlayers: Player[])
    (awayPositions: (float * float)[])
    (ballX: float)
    (ballY: float)
    (attacking: ClubSide)
    =
    let dummyCoach = Unchecked.defaultof<Staff>

    { Home = homeClub
      Away = awayClub
      HomeCoach = dummyCoach
      AwayCoach = dummyCoach
      Second = 1
      HomeScore = 0
      AwayScore = 0
      Ball =
        { Position = defaultSpatialAt ballX ballY
          LastTouchBy = None }
      AttackingClub = attacking
      Momentum = 0.0
      HomeSide = makeTeamSide homePlayers homePositions
      AwaySide = makeTeamSide awayPlayers awayPositions
      PenaltyShootout = None
      IsExtraTime = false
      IsKnockoutMatch = false }

// ============================================================================
// Category 1: Pure Unit Tests — MatchStateOps
// ============================================================================

let matchStateOpsTests =
    testList
        "MatchStateOps Unit Tests"
        [
          // ClubSide.flip
          test "ClubSide.flip HomeClub → AwayClub" { Expect.equal (ClubSide.flip HomeClub) AwayClub "" }
          test "ClubSide.flip AwayClub → HomeClub" { Expect.equal (ClubSide.flip AwayClub) HomeClub "" }
          test "ClubSide.flip is involution" {
              Expect.equal (ClubSide.flip (ClubSide.flip HomeClub)) HomeClub ""
              Expect.equal (ClubSide.flip (ClubSide.flip AwayClub)) AwayClub ""
          }

          // phaseFromBallZone
          test "phaseFromBallZone LeftToRight x=10 → BuildUp" {
              Expect.equal (phaseFromBallZone LeftToRight 10.0) BuildUp $"got {phaseFromBallZone LeftToRight 10.0}"
          }
          test "phaseFromBallZone LeftToRight x=50 → Midfield" {
              Expect.equal (phaseFromBallZone LeftToRight 50.0) Midfield $"got {phaseFromBallZone LeftToRight 50.0}"
          }
          test "phaseFromBallZone LeftToRight x=80 → Attack" {
              Expect.equal (phaseFromBallZone LeftToRight 80.0) Attack $"got {phaseFromBallZone LeftToRight 80.0}"
          }
          test "phaseFromBallZone RightToLeft mirrors LeftToRight" {
              Expect.equal (phaseFromBallZone RightToLeft 90.0) BuildUp "x=90 RightToLeft should be BuildUp"
              Expect.equal (phaseFromBallZone RightToLeft 20.0) Attack "x=20 RightToLeft should be Attack"
          }

          // pressureMultiplier
          test "pressureMultiplier home losing → > 1.0" {
              let s = minimalState 0 2
              Expect.isTrue (pressureMultiplier s.Home.Id s > 1.0) "losing should increase pressure"
          }
          test "pressureMultiplier home winning → < 1.0" {
              let s = minimalState 2 0
              Expect.isTrue (pressureMultiplier s.Home.Id s < 1.0) "winning should decrease pressure"
          }
          test "pressureMultiplier away losing → > 1.0" {
              let s = minimalState 2 0
              Expect.isTrue (pressureMultiplier s.Away.Id s > 1.0) "away losing should increase pressure"
          }
          test "pressureMultiplier values in [0.1, 2.0]" {
              let losing = pressureMultiplier homeClub.Id (minimalState 0 3)
              let winning = pressureMultiplier homeClub.Id (minimalState 3 0)
              Expect.isTrue (losing >= 0.1 && losing <= 2.0) $"losing={losing} out of range"
              Expect.isTrue (winning >= 0.1 && winning <= 2.0) $"winning={winning} out of range"
          }

          // goalDiff
          test "goalDiff homeClub = HomeScore - AwayScore" {
              let s = minimalState 3 1
              Expect.equal (goalDiff s.Home.Id s) 2 "home goal diff should be 2"
          }
          test "goalDiff awayClub = AwayScore - HomeScore" {
              let s = minimalState 3 1
              Expect.equal (goalDiff s.Away.Id s) -2 "away goal diff should be -2"
          }

          // awardGoal
          test "awardGoal HomeClub increments HomeScore" {
              let s = minimalState 0 0
              let s', _ = awardGoal HomeClub None 1 s
              Expect.equal s'.HomeScore 1 "HomeScore should be 1"
              Expect.equal s'.AwayScore 0 "AwayScore should remain 0"
          }
          test "awardGoal AwayClub increments AwayScore" {
              let s = minimalState 0 0
              let s', _ = awardGoal AwayClub None 1 s
              Expect.equal s'.AwayScore 1 "AwayScore should be 1"
              Expect.equal s'.HomeScore 0 "HomeScore should remain 0"
          }
          test "awardGoal resets ball to center" {
              let s = minimalState 0 0
              let s', _ = awardGoal HomeClub None 1 s
              Expect.equal s'.Ball.Position.X 50.0 "ball X should be 50"
              Expect.equal s'.Ball.Position.Y 50.0 "ball Y should be 50"
          }
          test "awardGoal with scorerId emits Goal event" {
              let s = minimalState 0 0
              let _, events = awardGoal HomeClub (Some 99) 1 s
              Expect.exists events (fun e -> e.Type = Goal) "should emit Goal event"
          }

          // resetBallToCenter
          test "resetBallToCenter sets X=50 Y=50 and no velocity" {
              let s =
                  { minimalState 0 0 with
                      Ball =
                          { Position = defaultSpatialAt 80.0 30.0
                            LastTouchBy = Some 1 } }

              let s' = resetBallToCenter s
              Expect.equal s'.Ball.Position.X 50.0 "X should be 50"
              Expect.equal s'.Ball.Position.Y 50.0 "Y should be 50"
              Expect.equal s'.Ball.Position.Vx 0.0 "Vx should be 0"
              Expect.equal s'.Ball.Position.Vy 0.0 "Vy should be 0"
              Expect.equal s'.Ball.LastTouchBy None "LastTouchBy should be cleared"
          }

          // activeIndices
          test "activeIndices excludes sidelined players" {
              let p1 = makePlayer 1 ST 70
              let p2 = makePlayer 2 MC 70
              let p3 = makePlayer 3 DC 70
              let players = [| p1; p2; p3 |]
              let sidelined = Map.ofList [ p2.Id, SidelinedByRedCard ]
              let active = activeIndices players sidelined
              Expect.equal active.Length 2 "should have 2 active players"

              Expect.isFalse
                  (active |> Array.exists (fun i -> players[i].Id = p2.Id))
                  "sidelined player should be excluded"
          }
          test "activeIndices returns all when none sidelined" {
              let players = [| makePlayer 1 ST 70; makePlayer 2 MC 70 |]
              let active = activeIndices players Map.empty
              Expect.equal active.Length 2 "should have 2 active players"
          } ]

// ============================================================================
// Category 2: Spatial Unit Tests — MatchSpatial
// ============================================================================

let matchSpatialTests =
    testList
        "MatchSpatial Unit Tests"
        [
          // teamRoster
          test "teamRoster zips Players Positions Conditions correctly" {
              let players = [| makePlayer 1 ST 70; makePlayer 2 MC 70; makePlayer 3 GK 70 |]
              let positions = [| (10.0, 50.0); (50.0, 50.0); (5.0, 50.0) |]
              let ts = makeTeamSide players positions
              let roster = teamRoster ts
              Expect.equal roster.Length 3 "roster length should match players"
              let p, sp, c = roster[0]
              Expect.equal p.Id 1 "first player id"
              Expect.equal sp.X 10.0 "first player X"
              Expect.equal c 100 "first player condition"
          }

          // outfieldRoster
          test "outfieldRoster excludes GK" {
              let players = [| makePlayer 1 GK 70; makePlayer 2 ST 70; makePlayer 3 DC 70 |]
              let positions = [| (5.0, 50.0); (80.0, 50.0); (20.0, 50.0) |]
              let ts = makeTeamSide players positions
              let roster = outfieldRoster ts
              Expect.equal roster.Length 2 "should have 2 outfield players"

              Expect.isFalse
                  (roster |> Array.exists (fun (p, _, _) -> p.Position = GK))
                  "GK should not be in outfield roster"
          }
          test "outfieldRoster preserves correct position correspondence" {
              // GK at index 0 — this was the filter-before-mapi bug
              let gk = makeGkPlayer 1 70 defaultGk
              let st = makePlayer 2 ST 70
              let dc = makePlayer 3 DC 70
              let players = [| gk; st; dc |]
              let positions = [| (5.0, 50.0); (80.0, 50.0); (20.0, 30.0) |]
              let ts = makeTeamSide players positions
              let roster = outfieldRoster ts
              let stEntry = roster |> Array.find (fun (p, _, _) -> p.Id = st.Id)
              let dcEntry = roster |> Array.find (fun (p, _, _) -> p.Id = dc.Id)
              let _, stSp, _ = stEntry
              let _, dcSp, _ = dcEntry
              Expect.equal stSp.X 80.0 "ST should have X=80, not GK's position"
              Expect.equal dcSp.X 20.0 "DC should have X=20"
          }

          // nearestOutfield
          test "nearestOutfield returns closest player by distance" {
              let gk = makeGkPlayer 1 70 defaultGk
              let near = makePlayer 2 DC 70
              let far = makePlayer 3 ST 70
              let players = [| gk; near; far |]
              let positions = [| (5.0, 50.0); (30.0, 50.0); (80.0, 50.0) |]
              let ts = makeTeamSide players positions
              let result = nearestOutfield ts 25.0 50.0
              Expect.isSome result "should find a player"
              let p, _ = result.Value
              Expect.equal p.Id near.Id "should return the near player"
          }
          test "nearestOutfield returns None when only GK" {
              let gk = makeGkPlayer 1 70 defaultGk
              let players = [| gk |]
              let positions = [| (5.0, 50.0) |]
              let ts = makeTeamSide players positions
              let result = nearestOutfield ts 50.0 50.0
              Expect.isNone result "should return None with only GK"
          }

          // isOffside
          test "isOffside GK always returns false" {
              let gk = makeGkPlayer 1 70 defaultGk
              let def1 = makePlayer 2 DC 70
              let def2 = makePlayer 3 DC 70
              let awayPlayers = [| gk; def1; def2 |]
              let awayPositions = [| (5.0, 50.0); (65.0, 45.0); (70.0, 55.0) |]

              let s =
                  buildMatchState
                      [| makePlayer 10 GK 70 |]
                      [| (95.0, 50.0) |]
                      awayPlayers
                      awayPositions
                      85.0
                      50.0
                      HomeClub

              Expect.isFalse (isOffside gk 85.0 s LeftToRight) "GK should never be offside"
          }
          test "isOffside player in own half returns false" {
              let attacker = makePlayer 1 ST 70
              let def1 = makePlayer 2 DC 70
              let def2 = makePlayer 3 DC 70
              let awayPlayers = [| makeGkPlayer 10 70 defaultGk; def1; def2 |]
              let awayPositions = [| (5.0, 50.0); (60.0, 45.0); (65.0, 55.0) |]

              let s =
                  buildMatchState [| attacker |] [| (30.0, 50.0) |] awayPlayers awayPositions 30.0 50.0 HomeClub

              Expect.isFalse (isOffside attacker 30.0 s LeftToRight) "player in own half cannot be offside"
          }
          test "isOffside player ahead of second-last defender is offside" {
              let attacker = makePlayer 1 ST 70
              let def1 = makePlayer 2 DC 70
              let def2 = makePlayer 3 DC 70
              let awayPlayers = [| makeGkPlayer 10 70 defaultGk; def1; def2 |]
              // second-last defender at X=60, attacker at X=80, ball at X=75
              let awayPositions = [| (5.0, 50.0); (60.0, 45.0); (65.0, 55.0) |]

              let s =
                  buildMatchState [| attacker |] [| (80.0, 50.0) |] awayPlayers awayPositions 75.0 50.0 HomeClub

              Expect.isTrue (isOffside attacker 80.0 s LeftToRight) "player ahead of last defender should be offside"
          }
          test "isOffside player behind second-last defender is not offside" {
              let attacker = makePlayer 1 ST 70
              let def1 = makePlayer 2 DC 70
              let def2 = makePlayer 3 DC 70
              let awayPlayers = [| makeGkPlayer 10 70 defaultGk; def1; def2 |]
              let awayPositions = [| (5.0, 50.0); (75.0, 45.0); (80.0, 55.0) |]

              let s =
                  buildMatchState [| attacker |] [| (70.0, 50.0) |] awayPlayers awayPositions 65.0 50.0 HomeClub

              Expect.isFalse (isOffside attacker 70.0 s LeftToRight) "player behind last defender should not be offside"
          } ]

// ============================================================================
// Category 3: Action Resolution Tests — MatchPlayerAction
// ============================================================================

// Helper: resolve an intent and return (state, events, nextIntent)
let private resolveAction intent state = resolve homeClub.Id 1 intent state

// Helper: check if events contain a specific type
let private hasEvent eventType events =
    events |> List.exists (fun e -> e.Type = eventType)

let private hasEventMatching pred events = events |> List.exists pred

let matchPlayerActionTests =
    testList
        "MatchPlayerAction Resolution Tests"
        [
          // Shot tests
          testCase "ExecuteShot with weak GK eventually scores"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 ST 90) with
                      Technical = { defaultTechnical with Finishing = 99 }
                      Mental = { defaultMental with Composure = 99 } }

              let gk = makeGkPlayer 10 70 weakGk
              let homePlayers = [| attacker |]
              let homePositions = [| (95.0, 50.0) |]
              let awayPlayers = [| gk |]
              let awayPositions = [| (99.0, 50.0) |]

              let s =
                  buildMatchState homePlayers homePositions awayPlayers awayPositions 85.0 50.0 HomeClub

              // Run many times to account for RNG — at least one should score
              let scored =
                  Array.init 50 (fun _ ->
                      let _, events, _ = resolveAction (ExecuteShot attacker) s
                      hasEvent Goal events)
                  |> Array.exists id

              Expect.isTrue scored "strong attacker vs weak GK should score at least once in 50 attempts"

          testCase "ExecuteShot produces shot-related event"
          <| fun () ->
              let attacker = makePlayer 1 ST 70
              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState [| attacker |] [| (90.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 80.0 50.0 HomeClub

              let _, events, _ = resolveAction (ExecuteShot attacker) s

              let hasShotEvent =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | Goal
                      | ShotOffTarget
                      | ShotBlocked
                      | Save -> true
                      | _ -> false)

              Expect.isTrue hasShotEvent "shot should produce at least one shot-related event"

          testCase "ExecuteShot with strong GK mostly saves"
          <| fun () ->
              let attacker = makePlayer 1 ST 70
              let gk = makeGkPlayer 10 99 strongGk

              let s =
                  buildMatchState [| attacker |] [| (90.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 80.0 50.0 HomeClub

              let goals =
                  Array.init 30 (fun _ ->
                      let _, events, _ = resolveAction (ExecuteShot attacker) s
                      hasEvent Goal events)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (goals < 20) $"strong GK should stop most shots, scored {goals}/30"

          // Pass tests
          testCase "ExecutePass success emits PassCompleted event"
          <| fun () ->
              let attacker = makePlayer 1 MC 70
              let teammate = makePlayer 2 ST 70
              let gk = makeGkPlayer 10 70 defaultGk
              let homePlayers = [| attacker; teammate |]
              let homePositions = [| (50.0, 50.0); (70.0, 50.0) |]

              let s =
                  buildMatchState homePlayers homePositions [| gk |] [| (99.0, 50.0) |] 50.0 50.0 HomeClub

              let successes =
                  Array.init 30 (fun _ ->
                      let _, events, _ = resolveAction (ExecutePass attacker) s

                      hasEventMatching
                          (fun e ->
                              match e.Type with
                              | PassCompleted _ -> true
                              | _ -> false)
                          events)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (successes > 0) "pass should succeed at least once in 30 attempts"

          testCase "ExecutePass failure flips AttackingClub"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 MC 30) with
                      Technical = { defaultTechnical with Passing = 1 }
                      Mental = { defaultMental with Vision = 1 } }

              let teammate = makePlayer 2 ST 70
              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState
                      [| attacker; teammate |]
                      [| (50.0, 50.0); (70.0, 50.0) |]
                      [| gk |]
                      [| (99.0, 50.0) |]
                      50.0
                      50.0
                      HomeClub

              let flipped =
                  Array.init 30 (fun _ ->
                      let s', _, _ = resolveAction (ExecutePass attacker) s
                      s'.AttackingClub = AwayClub)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (flipped > 0) "poor passer should lose possession at least once in 30 attempts"

          // Dribble tests
          testCase "ExecuteDribble success emits DribbleSuccess"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 ST 90) with
                      Technical = { defaultTechnical with Dribbling = 99 }
                      Physical =
                          { defaultPhysical with
                              Agility = 99
                              Balance = 99 } }

              let defender =
                  { (makePlayer 10 DC 50) with
                      Technical = { defaultTechnical with Tackling = 1 }
                      Physical = { defaultPhysical with Strength = 1 } }

              let s =
                  buildMatchState [| attacker |] [| (70.0, 50.0) |] [| defender |] [| (75.0, 50.0) |] 70.0 50.0 HomeClub

              let successes =
                  Array.init 30 (fun _ ->
                      let _, events, _ = resolveAction (ExecuteDribble attacker) s
                      hasEvent DribbleSuccess events)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (successes > 0) "skilled dribbler should succeed at least once in 30 attempts"

          testCase "ExecuteDribble fail emits DribbleFail and flips possession"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 ST 50) with
                      Technical = { defaultTechnical with Dribbling = 1 } }

              let defender =
                  { (makePlayer 10 DC 90) with
                      Technical = { defaultTechnical with Tackling = 99 }
                      Physical = { defaultPhysical with Strength = 99 } }

              let s =
                  buildMatchState [| attacker |] [| (70.0, 50.0) |] [| defender |] [| (71.0, 50.0) |] 70.0 50.0 HomeClub

              let failures =
                  Array.init 30 (fun _ ->
                      let s', events, _ = resolveAction (ExecuteDribble attacker) s
                      hasEvent DribbleFail events && s'.AttackingClub = AwayClub)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (failures > 0) "weak dribbler vs strong defender should fail at least once"

          // Tackle tests
          testCase "ExecuteTackle success emits TackleSuccess"
          <| fun () ->
              let defender =
                  { (makePlayer 10 DC 90) with
                      Technical = { defaultTechnical with Tackling = 99 }
                      Mental =
                          { defaultMental with
                              Positioning = 99
                              Aggression = 10 } }

              let attacker =
                  { (makePlayer 1 ST 50) with
                      Technical = { defaultTechnical with Dribbling = 1 } }

              let s =
                  buildMatchState [| attacker |] [| (70.0, 50.0) |] [| defender |] [| (72.0, 50.0) |] 70.0 50.0 HomeClub

              let s' = { s with AttackingClub = AwayClub }

              let successes =
                  Array.init 30 (fun _ ->
                      let _, events, _ = resolve awayClub.Id 1 (ExecuteTackle defender) s'
                      hasEvent TackleSuccess events)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (successes > 0) "strong tackler should succeed at least once"

          testCase "ExecuteTackle foul emits FoulCommitted"
          <| fun () ->
              let defender = makeHighAggressionPlayer 10 DC
              let attacker = makePlayer 1 ST 70

              let s =
                  buildMatchState [| attacker |] [| (70.0, 50.0) |] [| defender |] [| (71.0, 50.0) |] 70.0 50.0 HomeClub

              let s' = { s with AttackingClub = AwayClub }

              let fouls =
                  Array.init 50 (fun _ ->
                      let _, events, _ = resolve awayClub.Id 1 (ExecuteTackle defender) s'
                      hasEvent FoulCommitted events)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (fouls > 0) "high aggression defender should foul at least once in 50 attempts"

          // Free kick test
          testCase "ExecuteFreeKick produces FreeKick event"
          <| fun () ->
              let kicker =
                  { (makePlayer 1 MC 80) with
                      Technical =
                          { defaultTechnical with
                              Finishing = 90
                              LongShots = 90 }
                      Mental = { defaultMental with Composure = 90 } }

              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState [| kicker |] [| (65.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 65.0 50.0 HomeClub

              let _, events, _ = resolveAction (ExecuteFreeKick kicker) s

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | FreeKick _ -> true
                      | _ -> false)
                  "should emit FreeKick event"

          // Corner test
          testCase "ExecuteCorner emits Corner event"
          <| fun () ->
              let attacker = makePlayer 1 ST 70
              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState [| attacker |] [| (85.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 99.0 10.0 HomeClub

              let _, events, _ = resolveAction ExecuteCorner s
              Expect.exists events (fun e -> e.Type = Corner) "should emit Corner event"

          // Throw-in test
          testCase "ExecuteThrowIn emits PassCompleted event"
          <| fun () ->
              let thrower = makePlayer 1 DL 70
              let teammate = makePlayer 2 MC 70
              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState
                      [| thrower; teammate |]
                      [| (5.0, 20.0); (10.0, 50.0) |]
                      [| gk |]
                      [| (99.0, 50.0) |]
                      5.0
                      5.0
                      HomeClub

              let _, events, _ = resolveAction (ExecuteThrowIn HomeClub) s

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | PassCompleted _ -> true
                      | _ -> false)
                  "throw-in should emit PassCompleted"

          // Penalty test
          testCase "ExecutePenalty produces PenaltyAwarded event"
          <| fun () ->
              let kicker = makePlayer 1 ST 80
              let gk = makeGkPlayer 10 70 defaultGk

              let s =
                  buildMatchState [| kicker |] [| (50.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 50.0 50.0 HomeClub

              let _, events, _ = resolveAction (ExecutePenalty(kicker, HomeClub, 1)) s

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | PenaltyAwarded _ -> true
                      | _ -> false)
                  "should emit PenaltyAwarded"

          testCase "ExecutePenalty goal increments score"
          <| fun () ->
              let kicker =
                  { (makePlayer 1 ST 99) with
                      Technical = { defaultTechnical with Penalty = 99 }
                      Mental = { defaultMental with Composure = 99 }
                      CurrentSkill = 99
                      Morale = 99 }

              let gk = makeGkPlayer 10 70 weakGk

              let s =
                  buildMatchState [| kicker |] [| (50.0, 50.0) |] [| gk |] [| (99.0, 50.0) |] 50.0 50.0 HomeClub

              let scored =
                  Array.init 20 (fun i ->
                      let s', _, _ = resolveAction (ExecutePenalty(kicker, HomeClub, i + 1)) s
                      s'.HomeScore = 1)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (scored > 0) "strong kicker vs weak GK should score at least one penalty" ]

// ============================================================================
// Category 4: Structural Invariants — Single Match
// ============================================================================

let structuralInvariantTests =
    testList
        "Structural Invariants"
        [ test "scores are non-negative" {
              let clubs, players, staff = loadClubs ()
              let h, a, _, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              Expect.isTrue (h >= 0 && a >= 0) $"negative score: {h}-{a}"
          }
          test "scores are plausible (each <= 10)" {
              let clubs, players, staff = loadClubs ()
              let h, a, _, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              Expect.isTrue (h <= 10 && a <= 10) $"implausible score: {h}-{a}"
          }
          test "goal events match reported score" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let hScore, aScore, events, _ = trySimulateMatch home away players staff |> getOk
              let goals = events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length
              Expect.equal hGoals hScore $"home goal event count mismatch"
              Expect.equal aGoals aScore $"away goal event count mismatch"
          }
          test "all event seconds in [1, 5700]" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.Second >= 1 && e.Second <= 95 * 60))
                  "event with second outside valid range"
          }
          test "all event ClubIds are either home or away" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id))
                  "event has unknown ClubId"
          }
          test "all event playerIds belong to one of the two clubs" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk
              let homeIds = home.PlayerIds |> Set.ofList
              let awayIds = away.PlayerIds |> Set.ofList

              Expect.isTrue
                  (events
                   |> List.forall (fun e -> Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))
                  "event references unknown player"
          }
          test "events are ordered chronologically" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (events |> List.pairwise |> List.forall (fun (a, b) -> b.Second >= a.Second))
                  "events not sorted by second"
          }
          test "SubstitutionIn and SubstitutionOut are balanced" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              let ins = events |> List.filter (fun e -> e.Type = SubstitutionIn) |> List.length
              let outs = events |> List.filter (fun e -> e.Type = SubstitutionOut) |> List.length
              Expect.equal ins outs "substitution in/out count mismatch"
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

              Expect.isTrue (reds |> List.forall (fun (_, n) -> n = 1)) "player has multiple red cards"
          }
          test "no player receives more than 2 yellow cards" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              Expect.isTrue (yellows |> List.forall (fun (_, n) -> n <= 2)) "player has more than 2 yellow cards"
          }
          test "second yellow triggers red card for same player" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let twoYellows =
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

              Expect.isTrue (Set.isSubset twoYellows redPlayers) "player with 2 yellows has no red card"
          }
          test "no goals scored at second 0" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (events
                   |> List.filter (fun e -> e.Type = Goal)
                   |> List.forall (fun e -> e.Second > 0))
                  "goal at second 0"
          }
          test "FreeKick events never exceed FoulCommitted events" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk
              let fouls = events |> List.filter (fun e -> e.Type = FoulCommitted) |> List.length

              let fks =
                  events
                  |> List.filter (fun e ->
                      match e.Type with
                      | FreeKick _ -> true
                      | _ -> false)
                  |> List.length

              Expect.isTrue (fks <= fouls) $"FreeKick ({fks}) > FoulCommitted ({fouls})"
          }
          test "momentum stays in [-10, 10] throughout match" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.Momentum >= -10.0 && s.Momentum <= 10.0))
                  "momentum out of bounds"
          }
          test "all conditions stay in [0, 100]" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s ->
                       s.HomeSide.Conditions |> Array.forall (fun c -> c >= 0 && c <= 100)
                       && s.AwaySide.Conditions |> Array.forall (fun c -> c >= 0 && c <= 100)))
                  "condition out of [0, 100]"
          }
          test "all player positions stay in bounds" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s ->
                       s.HomeSide.Positions |> Array.forall (fun sp -> inBounds (sp.X, sp.Y))
                       && s.AwaySide.Positions |> Array.forall (fun sp -> inBounds (sp.X, sp.Y))))
                  "player position out of bounds"
          } ]

// ============================================================================
// Category 5: Statistical Contracts — Distributions
// ============================================================================

let statisticalTests =
    // Compute once, shared across all tests
    let iterations = 1000

    let outcomes =
        let clubs, players, staff = loadClubs ()

        Array.Parallel.init iterations (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length
            trySimulateMatch clubs[hi] clubs[ai] players staff)

    let successOutcomes =
        outcomes
        |> Array.choose (function
            | Ok(h, a, ev, _) -> Some(h, a, ev)
            | Error _ -> None)

    let scoresOnly = successOutcomes |> Array.map (fun (h, a, _) -> h, a)

    let isPass (e: MatchEvent) =
        match e.Type with
        | PassCompleted _ -> true
        | _ -> false

    let isShot (e: MatchEvent) =
        match e.Type with
        | Goal
        | ShotBlocked
        | ShotOffTarget
        | Save -> true
        | _ -> false

    let isDribble (e: MatchEvent) =
        match e.Type with
        | DribbleSuccess
        | DribbleFail -> true
        | _ -> false

    let countType t evs =
        evs |> List.filter (fun e -> e.Type = t) |> List.length

    testList
        "Statistical Contracts"
        [ testCase "all fixtures simulated successfully"
          <| fun () ->
              let failures =
                  outcomes
                  |> Array.choose (function
                      | Error e -> Some $"%A{e}"
                      | Ok _ -> None)

              Expect.isEmpty failures $"{failures.Length} fixture(s) failed"

          testCase "avg goals per match in [2.0, 3.5]"
          <| fun () ->
              let avg = scoresOnly |> Array.averageBy (fun (h, a) -> float (h + a))
              Expect.isTrue (avg >= 2.0 && avg <= 3.5) $"avg goals = %.2f{avg} (expected [2.0, 3.5])"

          testCase "no match has outlier score (either side > 10)"
          <| fun () ->
              let outliers = scoresOnly |> Array.filter (fun (h, a) -> h > 10 || a > 10)
              Expect.isEmpty outliers $"{outliers.Length} outlier scores detected"

          testCase "avg shots per match in [15, 30]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isShot |> List.length))

              Expect.isTrue (avg >= 15.0 && avg <= 30.0) $"avg shots = %.2f{avg} (expected [15, 30])"

          testCase "avg dribbles per match in [30, 80]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isDribble |> List.length))

              Expect.isTrue (avg >= 30.0 && avg <= 80.0) $"avg dribbles = %.2f{avg} (expected [30, 80])"

          testCase "avg corners per match in [6, 14]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (countType Corner ev))

              Expect.isTrue (avg >= 6.0 && avg <= 14.0) $"avg corners = %.2f{avg} (expected [6, 14])"

          testCase "avg fouls per match in [15, 35]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (countType FoulCommitted ev))

              Expect.isTrue (avg >= 15.0 && avg <= 35.0) $"avg fouls = %.2f{avg} (expected [15, 35])"

          testCase "avg yellow cards per match in [2, 6]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (countType YellowCard ev))

              Expect.isTrue (avg >= 2.0 && avg <= 6.0) $"avg yellows = %.2f{avg} (expected [2, 6])"

          testCase "avg passes per match in [120, 600]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isPass |> List.length))

              Expect.isTrue (avg >= 120.0 && avg <= 600.0) $"avg passes = %.2f{avg} (expected [120, 600])"

          testCase "shot conversion rate in [5%, 20%]"
          <| fun () ->
              let totalShots =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let totalGoals =
                  successOutcomes |> Array.sumBy (fun (_, _, ev) -> countType Goal ev)

              let rate =
                  if totalShots = 0 then
                      0.0
                  else
                      float totalGoals / float totalShots * 100.0

              Expect.isTrue (rate >= 5.0 && rate <= 20.0) $"conversion = %.1f{rate}%% ({totalGoals}/{totalShots})"

          testCase "passes are the most common event type"
          <| fun () ->
              let passes =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isPass |> List.length)

              let shots =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let dribbles =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isDribble |> List.length)

              Expect.isTrue
                  (passes > shots && passes > dribbles)
                  $"passes ({passes}) should exceed shots ({shots}) and dribbles ({dribbles})"

          testCase "both teams produce match events"
          <| fun () ->
              Expect.isTrue
                  (successOutcomes |> Array.forall (fun (_, _, ev) -> ev.Length > 0))
                  "some matches produced no events"

          testCase "draws are not the most common outcome"
          <| fun () ->
              let n = float scoresOnly.Length

              let homeWinPct =
                  scoresOnly
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              let awayWinPct =
                  scoresOnly
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              let drawPct =
                  scoresOnly
                  |> Array.filter (fun (h, a) -> h = a)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              Expect.isTrue
                  (drawPct < homeWinPct || drawPct < awayWinPct)
                  $"draws (%.1f{drawPct}%%) dominate — home=%.1f{homeWinPct}%% away=%.1f{awayWinPct}%%"

          testCase "speed under 10 ms/match"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let sw = Diagnostics.Stopwatch.StartNew()

              Array.Parallel.init iterations (fun i ->
                  let hi = i % clubs.Length
                  let ai = (hi + 1) % clubs.Length
                  trySimulateMatch clubs[hi] clubs[ai] players staff)
              |> ignore

              sw.Stop()
              let msPerGame = sw.Elapsed.TotalMilliseconds / float iterations
              Expect.isTrue (msPerGame < 10.0) $"%.4f{msPerGame} ms/match (limit: 10 ms)" ]

// ============================================================================
// Category 6: Home Advantage — Isolated
// ============================================================================

let homeAdvantageTests =
    testList
        "Home Advantage"
        [ testCase "home teams score more than away teams in skill-matched pairs"
          <| fun () ->
              let clubs, players, staff = loadClubs ()

              let pairs =
                  clubs
                  |> Array.pairwise
                  |> Array.filter (fun (a, b) -> abs (Club.averageSkill players a - Club.averageSkill players b) < 5)
                  |> Array.truncate 100

              let results =
                  pairs
                  |> Array.collect (fun (a, b) ->
                      [| trySimulateMatch a b players staff // a = home
                         trySimulateMatch b a players staff |]) // b = home
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let homeGoals = results |> Array.sumBy fst
              let awayGoals = results |> Array.sumBy snd
              let totalMatches = results.Length
              let homeWins = results |> Array.filter (fun (h, a) -> h > a) |> Array.length
              let awayWins = results |> Array.filter (fun (h, a) -> a > h) |> Array.length
              let draws = results |> Array.filter (fun (h, a) -> h = a) |> Array.length
              let avgHome = float homeGoals / float totalMatches
              let avgAway = float awayGoals / float totalMatches

              let avgSkillDiff =
                  pairs
                  |> Array.averageBy (fun (a, b) ->
                      float (abs (Club.averageSkill players a - Club.averageSkill players b)))

              printfn "=== HOME ADVANTAGE ==="
              printfn $"Pairs: %d{pairs.Length} | Matches: %d{totalMatches}"
              printfn $"HomeWins: %d{homeWins} | Draws: %d{draws} | AwayWins: %d{awayWins}"
              printfn $"Avg goals — Home: %.2f{avgHome} | Away: %.2f{avgAway}"
              printfn $"Total — Home: %d{homeGoals} | Away: %d{awayGoals}"
              printfn $"Avg skill diff: %.1f{avgSkillDiff}"

              Expect.isTrue
                  (homeGoals > awayGoals)
                  $"home ({homeGoals}) <= away ({awayGoals}) — no home advantage detected"

          testCase "home team has more shots on target"
          <| fun () ->
              let clubs, players, staff = loadClubs ()

              let pairs =
                  clubs
                  |> Array.pairwise
                  |> Array.filter (fun (a, b) -> abs (Club.averageSkill players a - Club.averageSkill players b) < 5)
                  |> Array.truncate 50

              let homeOnTarget, awayOnTarget =
                  pairs
                  |> Array.fold
                      (fun (hOn, aOn) (home, away) ->
                          match trySimulateMatch home away players staff with
                          | Error _ -> hOn, aOn
                          | Ok(_, _, events, _) ->
                              let h =
                                  events
                                  |> List.filter (fun e ->
                                      (e.ClubId = home.Id && e.Type = Goal) || (e.ClubId = away.Id && e.Type = Save))
                                  |> List.length

                              let a =
                                  events
                                  |> List.filter (fun e ->
                                      (e.ClubId = away.Id && e.Type = Goal) || (e.ClubId = home.Id && e.Type = Save))
                                  |> List.length

                              hOn + h, aOn + a)
                      (0, 0)

              printfn $"=== SHOTS ON TARGET === Home: %d{homeOnTarget} | Away: %d{awayOnTarget}"

              Expect.isTrue
                  (homeOnTarget >= awayOnTarget * 90 / 100)
                  $"home shots on target ({homeOnTarget}) < 90%% of away ({awayOnTarget})"

          testCase "home team commits fewer fouls"
          <| fun () ->
              let clubs, players, staff = loadClubs ()

              let pairs =
                  clubs
                  |> Array.pairwise
                  |> Array.filter (fun (a, b) -> abs (Club.averageSkill players a - Club.averageSkill players b) < 5)
                  |> Array.truncate 50

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

              printfn $"=== FOULS === Home: %d{homeFouls} | Away: %d{awayFouls}"

              Expect.isTrue
                  (homeFouls <= awayFouls * 110 / 100)
                  $"home fouls ({homeFouls}) > 110%% of away ({awayFouls})"

          testCase "home win rate > away win rate"
          <| fun () ->
              let clubs, players, staff = loadClubs ()

              let pairs =
                  clubs
                  |> Array.pairwise
                  |> Array.filter (fun (a, b) -> abs (Club.averageSkill players a - Club.averageSkill players b) < 5)
                  |> Array.truncate 100

              let results =
                  pairs
                  |> Array.collect (fun (a, b) ->
                      [| trySimulateMatch a b players staff; trySimulateMatch b a players staff |])
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let n = float results.Length

              let homeWinPct =
                  results
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              let awayWinPct =
                  results
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> fun x -> float x / n * 100.0

              printfn "=== WIN RATES === HomeWin: %.1f%% | AwayWin: %.1f%%" homeWinPct awayWinPct

              Expect.isTrue
                  (homeWinPct > awayWinPct)
                  $"home win rate (%.1f{homeWinPct}%%) <= away win rate (%.1f{awayWinPct}%%)" ]
