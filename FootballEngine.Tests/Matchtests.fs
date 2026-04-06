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
    { (makePlayer id pos 10) with
        Mental =
            { defaultMental with
                Aggression = 20
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

let private homeClub = makeClub 1
let private awayClub = makeClub 2

let private makeCtx homePlayers awayPlayers =
    { Home = homeClub
      Away = awayClub
      HomeCoach = Unchecked.defaultof<Staff>
      AwayCoach = Unchecked.defaultof<Staff>
      HomePlayers = homePlayers
      AwayPlayers = awayPlayers
      HomeBasePositions = Array.empty
      AwayBasePositions = Array.empty
      IsKnockoutMatch = false }

let private minimalState homeScore awayScore =
    let state = SimState()
    state.HomeScore <- homeScore
    state.AwayScore <- awayScore
    state

let private buildSimState
    (homePlayers: Player[])
    (homePositions: (float * float)[])
    (awayPlayers: Player[])
    (awayPositions: (float * float)[])
    (ballX: float)
    (ballY: float)
    (attacking: ClubSide)
    =
    let ctx =
        { Home = homeClub
          Away = awayClub
          HomeCoach = Unchecked.defaultof<Staff>
          AwayCoach = Unchecked.defaultof<Staff>
          HomePlayers = homePlayers
          AwayPlayers = awayPlayers
          HomeBasePositions = homePositions |> Array.map (fun (x, y) -> defaultSpatialAt x y)
          AwayBasePositions = awayPositions |> Array.map (fun (x, y) -> defaultSpatialAt x y)
          IsKnockoutMatch = false }

    let state = SimState()
    let hPos = homePositions |> Array.map (fun (x, y) -> defaultSpatialAt x y)
    let aPos = awayPositions |> Array.map (fun (x, y) -> defaultSpatialAt x y)

    state.HomeSlots <-
        Array.init homePlayers.Length (fun i ->
            let p = homePlayers[i]

            PlayerSlot.Active
                { Player = p
                  Pos = hPos[i]
                  Condition = p.Condition
                  Mental = MentalState.initial p
                  Directives = Array.empty })

    state.AwaySlots <-
        Array.init awayPlayers.Length (fun i ->
            let p = awayPlayers[i]

            PlayerSlot.Active
                { Player = p
                  Pos = aPos[i]
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
        { Position = defaultSpatialAt ballX ballY
          Spin = Spin.zero
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true }

    state.AttackingClub <- attacking
    ctx, state

let private resolveAction intent (ctx, state) = resolve homeClub.Id 1 intent ctx state

let private hasEvent eventType events =
    events |> List.exists (fun e -> e.Type = eventType)

let private hasEventMatching pred events = events |> List.exists pred

let matchStateOpsTests =
    testList
        "SimStateOps Unit Tests"
        [ test "ClubSide.flip HomeClub → AwayClub" { Expect.equal (ClubSide.flip HomeClub) AwayClub "" }
          test "ClubSide.flip AwayClub → HomeClub" { Expect.equal (ClubSide.flip AwayClub) HomeClub "" }
          test "ClubSide.flip is involution" {
              Expect.equal (ClubSide.flip (ClubSide.flip HomeClub)) HomeClub ""
              Expect.equal (ClubSide.flip (ClubSide.flip AwayClub)) AwayClub ""
          }

          test "phaseFromBallZone LeftToRight x=10 → BuildUp" {
              Expect.equal (phaseFromBallZone LeftToRight 10.0) BuildUp ""
          }
          test "phaseFromBallZone LeftToRight x=50 → Midfield" {
              Expect.equal (phaseFromBallZone LeftToRight 50.0) Midfield ""
          }
          test "phaseFromBallZone LeftToRight x=80 → Attack" {
              Expect.equal (phaseFromBallZone LeftToRight 80.0) Attack ""
          }
          test "phaseFromBallZone RightToLeft mirrors LeftToRight" {
              Expect.equal (phaseFromBallZone RightToLeft 95.0) BuildUp ""
              Expect.equal (phaseFromBallZone RightToLeft 20.0) Attack ""
          }

          test "pressureMultiplier home losing → > 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 2
              Expect.isTrue (pressureMultiplier homeClub.Id ctx s > 1.0) ""
          }
          test "pressureMultiplier home winning → < 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 2 0
              Expect.isTrue (pressureMultiplier homeClub.Id ctx s < 1.0) ""
          }
          test "pressureMultiplier away losing → > 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 2 0
              Expect.isTrue (pressureMultiplier awayClub.Id ctx s > 1.0) ""
          }
          test "pressureMultiplier values in [0.1, 2.0]" {
              let ctx = makeCtx Array.empty Array.empty
              let losing = pressureMultiplier homeClub.Id ctx (minimalState 0 3)
              let winning = pressureMultiplier homeClub.Id ctx (minimalState 3 0)
              Expect.isTrue (losing >= 0.1 && losing <= 2.0) $"losing={losing}"
              Expect.isTrue (winning >= 0.1 && winning <= 2.0) $"winning={winning}"
          }

          test "matchUrgency home losing late → > 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 2
              s.SubTick <- 1800
              Expect.isTrue (matchUrgency homeClub.Id ctx s > 1.0) ""
          }
          test "matchUrgency home winning → < 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 2 0
              Expect.isTrue (matchUrgency homeClub.Id ctx s < 1.0) ""
          }
          test "matchUrgency 0-0 early → ~1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              let u = matchUrgency homeClub.Id ctx s
              Expect.isTrue (u >= 0.8 && u <= 1.2) $"urgency={u}"
          }
          test "matchUrgency 0-0 late → > 1.0" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              s.SubTick <- 1800
              Expect.isTrue (matchUrgency homeClub.Id ctx s > 1.0) ""
          }

          test "adjustMomentum positive delta increases" {
              let s = minimalState 0 0
              s.Momentum <- 0.0
              adjustMomentum LeftToRight 2.0 s
              Expect.isTrue (s.Momentum > 0.0) $"momentum={s.Momentum}"
          }
          test "adjustMomentum clamps to [-10, 10]" {
              let s = minimalState 0 0
              s.Momentum <- 9.0
              adjustMomentum LeftToRight 5.0 s
              Expect.isTrue (s.Momentum <= 10.0) $"momentum={s.Momentum}"
          }

          test "goalDiff homeClub = HomeScore - AwayScore" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 3 1
              Expect.equal (goalDiff homeClub.Id ctx s) 2 ""
          }
          test "goalDiff awayClub = AwayScore - HomeScore" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 3 1
              Expect.equal (goalDiff awayClub.Id ctx s) -2 ""
          }

          test "awardGoal HomeClub increments HomeScore" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              let events = awardGoal HomeClub None 0 ctx s
              Expect.equal s.HomeScore 1 ""
              Expect.equal s.AwayScore 0 ""
              Expect.isEmpty events ""
          }
          test "awardGoal AwayClub increments AwayScore" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              let events = awardGoal AwayClub None 0 ctx s
              Expect.equal s.AwayScore 1 ""
              Expect.equal s.HomeScore 0 ""
              Expect.isEmpty events ""
          }
          test "awardGoal resets ball to centre" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              awardGoal HomeClub None 0 ctx s |> ignore
              Expect.equal s.Ball.Position.X PhysicsContract.HalfwayLineX ""
              Expect.equal s.Ball.Position.Y (PhysicsContract.PitchWidth / 2.0) ""
          }
          test "awardGoal with scorerId emits Goal event" {
              let ctx = makeCtx Array.empty Array.empty
              let s = minimalState 0 0
              let events = awardGoal HomeClub (Some 99) 0 ctx s
              Expect.exists events (fun e -> e.Type = Goal) ""
          }

          test "resetBallToCenter sets metric centre" {
              let s = minimalState 0 0

              s.Ball <-
                  { Position = defaultSpatialAt 80.0 30.0
                    Spin = Spin.zero
                    ControlledBy = Some 1
                    LastTouchBy = Some 1
                    IsInPlay = true }

              resetBallToCenter s
              Expect.equal s.Ball.Position.X PhysicsContract.HalfwayLineX ""
              Expect.equal s.Ball.Position.Y (PhysicsContract.PitchWidth / 2.0) ""
              Expect.equal s.Ball.Position.Vx 0.0 ""
              Expect.equal s.Ball.Position.Vy 0.0 ""
              Expect.equal s.Ball.LastTouchBy None ""
              Expect.equal s.Ball.ControlledBy None ""
          }


          test "elapsedSeconds returns correct real seconds" {
              let s = minimalState 0 0
              s.SubTick <- 1200
              Expect.equal (PhysicsContract.subTicksToSeconds s.SubTick) 30.0 ""
          }
          test "elapsedSeconds for full match" {
              let s = minimalState 0 0
              s.SubTick <- PhysicsContract.FullTimeSubTick
              Expect.equal (PhysicsContract.subTicksToSeconds s.SubTick) (95.0 * 60.0) ""
          } ]

let matchSpatialTests =
    testList
        "MatchSpatial Unit Tests"
        [ test "teamRoster zips Players Positions Conditions correctly" {
              let players = [| makePlayer 1 ST 10; makePlayer 2 MC 10; makePlayer 3 GK 10 |]

              let positions =
                  players
                  |> Array.mapi (fun i _ ->
                      match i with
                      | 0 -> defaultSpatialAt 10.0 34.0
                      | 1 -> defaultSpatialAt 52.5 34.0
                      | _ -> defaultSpatialAt 5.0 34.0)

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

              let positions =
                  [| defaultSpatialAt 5.0 34.0
                     defaultSpatialAt 80.0 34.0
                     defaultSpatialAt 20.0 34.0 |]

              let conditions = [| 100; 100; 100 |]
              let roster = outfieldRoster players positions conditions
              Expect.equal roster.Length 2 ""
              Expect.isFalse (roster |> Array.exists (fun (p, _, _) -> p.Position = GK)) ""
          }

          test "nearestOutfield returns closest player by distance" {
              let gk = makeGkPlayer 1 10 defaultGk
              let near = makePlayer 2 DC 10
              let far = makePlayer 3 ST 10
              let players = [| gk; near; far |]

              let positions =
                  [| defaultSpatialAt 5.0 34.0
                     defaultSpatialAt 30.0 34.0
                     defaultSpatialAt 80.0 34.0 |]

              let result = nearestOutfield players positions 25.0 34.0
              Expect.isSome result ""
              let p, _ = result.Value
              Expect.equal p.Id near.Id ""
          }

          test "isOffside GK always returns false" {
              let gk = makeGkPlayer 1 10 defaultGk
              let def1 = makePlayer 2 DC 10
              let def2 = makePlayer 3 DC 10
              let awayPlayers = [| gk; def1; def2 |]
              let awayPositions = [| (5.0, 34.0); (65.0, 30.0); (70.0, 38.0) |]

              let ctx, state =
                  buildSimState
                      [| makePlayer 10 GK 10 |]
                      [| (95.0, 34.0) |]
                      awayPlayers
                      awayPositions
                      85.0
                      34.0
                      HomeClub

              Expect.isFalse (isOffside gk 85.0 ctx state LeftToRight) ""
          }

          test "isOffside player in own half returns false" {
              let attacker = makePlayer 1 ST 10

              let awayPlayers =
                  [| makeGkPlayer 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]

              let awayPositions = [| (5.0, 34.0); (60.0, 30.0); (65.0, 38.0) |]

              let ctx, state =
                  buildSimState [| attacker |] [| (30.0, 34.0) |] awayPlayers awayPositions 30.0 34.0 HomeClub

              Expect.isFalse (isOffside attacker 30.0 ctx state LeftToRight) ""
          }

          test "isOffside player ahead of second-last defender is offside" {
              let attacker = makePlayer 1 ST 10

              let awayPlayers =
                  [| makeGkPlayer 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]

              let awayPositions = [| (5.0, 34.0); (60.0, 30.0); (65.0, 38.0) |]

              let ctx, state =
                  buildSimState [| attacker |] [| (80.0, 34.0) |] awayPlayers awayPositions 75.0 34.0 HomeClub

              Expect.isTrue (isOffside attacker 80.0 ctx state LeftToRight) ""
          }

          test "isOffside player behind second-last defender is not offside" {
              let attacker = makePlayer 1 ST 10

              let awayPlayers =
                  [| makeGkPlayer 10 10 defaultGk; makePlayer 2 DC 10; makePlayer 3 DC 10 |]

              let awayPositions = [| (5.0, 34.0); (75.0, 30.0); (80.0, 38.0) |]

              let ctx, state =
                  buildSimState [| attacker |] [| (70.0, 34.0) |] awayPlayers awayPositions 65.0 34.0 HomeClub

              Expect.isFalse (isOffside attacker 70.0 ctx state LeftToRight) ""
          } ]

let matchPlayerActionTests =
    testList
        "MatchPlayerAction Resolution Tests"
        [ testCase "ExecuteShot with weak GK eventually scores"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 ST 20) with
                      Technical = { defaultTechnical with Finishing = 20 }
                      Mental = { defaultMental with Composure = 20 } }

              let gk = makeGkPlayer 10 1 weakGk

              let ctx, state =
                  buildSimState [| attacker |] [| (95.0, 34.0) |] [| gk |] [| (99.0, 34.0) |] 85.0 34.0 HomeClub

              let scored =
                  Array.init 50 (fun _ -> resolveAction Shoot (ctx, state) |> hasEvent Goal)
                  |> Array.exists id

              Expect.isTrue scored "strong attacker vs weak GK should score"

          testCase "ExecuteShot produces shot-related event"
          <| fun () ->
              let attacker = makePlayer 1 ST 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState [| attacker |] [| (90.0, 34.0) |] [| gk |] [| (99.0, 34.0) |] 80.0 34.0 HomeClub

              let events = resolveAction Shoot (ctx, state)

              let hasShotEvent =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | Goal
                      | ShotOffTarget
                      | ShotBlocked
                      | Save -> true
                      | _ -> false)

              Expect.isTrue hasShotEvent ""

          testCase "ExecuteShot with strong GK mostly saves"
          <| fun () ->
              let attacker = makePlayer 1 ST 10
              let gk = makeGkPlayer 10 20 strongGk

              let ctx, state =
                  buildSimState [| attacker |] [| (90.0, 34.0) |] [| gk |] [| (99.0, 34.0) |] 80.0 34.0 HomeClub

              let goals =
                  Array.init 30 (fun _ -> resolveAction Shoot (ctx, state) |> hasEvent Goal)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (goals < 20) $"strong GK stopped most, scored {goals}/30"

          testCase "ExecutePass success emits PassCompleted event"
          <| fun () ->
              let attacker = makePlayer 1 MC 10
              let teammate = makePlayer 2 ST 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState
                      [| attacker; teammate |]
                      [| (52.5, 34.0); (70.0, 34.0) |]
                      [| gk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let successes =
                  Array.init 30 (fun _ ->
                      resolveAction (Pass attacker) (ctx, state)
                      |> hasEventMatching (fun e ->
                          match e.Type with
                          | PassCompleted _ -> true
                          | _ -> false))
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (successes > 0) ""

          testCase "ExecutePass failure flips AttackingClub"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 MC 1) with
                      Technical = { defaultTechnical with Passing = 1 }
                      Mental = { defaultMental with Vision = 1 } }

              let teammate = makePlayer 2 ST 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState
                      [| attacker; teammate |]
                      [| (52.5, 34.0); (70.0, 34.0) |]
                      [| gk |]
                      [| (99.0, 34.0) |]
                      52.5
                      34.0
                      HomeClub

              let flipped =
                  Array.init 30 (fun _ ->
                      resolveAction (Pass attacker) (ctx, state) |> ignore
                      state.AttackingClub = AwayClub)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (flipped > 0) ""

          testCase "ExecuteDribble success emits DribbleSuccess"
          <| fun () ->
              let attacker =
                  { (makePlayer 1 ST 20) with
                      Technical = { defaultTechnical with Dribbling = 20 }
                      Physical =
                          { defaultPhysical with
                              Agility = 20
                              Balance = 20 } }

              let defender =
                  { (makePlayer 10 DC 1) with
                      Technical = { defaultTechnical with Tackling = 1 }
                      Physical = { defaultPhysical with Strength = 1 } }

              let ctx, state =
                  buildSimState [| attacker |] [| (70.0, 34.0) |] [| defender |] [| (75.0, 34.0) |] 70.0 34.0 HomeClub

              let successes =
                  Array.init 30 (fun _ -> resolveAction Dribble (ctx, state) |> hasEvent DribbleSuccess)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (successes > 0) ""

          testCase "ExecuteTackle foul emits FoulCommitted"
          <| fun () ->
              let defender = makeHighAggressionPlayer 10 DC
              let attacker = makePlayer 1 ST 10

              let ctx, state =
                  buildSimState [| attacker |] [| (70.0, 34.0) |] [| defender |] [| (71.0, 34.0) |] 70.0 34.0 AwayClub

              let fouls =
                  Array.init 50 (fun _ -> resolve awayClub.Id 1 (Tackle defender) ctx state |> hasEvent FoulCommitted)
                  |> Array.filter id
                  |> Array.length

              Expect.isTrue (fouls > 0) ""

          testCase "ExecuteFreeKick produces FreeKick event"
          <| fun () ->
              let kicker =
                  { (makePlayer 1 MC 16) with
                      Technical =
                          { defaultTechnical with
                              Finishing = 18
                              LongShots = 18 }
                      Mental = { defaultMental with Composure = 18 } }

              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState [| kicker |] [| (65.0, 34.0) |] [| gk |] [| (99.0, 34.0) |] 65.0 34.0 HomeClub

              let events = resolveAction PlayerAction.FreeKick (ctx, state)

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | FreeKick _ -> true
                      | _ -> false)
                  ""

          testCase "ExecuteCorner emits Corner event"
          <| fun () ->
              let attacker = makePlayer 1 ST 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState [| attacker |] [| (95.0, 34.0) |] [| gk |] [| (99.0, 34.0) |] 104.0 10.0 HomeClub

              let events = resolveAction PlayerAction.Corner (ctx, state)
              Expect.exists events (fun e -> e.Type = Corner) ""

          testCase "ExecuteThrowIn emits PassCompleted event"
          <| fun () ->
              let thrower = makePlayer 1 DL 10
              let teammate = makePlayer 2 MC 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState
                      [| thrower; teammate |]
                      [| (5.0, 20.0); (10.0, 34.0) |]
                      [| gk |]
                      [| (99.0, 34.0) |]
                      5.0
                      5.0
                      HomeClub

              let events = resolveAction (ThrowIn HomeClub) (ctx, state)

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | PassCompleted _ -> true
                      | _ -> false)
                  ""

          testCase "ExecutePenalty produces PenaltyAwarded event"
          <| fun () ->
              let kicker = makePlayer 1 ST 16
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState [| kicker |] [| (52.5, 34.0) |] [| gk |] [| (99.0, 34.0) |] 52.5 34.0 HomeClub

              let events = resolveAction (Penalty(kicker, HomeClub, 1)) (ctx, state)

              Expect.exists
                  events
                  (fun e ->
                      match e.Type with
                      | PenaltyAwarded _ -> true
                      | _ -> false)
                  "" ]

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
              Expect.equal hGoals hScore "home goal event count mismatch"
              Expect.equal aGoals aScore "away goal event count mismatch"
          }
          test "all event SubTicks in [0, FullTimeSubTick]" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (events
                   |> List.forall (fun e -> e.SubTick >= 0 && e.SubTick <= PhysicsContract.FullTimeSubTick))
                  "event with SubTick outside valid range"
          }
          test "all event ClubIds are either home or away" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = trySimulateMatch home away players staff |> getOk
              Expect.isTrue (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id)) ""
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
                  ""
          }
          test "events are ordered chronologically" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (events |> List.pairwise |> List.forall (fun (a, b) -> b.SubTick >= a.SubTick))
                  "events not sorted"
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

              Expect.isTrue (reds |> List.forall (fun (_, n) -> n = 1)) ""
          }
          test "no player receives more than 2 yellow cards" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = trySimulateMatch clubs[0] clubs[1] players staff |> getOk

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              Expect.isTrue (yellows |> List.forall (fun (_, n) -> n <= 2)) ""
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

              Expect.isTrue (Set.isSubset twoYellows redPlayers) ""
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
                  ""
          }
          test "all conditions stay in [0, 100]" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s ->
                       s.HomeConditions |> Array.forall (fun c -> c >= 0 && c <= 100)
                       && s.AwayConditions |> Array.forall (fun c -> c >= 0 && c <= 100)))
                  ""
          }
          test "all player positions stay in metric bounds [0..105] x [0..68]" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s ->
                       s.HomePositions
                       |> Array.forall (fun sp ->
                           sp.X >= 0.0
                           && sp.X <= PhysicsContract.PitchLength
                           && sp.Y >= 0.0
                           && sp.Y <= PhysicsContract.PitchWidth)
                       && s.AwayPositions
                          |> Array.forall (fun sp ->
                              sp.X >= 0.0
                              && sp.X <= PhysicsContract.PitchLength
                              && sp.Y >= 0.0
                              && sp.Y <= PhysicsContract.PitchWidth)))
                  "player position out of metric bounds"
          } ]

let biomechanicsTests =
    testList
        "Biomechanics & Physics Fidelity"
        [ test "no player exceeds 10.5 m/s" {
              let clubs, players, staff = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players staff |> getOk

              for snap in replay.Snapshots do
                  for i in 0 .. snap.HomePositions.Length - 1 do
                      let pos = snap.HomePositions[i]
                      let speed = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy)

                      if speed > 10.5 then
                          Expect.isTrue false $"Player at index {i} exceeded 10.5 m/s: {speed:F2}"

                  for i in 0 .. snap.AwayPositions.Length - 1 do
                      let pos = snap.AwayPositions[i]
                      let speed = sqrt (pos.Vx * pos.Vx + pos.Vy * pos.Vy)

                      if speed > 10.5 then
                          Expect.isTrue false $"Player at index {i} exceeded 10.5 m/s: {speed:F2}"
          }
          test "Pace 20 is >= 30% faster than Pace 1" {
              let speedMax = PhysicsContract.playerMaxSpeed 20 100
              let speedMin = PhysicsContract.playerMaxSpeed 1 100
              let ratio = speedMax / speedMin
              Expect.isTrue (ratio >= 1.30) $"ratio={ratio:F2}, need >= 1.30"
          }
          test "ball at Z=1.5m is above contact threshold" {
              let attacker = makePlayer 1 ST 10
              let gk = makeGkPlayer 10 10 defaultGk

              let ctx, state =
                  buildSimState [| attacker |] [| (52.5, 34.0) |] [| gk |] [| (99.0, 34.0) |] 52.5 34.0 HomeClub

              state.Ball <-
                  { state.Ball with
                      Position = { state.Ball.Position with Z = 1.5 } }

              Expect.isTrue (state.Ball.Position.Z > 0.8) "ball should be above 0.8m"
          }
          test "ball speed after physics step stays within realistic bounds" {
              let fastBall =
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

              let stepped = BallPhysics.update fastBall

              let speed =
                  sqrt (
                      stepped.Position.Vx ** 2.0
                      + stepped.Position.Vy ** 2.0
                      + stepped.Position.Vz ** 2.0
                  )

              let origSpeed =
                  sqrt (
                      fastBall.Position.Vx ** 2.0
                      + fastBall.Position.Vy ** 2.0
                      + fastBall.Position.Vz ** 2.0
                  )

              Expect.isTrue (speed <= origSpeed * 1.01) $"ball accelerated from {origSpeed:F1} to {speed:F1}"
              Expect.isTrue (speed >= 0.0) $"ball has negative velocity: {speed}"
          } ]

let statisticalTests =
    let iterations = 100

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

    let isPass (e: MatchEvent) =
        match e.Type with
        | PassCompleted _ -> true
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
                      | Error e -> Some $"{e}"
                      | Ok _ -> None)

              Expect.isEmpty failures $"{failures.Length} fixture(s) failed"

          testCase "avg goals per match in [1.5, 4.0]"
          <| fun () ->
              let avg = scoresOnly |> Array.averageBy (fun (h, a) -> float (h + a))
              Expect.isTrue (avg >= 1.5 && avg <= 4.0) $"avg goals = {avg:F2}"

          testCase "no match has outlier score (either side > 10)"
          <| fun () ->
              let outliers = scoresOnly |> Array.filter (fun (h, a) -> h > 10 || a > 10)
              Expect.isEmpty outliers $"{outliers.Length} outlier scores"

          testCase "avg shots per match in [10, 40]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isShot |> List.length))

              Expect.isTrue (avg >= 10.0 && avg <= 40.0) $"avg shots = {avg:F2}"

          testCase "avg dribbles per match in [15, 100]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isDribble |> List.length))

              Expect.isTrue (avg >= 15.0 && avg <= 100.0) $"avg dribbles = {avg:F2}"

          testCase "avg corners per match in [3, 20]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (countType Corner ev))

              Expect.isTrue (avg >= 3.0 && avg <= 20.0) $"avg corners = {avg:F2}"

          testCase "avg fouls per match in [10, 50]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (countType FoulCommitted ev))

              Expect.isTrue (avg >= 10.0 && avg <= 50.0) $"avg fouls = {avg:F2}"

          testCase "avg passes per match in [50, 800]"
          <| fun () ->
              let avg =
                  successOutcomes
                  |> Array.averageBy (fun (_, _, ev) -> float (ev |> List.filter isPass |> List.length))

              Expect.isTrue (avg >= 50.0 && avg <= 800.0) $"avg passes = {avg:F2}"

          testCase "shot conversion rate in [3%, 25%]"
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

              Expect.isTrue (rate >= 3.0 && rate <= 25.0) $"conversion = {rate:F1}%% ({totalGoals}/{totalShots})"

          testCase "both teams produce match events"
          <| fun () -> Expect.isTrue (successOutcomes |> Array.forall (fun (_, _, ev) -> ev.Length > 0)) ""

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
                  $"draws ({drawPct:F1}%%) dominate — home={homeWinPct:F1}%% away={awayWinPct:F1}%%"

          testCase "speed under 50 ms/match"
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
              Expect.isTrue (msPerGame < 50.0) $"{msPerGame:F4} ms/match (limit: 50 ms)" ]

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
                      [| trySimulateMatch a b players staff; trySimulateMatch b a players staff |])
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let homeGoals = results |> Array.sumBy fst
              let awayGoals = results |> Array.sumBy snd
              let totalMatches = results.Length
              let homeWins = results |> Array.filter (fun (h, a) -> h > a) |> Array.length
              let awayWins = results |> Array.filter (fun (h, a) -> a > h) |> Array.length
              let draws = results |> Array.filter (fun (h, a) -> h = a) |> Array.length
              printfn "=== HOME ADVANTAGE ==="
              printfn $"Pairs: {pairs.Length} | Matches: {totalMatches}"
              printfn $"HomeWins: {homeWins} | Draws: {draws} | AwayWins: {awayWins}"

              printfn
                  $"Avg goals — Home: {float homeGoals / float totalMatches:F2} | Away: {float awayGoals / float totalMatches:F2}"

              Expect.isTrue (homeGoals > awayGoals) $"home ({homeGoals}) <= away ({awayGoals})"

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

              printfn $"=== FOULS === Home: {homeFouls} | Away: {awayFouls}"

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

              printfn "=== WIN RATES === HomeWin: %f%% | AwayWin: %f%%" homeWinPct awayWinPct

              Expect.isTrue
                  (homeWinPct > awayWinPct)
                  $"home win rate ({homeWinPct:F1}%%) <= away win rate ({awayWinPct:F1}%%)" ]
