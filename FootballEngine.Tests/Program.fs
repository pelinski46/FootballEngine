module FootballEngine.Tests

open System
open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Engine
open FootballEngine.Lineup
open FootballEngine.MatchSimulator
open FootballEngine.MatchState

let private loadGame () =
    match Db.loadGame().GetAwaiter().GetResult() with
    | None -> failtest "No saved game — run generateNewGame first."
    | Some game -> game

let private makeReadyClub (c: Club) =
    autoLineup { c with CurrentLineup = None } (bestFormation c)

let private loadClubs () =
    let game = loadGame ()
    let clubs = game.Clubs |> Map.toArray |> Array.map (snd >> makeReadyClub)

    if clubs.Length < 2 then
        failtest "Need at least 2 clubs."

    clubs

let private inBounds (x: float, y: float) =
    x >= 0.0 && x <= 100.0 && y >= 0.0 && y <= 100.0

let private allPositionsInBounds (positions: Map<PlayerId, float * float>) =
    positions |> Map.forall (fun _ pos -> inBounds pos)

let private isSeasonOver (state: GameState) =
    state.Competitions
    |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.Played))

let private getOk result =
    match result with
    | Ok v -> v
    | Error e -> failtestf $"Expected Ok but got Error: %A{e}"

let private emptyStanding clubId =
    { ClubId = clubId
      Played = 0
      Won = 0
      Drawn = 0
      Lost = 0
      GoalsFor = 0
      GoalsAgainst = 0
      Points = 0 }

let singleMatchTests =
    testList
        "Single Match Invariants"
        [ test "simulateMatch returns Ok" {
              let clubs = loadClubs ()
              Expect.isOk (trySimulateMatch clubs[0] clubs[1]) "trySimulateMatch returned Error — check lineup/GK"
          }
          test "scores are non-negative" {
              let clubs = loadClubs ()
              let h, a, _ = trySimulateMatch clubs[0] clubs[1] |> getOk
              Expect.isTrue (h >= 0 && a >= 0) $"negative score: {h}-{a}"
          }
          test "scores are plausible (each <= 15)" {
              let clubs = loadClubs ()
              let h, a, _ = trySimulateMatch clubs[0] clubs[1] |> getOk
              Expect.isTrue (h <= 15 && a <= 15) $"implausible score: {h}-{a}"
          }
          test "goal events match reported score" {
              let clubs = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let hScore, aScore, events = trySimulateMatch home away |> getOk
              let goals = events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length

              Expect.isTrue
                  (hGoals = hScore && aGoals = aScore)
                  $"goal event count mismatch for score {hScore}-{aScore}"
          }
          test "all event seconds in [0, 5700]" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.Second >= 0 && e.Second <= 95 * 60))
                  "event with second outside valid range"
          }
          test "all event playerIds belong to one of the two clubs" {
              let clubs = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away |> getOk
              let homeIds = home.Players |> List.map _.Id |> Set.ofList
              let awayIds = away.Players |> List.map _.Id |> Set.ofList

              Expect.isTrue
                  (events
                   |> List.forall (fun e -> Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))
                  "event references unknown player"
          }
          test "events are ordered chronologically" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk
              let ordered = List.rev events

              Expect.isTrue
                  (ordered |> List.pairwise |> List.forall (fun (a, b) -> b.Second >= a.Second))
                  "events not sorted by second"
          }
          test "no duplicate goal events for same player at same second" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk

              let goalKeys =
                  events
                  |> List.filter (fun e -> e.Type = Goal)
                  |> List.map (fun e -> e.PlayerId, e.Second)

              Expect.isTrue (goalKeys.Length = (goalKeys |> List.distinct).Length) "duplicate goal event detected"
          }
          test "all event ClubIds are either home or away" {
              let clubs = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id))
                  "event has unknown ClubId"
          }
          test "SubstitutionIn and SubstitutionOut are balanced" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk
              let ins = events |> List.filter (fun e -> e.Type = SubstitutionIn) |> List.length
              let outs = events |> List.filter (fun e -> e.Type = SubstitutionOut) |> List.length
              Expect.equal ins outs "substitution in/out count mismatch"
          }
          test "at most 3 substitutions per team" {
              let clubs = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away |> getOk

              let homeSubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = home.Id)
                  |> List.length

              let awaySubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = away.Id)
                  |> List.length

              Expect.isTrue (homeSubs <= 3 && awaySubs <= 3) "more than 3 substitutions for a team"
          }
          test "no player receives more than 1 red card" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk

              let reds =
                  events |> List.filter (fun e -> e.Type = RedCard) |> List.countBy _.PlayerId

              Expect.isTrue (reds |> List.forall (fun (_, count) -> count = 1)) "player has multiple red cards"
          }
          test "no player receives more than 2 yellow cards" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              Expect.isTrue
                  (yellows |> List.forall (fun (_, count) -> count <= 2))
                  "player has more than 2 yellow cards"
          }
          test "no goals scored at second 0" {
              let clubs = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (events
                   |> List.filter (fun e -> e.Type = Goal)
                   |> List.forall (fun e -> e.Second > 0))
                  "goal scored at second 0"
          } ]

type private FixtureOutcome =
    | Success of homeGoals: int * awayGoals: int
    | KnownError of home: string * away: string * error: SimulationError
    | UnhandledException of home: string * away: string * exn: exn

let statisticalTests =
    let iterations = 1000

    let runOutcomes () =
        let clubs = loadClubs ()

        Array.Parallel.init iterations (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length

            try
                match trySimulateMatch clubs[hi] clubs[ai] with
                | Ok(h, a, _) -> Success(h, a)
                | Error e -> KnownError(clubs[hi].Name, clubs[ai].Name, e)
            with ex ->
                UnhandledException(clubs[hi].Name, clubs[ai].Name, ex))

    testList
        "Statistical Contracts"
        [ testCase "all fixtures simulated successfully"
          <| fun () ->
              let failures =
                  runOutcomes ()
                  |> Array.choose (function
                      | KnownError(h, a, e) -> Some $"{h} vs {a} — SimulationError: %A{e}"
                      | UnhandledException(h, a, ex) -> Some $"{h} vs {a} — UNHANDLED {ex.GetType().Name}: {ex.Message}"
                      | Success _ -> None)

              Expect.isEmpty failures $"%d{failures.Length} fixture(s) failed"

          testCase "avg goals in range [1.5, 5.0]"
          <| fun () ->
              let totals =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h + a)
                      | _ -> None)

              let avg = float (Array.sum totals) / float totals.Length
              Expect.isTrue (avg >= 1.5 && avg <= 5.0) $"avg goals = %.2f{avg} (expected [1.5, 5.0])"

          testCase "home wins more often than away"
          <| fun () ->
              let results =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h, a)
                      | _ -> None)

              let n = float results.Length

              let homePct =
                  results
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              let awayPct =
                  results
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              Expect.isTrue (homePct > awayPct) (sprintf "home %.1f%% <= away %.1f%%" homePct awayPct)

          testCase "draws are not the most common outcome"
          <| fun () ->
              let results =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h, a)
                      | _ -> None)

              let n = float results.Length

              let homePct =
                  results
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              let awayPct =
                  results
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              let drawPct =
                  results
                  |> Array.filter (fun (h, a) -> h = a)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              Expect.isTrue (drawPct < homePct && drawPct < awayPct) (sprintf "draws %.1f%% dominate" drawPct)

          testCase "outcome percentages sum to 100"
          <| fun () ->
              let results =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h, a)
                      | _ -> None)

              let n = float results.Length

              let homePct =
                  results
                  |> Array.filter (fun (h, a) -> h > a)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              let awayPct =
                  results
                  |> Array.filter (fun (h, a) -> a > h)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              let drawPct =
                  results
                  |> Array.filter (fun (h, a) -> h = a)
                  |> Array.length
                  |> float
                  |> fun x -> x / n * 100.0

              Expect.isTrue
                  (abs (homePct + awayPct + drawPct - 100.0) < 0.01)
                  $"sum = %.4f{homePct + awayPct + drawPct}"

          testCase "speed under 5 ms/match"
          <| fun () ->
              let clubs = loadClubs ()
              let sw = Diagnostics.Stopwatch.StartNew()

              Array.Parallel.init iterations (fun i ->
                  let hi = i % clubs.Length
                  let ai = (hi + 1) % clubs.Length
                  trySimulateMatch clubs[hi] clubs[ai])
              |> ignore

              sw.Stop()
              let msPerGame = sw.Elapsed.TotalMilliseconds / float iterations
              Expect.isTrue (msPerGame < 5.0) $"%.4f{msPerGame} ms/match (limit: 5 ms)" ]

let errorHandlingTests =
    testList
        "Error Handling Contracts"
        [ test "no lineup → MissingLineup" {
              let clubs = loadClubs ()
              let noLineup = { clubs[0] with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch noLineup clubs[1] with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup"
          }
          test "empty lineup slots → IncompleteLineup" {
              let clubs = loadClubs ()
              let home = clubs[0]

              let emptyLineup =
                  match home.CurrentLineup with
                  | None -> home
                  | Some lu ->
                      { home with
                          CurrentLineup =
                              Some
                                  { lu with
                                      Slots = lu.Slots |> List.map (fun s -> { s with PlayerId = None }) } }

              Expect.isTrue
                  (match trySimulateMatch emptyLineup clubs[1] with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup"
          }
          test "partial lineup (5 players) → IncompleteLineup" {
              let clubs = loadClubs ()
              let home = clubs[0]

              let partialLineup =
                  match home.CurrentLineup with
                  | None -> home
                  | Some lu ->
                      { home with
                          CurrentLineup =
                              Some
                                  { lu with
                                      Slots =
                                          lu.Slots
                                          |> List.mapi (fun i s -> if i < 5 then s else { s with PlayerId = None }) } }

              Expect.isTrue
                  (match trySimulateMatch partialLineup clubs[1] with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup for partial lineup"
          }
          test "MissingLineup error carries club name" {
              let clubs = loadClubs ()
              let home = clubs[0]
              let noLineup = { home with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch noLineup clubs[1] with
                   | Error(MissingLineup name) -> name = home.Name
                   | _ -> false)
                  "MissingLineup did not carry correct club name"
          }
          test "swapped home/away still simulates" {
              let clubs = loadClubs ()
              Expect.isOk (trySimulateMatch clubs[1] clubs[0]) "swap broke the simulation"
          }
          test "same club vs itself returns Ok or a defined error" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[0] with
                   | Ok _
                   | Error(MissingLineup _)
                   | Error(IncompleteLineup _) -> true)
                  "unexpected exception simulating club vs itself"
          }
          test "away has no lineup → MissingLineup" {
              let clubs = loadClubs ()
              let noLineup = { clubs[1] with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch clubs[0] noLineup with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup for away club"
          }
          test "no GK in lineup never throws" {
              let clubs = loadClubs ()
              let home = clubs[0]

              let noGk =
                  match home.CurrentLineup with
                  | None -> home
                  | Some lu ->
                      let withoutGk =
                          lu.Slots
                          |> List.map (fun s ->
                              match s.PlayerId with
                              | Some pid when home.Players |> List.exists (fun p -> p.Id = pid && p.Position = GK) ->
                                  { s with PlayerId = None }
                              | _ -> s)

                      { home with
                          CurrentLineup = Some { lu with Slots = withoutGk } }

              Expect.isTrue
                  (match trySimulateMatch noGk clubs[1] with
                   | Ok _
                   | Error(IncompleteLineup _)
                   | Error(MissingLineup _) -> true)
                  "unexpected exception when GK missing"
          } ]

let private snapshotChecks (label: string) (s: MatchState) =
    let homeActive =
        s.HomePlayers
        |> Array.filter (fun p -> not (Map.containsKey p.Id s.HomeSidelined))
        |> Array.length

    let awayActive =
        s.AwayPlayers
        |> Array.filter (fun p -> not (Map.containsKey p.Id s.AwaySidelined))
        |> Array.length

    testList
        label
        [ test "ball in bounds" { Expect.isTrue (inBounds s.BallPosition) $"ball at {s.BallPosition}" }
          test "home positions in bounds" {
              Expect.isTrue (allPositionsInBounds s.HomePositions) "home player outside 0-100"
          }
          test "away positions in bounds" {
              Expect.isTrue (allPositionsInBounds s.AwayPositions) "away player outside 0-100"
          }
          test "scores non-negative" {
              Expect.isTrue (s.HomeScore >= 0 && s.AwayScore >= 0) $"score {s.HomeScore}-{s.AwayScore}"
          }
          test "second in [0, 5700]" { Expect.isTrue (s.Second >= 0 && s.Second <= 95 * 60) $"second = {s.Second}" }
          test "momentum in [-10, 10]" {
              Expect.isTrue (s.Momentum >= -10.0 && s.Momentum <= 10.0) $"momentum = {s.Momentum}"
          }
          test "home has at least 1 active player" {
              Expect.isTrue (homeActive >= 1) $"active home players = {homeActive}"
          }
          test "away has at least 1 active player" {
              Expect.isTrue (awayActive >= 1) $"active away players = {awayActive}"
          }
          test "home conditions length matches players" {
              Expect.equal s.HomeConditions.Length s.HomePlayers.Length "home conditions/players mismatch"
          }
          test "away conditions length matches players" {
              Expect.equal s.AwayConditions.Length s.AwayPlayers.Length "away conditions/players mismatch"
          }
          test "all home conditions in [0, 100]" {
              Expect.isTrue
                  (s.HomeConditions |> Array.forall (fun c -> c >= 0 && c <= 100))
                  "home condition out of range"
          }
          test "all away conditions in [0, 100]" {
              Expect.isTrue
                  (s.AwayConditions |> Array.forall (fun c -> c >= 0 && c <= 100))
                  "away condition out of range"
          }
          test "each home player has a position entry" {
              Expect.isTrue
                  (s.HomePlayers |> Array.forall (fun p -> Map.containsKey p.Id s.HomePositions))
                  "home player missing position"
          }
          test "each away player has a position entry" {
              Expect.isTrue
                  (s.AwayPlayers |> Array.forall (fun p -> Map.containsKey p.Id s.AwayPositions))
                  "away player missing position"
          }
          test "no player on both teams" {
              let homeIds = s.HomePlayers |> Array.map _.Id |> Set.ofArray
              let awayIds = s.AwayPlayers |> Array.map _.Id |> Set.ofArray
              Expect.isEmpty (Set.intersect homeIds awayIds) "player appears in both squads"
          }
          test "subs used in [0, 3]" {
              Expect.isTrue
                  (s.HomeSubsUsed >= 0
                   && s.HomeSubsUsed <= 3
                   && s.AwaySubsUsed >= 0
                   && s.AwaySubsUsed <= 3)
                  $"subs home={s.HomeSubsUsed} away={s.AwaySubsUsed}"
          }
          test "home base positions count matches players" {
              Expect.equal s.HomeBasePositions.Count s.HomePlayers.Length "home base positions mismatch"
          }
          test "away base positions count matches players" {
              Expect.equal s.AwayBasePositions.Count s.AwayPlayers.Length "away base positions mismatch"
          } ]

let replayTests =
    testList
        "Replay UI Contracts"
        [ testCase "replay has snapshots"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              Expect.isTrue (replay.Snapshots.Length > 0) "no snapshots — viewer slider will be empty"
          testCase "snapshot seconds are non-decreasing"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.Second >= a.Second))
                  "time went backwards between snapshots"
          testCase "home score never decreases"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.HomeScore >= a.HomeScore))
                  "home score decreased between snapshots"
          testCase "away score never decreases"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.AwayScore >= a.AwayScore))
                  "away score decreased between snapshots"
          testCase "final score >= last snapshot score"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              let last = replay.Snapshots[replay.Snapshots.Length - 1]

              Expect.isTrue
                  (replay.Final.HomeScore >= last.HomeScore
                   && replay.Final.AwayScore >= last.AwayScore)
                  "final score lower than last snapshot"
          testCase "final second = 5700 (95 min)"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              Expect.equal replay.Final.Second (95 * 60) "final.Second != 5700"
          testCase "goal events match final score"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              let final = replay.Final
              let goals = final.EventsRev |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = final.Home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = final.Away.Id) |> List.length

              Expect.isTrue
                  (hGoals = final.HomeScore && aGoals = final.AwayScore)
                  $"goal events don't match {final.HomeScore}-{final.AwayScore}"
          testCase "all snapshots have [11, 14] home players"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.HomePlayers.Length >= 11 && s.HomePlayers.Length <= 14))
                  "snapshot with invalid home player count"
          testCase "all snapshots have [11, 14] away players"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.AwayPlayers.Length >= 11 && s.AwayPlayers.Length <= 14))
                  "snapshot with invalid away player count"
          testCase "first snapshot second > 0"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              Expect.isTrue (replay.Snapshots.Length = 0 || replay.Snapshots[0].Second > 0) "first snapshot at second 0"
          testCase "last snapshot second <= 5700"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk

              Expect.isTrue
                  (replay.Snapshots.Length = 0
                   || replay.Snapshots[replay.Snapshots.Length - 1].Second <= 95 * 60)
                  "last snapshot beyond 95 min"
          testCase "final snapshot invariants"
          <| fun () ->
              let clubs = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] |> getOk
              runTestsWithCLIArgs [] [||] (snapshotChecks "final" replay.Final) |> ignore ]

let batchTests =
    testList
        "Batch & Standings Contracts"
        [ testCase "batch simulates fixtures correctly"
          <| fun () ->
              let game = loadGame ()

              let fixtures =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 20

              if fixtures.IsEmpty then
                  failtest "No unplayed fixtures found — start a new game"

              let result = simulateFixtures game fixtures
              let simulated = fixtures.Length - result.Errors.Length
              Expect.isTrue (simulated > 0) "all fixtures failed to simulate"
              Expect.isEmpty result.Errors $"%d{result.Errors.Length} errors"
              Expect.equal result.Logs.Length simulated "log count doesn't match simulated count"
          testCase "standings are mathematically sane"
          <| fun () ->
              let game = loadGame ()

              let fixtures =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, c) -> c.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 20

              let result = simulateFixtures game fixtures

              let sane =
                  result.GameState.Competitions
                  |> Map.forall (fun _ comp ->
                      comp.Standings
                      |> Map.forall (fun _ s ->
                          s.Played >= 0
                          && s.Won >= 0
                          && s.Drawn >= 0
                          && s.Lost >= 0
                          && s.Won + s.Drawn + s.Lost = s.Played
                          && s.Points = s.Won * 3 + s.Drawn
                          && s.GoalsFor >= 0
                          && s.GoalsAgainst >= 0))

              Expect.isTrue sane "W+D+L ≠ Played or Points ≠ 3W+D or negative values"
          testCase "all played fixtures have scores"
          <| fun () ->
              let game = loadGame ()

              let fixtures =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, c) -> c.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 20

              let result = simulateFixtures game fixtures

              Expect.isTrue
                  (result.GameState.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Fixtures
                       |> Map.forall (fun _ f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome))))
                  "played fixture missing HomeScore or AwayScore"
          testCase "Played never exceeds 2*(clubs-1)"
          <| fun () ->
              let game = loadGame ()

              let fixtures =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, c) -> c.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 20

              let result = simulateFixtures game fixtures

              Expect.isTrue
                  (result.GameState.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Standings |> Map.forall (fun _ s -> s.Played <= comp.ClubIds.Length * 2)))
                  "a club has played more games than possible"
          testCase "GoalsFor = GoalsAgainst across all standings"
          <| fun () ->
              let game = loadGame ()

              let fixtures =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, c) -> c.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 20

              let result = simulateFixtures game fixtures

              let symmetric =
                  result.GameState.Competitions
                  |> Map.forall (fun _ comp ->
                      let totalFor = comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsFor)

                      let totalAgainst =
                          comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsAgainst)

                      totalFor = totalAgainst)

              Expect.isTrue symmetric "total goals for ≠ total goals against" ]

let doubleSimGuardTests =
    testList
        "Double-Simulation Guard"
        [ testCase "standings unchanged after re-simulating played fixtures"
          <| fun () ->
              let game = loadGame ()

              let gameWithLineups =
                  { game with
                      Clubs = game.Clubs |> Map.map (fun _ c -> makeReadyClub c) }

              let fixtures =
                  gameWithLineups.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 5

              if fixtures.IsEmpty then
                  failtest "No unplayed fixtures to test"

              let result1 = simulateFixtures gameWithLineups fixtures
              let result2 = simulateFixtures result1.GameState fixtures

              let collectStandings (gs: GameState) =
                  gs.Competitions
                  |> Map.toList
                  |> List.collect (fun (compId, c) ->
                      c.Standings |> Map.toList |> List.map (fun (clubId, s) -> (compId, clubId), s))

              let standingsAfter1 = collectStandings result1.GameState
              let standingsAfter2 = collectStandings result2.GameState

              let unchanged =
                  standingsAfter1
                  |> List.forall (fun (key, s1) ->
                      standingsAfter2
                      |> List.tryFind (fun (k, _) -> k = key)
                      |> Option.map (fun (_, s2) -> s1.Played = s2.Played && s1.Won = s2.Won && s1.Points = s2.Points)
                      |> Option.defaultValue true)

              let maxPts1 = standingsAfter1 |> List.map (snd >> _.Points) |> List.max
              let maxPts2 = standingsAfter2 |> List.map (snd >> _.Points) |> List.max
              Expect.isTrue unchanged "standings changed — fixture was processed twice"
              Expect.equal maxPts2 maxPts1 "points doubled — alreadyPlayed guard is not working"
              Expect.isEmpty result2.Logs "second batch added log entries for already-played fixtures"
              Expect.isEmpty result2.Errors "second batch produced unexpected errors" ]

let gameStateIntegrityTests =
    testList
        "Game State Integrity"
        [ test "UserClubId exists in Clubs" {
              let game = loadGame ()
              Expect.isTrue (game.Clubs |> Map.containsKey game.UserClubId) $"UserClubId {game.UserClubId} not found"
          }
          test "all club.Players reference valid PlayerIds" {
              let game = loadGame ()
              let allPlayerIds = game.Players |> Map.toList |> List.map fst |> Set.ofList

              Expect.isTrue
                  (game.Clubs
                   |> Map.forall (fun _ c -> c.Players |> List.forall (fun p -> Set.contains p.Id allPlayerIds)))
                  "club references unknown player"
          }
          test "all players reference valid ClubId" {
              let game = loadGame ()
              let allClubIds = game.Clubs |> Map.toList |> List.map fst |> Set.ofList

              Expect.isTrue
                  (game.Players |> Map.forall (fun _ p -> Set.contains p.ClubId allClubIds))
                  "player references unknown club"
          }
          test "no player appears in two clubs" {
              let game = loadGame ()

              let allRefs =
                  game.Clubs
                  |> Map.toList
                  |> List.collect (fun (_, c) -> c.Players |> List.map (fun p -> p.Id, c.Id))

              let grouped = allRefs |> List.groupBy fst
              Expect.isTrue (grouped |> List.forall (fun (_, refs) -> refs.Length = 1)) "player found in multiple clubs"
          }
          test "no fixture has HomeClubId = AwayClubId" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.HomeClubId <> f.AwayClubId)))
                  "fixture has same home and away club"
          }
          test "played fixtures have both scores" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Fixtures
                       |> Map.forall (fun _ f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome))))
                  "played fixture missing score"
          }
          test "unplayed fixtures have no scores" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Fixtures
                       |> Map.forall (fun _ f -> f.Played || (f.HomeScore.IsNone && f.AwayScore.IsNone))))
                  "unplayed fixture has a score"
          }
          test "all fixture IDs are unique across competitions" {
              let game = loadGame ()

              let allIds =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList |> List.map fst)

              Expect.equal allIds.Length (allIds |> List.distinct).Length "duplicate fixture ID detected"
          }
          test "all fixture.CompetitionId matches parent competition" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun compId comp -> comp.Fixtures |> Map.forall (fun _ f -> f.CompetitionId = compId)))
                  "fixture has wrong CompetitionId"
          }
          test "all competition seasons match game season" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions |> Map.forall (fun _ comp -> comp.Season = game.Season))
                  "competition season mismatch"
          }
          test "all competition.ClubIds reference valid clubs" {
              let game = loadGame ()
              let allClubIds = game.Clubs |> Map.toList |> List.map fst |> Set.ofList

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp -> comp.ClubIds |> List.forall (fun cid -> Set.contains cid allClubIds)))
                  "competition references unknown club"
          }
          test "all knockout tie club refs are valid" {
              let game = loadGame ()
              let allClubIds = game.Clubs |> Map.toList |> List.map fst |> Set.ofList

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.KnockoutTies
                       |> Map.forall (fun _ tie ->
                           Set.contains tie.HomeClubId allClubIds && Set.contains tie.AwayClubId allClubIds)))
                  "knockout tie references unknown club"
          } ]

let playerDataTests =
    testList
        "Player & Club Data Contracts"
        [ test "all CurrentSkill in [1,200] and PA >= CA" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       p.CurrentSkill >= 1
                       && p.CurrentSkill <= 200
                       && p.PotentialSkill >= p.CurrentSkill
                       && p.PotentialSkill <= 200))
                  "player CA/PA out of range"
          }
          test "all player ages in [15, 45]" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       let age = (game.CurrentDate - p.Birthday).TotalDays / 365.25 in age >= 15.0 && age <= 45.0))
                  "player age unreasonable"
          }
          test "all MatchFitness in [0, 100]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players
                   |> Map.forall (fun _ p -> p.MatchFitness >= 0 && p.MatchFitness <= 100))
                  "player fitness out of range"
          }
          test "all Condition in [0, 100]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players |> Map.forall (fun _ p -> p.Condition >= 0 && p.Condition <= 100))
                  "player condition out of range"
          }
          test "all Morale in [0, 100]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players |> Map.forall (fun _ p -> p.Morale >= 0 && p.Morale <= 100))
                  "player morale out of range"
          }
          test "all Value >= 0" {
              let game = loadGame ()
              Expect.isTrue (game.Players |> Map.forall (fun _ p -> p.Value >= 0m)) "player has negative value"
          }
          test "all Salary >= 0" {
              let game = loadGame ()
              Expect.isTrue (game.Players |> Map.forall (fun _ p -> p.Salary >= 0m)) "player has negative salary"
          }
          test "all technical stats in [1, 20]" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       let t = p.Technical

                       [ t.Finishing
                         t.Dribbling
                         t.Passing
                         t.BallControl
                         t.Tackling
                         t.Marking
                         t.LongShots
                         t.Crossing
                         t.Heading
                         t.FreeKick
                         t.Penalty ]
                       |> List.forall (fun v -> v >= 1 && v <= 20)))
                  "technical stat out of range"
          }
          test "all physical stats in [1, 20]" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       let ph = p.Physical

                       [ ph.Pace
                         ph.Stamina
                         ph.Strength
                         ph.Agility
                         ph.Acceleration
                         ph.Balance
                         ph.JumpingReach ]
                       |> List.forall (fun v -> v >= 1 && v <= 20)))
                  "physical stat out of range"
          }
          test "all mental stats in [1, 20]" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       let m = p.Mental

                       [ m.Vision
                         m.Composure
                         m.Positioning
                         m.WorkRate
                         m.Aggression
                         m.Bravery
                         m.Concentration
                         m.Leadership ]
                       |> List.forall (fun v -> v >= 1 && v <= 20)))
                  "mental stat out of range"
          }
          test "all GK stats in [1, 20]" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       let g = p.Goalkeeping

                       [ g.Reflexes; g.Handling; g.Kicking; g.OneOnOne; g.AerialReach ]
                       |> List.forall (fun v -> v >= 1 && v <= 20)))
                  "GK stat out of range"
          }
          test "all player heights in [150, 220]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players |> Map.forall (fun _ p -> p.Height >= 150 && p.Height <= 220))
                  "player height unreasonable"
          }
          test "all player weights in [50, 120]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players |> Map.forall (fun _ p -> p.Weight >= 50 && p.Weight <= 120))
                  "player weight unreasonable"
          }
          test "all player reputations in [0, 1000]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Players
                   |> Map.forall (fun _ p -> p.Reputation >= 0 && p.Reputation <= 1000))
                  "player reputation out of range"
          }
          test "every club has at least 1 GK" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Clubs
                   |> Map.forall (fun _ c -> c.Players |> List.exists (fun p -> p.Position = GK)))
                  "club has no goalkeeper"
          }
          test "all club budgets >= 0" {
              let game = loadGame ()
              Expect.isTrue (game.Clubs |> Map.forall (fun _ c -> c.Budget >= 0m)) "club has negative budget"
          }
          test "all clubs have at least 1 player" {
              let game = loadGame ()
              Expect.isTrue (game.Clubs |> Map.forall (fun _ c -> c.Players.Length > 0)) "club has no players"
          }
          test "all club morale in [0, 100]" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Clubs |> Map.forall (fun _ c -> c.Morale >= 0 && c.Morale <= 100))
                  "club morale out of range"
          }
          test "all club reputations >= 0" {
              let game = loadGame ()
              Expect.isTrue (game.Clubs |> Map.forall (fun _ c -> c.Reputation >= 0)) "club has negative reputation"
          }
          test "all player names are non-empty" {
              let game = loadGame ()
              Expect.isTrue (game.Players |> Map.forall (fun _ p -> p.Name.Trim().Length > 0)) "player has empty name"
          }
          test "all club names are non-empty" {
              let game = loadGame ()
              Expect.isTrue (game.Clubs |> Map.forall (fun _ c -> c.Name.Trim().Length > 0)) "club has empty name"
          }
          test "all player IDs are unique" {
              let game = loadGame ()
              let ids = game.Players |> Map.toList |> List.map fst
              Expect.equal ids.Length (ids |> List.distinct).Length "duplicate player ID detected"
          }
          test "all player.ClubId back-references roster" {
              let game = loadGame ()
              let players = game.Players |> Map.toList |> List.map snd

              Expect.isTrue
                  (players
                   |> List.forall (fun p ->
                       game.Clubs
                       |> Map.tryFind p.ClubId
                       |> Option.map (fun c -> c.Players |> List.exists (fun cp -> cp.Id = p.Id))
                       |> Option.defaultValue false))
                  "player.ClubId does not match roster"
          } ]

let seasonProgressTests =
    testList
        "Season Progress Contracts"
        [ test "game has at least 1 fixture" {
              let game = loadGame ()

              let total =
                  game.Competitions |> Map.toList |> List.sumBy (fun (_, c) -> c.Fixtures.Count)

              Expect.isTrue (total > 0) "no fixtures found"
          }
          test "played <= total fixtures" {
              let game = loadGame ()

              let total =
                  game.Competitions |> Map.toList |> List.sumBy (fun (_, c) -> c.Fixtures.Count)

              let played =
                  game.Competitions
                  |> Map.toList
                  |> List.sumBy (fun (_, c) -> c.Fixtures |> Map.filter (fun _ f -> f.Played) |> Map.count)

              Expect.isTrue (played <= total) $"played {played} > total {total}"
          }
          test "total points <= 3 * played fixtures" {
              let game = loadGame ()

              let played =
                  game.Competitions
                  |> Map.toList
                  |> List.sumBy (fun (_, c) -> c.Fixtures |> Map.filter (fun _ f -> f.Played) |> Map.count)

              let totalPoints =
                  game.Competitions
                  |> Map.toList
                  |> List.sumBy (fun (_, c) -> c.Standings |> Map.toList |> List.sumBy (snd >> _.Points))

              Expect.isTrue (totalPoints <= played * 3) $"total points {totalPoints} > max possible {played * 3}"
          }
          test "no team has more points than 3 * played" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp -> comp.Standings |> Map.forall (fun _ s -> s.Points <= s.Played * 3)))
                  "team has impossible points total"
          }
          test "unplayed fixtures are scheduled in the future" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Fixtures
                       |> Map.forall (fun _ f -> f.Played || f.ScheduledDate >= game.CurrentDate.AddDays(-1.0))))
                  "unplayed fixture scheduled in the past"
          }
          test "standings club count matches competition ClubIds" {
              let game = loadGame ()

              Expect.isTrue
                  (game.Competitions
                   |> Map.forall (fun _ comp -> comp.Standings.Count = 0 || comp.Standings.Count = comp.ClubIds.Length))
                  "standings count mismatch"
          }
          test "isSeasonOver is consistent with fixture state" {
              let game = loadGame ()
              let seasonOver = isSeasonOver game

              let allPlayed =
                  game.Competitions
                  |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.Played))

              Expect.isTrue (not seasonOver || allPlayed) "isSeasonOver=true but unplayed fixtures remain"
          } ]

let matchStateOpsTests =
    let minimalState homeScore awayScore =
        { Home = Unchecked.defaultof<Club>
          Away = Unchecked.defaultof<Club>
          Second = 0
          HomeScore = homeScore
          AwayScore = awayScore
          BallPosition = 50.0, 50.0
          Possession = Home
          Momentum = 0.0
          HomePlayers = Array.empty
          AwayPlayers = Array.empty
          HomeConditions = Array.empty
          AwayConditions = Array.empty
          HomeSidelined = Map.empty
          AwaySidelined = Map.empty
          HomeYellows = Map.empty
          AwayYellows = Map.empty
          HomeSubsUsed = 0
          AwaySubsUsed = 0
          EventsRev = []
          HomePositions = Map.empty
          AwayPositions = Map.empty
          HomeBasePositions = Map.empty
          AwayBasePositions = Map.empty }

    testList
        "MatchStateOps Unit Contracts"
        [ test "flipPossession Home → Away" { Expect.equal (flipPossession Home) Away "expected Away" }
          test "flipPossession Away → Home" { Expect.equal (flipPossession Away) Home "expected Home" }
          test "flipPossession is involution" {
              Expect.equal (flipPossession (flipPossession Home)) Home "flip(flip(Home)) ≠ Home"
          }
          test "zone 15 → BuildUp" { Expect.equal (phaseFromBallZone 15.0) BuildUp $"got {phaseFromBallZone 15.0}" }
          test "zone 50 → Midfield" { Expect.equal (phaseFromBallZone 50.0) Midfield $"got {phaseFromBallZone 50.0}" }
          test "zone 45 → Midfield (in [40,60] band)" {
              Expect.equal (phaseFromBallZone 45.0) Midfield $"got {phaseFromBallZone 45.0}"
          }
          test "zone 85 → BuildUp (opponent half)" {
              Expect.equal (phaseFromBallZone 85.0) BuildUp $"got {phaseFromBallZone 85.0}"
          }
          test "pressureMultiplier losing > 1.0" {
              let pm = pressureMultiplier true (minimalState 0 3)
              Expect.isTrue (pm > 1.0) $"losing multiplier = {pm}"
          }
          test "pressureMultiplier winning < 1.0" {
              let pm = pressureMultiplier true (minimalState 3 0)
              Expect.isTrue (pm < 1.0) $"winning multiplier = {pm}"
          }
          test "losing pressure > winning pressure" {
              let winning = pressureMultiplier true (minimalState 3 0)
              let losing = pressureMultiplier true (minimalState 0 3)
              Expect.isTrue (losing > winning) $"losing={losing} winning={winning}"
          }
          test "pressure values in [0.1, 2.0]" {
              let winning = pressureMultiplier true (minimalState 3 0)
              let losing = pressureMultiplier true (minimalState 0 3)

              Expect.isTrue
                  (winning >= 0.1 && winning <= 2.0 && losing >= 0.1 && losing <= 2.0)
                  "pressure out of [0.1, 2.0]"
          } ]

let standingUpdateTests =
    testList
        "Standing Update Contracts"
        [ testCase "standing arithmetic is correct for one fixture"
          <| fun () ->
              let game = loadGame ()

              let unplayed =
                  game.Competitions
                  |> Map.toList
                  |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
                  |> List.filter (fun (_, f) -> not f.Played)
                  |> List.truncate 1

              if unplayed.IsEmpty then
                  failtest "No unplayed fixture to use"

              let fixtureId, fixture = unplayed[0]

              let comp =
                  game.Competitions
                  |> Map.toList
                  |> List.tryFind (fun (_, c) -> Map.containsKey fixtureId c.Fixtures)
                  |> Option.map snd

              match comp with
              | None -> failtest "Could not find competition for fixture"
              | Some comp ->
                  let result = simulateFixtures game unplayed

                  let homeAfter =
                      result.GameState.Competitions
                      |> Map.toList
                      |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.HomeClubId)

                  let awayAfter =
                      result.GameState.Competitions
                      |> Map.toList
                      |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.AwayClubId)

                  let homeBefore =
                      comp.Standings
                      |> Map.tryFind fixture.HomeClubId
                      |> Option.defaultValue (emptyStanding fixture.HomeClubId)

                  let awayBefore =
                      comp.Standings
                      |> Map.tryFind fixture.AwayClubId
                      |> Option.defaultValue (emptyStanding fixture.AwayClubId)

                  match homeAfter, awayAfter with
                  | Some h, Some a ->
                      Expect.isTrue
                          (h.Played = homeBefore.Played + 1 && a.Played = awayBefore.Played + 1)
                          "Played not incremented by 1"

                      let hPts = h.Points - homeBefore.Points
                      let aPts = a.Points - awayBefore.Points

                      Expect.isTrue
                          ((hPts = 3 && aPts = 0) || (hPts = 0 && aPts = 3) || (hPts = 1 && aPts = 1))
                          "points distribution formula wrong"

                      Expect.isTrue
                          (h.GoalsFor - homeBefore.GoalsFor >= 0 && a.GoalsFor - awayBefore.GoalsFor >= 0)
                          "GoalsFor not updated"

                      let hGoalsAdded = h.GoalsFor - homeBefore.GoalsFor
                      let aGoalsAdded = a.GoalsFor - awayBefore.GoalsFor
                      let hConceded = h.GoalsAgainst - homeBefore.GoalsAgainst
                      let aConceded = a.GoalsAgainst - awayBefore.GoalsAgainst

                      Expect.isTrue
                          (hGoalsAdded = aConceded && aGoalsAdded = hConceded)
                          "goals asymmetry: home.GoalsFor ≠ away.GoalsAgainst"
                  | _ -> failtest "standings not found after simulation" ]

let lineupTests =
    testList
        "Lineup & Formation Contracts"
        [ test "all formations produce 11-slot lineup" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       FormationLineUps.all
                       |> List.forall (fun f ->
                           match (autoLineup { c with CurrentLineup = None } f).CurrentLineup with
                           | None -> false
                           | Some lu -> lu.Slots.Length = 11)))
                  "formation produced <11 slots"
          }
          test "lineup has exactly 11 filled slots" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       match c.CurrentLineup with
                       | None -> false
                       | Some lu -> lu.Slots |> List.filter (fun s -> s.PlayerId.IsSome) |> List.length = 11))
                  "lineup does not have 11 filled players"
          }
          test "all lineup players belong to the club" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       match c.CurrentLineup with
                       | None -> true
                       | Some lu ->
                           let clubIds = c.Players |> List.map _.Id |> Set.ofList

                           lu.Slots
                           |> List.forall (fun s -> s.PlayerId |> Option.forall (fun pid -> Set.contains pid clubIds))))
                  "lineup references foreign player"
          }
          test "lineup always has a GK" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       match c.CurrentLineup with
                       | None -> false
                       | Some lu -> lu.Slots |> List.exists (fun s -> s.Role = GK && s.PlayerId.IsSome)))
                  "lineup has no goalkeeper"
          }
          test "all lineup slot positions in [0, 1]" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       match c.CurrentLineup with
                       | None -> true
                       | Some lu ->
                           lu.Slots
                           |> List.forall (fun s -> s.X >= 0.0 && s.X <= 1.0 && s.Y >= 0.0 && s.Y <= 1.0)))
                  "slot position out of [0,1]"
          }
          test "no player appears twice in a lineup" {
              let clubs = loadClubs ()

              Expect.isTrue
                  (clubs
                   |> Array.forall (fun c ->
                       match c.CurrentLineup with
                       | None -> true
                       | Some lu ->
                           let ids = lu.Slots |> List.choose _.PlayerId
                           ids.Length = (ids |> List.distinct).Length))
                  "player assigned to two slots"
          } ]

let isSeasonOverTests =
    testList
        "isSeasonOver Edge Cases"
        [ test "no fixtures → season is over" {
              let game = loadGame ()

              let noFixtures =
                  { game with
                      Competitions = game.Competitions |> Map.map (fun _ comp -> { comp with Fixtures = Map.empty }) }

              Expect.isTrue (isSeasonOver noFixtures) "expected true when no fixtures"
          }
          test "all played → season is over" {
              let game = loadGame ()

              let allPlayed =
                  { game with
                      Competitions =
                          game.Competitions
                          |> Map.map (fun _ comp ->
                              { comp with
                                  Fixtures =
                                      comp.Fixtures
                                      |> Map.map (fun _ f ->
                                          { f with
                                              Played = true
                                              HomeScore = Some 1
                                              AwayScore = Some 0 }) }) }

              Expect.isTrue (isSeasonOver allPlayed) "expected true when all played"
          }
          test "fresh game → season is NOT over" {
              let game = loadGame ()
              Expect.isFalse (isSeasonOver game) "expected false for fresh game with unplayed fixtures"
          } ]

let multiMatchTests =
    testList
        "Multi-Match Consistency"
        [ test "all repeated match scores non-negative" {
              let clubs = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1])
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h >= 0 && a >= 0)) "negative score detected"
          }
          test "matches are not deterministic" {
              let clubs = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1])
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue
                  (results.Length < 2 || (results |> Array.distinct).Length > 1)
                  "all 20 simulations produced identical results — RNG may be broken"
          }
          test "no match has outlier score (> 15 each)" {
              let clubs = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1])
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h <= 15 && a <= 15)) "outlier score detected"
          } ]

let fixtureIntegrityTests =
    testList
        "Fixture Integrity"
        [ testCase "each club plays exactly n-1*2 matches in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let n = comp.ClubIds.Length
                  let expected = (n - 1) * 2

                  let gamesPerClub =
                      comp.Fixtures
                      |> Map.toList
                      |> List.collect (fun (_, f) -> [ f.HomeClubId; f.AwayClubId ])
                      |> List.countBy id

                  gamesPerClub
                  |> List.iter (fun (clubId, count) ->
                      Expect.equal count expected $"{comp.Name}: club {clubId} played {count} (expected {expected})"))
          testCase "no duplicate fixtures in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let keys =
                      comp.Fixtures
                      |> Map.toList
                      |> List.map (fun (_, f) -> f.HomeClubId, f.AwayClubId)

                  Expect.equal keys.Length (keys |> List.distinct).Length $"{comp.Name}: duplicate fixtures detected")
          testCase "total fixtures = n*(n-1) in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let n = comp.ClubIds.Length

                  Expect.equal
                      comp.Fixtures.Count
                      (n * (n - 1))
                      $"{comp.Name}: expected {n * (n - 1)} fixtures, got {comp.Fixtures.Count}") ]

[<EntryPoint>]
let main argv =
    let all =
        testList
            "FootballEngine"
            [ singleMatchTests
              statisticalTests |> testSequenced
              errorHandlingTests
              replayTests
              batchTests
              doubleSimGuardTests
              gameStateIntegrityTests
              playerDataTests
              seasonProgressTests
              matchStateOpsTests
              standingUpdateTests
              lineupTests
              isSeasonOverTests
              multiMatchTests
              fixtureIntegrityTests ]

    runTestsWithCLIArgs [] argv all
