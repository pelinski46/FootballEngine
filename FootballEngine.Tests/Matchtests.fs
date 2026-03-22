module FootballEngine.Tests.MatchTests

open System
open Expecto
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open FootballEngine.MatchState
open FootballEngine.Tests.Helpers

let singleMatchTests =
    testList
        "Single Match Invariants"
        [ test "simulateMatch returns Ok" {
              let clubs, players = loadClubs ()

              Expect.isOk
                  (trySimulateMatch clubs[0] clubs[1] players)
                  "trySimulateMatch returned Error — check lineup/GK"
          }
          test "scores are non-negative" {
              let clubs, players = loadClubs ()
              let h, a, _ = trySimulateMatch clubs[0] clubs[1] players |> getOk
              Expect.isTrue (h >= 0 && a >= 0) $"negative score: {h}-{a}"
          }
          test "scores are plausible (each <= 10)" {
              let clubs, players = loadClubs ()
              let h, a, _ = trySimulateMatch clubs[0] clubs[1] players |> getOk
              Expect.isTrue (h <= 10 && a <= 10) $"implausible score: {h}-{a}"
          }
          test "goal events match reported score" {
              let clubs, players = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let hScore, aScore, events = trySimulateMatch home away players |> getOk
              let goals = events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length

              Expect.isTrue
                  (hGoals = hScore && aGoals = aScore)
                  $"goal event count mismatch for score {hScore}-{aScore}"
          }
          test "all event seconds in [1, 5700]" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.Second >= 1 && e.Second <= 95 * 60))
                  "event with second outside valid range"
          }
          test "all event playerIds belong to one of the two clubs" {
              let clubs, players = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away players |> getOk
              let homeIds = home.PlayerIds |> Set.ofList
              let awayIds = away.PlayerIds |> Set.ofList

              Expect.isTrue
                  (events
                   |> List.forall (fun e -> Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))
                  "event references unknown player"
          }
          test "events are ordered chronologically" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk
              let ordered = List.rev events

              Expect.isTrue
                  (ordered |> List.pairwise |> List.forall (fun (a, b) -> b.Second >= a.Second))
                  "events not sorted by second"
          }
          test "no duplicate goal events for same player at same second" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

              let goalKeys =
                  events
                  |> List.filter (fun e -> e.Type = Goal)
                  |> List.map (fun e -> e.PlayerId, e.Second)

              Expect.isTrue (goalKeys.Length = (goalKeys |> List.distinct).Length) "duplicate goal event detected"
          }
          test "all event ClubIds are either home or away" {
              let clubs, players = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away players |> getOk

              Expect.isTrue
                  (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id))
                  "event has unknown ClubId"
          }
          test "SubstitutionIn and SubstitutionOut are balanced" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk
              let ins = events |> List.filter (fun e -> e.Type = SubstitutionIn) |> List.length
              let outs = events |> List.filter (fun e -> e.Type = SubstitutionOut) |> List.length
              Expect.equal ins outs "substitution in/out count mismatch"
          }
          test "at most 3 substitutions per team" {
              let clubs, players = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events = trySimulateMatch home away players |> getOk

              let homeSubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = home.Id)
                  |> List.length

              let awaySubs =
                  events
                  |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = away.Id)
                  |> List.length

              Expect.isTrue
                  (homeSubs <= 3 && awaySubs <= 3)
                  $"more than 3 substitutions — home={homeSubs} away={awaySubs}"
          }
          test "no player receives more than 1 red card" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

              let reds =
                  events |> List.filter (fun e -> e.Type = RedCard) |> List.countBy _.PlayerId

              Expect.isTrue (reds |> List.forall (fun (_, count) -> count = 1)) "player has multiple red cards"
          }
          test "second yellow triggers a red card for same player" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

              let orderedEvents = List.rev events

              let playersWithTwoYellows =
                  orderedEvents
                  |> List.filter (fun e -> e.Type = YellowCard)
                  |> List.countBy _.PlayerId
                  |> List.filter (fun (_, count) -> count >= 2)
                  |> List.map fst
                  |> Set.ofList

              let redCardPlayers =
                  orderedEvents
                  |> List.filter (fun e -> e.Type = RedCard)
                  |> List.map _.PlayerId
                  |> Set.ofList

              Expect.isTrue
                  (Set.isSubset playersWithTwoYellows redCardPlayers)
                  "player received 2 yellows but no red card"
          }
          test "no player receives more than 2 yellow cards" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              Expect.isTrue
                  (yellows |> List.forall (fun (_, count) -> count <= 2))
                  "player has more than 2 yellow cards"
          }
          test "no goals scored at second 0" {
              let clubs, players = loadClubs ()
              let _, _, events = trySimulateMatch clubs[0] clubs[1] players |> getOk

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
        let clubs, players = loadClubs ()

        Array.Parallel.init iterations (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length

            try
                match trySimulateMatch clubs[hi] clubs[ai] players with
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

          testCase "avg goals per match in [1.8, 3.8]"
          <| fun () ->
              let totals =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h + a)
                      | _ -> None)

              let avg = float (Array.sum totals) / float totals.Length
              Expect.isTrue (avg >= 1.8 && avg <= 3.8) $"avg goals = %.2f{avg} (expected [1.8, 3.8])"

          testCase "no match has outlier score (either side > 10)"
          <| fun () ->
              let outliers =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) when h > 10 || a > 10 -> Some $"{h}-{a}"
                      | _ -> None)

              Expect.isEmpty outliers $"outlier scores detected: {outliers |> Array.truncate 5}"

          testCase "home wins more often than away"
          <| fun () ->
              let results =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h, a)
                      | _ -> None)

              let n = float results.Length

              let pct f =
                  results |> Array.filter f |> Array.length |> float |> (fun x -> x / n * 100.0)

              let homePct = pct (fun (h, a) -> h > a)
              let awayPct = pct (fun (h, a) -> a > h)
              Expect.isTrue (homePct > awayPct) (sprintf "home %.1f%% <= away %.1f%%" homePct awayPct)

          testCase "draws are not the most common outcome"
          <| fun () ->
              let results =
                  runOutcomes ()
                  |> Array.choose (function
                      | Success(h, a) -> Some(h, a)
                      | _ -> None)

              let n = float results.Length

              let pct f =
                  results |> Array.filter f |> Array.length |> float |> (fun x -> x / n * 100.0)

              let homePct = pct (fun (h, a) -> h > a)
              let awayPct = pct (fun (h, a) -> a > h)
              let drawPct = pct (fun (h, a) -> h = a)

              Expect.isTrue
                  (drawPct < homePct && drawPct < awayPct)
                  (sprintf "draws %.1f%% dominate (home=%.1f%% away=%.1f%%)" drawPct homePct awayPct)

          testCase "speed under 5 ms/match"
          <| fun () ->
              let clubs, players = loadClubs ()
              let sw = Diagnostics.Stopwatch.StartNew()

              Array.Parallel.init iterations (fun i ->
                  let hi = i % clubs.Length
                  let ai = (hi + 1) % clubs.Length
                  trySimulateMatch clubs[hi] clubs[ai] players)
              |> ignore

              sw.Stop()
              let msPerGame = sw.Elapsed.TotalMilliseconds / float iterations
              Expect.isTrue (msPerGame < 5.0) $"%.4f{msPerGame} ms/match (limit: 5 ms)" ]

let errorHandlingTests =
    testList
        "Error Handling Contracts"
        [ test "no lineup → MissingLineup" {
              let clubs, players = loadClubs ()
              let noLineup = { clubs[0] with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch noLineup clubs[1] players with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup"
          }
          test "empty lineup slots → IncompleteLineup" {
              let clubs, players = loadClubs ()
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
                  (match trySimulateMatch emptyLineup clubs[1] players with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup"
          }
          test "partial lineup (5 players) → IncompleteLineup" {
              let clubs, players = loadClubs ()
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
                  (match trySimulateMatch partialLineup clubs[1] players with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup for partial lineup"
          }
          test "MissingLineup error carries club name" {
              let clubs, players = loadClubs ()
              let home = clubs[0]
              let noLineup = { home with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch noLineup clubs[1] players with
                   | Error(MissingLineup name) -> name = home.Name
                   | _ -> false)
                  "MissingLineup did not carry correct club name"
          }
          test "swapped home/away still simulates" {
              let clubs, players = loadClubs ()
              Expect.isOk (trySimulateMatch clubs[1] clubs[0] players) "swap broke the simulation"
          }
          test "same club vs itself returns SameClub error" {
              let clubs, players = loadClubs ()

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[0] players with
                   | Error(SameClub _) -> true
                   | _ -> false)
                  "expected SameClub error when home.Id = away.Id"
          }
          test "away has no lineup → MissingLineup" {
              let clubs, players = loadClubs ()
              let noLineup = { clubs[1] with CurrentLineup = None }

              Expect.isTrue
                  (match trySimulateMatch clubs[0] noLineup players with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup for away club"
          }
          test "no GK in lineup → IncompleteLineup or MissingLineup, never throws" {
              let clubs, players = loadClubs ()
              let home = clubs[0]

              let noGk =
                  match home.CurrentLineup with
                  | None -> home
                  | Some lu ->
                      let withoutGk =
                          lu.Slots
                          |> List.map (fun s ->
                              match s.PlayerId with
                              | Some pid when players |> Map.tryFind pid |> Option.exists (fun p -> p.Position = GK) ->
                                  { s with PlayerId = None }
                              | _ -> s)

                      { home with
                          CurrentLineup = Some { lu with Slots = withoutGk } }

              Expect.isTrue
                  (match trySimulateMatch noGk clubs[1] players with
                   | Error(IncompleteLineup _)
                   | Error(MissingLineup _) -> true
                   | Error(SameClub _)
                   | Ok _ -> false)
                  "expected IncompleteLineup or MissingLineup when GK missing"
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
      test "home has at least 1 active player" { Expect.isTrue (homeActive >= 1) $"active home players = {homeActive}" }
      test "away has at least 1 active player" { Expect.isTrue (awayActive >= 1) $"active away players = {awayActive}" }
      test "home conditions length matches players" {
          Expect.equal s.HomeConditions.Length s.HomePlayers.Length "home conditions/players mismatch"
      }
      test "away conditions length matches players" {
          Expect.equal s.AwayConditions.Length s.AwayPlayers.Length "away conditions/players mismatch"
      }
      test "all home conditions in [0, 100]" {
          Expect.isTrue (s.HomeConditions |> Array.forall (fun c -> c >= 0 && c <= 100)) "home condition out of range"
      }
      test "all away conditions in [0, 100]" {
          Expect.isTrue (s.AwayConditions |> Array.forall (fun c -> c >= 0 && c <= 100)) "away condition out of range"
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
    |> testList label

let replayTests =
    testList
        "Replay UI Contracts"
        [ testCase "replay has snapshots"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              Expect.isTrue (replay.Snapshots.Length > 0) "no snapshots — viewer slider will be empty"
          testCase "snapshot seconds are non-decreasing"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.Second >= a.Second))
                  "time went backwards between snapshots"
          testCase "home score never decreases"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.HomeScore >= a.HomeScore))
                  "home score decreased between snapshots"
          testCase "away score never decreases"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.AwayScore >= a.AwayScore))
                  "away score decreased between snapshots"
          testCase "final score >= last snapshot score"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              let last = replay.Snapshots[replay.Snapshots.Length - 1]

              Expect.isTrue
                  (replay.Final.HomeScore >= last.HomeScore
                   && replay.Final.AwayScore >= last.AwayScore)
                  "final score lower than last snapshot"
          testCase "final second = 5700 (95 min)"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              Expect.equal replay.Final.Second (95 * 60) "final.Second != 5700"
          testCase "goal events match final score"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              let final = replay.Final
              let goals = final.EventsRev |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = final.Home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = final.Away.Id) |> List.length

              Expect.isTrue
                  (hGoals = final.HomeScore && aGoals = final.AwayScore)
                  $"goal events don't match {final.HomeScore}-{final.AwayScore}"
          testCase "all snapshots have [11, 14] home players"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.HomePlayers.Length >= 11 && s.HomePlayers.Length <= 14))
                  "snapshot with invalid home player count"
          testCase "all snapshots have [11, 14] away players"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.AwayPlayers.Length >= 11 && s.AwayPlayers.Length <= 14))
                  "snapshot with invalid away player count"
          testCase "first snapshot second > 0"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              Expect.isTrue (replay.Snapshots.Length = 0 || replay.Snapshots[0].Second > 0) "first snapshot at second 0"
          testCase "last snapshot second <= 5700"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk

              Expect.isTrue
                  (replay.Snapshots.Length = 0
                   || replay.Snapshots[replay.Snapshots.Length - 1].Second <= 95 * 60)
                  "last snapshot beyond 95 min"
          testCase "final snapshot invariants pass"
          <| fun () ->
              let clubs, players = loadClubs ()
              let replay = trySimulateMatchFull clubs[0] clubs[1] players |> getOk
              let result = runTestsWithCLIArgs [] [||] (snapshotChecks "final" replay.Final)
              Expect.equal result 0 "one or more final snapshot invariants failed" ]

let matchStateOpsTests =
    let minimalState homeScore awayScore =
        { Home = Unchecked.defaultof<_>
          Away = Unchecked.defaultof<_>
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

let multiMatchTests =
    testList
        "Multi-Match Consistency"
        [ test "all repeated match scores non-negative" {
              let clubs, players = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1] players)
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h >= 0 && a >= 0)) "negative score detected"
          }
          test "matches produce at least 5 distinct scores across 50 runs" {
              let clubs, players = loadClubs ()

              let distinctScores =
                  Array.init 50 (fun _ -> trySimulateMatch clubs[0] clubs[1] players)
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)
                  |> Array.distinct

              Expect.isTrue
                  (distinctScores.Length >= 5)
                  $"only {distinctScores.Length} distinct scores in 50 runs — RNG may be broken"
          }
          test "no match has outlier score (> 10 each)" {
              let clubs, players = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1] players)
                  |> Array.choose (function
                      | Ok(h, a, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h <= 10 && a <= 10)) "outlier score detected"
          } ]
