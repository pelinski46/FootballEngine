namespace FootballEngine.Test

open System
open FootballEngine
open FootballEngine.Client.AI.ManagerAI
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FootballEngine.MatchContext
open FootballEngine.MatchSimulator

module MatchEngineTests =

    type TestResult =
        | Pass of name: string
        | Fail of name: string * reason: string

    let private check name condition msg =
        if condition then Pass name else Fail(name, msg)

    let private printResult =
        function
        | Pass name -> printfn $"  ✅ %s{name}"
        | Fail(name, reason) -> printfn $"  ❌ %s{name} — %s{reason}"

    let runSuite suiteName (tests: TestResult list) =
        printfn $"\n── %s{suiteName} ──"
        tests |> List.iter printResult

        let failures =
            tests
            |> List.choose (function
                | Fail(n, r) -> Some(n, r)
                | _ -> None)

        if failures.IsEmpty then
            printfn "   All passed.\n"
        else
            printfn $"   %d{failures.Length} failure(s).\n"

        failures

    let private makeReadyClub (c: Club) =
        ensureLineup { c with CurrentLineup = None } (pickBestFormation c)

    let private loadClubs () =
        match Db.loadGame () with
        | None -> failwith "No saved game — run generateNewGame first."
        | Some game ->
            let clubs = game.Clubs |> Map.toArray |> Array.map (snd >> makeReadyClub)

            if clubs.Length < 2 then
                failwith "Need at least 2 clubs."

            clubs

    // ------------------------------------------------------------------ //
    //  Single match                                                        //
    // ------------------------------------------------------------------ //

    let singleMatchContracts () =
        let clubs = loadClubs ()
        let home = clubs[0]
        let away = clubs[1]
        let result = trySimulateMatch home away

        let tests =
            [ check "simulateMatch returns Ok" result.IsOk "trySimulateMatch returned Error — check lineup/GK"

              match result with
              | Error _ -> ()
              | Ok(hScore, aScore, events) ->

                  check "scores are non-negative" (hScore >= 0 && aScore >= 0) $"negative score: {hScore}-{aScore}"

                  check
                      "scores are plausible (each <= 15)"
                      (hScore <= 15 && aScore <= 15)
                      $"implausible score: {hScore}-{aScore}"

                  check
                      "goal events match reported score"
                      (let goals = events |> List.filter (fun e -> e.Type = Goal)
                       let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
                       let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length
                       hGoals = hScore && aGoals = aScore)
                      $"goal event count mismatch for score {hScore}-{aScore}"

                  check
                      "all event seconds in [0, 5700]"
                      (events |> List.forall (fun e -> e.Second >= 0 && e.Second <= 95 * 60))
                      "event with second outside valid range"

                  check
                      "all event playerIds belong to one of the two clubs"
                      (let homeIds = home.Players |> List.map _.Id |> Set.ofList
                       let awayIds = away.Players |> List.map _.Id |> Set.ofList

                       events
                       |> List.forall (fun e -> Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))
                      "event references unknown player" ]

        runSuite "Single Match Invariants" tests

    // ------------------------------------------------------------------ //
    //  Statistical contracts                                               //
    // ------------------------------------------------------------------ //

    // Expected football distributions (real-world references):
    //   avg goals/match : ~2.5–2.8  (top European leagues)
    //   home win rate   : ~44–46%
    //   draw rate       : ~24–27%
    //   away win rate   : ~27–30%

    type private Bucket =
        { Goals: int
          HomeWins: int
          AwayWins: int
          Draws: int
          Count: int }

    type private FixtureOutcome =
        | Success of homeGoals: int * awayGoals: int
        | KnownError of home: string * away: string * error: SimulationError
        | UnhandledException of home: string * away: string * exn: exn

    let statisticalContracts (iterations: int) =
        printfn $"\n── Statistical Contracts (%d{iterations} matches) ──"
        let clubs = loadClubs ()
        let sw = Diagnostics.Stopwatch.StartNew()

        let outcomes =
            Array.Parallel.init iterations (fun i ->
                let hi = i % clubs.Length
                let ai = (hi + 1) % clubs.Length
                let home = clubs[hi]
                let away = clubs[ai]

                try
                    match trySimulateMatch home away with
                    | Ok(h, a, _) -> Success(h, a)
                    | Error e -> KnownError(home.Name, away.Name, e)
                with ex ->
                    UnhandledException(home.Name, away.Name, ex))

        sw.Stop()

        let failures =
            outcomes
            |> Array.choose (function
                | KnownError(h, a, e) -> Some(h, a, $"SimulationError: %A{e}")
                | UnhandledException(h, a, ex) -> Some(h, a, $"UNHANDLED %s{ex.GetType().Name}: %s{ex.Message}")
                | Success _ -> None)

        if failures.Length > 0 then
            printfn $"  ⚠  %d{failures.Length} fixture(s) failed:"

            for h, a, msg in failures do
                printfn $"       • %s{h} vs %s{a}"
                printfn $"         %s{msg}"

            printfn ""

        let totals =
            outcomes
            |> Array.fold
                (fun acc outcome ->
                    match outcome with
                    | Success(h, a) ->
                        { acc with
                            Goals = acc.Goals + h + a
                            HomeWins = acc.HomeWins + (if h > a then 1 else 0)
                            AwayWins = acc.AwayWins + (if a > h then 1 else 0)
                            Draws = acc.Draws + (if h = a then 1 else 0)
                            Count = acc.Count + 1 }
                    | _ -> acc)
                { Goals = 0
                  HomeWins = 0
                  AwayWins = 0
                  Draws = 0
                  Count = 0 }

        let n = float totals.Count
        let avg = float totals.Goals / n
        let homePct = float totals.HomeWins / n * 100.0
        let awayPct = float totals.AwayWins / n * 100.0
        let drawPct = float totals.Draws / n * 100.0
        let msPerGame = sw.Elapsed.TotalMilliseconds / float iterations

        printfn $"  Simulated : %d{totals.Count} / %d{iterations}"
        printfn $"  Avg goals : %.2f{avg}  (real: ~2.5-2.8)"
        printfn "  Home wins : %.1f%%  (real: ~44-46%%)" homePct
        printfn "  Away wins : %.1f%%  (real: ~27-30%%)" awayPct
        printfn "  Draws     : %.1f%%  (real: ~24-27%%)" drawPct
        printfn $"  Speed     : %.4f{msPerGame} ms/match"
        printfn ""

        let tests =
            [ check
                  "all fixtures simulated successfully"
                  (totals.Count = iterations)
                  $"%d{iterations - totals.Count} fixtures failed"

              check "avg goals in range [1.5, 5.0]" (avg >= 1.5 && avg <= 5.0) $"avg = %.2f{avg}"

              check
                  "home wins more often than away"
                  (homePct > awayPct)
                  (sprintf "home %.1f%% <= away %.1f%%" homePct awayPct)

              check
                  "draws are not the most common outcome"
                  (drawPct < homePct && drawPct < awayPct)
                  (sprintf "draws %.1f%% dominate" drawPct)

              check
                  "outcome percentages sum to 100"
                  (abs (homePct + awayPct + drawPct - 100.0) < 0.01)
                  $"sum = %.4f{homePct + awayPct + drawPct}"

              check "speed under 5 ms/match" (msPerGame < 5.0) $"%.4f{msPerGame} ms/match" ]

        runSuite "Statistical Contracts" tests

    // ------------------------------------------------------------------ //
    //  Error handling                                                      //
    // ------------------------------------------------------------------ //

    let errorHandlingContracts () =
        let clubs = loadClubs ()
        let home = clubs[0]
        let away = clubs[1]

        let noLineup = { home with CurrentLineup = None }

        let emptyLineup =
            match home.CurrentLineup with
            | None -> home
            | Some lu ->
                let cleared = lu.Slots |> List.map (fun s -> { s with PlayerId = None })

                { home with
                    CurrentLineup = Some { lu with Slots = cleared } }

        let tests =
            [ check
                  "no lineup → MissingLineup"
                  (match trySimulateMatch noLineup away with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup"

              check
                  "empty lineup slots → IncompleteLineup"
                  (match trySimulateMatch emptyLineup away with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup"

              check
                  "swapped home/away still simulates"
                  (trySimulateMatch away home |> Result.isOk)
                  "swap broke the simulation" ]

        runSuite "Error Handling Contracts" tests

    // ------------------------------------------------------------------ //
    //  Replay UI contracts                                                 //
    // ------------------------------------------------------------------ //

    let private inBounds (x: float, y: float) =
        x >= 0.0 && x <= 100.0 && y >= 0.0 && y <= 100.0

    let private allPositionsInBounds (positions: Map<PlayerId, float * float>) =
        positions |> Map.forall (fun _ pos -> inBounds pos)

    let private checkSnapshot (label: string) (s: MatchState) : TestResult list =
        let homeActiveCount =
            s.HomePlayers
            |> Array.filter (fun p -> not (Map.containsKey p.Id s.HomeSidelined))
            |> Array.length

        let awayActiveCount =
            s.AwayPlayers
            |> Array.filter (fun p -> not (Map.containsKey p.Id s.AwaySidelined))
            |> Array.length

        [ check $"{label}: ball in bounds" (inBounds s.BallPosition) $"ball at {s.BallPosition}"

          check $"{label}: home positions in bounds" (allPositionsInBounds s.HomePositions) "home player outside 0-100"

          check $"{label}: away positions in bounds" (allPositionsInBounds s.AwayPositions) "away player outside 0-100"

          check
              $"{label}: scores non-negative"
              (s.HomeScore >= 0 && s.AwayScore >= 0)
              $"score {s.HomeScore}-{s.AwayScore}"

          check $"{label}: second in [0, 5700]" (s.Second >= 0 && s.Second <= 95 * 60) $"second = {s.Second}"

          check $"{label}: momentum in [-10, 10]" (s.Momentum >= -10.0 && s.Momentum <= 10.0) $"momentum = {s.Momentum}"

          check
              $"{label}: home has at least 1 active player"
              (homeActiveCount >= 1)
              $"active home players = {homeActiveCount}"

          check
              $"{label}: away has at least 1 active player"
              (awayActiveCount >= 1)
              $"active away players = {awayActiveCount}"

          check
              $"{label}: home conditions length matches players"
              (s.HomeConditions.Length = s.HomePlayers.Length)
              $"players={s.HomePlayers.Length} conditions={s.HomeConditions.Length}"

          check
              $"{label}: away conditions length matches players"
              (s.AwayConditions.Length = s.AwayPlayers.Length)
              $"players={s.AwayPlayers.Length} conditions={s.AwayConditions.Length}"

          check
              $"{label}: all home conditions in [0, 100]"
              (s.HomeConditions |> Array.forall (fun c -> c >= 0 && c <= 100))
              "home condition out of range"

          check
              $"{label}: all away conditions in [0, 100]"
              (s.AwayConditions |> Array.forall (fun c -> c >= 0 && c <= 100))
              "away condition out of range"

          check
              $"{label}: each home player has a position entry"
              (s.HomePlayers |> Array.forall (fun p -> Map.containsKey p.Id s.HomePositions))
              "home player missing position"

          check
              $"{label}: each away player has a position entry"
              (s.AwayPlayers |> Array.forall (fun p -> Map.containsKey p.Id s.AwayPositions))
              "away player missing position"

          check
              $"{label}: no player on both teams"
              (let homeIds = s.HomePlayers |> Array.map _.Id |> Set.ofArray
               let awayIds = s.AwayPlayers |> Array.map _.Id |> Set.ofArray
               Set.isEmpty (Set.intersect homeIds awayIds))
              "player appears in both squads"

          check
              $"{label}: subs used in [0, 3]"
              (s.HomeSubsUsed >= 0
               && s.HomeSubsUsed <= 3
               && s.AwaySubsUsed >= 0
               && s.AwaySubsUsed <= 3)
              $"subs home={s.HomeSubsUsed} away={s.AwaySubsUsed}" ]

    let private checkReplayProgression (replay: MatchReplay) : TestResult list =
        let snapshots = replay.Snapshots
        let final = replay.Final

        [ check "replay has snapshots" (snapshots.Length > 0) "no snapshots — viewer slider will be empty"

          check
              "snapshot seconds are non-decreasing"
              (snapshots |> Array.pairwise |> Array.forall (fun (a, b) -> b.Second >= a.Second))
              "time went backwards between snapshots"

          check
              "home score never decreases"
              (snapshots
               |> Array.pairwise
               |> Array.forall (fun (a, b) -> b.HomeScore >= a.HomeScore))
              "home score decreased between snapshots"

          check
              "away score never decreases"
              (snapshots
               |> Array.pairwise
               |> Array.forall (fun (a, b) -> b.AwayScore >= a.AwayScore))
              "away score decreased between snapshots"

          check
              "final score >= last snapshot score"
              (snapshots.Length = 0
               || let last = snapshots[snapshots.Length - 1] in
                  final.HomeScore >= last.HomeScore && final.AwayScore >= last.AwayScore)
              "final score lower than last snapshot"

          check "final second = 5700 (95 min)" (final.Second = 95 * 60) $"final.Second = {final.Second}"

          check
              "goal events match final score"
              (let goals = final.EventsRev |> List.filter (fun e -> e.Type = Goal)
               let hGoals = goals |> List.filter (fun e -> e.ClubId = final.Home.Id) |> List.length
               let aGoals = goals |> List.filter (fun e -> e.ClubId = final.Away.Id) |> List.length
               hGoals = final.HomeScore && aGoals = final.AwayScore)
              $"goal events don't match {final.HomeScore}-{final.AwayScore}" ]

    let replayUIContracts () =
        let clubs = loadClubs ()
        let home = clubs[0]
        let away = clubs[1]

        match trySimulateMatchFull home away with
        | Error e -> runSuite "Replay UI Contracts" [ Fail("simulation", $"SimulationError: %A{e}") ]
        | Ok replay ->
            let finalTests = checkSnapshot "final" replay.Final
            let progressTests = checkReplayProgression replay



            runSuite "Replay UI Contracts" (finalTests @ progressTests)

    // ------------------------------------------------------------------ //
    //  Entry point                                                         //
    // ------------------------------------------------------------------ //

    let runAll () =
        printfn "\n====== Football Engine — Match Simulation Tests ======"

        // Correr cada suite y acumular solo los fallos
        let allFailures =
            [ yield! singleMatchContracts ()
              yield! statisticalContracts 10000
              yield! errorHandlingContracts ()
              yield! replayUIContracts () ]

        printfn "======================================================"

        if allFailures.IsEmpty then
            printfn "All contracts passed.\n"
            0
        else
            printfn $"{allFailures.Length} contract(s) failed:"
            allFailures |> List.iter (fun (n, r) -> printfn $"  • {n}: {r}")
            1
