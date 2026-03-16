namespace FootballEngine.Test

open System
open FootballEngine
open FootballEngine.Client.AI.ManagerAI
open FootballEngine.Domain
open FootballEngine.Engine
open FootballEngine.MatchSimulator
open FootballEngine.MatchStateOps

module MatchEngineTests =

    // ══════════════════════════════════════════════════════════════════════
    //  Test harness
    // ══════════════════════════════════════════════════════════════════════

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


    // ══════════════════════════════════════════════════════════════════════
    //  Helpers
    // ══════════════════════════════════════════════════════════════════════

    let private makeReadyClub (c: Club) =
        ensureLineup { c with CurrentLineup = None } (pickBestFormation c)

    let private loadGame () =
        match Db.loadGame () with
        | None -> failwith "No saved game — run generateNewGame first."
        | Some game -> game

    let private loadClubs () =
        let game = loadGame ()
        let clubs = game.Clubs |> Map.toArray |> Array.map (snd >> makeReadyClub)

        if clubs.Length < 2 then
            failwith "Need at least 2 clubs."

        clubs

    let private inBounds (x: float, y: float) =
        x >= 0.0 && x <= 100.0 && y >= 0.0 && y <= 100.0

    let private allPositionsInBounds (positions: Map<PlayerId, float * float>) =
        positions |> Map.forall (fun _ pos -> inBounds pos)


    // ══════════════════════════════════════════════════════════════════════
    //  1. Single match invariants
    // ══════════════════════════════════════════════════════════════════════

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
                      "event references unknown player"

                  check
                      "events are ordered chronologically"
                      (let ordered = List.rev events
                       ordered |> List.pairwise |> List.forall (fun (a, b) -> b.Second >= a.Second))
                      "events not sorted by second (EventsRev reversed)"

                  check
                      "no duplicate goal events for same player at same second"
                      (let goalKeys =
                          events
                          |> List.filter (fun e -> e.Type = Goal)
                          |> List.map (fun e -> e.PlayerId, e.Second)

                       goalKeys.Length = (goalKeys |> List.distinct).Length)
                      "duplicate goal event detected"

                  check
                      "all event ClubIds are either home or away"
                      (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id))
                      "event has unknown ClubId"

                  check
                      "SubstitutionIn and SubstitutionOut are balanced"
                      (let ins = events |> List.filter (fun e -> e.Type = SubstitutionIn) |> List.length
                       let outs = events |> List.filter (fun e -> e.Type = SubstitutionOut) |> List.length
                       ins = outs)
                      "substitution in/out count mismatch"

                  check
                      "at most 3 substitutions per team"
                      (let homeSubs =
                          events
                          |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = home.Id)
                          |> List.length

                       let awaySubs =
                           events
                           |> List.filter (fun e -> e.Type = SubstitutionIn && e.ClubId = away.Id)
                           |> List.length

                       homeSubs <= 3 && awaySubs <= 3)
                      "more than 3 substitutions for a team"

                  check
                      "no player receives more than 1 red card"
                      (let reds =
                          events |> List.filter (fun e -> e.Type = RedCard) |> List.countBy _.PlayerId

                       reds |> List.forall (fun (_, count) -> count = 1))
                      "player has multiple red cards"

                  check
                      "no player receives more than 2 yellow cards"
                      (let yellows =
                          events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

                       yellows |> List.forall (fun (_, count) -> count <= 2))
                      "player has more than 2 yellow cards"

                  check
                      "no goals scored at second 0"
                      (events
                       |> List.filter (fun e -> e.Type = Goal)
                       |> List.forall (fun e -> e.Second > 0))
                      "goal scored at second 0" ]

        runSuite "Single Match Invariants" tests


    // ══════════════════════════════════════════════════════════════════════
    //  2. Statistical contracts
    // ══════════════════════════════════════════════════════════════════════

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
                printfn $"       • %s{h} vs %s{a} — %s{msg}"

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

        runSuite
            "Statistical Contracts"
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


    // ══════════════════════════════════════════════════════════════════════
    //  3. Error handling contracts
    // ══════════════════════════════════════════════════════════════════════

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

        let partialLineup =
            match home.CurrentLineup with
            | None -> home
            | Some lu ->
                let partial =
                    lu.Slots
                    |> List.mapi (fun i s -> if i < 5 then s else { s with PlayerId = None })

                { home with
                    CurrentLineup = Some { lu with Slots = partial } }

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

        runSuite
            "Error Handling Contracts"
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
                  "partial lineup (5 players) → IncompleteLineup"
                  (match trySimulateMatch partialLineup away with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup for partial lineup"

              check
                  "MissingLineup error carries club name"
                  (match trySimulateMatch noLineup away with
                   | Error(MissingLineup name) -> name = home.Name
                   | _ -> false)
                  "MissingLineup did not carry correct club name"

              check
                  "swapped home/away still simulates"
                  (trySimulateMatch away home |> Result.isOk)
                  "swap broke the simulation"

              check
                  "same club vs itself returns Ok or a defined error"
                  (match trySimulateMatch home home with
                   | Ok _ -> true
                   | Error(MissingLineup _) -> true
                   | Error(IncompleteLineup _) -> true)
                  "unexpected exception simulating club vs itself"

              check
                  "away has no lineup → MissingLineup"
                  (match trySimulateMatch home noLineup with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup for away club"

              check
                  "no GK in lineup still returns Ok or IncompleteLineup — never throws"
                  (match trySimulateMatch noGk away with
                   | Ok _ -> true
                   | Error(IncompleteLineup _) -> true
                   | Error(MissingLineup _) -> true)
                  "unexpected exception when GK missing" ]


    // ══════════════════════════════════════════════════════════════════════
    //  4. Replay UI contracts
    // ══════════════════════════════════════════════════════════════════════

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
              $"subs home={s.HomeSubsUsed} away={s.AwaySubsUsed}"
          check
              $"{label}: home base positions count matches players"
              (s.HomeBasePositions.Count = s.HomePlayers.Length)
              $"base positions={s.HomeBasePositions.Count} players={s.HomePlayers.Length}"
          check
              $"{label}: away base positions count matches players"
              (s.AwayBasePositions.Count = s.AwayPlayers.Length)
              $"base positions={s.AwayBasePositions.Count} players={s.AwayPlayers.Length}" ]

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
               || (let last = snapshots[snapshots.Length - 1]
                   final.HomeScore >= last.HomeScore && final.AwayScore >= last.AwayScore))
              "final score lower than last snapshot"

          check "final second = 5700 (95 min)" (final.Second = 95 * 60) $"final.Second = {final.Second}"

          check
              "goal events match final score"
              (let goals = final.EventsRev |> List.filter (fun e -> e.Type = Goal)
               let hGoals = goals |> List.filter (fun e -> e.ClubId = final.Home.Id) |> List.length
               let aGoals = goals |> List.filter (fun e -> e.ClubId = final.Away.Id) |> List.length
               hGoals = final.HomeScore && aGoals = final.AwayScore)
              $"goal events don't match {final.HomeScore}-{final.AwayScore}"

          check
              "snapshot count is reasonable (at least 1)"
              (snapshots.Length >= 1)
              $"snapshot count = {snapshots.Length}"

          check
              "all snapshots have at least 11 home players in array (subs append)"
              (snapshots |> Array.forall (fun s -> s.HomePlayers.Length >= 11))
              "snapshot with < 11 home players"

          check
              "all snapshots have at most 14 home players in array (max 3 subs)"
              (snapshots |> Array.forall (fun s -> s.HomePlayers.Length <= 14))
              "snapshot with > 14 home players"

          check
              "all snapshots have at least 11 away players in array (subs append)"
              (snapshots |> Array.forall (fun s -> s.AwayPlayers.Length >= 11))
              "snapshot with < 11 away players"

          check
              "all snapshots have at most 14 away players in array (max 3 subs)"
              (snapshots |> Array.forall (fun s -> s.AwayPlayers.Length <= 14))
              "snapshot with > 14 away players"

          check
              "first snapshot second > 0"
              (snapshots.Length = 0 || snapshots[0].Second > 0)
              "first snapshot at second 0 — match hasn't started"

          check
              "last snapshot second <= 5700"
              (snapshots.Length = 0 || snapshots[snapshots.Length - 1].Second <= 95 * 60)
              "last snapshot beyond 95 min" ]

    let replayUIContracts () =
        let clubs = loadClubs ()
        let home = clubs[0]
        let away = clubs[1]

        match trySimulateMatchFull home away with
        | Error e -> runSuite "Replay UI Contracts" [ Fail("simulation", $"SimulationError: %A{e}") ]
        | Ok replay ->
            runSuite "Replay UI Contracts" (checkSnapshot "final" replay.Final @ checkReplayProgression replay)


    // ══════════════════════════════════════════════════════════════════════
    //  5. Engine batch / standings contracts
    // ══════════════════════════════════════════════════════════════════════

    let batchAndStandingsContracts () =
        let game = loadGame ()

        let allFixtures =
            game.Competitions
            |> Map.toList
            |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
            |> List.filter (fun (_, f) -> not f.Played)
            |> List.truncate 20

        if allFixtures.IsEmpty then
            runSuite
                "Batch & Standings Contracts"
                [ Fail("fixtures available", "No unplayed fixtures found — start a new game") ]
        else

            let result = simulateFixtures game allFixtures
            let simulated = allFixtures.Length - result.Errors.Length

            let standingsSane =
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

            let noUnplayedBecomePlayedTwice =
                result.GameState.Competitions
                |> Map.forall (fun _ comp ->
                    allFixtures
                    |> List.forall (fun (id, _) ->
                        comp.Fixtures
                        |> Map.tryFind id
                        |> Option.map (fun f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome))
                        |> Option.defaultValue true))

            let allPlayedFixturesHaveScores =
                result.GameState.Competitions
                |> Map.forall (fun _ comp ->
                    comp.Fixtures
                    |> Map.forall (fun _ f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome)))

            let noNegativePoints =
                result.GameState.Competitions
                |> Map.forall (fun _ comp -> comp.Standings |> Map.forall (fun _ s -> s.Points >= 0))

            let pointsConsistency =
                result.GameState.Competitions
                |> Map.forall (fun _ comp -> comp.Standings |> Map.forall (fun _ s -> s.Points = s.Won * 3 + s.Drawn))

            let playedConsistency =
                result.GameState.Competitions
                |> Map.forall (fun _ comp ->
                    comp.Standings |> Map.forall (fun _ s -> s.Won + s.Drawn + s.Lost = s.Played))

            let maxReasonablePlayed =
                result.GameState.Competitions
                |> Map.forall (fun _ comp ->
                    comp.Standings |> Map.forall (fun _ s -> s.Played <= comp.ClubIds.Length * 2))

            let goalDiffConsistency =
                result.GameState.Competitions
                |> Map.forall (fun _ comp ->
                    let totalFor = comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsFor)

                    let totalAgainst =
                        comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsAgainst)

                    totalFor = totalAgainst)

            let batchResultHasLogs = simulated > 0 && result.Logs.Length > 0

            let logsMatchSimulated = result.Logs.Length = simulated

            runSuite
                "Batch & Standings Contracts"
                [ check "batch simulated at least 1 fixture" (simulated > 0) "all fixtures failed to simulate"

                  check
                      "no errors in batch"
                      (result.Errors.IsEmpty)
                      (let errList =
                          result.Errors
                          |> List.map (fun (id, e) -> sprintf "%d:%A" id e)
                          |> String.concat ", "

                       sprintf "%d errors: %s" result.Errors.Length errList)

                  check
                      "all standings are mathematically sane"
                      standingsSane
                      "W+D+L ≠ Played or Points ≠ 3W+D or negative values"

                  check
                      "all played fixtures have scores"
                      allPlayedFixturesHaveScores
                      "played fixture missing HomeScore or AwayScore"
                  check "no negative points" noNegativePoints "club has negative points"
                  check "points = 3W + D" pointsConsistency "points formula violated"
                  check "Won + Drawn + Lost = Played" playedConsistency "played count inconsistent"
                  check
                      "Played never exceeds 2*(clubs-1)"
                      maxReasonablePlayed
                      "a club has played more games than possible"
                  check
                      "GoalsFor = GoalsAgainst across all standings"
                      goalDiffConsistency
                      "total goals for ≠ total goals against"
                  check "batch logs are produced" batchResultHasLogs "no log entries despite simulating fixtures"
                  check
                      "log count matches simulated count"
                      logsMatchSimulated
                      $"logs={result.Logs.Length} simulated={simulated}" ]


    // ══════════════════════════════════════════════════════════════════════
    //  6. Double-simulation guard contracts
    // ══════════════════════════════════════════════════════════════════════

    let doubleSimulationGuardContracts () =
        let game = loadGame ()

        let fixtures =
            game.Competitions
            |> Map.toList
            |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
            |> List.filter (fun (_, f) -> not f.Played)
            |> List.truncate 5

        if fixtures.IsEmpty then
            runSuite "Double-Simulation Guard" [ Fail("fixtures available", "No unplayed fixtures to test") ]
        else

            let result1 = simulateFixtures game fixtures
            let result2 = simulateFixtures result1.GameState fixtures

            let standingsAfterFirst =
                result1.GameState.Competitions
                |> Map.toList
                |> List.collect (fun (_, comp) -> comp.Standings |> Map.toList)

            let standingsAfterSecond =
                result2.GameState.Competitions
                |> Map.toList
                |> List.collect (fun (_, comp) -> comp.Standings |> Map.toList)

            let standingsUnchanged =
                standingsAfterFirst
                |> List.forall (fun (clubId, s1) ->
                    standingsAfterSecond
                    |> List.tryFind (fun (id, _) -> id = clubId)
                    |> Option.map (fun (_, s2) -> s1.Played = s2.Played && s1.Won = s2.Won && s1.Points = s2.Points)
                    |> Option.defaultValue true)

            let pointsNotDoubled =
                let maxPointsFirst = standingsAfterFirst |> List.map (snd >> _.Points) |> List.max
                let maxPointsSecond = standingsAfterSecond |> List.map (snd >> _.Points) |> List.max
                maxPointsSecond = maxPointsFirst

            let secondBatchHasNoNewLogs = result2.Logs.IsEmpty

            let secondBatchHasNoErrors = result2.Errors.IsEmpty

            runSuite
                "Double-Simulation Guard"
                [ check
                      "standings unchanged after simulating already-played fixtures again"
                      standingsUnchanged
                      "standings changed — fixture was processed twice"

                  check
                      "max points not doubled after second pass"
                      pointsNotDoubled
                      "points doubled — alreadyPlayed guard is not working"

                  check
                      "second batch produces no new log entries"
                      secondBatchHasNoNewLogs
                      "second batch added log entries for already-played fixtures"

                  check
                      "second batch produces no errors"
                      secondBatchHasNoErrors
                      "second batch produced unexpected errors" ]


    // ══════════════════════════════════════════════════════════════════════
    //  7. Game state integrity contracts
    // ══════════════════════════════════════════════════════════════════════

    let gameStateIntegrityContracts () =
        let game = loadGame ()

        let allPlayerIds = game.Players |> Map.toList |> List.map fst |> Set.ofList
        let allClubIds = game.Clubs |> Map.toList |> List.map fst |> Set.ofList
        let allCompIds = game.Competitions |> Map.toList |> List.map fst |> Set.ofList

        let clubPlayerRefValid =
            game.Clubs
            |> Map.forall (fun _ club -> club.Players |> List.forall (fun p -> Set.contains p.Id allPlayerIds))

        let playerClubRefValid =
            game.Players |> Map.forall (fun _ p -> Set.contains p.ClubId allClubIds)

        let fixtureClubRefsValid =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Fixtures
                |> Map.forall (fun _ f -> Set.contains f.HomeClubId allClubIds && Set.contains f.AwayClubId allClubIds))

        let fixtureCompRefValid =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Fixtures |> Map.forall (fun _ f -> Set.contains f.CompetitionId allCompIds))

        let standingClubRefsValid =
            game.Competitions
            |> Map.forall (fun _ comp -> comp.Standings |> Map.forall (fun clubId _ -> Set.contains clubId allClubIds))

        let noFixtureHomeEqualsAway =
            game.Competitions
            |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.HomeClubId <> f.AwayClubId))

        let userClubExists = Set.contains game.UserClubId allClubIds

        let noPlayerInTwoClubs =
            let allRefs =
                game.Clubs
                |> Map.toList
                |> List.collect (fun (_, c) -> c.Players |> List.map (fun p -> p.Id, c.Id))

            let grouped = allRefs |> List.groupBy fst
            grouped |> List.forall (fun (_, refs) -> refs.Length = 1)

        let playedFixturesHaveBothScores =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Fixtures
                |> Map.forall (fun _ f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome)))

        let unplayedFixturesHaveNoScores =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Fixtures
                |> Map.forall (fun _ f -> f.Played || (f.HomeScore.IsNone && f.AwayScore.IsNone)))

        let fixtureIdsAreUnique =
            let allIds =
                game.Competitions
                |> Map.toList
                |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList |> List.map fst)

            allIds.Length = (allIds |> List.distinct).Length

        let competitionClubIdsValid =
            game.Competitions
            |> Map.forall (fun _ comp -> comp.ClubIds |> List.forall (fun cid -> Set.contains cid allClubIds))

        let competitionSeasonMatchesGame =
            game.Competitions |> Map.forall (fun _ comp -> comp.Season = game.Season)

        let fixtureCompetitionIdMatchesParent =
            game.Competitions
            |> Map.forall (fun compId comp -> comp.Fixtures |> Map.forall (fun _ f -> f.CompetitionId = compId))

        let knockoutTieClubRefsValid =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.KnockoutTies
                |> Map.forall (fun _ tie ->
                    Set.contains tie.HomeClubId allClubIds && Set.contains tie.AwayClubId allClubIds))

        runSuite
            "Game State Integrity"
            [ check "UserClubId exists in Clubs" userClubExists $"UserClubId {game.UserClubId} not found"
              check "all club.Players reference valid PlayerIds" clubPlayerRefValid "club references unknown player"
              check "all players reference valid ClubId" playerClubRefValid "player references unknown club"
              check "no player appears in two clubs" noPlayerInTwoClubs "player found in multiple clubs"
              check "all fixture club refs are valid" fixtureClubRefsValid "fixture references unknown club"
              check
                  "all fixture competitionId refs are valid"
                  fixtureCompRefValid
                  "fixture references unknown competition"
              check "all standing club refs are valid" standingClubRefsValid "standing references unknown club"
              check
                  "no fixture has HomeClubId = AwayClubId"
                  noFixtureHomeEqualsAway
                  "fixture has same home and away club"
              check "played fixtures have both scores" playedFixturesHaveBothScores "played fixture missing score"
              check "unplayed fixtures have no scores" unplayedFixturesHaveNoScores "unplayed fixture has a score"
              check "all fixture IDs are unique across competitions" fixtureIdsAreUnique "duplicate fixture ID detected"
              check
                  "all competition.ClubIds reference valid clubs"
                  competitionClubIdsValid
                  "competition references unknown club"
              check
                  "all competition seasons match game season"
                  competitionSeasonMatchesGame
                  "competition season mismatch"
              check
                  "all fixture.CompetitionId matches parent competition"
                  fixtureCompetitionIdMatchesParent
                  "fixture has wrong CompetitionId"
              check
                  "all knockout tie club refs are valid"
                  knockoutTieClubRefsValid
                  "knockout tie references unknown club" ]


    // ══════════════════════════════════════════════════════════════════════
    //  8. Player and club data contracts
    // ══════════════════════════════════════════════════════════════════════

    let playerAndClubDataContracts () =
        let game = loadGame ()
        let allPlayers = game.Players |> Map.toList |> List.map snd

        let skillsInRange =
            allPlayers
            |> List.forall (fun p ->
                p.CurrentSkill >= 1
                && p.CurrentSkill <= 200
                && p.PotentialSkill >= p.CurrentSkill
                && p.PotentialSkill <= 200)

        let ageReasonable =
            allPlayers
            |> List.forall (fun p ->
                let age = (game.CurrentDate - p.Birthday).TotalDays / 365.25
                age >= 15.0 && age <= 45.0)

        let fitnessInRange =
            allPlayers
            |> List.forall (fun p -> p.MatchFitness >= 0 && p.MatchFitness <= 100)

        let conditionInRange =
            allPlayers |> List.forall (fun p -> p.Condition >= 0 && p.Condition <= 100)

        let moraleInRange =
            allPlayers |> List.forall (fun p -> p.Morale >= 0 && p.Morale <= 100)

        let valuePositive = allPlayers |> List.forall (fun p -> p.Value >= 0m)
        let salaryPositive = allPlayers |> List.forall (fun p -> p.Salary >= 0m)

        let technicalStatsInRange =
            allPlayers
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
                |> List.forall (fun v -> v >= 1 && v <= 20))

        let physicalStatsInRange =
            allPlayers
            |> List.forall (fun p ->
                let ph = p.Physical

                [ ph.Pace
                  ph.Stamina
                  ph.Strength
                  ph.Agility
                  ph.Acceleration
                  ph.Balance
                  ph.JumpingReach ]
                |> List.forall (fun v -> v >= 1 && v <= 20))

        let mentalStatsInRange =
            allPlayers
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
                |> List.forall (fun v -> v >= 1 && v <= 20))

        let gkStatsInRange =
            allPlayers
            |> List.forall (fun p ->
                let g = p.Goalkeeping

                [ g.Reflexes; g.Handling; g.Kicking; g.OneOnOne; g.AerialReach ]
                |> List.forall (fun v -> v >= 1 && v <= 20))

        let heightReasonable =
            allPlayers |> List.forall (fun p -> p.Height >= 150 && p.Height <= 220)

        let weightReasonable =
            allPlayers |> List.forall (fun p -> p.Weight >= 50 && p.Weight <= 120)

        let reputationInRange =
            allPlayers |> List.forall (fun p -> p.Reputation >= 0 && p.Reputation <= 1000)

        let eachClubHasExactlyOneGk =
            game.Clubs
            |> Map.forall (fun _ c -> c.Players |> List.filter (fun p -> p.Position = GK) |> List.length >= 1)

        let clubBudgetNonNegative = game.Clubs |> Map.forall (fun _ c -> c.Budget >= 0m)
        let clubHasPlayers = game.Clubs |> Map.forall (fun _ c -> c.Players.Length > 0)

        let clubMoraleInRange =
            game.Clubs |> Map.forall (fun _ c -> c.Morale >= 0 && c.Morale <= 100)

        let clubReputationInRange = game.Clubs |> Map.forall (fun _ c -> c.Reputation >= 0)

        let playerNamesNonEmpty =
            allPlayers |> List.forall (fun p -> p.Name.Trim().Length > 0)

        let clubNamesNonEmpty =
            game.Clubs |> Map.forall (fun _ c -> c.Name.Trim().Length > 0)

        let playerIdsUnique =
            let ids = allPlayers |> List.map _.Id
            ids.Length = (ids |> List.distinct).Length

        let playerClubIdMatchesRoster =
            allPlayers
            |> List.forall (fun p ->
                game.Clubs
                |> Map.tryFind p.ClubId
                |> Option.map (fun c -> c.Players |> List.exists (fun cp -> cp.Id = p.Id))
                |> Option.defaultValue false)

        runSuite
            "Player & Club Data Contracts"
            [ check "all CurrentSkill in [1,200] and PA >= CA" skillsInRange "player CA/PA out of range"
              check "all player ages in [15, 45]" ageReasonable "player age unreasonable"
              check "all MatchFitness in [0, 100]" fitnessInRange "player fitness out of range"
              check "all Condition in [0, 100]" conditionInRange "player condition out of range"
              check "all Morale in [0, 100]" moraleInRange "player morale out of range"
              check "all Value >= 0" valuePositive "player has negative value"
              check "all Salary >= 0" salaryPositive "player has negative salary"
              check "all technical stats in [1, 20]" technicalStatsInRange "technical stat out of range"
              check "all physical stats in [1, 20]" physicalStatsInRange "physical stat out of range"
              check "all mental stats in [1, 20]" mentalStatsInRange "mental stat out of range"
              check "all GK stats in [1, 20]" gkStatsInRange "GK stat out of range"
              check "all player heights in [150, 220]" heightReasonable "player height unreasonable"
              check "all player weights in [50, 120]" weightReasonable "player weight unreasonable"
              check "all player reputations in [0, 1000]" reputationInRange "player reputation out of range"
              check "every club has at least 1 GK" eachClubHasExactlyOneGk "club has no goalkeeper"
              check "all club budgets >= 0" clubBudgetNonNegative "club has negative budget"
              check "all clubs have at least 1 player" clubHasPlayers "club has no players"
              check "all club morale in [0, 100]" clubMoraleInRange "club morale out of range"
              check "all club reputations >= 0" clubReputationInRange "club has negative reputation"
              check "all player names are non-empty" playerNamesNonEmpty "player has empty name"
              check "all club names are non-empty" clubNamesNonEmpty "club has empty name"
              check "all player IDs are unique" playerIdsUnique "duplicate player ID detected"
              check
                  "all player.ClubId back-references roster"
                  playerClubIdMatchesRoster
                  "player.ClubId does not match roster" ]


    // ══════════════════════════════════════════════════════════════════════
    //  9. Season progress contracts
    // ══════════════════════════════════════════════════════════════════════

    let seasonProgressContracts () =
        let game = loadGame ()

        let totalFixtures =
            game.Competitions
            |> Map.toList
            |> List.sumBy (fun (_, comp) -> comp.Fixtures.Count)

        let playedFixtures =
            game.Competitions
            |> Map.toList
            |> List.sumBy (fun (_, comp) -> comp.Fixtures |> Map.filter (fun _ f -> f.Played) |> Map.count)

        let totalPoints =
            game.Competitions
            |> Map.toList
            |> List.sumBy (fun (_, comp) -> comp.Standings |> Map.toList |> List.sumBy (snd >> _.Points))

        let expectedMaxPoints = playedFixtures * 3

        let topTeamPointsReasonable =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Standings
                |> Map.forall (fun _ s ->
                    let maxPossible = s.Played * 3
                    s.Points <= maxPossible))

        let scheduledDatesNotInPast =
            game.Competitions
            |> Map.forall (fun _ comp ->
                comp.Fixtures
                |> Map.forall (fun _ f -> f.Played || f.ScheduledDate >= game.CurrentDate.AddDays(-1.0)))

        let isSeasonOverResult = isSeasonOver game

        let allFixturesPlayedWhenSeasonOver =
            not isSeasonOverResult
            || (game.Competitions
                |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.Played)))

        let standingsClubCountMatchesCompetition =
            game.Competitions
            |> Map.forall (fun _ comp -> comp.Standings.Count = 0 || comp.Standings.Count = comp.ClubIds.Length)

        runSuite
            "Season Progress Contracts"
            [ check "game has at least 1 fixture" (totalFixtures > 0) "no fixtures found"
              check
                  "played <= total fixtures"
                  (playedFixtures <= totalFixtures)
                  $"played {playedFixtures} > total {totalFixtures}"
              check
                  "total points distributed <= 3 * played fixtures"
                  (totalPoints <= expectedMaxPoints)
                  $"total points {totalPoints} > max possible {expectedMaxPoints}"
              check "no team has more points than 3 * played" topTeamPointsReasonable "team has impossible points total"
              check
                  "unplayed fixtures are scheduled in the future"
                  scheduledDatesNotInPast
                  "unplayed fixture scheduled in the past"
              check
                  "isSeasonOver is consistent with fixture state"
                  allFixturesPlayedWhenSeasonOver
                  "isSeasonOver=true but unplayed fixtures remain"
              check
                  "standings club count matches competition ClubIds"
                  standingsClubCountMatchesCompetition
                  "standings count mismatch" ]


    // ══════════════════════════════════════════════════════════════════════
    //  10. MatchStateOps unit contracts (pure logic, no DB needed)
    // ══════════════════════════════════════════════════════════════════════

    let matchStateOpsContracts () =
        let flipHome = flipPossession Home
        let flipAway = flipPossession Away

        let phaseLeft = phaseFromBallZone 15.0
        let phaseMid = phaseFromBallZone 50.0
        let phaseAtt = phaseFromBallZone 45.0
        let phaseRight = phaseFromBallZone 85.0

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

        let pmWinning = pressureMultiplier true (minimalState 3 0)
        let pmLosing = pressureMultiplier true (minimalState 0 3)

        runSuite
            "MatchStateOps Unit Contracts"
            [ check "flipPossession Home → Away" (flipHome = Away) $"got {flipHome}"
              check "flipPossession Away → Home" (flipAway = Home) $"got {flipAway}"
              check
                  "flipPossession is involution"
                  (flipPossession (flipPossession Home) = Home)
                  "flip(flip(Home)) ≠ Home"

              check "zone 15 → BuildUp" (phaseLeft = BuildUp) $"got {phaseLeft}"
              check "zone 50 → Midfield" (phaseMid = Midfield) $"got {phaseMid}"
              check "zone 45 → Midfield (in [40,60] band)" (phaseAtt = Midfield) $"got {phaseAtt}"
              check "zone 85 → BuildUp (opponent half)" (phaseRight = BuildUp) $"got {phaseRight}"

              check
                  "pressureMultiplier losing team > 1.0 (they press desperately)"
                  (pmLosing > 1.0)
                  $"losing multiplier = {pmLosing}"

              check
                  "pressureMultiplier winning team < 1.0 (they sit back)"
                  (pmWinning < 1.0)
                  $"winning multiplier = {pmWinning}"

              check
                  "pressureMultiplier losing team > winning team"
                  (pmLosing > pmWinning)
                  $"losing={pmLosing} winning={pmWinning}"

              check
                  "pressureMultiplier values in reasonable range [0.1, 2.0]"
                  (pmWinning >= 0.1 && pmWinning <= 2.0 && pmLosing >= 0.1 && pmLosing <= 2.0)
                  $"out of range: winning={pmWinning} losing={pmLosing}" ]


    // ══════════════════════════════════════════════════════════════════════
    //  11. Standing arithmetic contracts (via simulateFixtures)
    //  updateStanding is private — we verify its correctness through the
    //  public batch API using a controlled 1-fixture scenario.
    // ══════════════════════════════════════════════════════════════════════

    let standingUpdateContracts () =
        let game = loadGame ()

        let unplayed =
            game.Competitions
            |> Map.toList
            |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
            |> List.filter (fun (_, f) -> not f.Played)
            |> List.truncate 1

        if unplayed.IsEmpty then
            runSuite "Standing Update Contracts" [ Fail("fixture available", "No unplayed fixture to use") ]
        else
            let fixtureId, fixture = unplayed[0]

            let comp =
                game.Competitions
                |> Map.toList
                |> List.tryFind (fun (_, c) -> Map.containsKey fixtureId c.Fixtures)
                |> Option.map snd

            match comp with
            | None ->
                runSuite
                    "Standing Update Contracts"
                    [ Fail("competition found", "Could not find competition for fixture") ]
            | Some comp ->

                let result = simulateFixtures game unplayed

                let homeStandingAfter =
                    result.GameState.Competitions
                    |> Map.toList
                    |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.HomeClubId)

                let awayStandingAfter =
                    result.GameState.Competitions
                    |> Map.toList
                    |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.AwayClubId)

                let homeBefore =
                    comp.Standings
                    |> Map.tryFind fixture.HomeClubId
                    |> Option.defaultValue
                        { ClubId = fixture.HomeClubId
                          Played = 0
                          Won = 0
                          Drawn = 0
                          Lost = 0
                          GoalsFor = 0
                          GoalsAgainst = 0
                          Points = 0 }

                let awayBefore =
                    comp.Standings
                    |> Map.tryFind fixture.AwayClubId
                    |> Option.defaultValue
                        { ClubId = fixture.AwayClubId
                          Played = 0
                          Won = 0
                          Drawn = 0
                          Lost = 0
                          GoalsFor = 0
                          GoalsAgainst = 0
                          Points = 0 }

                let playedIncremented =
                    match homeStandingAfter, awayStandingAfter with
                    | Some h, Some a -> h.Played = homeBefore.Played + 1 && a.Played = awayBefore.Played + 1
                    | _ -> false

                let pointsArithmetic =
                    match homeStandingAfter, awayStandingAfter with
                    | Some h, Some a ->
                        let hPoints = h.Points - homeBefore.Points
                        let aPoints = a.Points - awayBefore.Points
                        let totalNew = hPoints + aPoints
                        totalNew >= 1 && totalNew <= 3
                    | _ -> false

                let goalsRecorded =
                    match homeStandingAfter, awayStandingAfter with
                    | Some h, Some a -> h.GoalsFor > homeBefore.GoalsFor || a.GoalsFor > awayBefore.GoalsFor
                    | _ -> false

                let goalsSymmetric =
                    match homeStandingAfter, awayStandingAfter with
                    | Some h, Some a ->
                        let hGoalsAdded = h.GoalsFor - homeBefore.GoalsFor
                        let aGoalsAdded = a.GoalsFor - awayBefore.GoalsFor
                        let hConceded = h.GoalsAgainst - homeBefore.GoalsAgainst
                        let aConceded = a.GoalsAgainst - awayBefore.GoalsAgainst
                        hGoalsAdded = aConceded && aGoalsAdded = hConceded
                    | _ -> false

                let winnerGets3OrDrawGets1 =
                    match homeStandingAfter, awayStandingAfter with
                    | Some h, Some a ->
                        let hPts = h.Points - homeBefore.Points
                        let aPts = a.Points - awayBefore.Points
                        (hPts = 3 && aPts = 0) || (hPts = 0 && aPts = 3) || (hPts = 1 && aPts = 1)
                    | _ -> false

                runSuite
                    "Standing Update Contracts"
                    [ check
                          "Played incremented by 1 for both clubs"
                          playedIncremented
                          "Played count not incremented correctly"
                      check "points sum is 1 (draw) or 3 (win/loss)" pointsArithmetic "unexpected points delta"
                      check
                          "winner gets 3 pts or draw gives 1 each"
                          winnerGets3OrDrawGets1
                          "points distribution formula wrong"
                      check "goals are recorded in standings" goalsRecorded "GoalsFor not updated after fixture"
                      check
                          "GoalsFor/GoalsAgainst are symmetric between clubs"
                          goalsSymmetric
                          "goals asymmetry: home.GoalsFor ≠ away.GoalsAgainst" ]


    // ══════════════════════════════════════════════════════════════════════
    //  12. Lineup / formation contracts
    // ══════════════════════════════════════════════════════════════════════

    let lineupContracts () =
        let clubs = loadClubs ()

        let allFormations = ClubFormation.all

        let allFormationsProduceValidLineup =
            clubs
            |> Array.forall (fun c ->
                allFormations
                |> List.forall (fun f ->
                    let c' = ensureLineup { c with CurrentLineup = None } f

                    match c'.CurrentLineup with
                    | None -> false
                    | Some lu -> lu.Slots.Length = 11))

        let lineupHasExactly11Filled =
            clubs
            |> Array.forall (fun c ->
                match c.CurrentLineup with
                | None -> false
                | Some lu ->
                    let filled = lu.Slots |> List.filter (fun s -> s.PlayerId.IsSome) |> List.length
                    filled = 11)

        let lineupAllPlayersBelongToClub =
            clubs
            |> Array.forall (fun c ->
                match c.CurrentLineup with
                | None -> true
                | Some lu ->
                    let clubIds = c.Players |> List.map _.Id |> Set.ofList

                    lu.Slots
                    |> List.forall (fun s -> s.PlayerId |> Option.forall (fun pid -> Set.contains pid clubIds)))

        let lineupHasGk =
            clubs
            |> Array.forall (fun c ->
                match c.CurrentLineup with
                | None -> false
                | Some lu -> lu.Slots |> List.exists (fun s -> s.Role = GK && s.PlayerId.IsSome))

        let lineupSlotPositionsInBounds =
            clubs
            |> Array.forall (fun c ->
                match c.CurrentLineup with
                | None -> true
                | Some lu ->
                    lu.Slots
                    |> List.forall (fun s -> s.X >= 0.0 && s.X <= 1.0 && s.Y >= 0.0 && s.Y <= 1.0))

        let noPlayerInTwoSlots =
            clubs
            |> Array.forall (fun c ->
                match c.CurrentLineup with
                | None -> true
                | Some lu ->
                    let ids = lu.Slots |> List.choose _.PlayerId
                    ids.Length = (ids |> List.distinct).Length)

        runSuite
            "Lineup & Formation Contracts"
            [ check
                  "all formations produce 11-slot lineup"
                  allFormationsProduceValidLineup
                  "formation produced <11 slots"
              check
                  "lineup has exactly 11 filled slots"
                  lineupHasExactly11Filled
                  "lineup does not have 11 filled players"
              check
                  "all lineup players belong to the club"
                  lineupAllPlayersBelongToClub
                  "lineup references foreign player"
              check "lineup always has a GK" lineupHasGk "lineup has no goalkeeper"
              check "all lineup slot positions in [0, 1]" lineupSlotPositionsInBounds "slot position out of [0,1]"
              check "no player appears twice in a lineup" noPlayerInTwoSlots "player assigned to two slots" ]


    // ══════════════════════════════════════════════════════════════════════
    //  13. isSeasonOver edge case contracts
    // ══════════════════════════════════════════════════════════════════════

    let isSeasonOverContracts () =
        let game = loadGame ()

        let gameWithNoFixtures =
            { game with
                Competitions = game.Competitions |> Map.map (fun _ comp -> { comp with Fixtures = Map.empty }) }

        let gameAllPlayed =
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

        runSuite
            "isSeasonOver Edge Cases"
            [ check "no fixtures → season is over" (isSeasonOver gameWithNoFixtures) "expected true when no fixtures"
              check "all played → season is over" (isSeasonOver gameAllPlayed) "expected true when all played"
              check
                  "fresh game → season is NOT over"
                  (not (isSeasonOver game))
                  "expected false for fresh game with unplayed fixtures" ]


    // ══════════════════════════════════════════════════════════════════════
    //  14. Multi-match consistency contracts
    // ══════════════════════════════════════════════════════════════════════

    let multiMatchConsistencyContracts () =
        let clubs = loadClubs ()
        let home = clubs[0]
        let away = clubs[1]

        let results =
            Array.init 20 (fun _ ->
                match trySimulateMatch home away with
                | Ok(h, a, _) -> Some(h, a)
                | Error _ -> None)
            |> Array.choose id

        let allScoresNonNegative = results |> Array.forall (fun (h, a) -> h >= 0 && a >= 0)

        let notAllIdentical = results.Length < 2 || (results |> Array.distinct).Length > 1

        let noOutlierScores = results |> Array.forall (fun (h, a) -> h <= 15 && a <= 15)

        runSuite
            "Multi-Match Consistency"
            [ check "all repeated match scores non-negative" allScoresNonNegative "negative score detected"
              check
                  "matches are not deterministic (some variation expected)"
                  notAllIdentical
                  "all 20 simulations produced identical results — RNG may be broken"
              check "no match has outlier score (> 15 each)" noOutlierScores "outlier score detected in repeated runs" ]

    let fixtureIntegrityContracts () =
        let game = loadGame ()

        game.Competitions
        |> Map.toList
        |> List.choose (fun (_, comp) ->
            match comp.Type with
            | NationalLeague _ -> Some comp
            | _ -> None)
        |> List.collect (fun comp ->
            let n = comp.ClubIds.Length
            let expectedPerClub = (n - 1) * 2

            let gamesPerClub =
                comp.Fixtures
                |> Map.toList
                |> List.collect (fun (_, f) -> [ f.HomeClubId; f.AwayClubId ])
                |> List.countBy id

            let fixtureKeys =
                comp.Fixtures
                |> Map.toList
                |> List.map (fun (_, f) -> f.HomeClubId, f.AwayClubId)

            let duplicates = fixtureKeys.Length - (fixtureKeys |> List.distinct).Length

            [ check
                  $"{comp.Name}: each club plays exactly {expectedPerClub} matches"
                  (gamesPerClub |> List.forall (fun (_, count) -> count = expectedPerClub))
                  (gamesPerClub
                   |> List.filter (fun (_, c) -> c <> expectedPerClub)
                   |> List.map (fun (id, c) -> $"club {id} played {c}")
                   |> String.concat ", ")

              check $"{comp.Name}: no duplicate fixtures" (duplicates = 0) $"{duplicates} duplicate(s) detected"

              check
                  $"{comp.Name}: total fixtures = n*(n-1)"
                  (comp.Fixtures.Count = n * (n - 1))
                  $"expected {n * (n - 1)}, got {comp.Fixtures.Count}" ])
        |> runSuite "Fixture Integrity"
    // ══════════════════════════════════════════════════════════════════════
    //  Entry point
    // ══════════════════════════════════════════════════════════════════════

    let runAll () =
        printfn "\n====== Football Engine — Full Test Suite ======"

        let allFailures =
            [ yield! singleMatchContracts ()
              yield! statisticalContracts 10000
              yield! errorHandlingContracts ()
              yield! replayUIContracts ()
              yield! batchAndStandingsContracts ()
              yield! doubleSimulationGuardContracts ()
              yield! gameStateIntegrityContracts ()
              yield! playerAndClubDataContracts ()
              yield! seasonProgressContracts ()
              yield! matchStateOpsContracts ()
              yield! standingUpdateContracts ()
              yield! lineupContracts ()
              yield! isSeasonOverContracts ()
              yield! multiMatchConsistencyContracts ()
              yield! fixtureIntegrityContracts () ]

        printfn "======================================================"

        if allFailures.IsEmpty then
            printfn "All contracts passed.\n"
            0
        else
            printfn $"{allFailures.Length} contract(s) failed:"
            allFailures |> List.iter (fun (n, r) -> printfn $"  • {n}: {r}")
            1
