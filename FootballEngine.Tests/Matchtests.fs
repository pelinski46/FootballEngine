module FootballEngine.Tests.MatchTests

open System
open Expecto
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open FootballEngine.MatchState
open FootballEngine.Tests.Helpers

// ============================================================================
// Helpers
// ============================================================================

let private sim home away players staff =
    trySimulateMatch home away players staff |> getOk

let private simFull home away players staff =
    trySimulateMatchFull home away players staff |> getOk

let private countType eventType events =
    events |> List.filter (fun e -> e.Type = eventType) |> List.length

let private isPass (e: MatchEvent) =
    match e.Type with
    | PassCompleted _ -> true
    | _ -> false

let private isShot (e: MatchEvent) =
    match e.Type with
    | Goal
    | ShotBlocked
    | ShotOffTarget
    | Save -> true
    | _ -> false

let private isDribble (e: MatchEvent) =
    match e.Type with
    | DribbleSuccess
    | DribbleFail -> true
    | _ -> false

// ============================================================================
// Category 1: Single-match structural invariants
// These must ALWAYS hold regardless of engine tuning.
// ============================================================================

let singleMatchTests =
    testList
        "Single Match Invariants"
        [ test "simulateMatch returns Ok" {
              let clubs, players, staff = loadClubs ()

              Expect.isOk
                  (trySimulateMatch clubs[0] clubs[1] players staff)
                  "trySimulateMatch returned Error — check lineup/GK"
          }
          test "scores are non-negative" {
              let clubs, players, staff = loadClubs ()
              let h, a, _, _ = sim clubs[0] clubs[1] players staff
              Expect.isTrue (h >= 0 && a >= 0) $"negative score: {h}-{a}"
          }
          test "scores are plausible (each <= 10)" {
              let clubs, players, staff = loadClubs ()
              let h, a, _, _ = sim clubs[0] clubs[1] players staff
              Expect.isTrue (h <= 10 && a <= 10) $"implausible score: {h}-{a}"
          }
          test "goal events match reported score" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let hScore, aScore, events, _ = sim home away players staff
              let goals = events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = away.Id) |> List.length

              Expect.isTrue
                  (hGoals = hScore && aGoals = aScore)
                  $"goal event count mismatch for score {hScore}-{aScore}"
          }
          test "all event seconds in [1, 5700]" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              Expect.isTrue
                  (events |> List.forall (fun e -> e.Second >= 1 && e.Second <= 95 * 60))
                  "event with second outside valid range"
          }
          test "all event playerIds belong to one of the two clubs" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = sim home away players staff
              let homeIds = home.PlayerIds |> Set.ofList
              let awayIds = away.PlayerIds |> Set.ofList

              Expect.isTrue
                  (events
                   |> List.forall (fun e -> Set.contains e.PlayerId homeIds || Set.contains e.PlayerId awayIds))
                  "event references unknown player"
          }

          // Events are accumulated in reverse order internally (tickEvents @ evs pattern in Simulator.runLoop),
          // then reversed once at the end before returning. The consumer-facing list is chronological.
          test "events are ordered chronologically" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              Expect.isTrue
                  (events |> List.pairwise |> List.forall (fun (a, b) -> b.Second >= a.Second))
                  "events not sorted by second"
          }
          test "no duplicate goal events for same player at same second" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              let goalKeys =
                  events
                  |> List.filter (fun e -> e.Type = Goal)
                  |> List.map (fun e -> e.PlayerId, e.Second)

              Expect.isTrue (goalKeys.Length = (goalKeys |> List.distinct).Length) "duplicate goal event detected"
          }
          test "all event ClubIds are either home or away" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = sim home away players staff

              Expect.isTrue
                  (events |> List.forall (fun e -> e.ClubId = home.Id || e.ClubId = away.Id))
                  "event has unknown ClubId"
          }

          // Substitution balance: every SubIn must have a corresponding SubOut
          test "SubstitutionIn and SubstitutionOut are balanced" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff
              let ins = countType SubstitutionIn events
              let outs = countType SubstitutionOut events
              Expect.equal ins outs "substitution in/out count mismatch"
          }
          test "at most 3 substitutions per team" {
              let clubs, players, staff = loadClubs ()
              let home, away = clubs[0], clubs[1]
              let _, _, events, _ = sim home away players staff

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
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              let reds =
                  events |> List.filter (fun e -> e.Type = RedCard) |> List.countBy _.PlayerId

              Expect.isTrue (reds |> List.forall (fun (_, count) -> count = 1)) "player has multiple red cards"
          }
          test "second yellow triggers a red card for same player" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              let playersWithTwoYellows =
                  events
                  |> List.filter (fun e -> e.Type = YellowCard)
                  |> List.countBy _.PlayerId
                  |> List.filter (fun (_, count) -> count >= 2)
                  |> List.map fst
                  |> Set.ofList

              let redCardPlayers =
                  events
                  |> List.filter (fun e -> e.Type = RedCard)
                  |> List.map _.PlayerId
                  |> Set.ofList

              Expect.isTrue
                  (Set.isSubset playersWithTwoYellows redCardPlayers)
                  "player received 2 yellows but no red card"
          }
          test "no player receives more than 2 yellow cards" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              let yellows =
                  events |> List.filter (fun e -> e.Type = YellowCard) |> List.countBy _.PlayerId

              Expect.isTrue
                  (yellows |> List.forall (fun (_, count) -> count <= 2))
                  "player has more than 2 yellow cards"
          }
          test "no goals scored at second 0" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              Expect.isTrue
                  (events
                   |> List.filter (fun e -> e.Type = Goal)
                   |> List.forall (fun e -> e.Second > 0))
                  "goal scored at second 0"
          }

          // Every FreeKick should be preceded by a FoulCommitted (FoulCommitted count >= FreeKick count)
          test "FreeKick events never exceed FoulCommitted events" {
              let clubs, players, staff = loadClubs ()
              let _, _, events, _ = sim clubs[0] clubs[1] players staff

              let fouls = countType FoulCommitted events

              let freeKicks =
                  events
                  |> List.filter (fun e ->
                      match e.Type with
                      | FreeKick _ -> true
                      | _ -> false)
                  |> List.length

              Expect.isTrue (freeKicks <= fouls) $"FreeKick count ({freeKicks}) exceeds FoulCommitted count ({fouls})"
          } ]

// ============================================================================
// Category 2: Statistical contracts
// These test that the engine produces realistic distributions.
// Ranges that fail are ENGINE BUGS, not test bugs.
// ============================================================================

type private FixtureOutcome =
    | Success of homeGoals: int * awayGoals: int * events: MatchEvent list
    | KnownError of home: string * away: string * error: SimulationError
    | UnhandledException of home: string * away: string * exn: exn

let statisticalTests =
    let iterations = 1000

    // Compute ONCE and share across all tests in this list.
    // This avoids re-simulating 1000 matches per test case.
    let outcomes =
        let clubs, players, staff = loadClubs ()

        Array.Parallel.init iterations (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length

            try
                match trySimulateMatch clubs[hi] clubs[ai] players staff with
                | Ok(h, a, events, _) -> Success(h, a, events)
                | Error e -> KnownError(clubs[hi].Name, clubs[ai].Name, e)
            with ex ->
                UnhandledException(clubs[hi].Name, clubs[ai].Name, ex))

    let successOutcomes =
        outcomes
        |> Array.choose (function
            | Success(h, a, ev) -> Some(h, a, ev)
            | _ -> None)

    let scoresOnly = successOutcomes |> Array.map (fun (h, a, _) -> h, a)

    let pct f (arr: (int * int)[]) =
        let n = float arr.Length

        if n = 0.0 then
            0.0
        else
            arr |> Array.filter f |> Array.length |> float |> (fun x -> x / n * 100.0)

    testList
        "Statistical Contracts"
        [ // --- Structural checks ---
          testCase "all fixtures simulated successfully"
          <| fun () ->
              let failures =
                  outcomes
                  |> Array.choose (function
                      | KnownError(h, a, e) -> Some $"{h} vs {a} — SimulationError: %A{e}"
                      | UnhandledException(h, a, ex) -> Some $"{h} vs {a} — UNHANDLED {ex.GetType().Name}: {ex.Message}"
                      | Success _ -> None)

              Expect.isEmpty failures $"%d{failures.Length} fixture(s) failed"

          // --- Goal distribution ---
          testCase "avg goals per match in [2.0, 3.5]"
          <| fun () ->
              let totals = scoresOnly |> Array.map (fun (h, a) -> h + a)
              let avg = float (Array.sum totals) / float totals.Length
              Expect.isTrue (avg >= 2.0 && avg <= 3.5) $"avg goals = %.2f{avg} (expected [2.0, 3.5])"

          testCase "no match has outlier score (either side > 10)"
          <| fun () ->
              let outliers =
                  scoresOnly
                  |> Array.choose (fun (h, a) -> if h > 10 || a > 10 then Some $"{h}-{a}" else None)

              Expect.isEmpty outliers $"outlier scores detected: {outliers |> Array.truncate 5}"

          // --- Event counts (strict ranges — failures indicate engine bugs) ---

      
          // The engine generates ExecuteShot too aggressively, even from poor positions.
          testCase "avg shots per match in [15, 30]"
          <| fun () ->
              let shots =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let avg = float shots / float successOutcomes.Length

              Expect.isTrue
                  (avg >= 15.0 && avg <= 30.0)
                  $"avg shots = %.2f{avg} (expected [15, 30]) — ENGINE BUG if fails"

         
          // Dribble cascades occur when shot/pass fail and dribble becomes the fallback.
          testCase "avg dribbles per match in [30, 80]"
          <| fun () ->
              let dribbles =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isDribble |> List.length)

              let avg = float dribbles / float successOutcomes.Length

              Expect.isTrue
                  (avg >= 30.0 && avg <= 80.0)
                  $"avg dribbles = %.2f{avg} (expected [30, 80]) — ENGINE BUG if fails"


          // Cross attempts don't produce enough corners.
          testCase "avg corners per match in [6, 14]"
          <| fun () ->
              let corners = successOutcomes |> Array.sumBy (fun (_, _, ev) -> countType Corner ev)
              let avg = float corners / float successOutcomes.Length

              Expect.isTrue
                  (avg >= 6.0 && avg <= 14.0)
                  $"avg corners = %.2f{avg} (expected [6, 14]) — ENGINE BUG if fails"


          // Only count FoulCommitted (not FreeKick) to avoid double-counting.
          testCase "avg fouls per match in [15, 35]"
          <| fun () ->
              let fouls =
                  successOutcomes |> Array.sumBy (fun (_, _, ev) -> countType FoulCommitted ev)

              let avg = float fouls / float successOutcomes.Length

              Expect.isTrue
                  (avg >= 15.0 && avg <= 35.0)
                  $"avg fouls = %.2f{avg} (expected [15, 35]) — ENGINE BUG if fails"

          testCase "avg yellow cards per match in [2, 6]"
          <| fun () ->
              let yellows =
                  successOutcomes |> Array.sumBy (fun (_, _, ev) -> countType YellowCard ev)

              let avg = float yellows / float successOutcomes.Length
              Expect.isTrue (avg >= 2.0 && avg <= 6.0) $"avg yellows = %.2f{avg} (expected [2, 6])"


          // Chains die too early, reducing pass volume.
          testCase "avg passes per match in [120, 600]"
          <| fun () ->
              let passes =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isPass |> List.length)

              let avg = float passes / float successOutcomes.Length
              Expect.isTrue (avg >= 120.0 && avg <= 600.0) $"avg passes = %.2f{avg} (expected [120, 600])"

          // --- New diagnostic tests ---

          // Shot conversion: goals / total shots should be ~8-15% in real football
          testCase "shot conversion rate is realistic (5-20%)"
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

              Expect.isTrue
                  (rate >= 5.0 && rate <= 20.0)
                  (sprintf "shot conversion = %.1f%% (%d/%d) — expected [5%%, 20%%]" rate totalGoals totalShots)

          // Event distribution: passes should be the most common event type
          testCase "passes are the most common event type"
          <| fun () ->
              let passCount =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isPass |> List.length)

              let shotCount =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isShot |> List.length)

              let dribbleCount =
                  successOutcomes
                  |> Array.sumBy (fun (_, _, ev) -> ev |> List.filter isDribble |> List.length)

              Expect.isTrue
                  (passCount > shotCount && passCount > dribbleCount)
                  $"passes ({passCount}) should exceed shots ({shotCount}) and dribbles ({dribbleCount})"

          // Possession proxy: both teams should produce events (neither team is invisible)
          testCase "both teams produce match events"
          <| fun () ->
              let allHaveEvents =
                  successOutcomes |> Array.forall (fun (_, _, ev) -> ev.Length > 0)

              Expect.isTrue allHaveEvents "some matches produced no events"

          // --- Match outcome distribution ---

          // Home advantage test: simulate equal-strength matchups with home/away swapped
          // to isolate the home advantage effect from team strength differences
          testCase "home teams score more on average than away teams"
          <| fun () ->
              let clubs, players, staff = loadClubs ()

              // Pick pairs of similar-strength clubs and simulate both home/away
              let homeAwaySwapped =
                  clubs
                  |> Array.pairwise
                  |> Array.filter (fun (a, b) ->
                      let skillA = Club.averageSkill players a
                      let skillB = Club.averageSkill players b
                      abs (skillA - skillB) < 5)
                  |> Array.take (min 5 (clubs.Length / 2))
                  |> Array.collect (fun (a, b) ->
                      let ab = trySimulateMatch a b players staff
                      let ba = trySimulateMatch b a players staff
                      [| ab; ba |])
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              let homeGoals = homeAwaySwapped |> Array.sumBy fst
              let awayGoals = homeAwaySwapped |> Array.sumBy snd

              Expect.isTrue
                  (homeGoals > awayGoals)
                  $"home goals ({homeGoals}) <= away goals ({awayGoals}) — no home advantage detected in equal-strength matchups"

          // Draws should not be the single most common outcome across diverse matchups
          testCase "draws are not the most common outcome"
          <| fun () ->
              let homeWins = pct (fun (h, a) -> h > a) scoresOnly
              let awayWins = pct (fun (h, a) -> a > h) scoresOnly
              let draws = pct (fun (h, a) -> h = a) scoresOnly

              Expect.isTrue
                  (draws < homeWins || draws < awayWins)
                  (sprintf "draws %.1f%% dominate (home=%.1f%% away=%.1f%%)" draws homeWins awayWins)

          // --- Performance ---
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
// Category 3: Error handling
// ============================================================================

let errorHandlingTests =
    testList
        "Error Handling Contracts"
        [ test "no lineup → MissingLineup" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[0].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let staffNoLineup =
                  staff
                  |> Map.change
                      headCoachId
                      (Option.map (fun hc ->
                          { hc with
                              Attributes =
                                  { hc.Attributes with
                                      Coaching =
                                          { hc.Attributes.Coaching with
                                              Lineup = None } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players staffNoLineup with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup"
          }
          test "empty lineup slots → IncompleteLineup" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[0].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let emptyLineupStaff =
                  match staff |> Map.tryFind headCoachId with
                  | None -> staff
                  | Some hc ->
                      match hc.Attributes.Coaching.Lineup with
                      | None -> staff
                      | Some lu ->
                          let emptyLu =
                              { lu with
                                  Slots = lu.Slots |> List.map (fun s -> { s with PlayerId = None }) }

                          staff
                          |> Map.change
                              headCoachId
                              (Option.map (fun h ->
                                  { h with
                                      Attributes =
                                          { h.Attributes with
                                              Coaching =
                                                  { h.Attributes.Coaching with
                                                      Lineup = Some emptyLu } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players emptyLineupStaff with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup"
          }
          test "partial lineup (5 players) → IncompleteLineup" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[0].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let partialLineupStaff =
                  match staff |> Map.tryFind headCoachId with
                  | None -> staff
                  | Some hc ->
                      match hc.Attributes.Coaching.Lineup with
                      | None -> staff
                      | Some lu ->
                          let partialLu =
                              { lu with
                                  Slots =
                                      lu.Slots
                                      |> List.mapi (fun i s -> if i < 5 then s else { s with PlayerId = None }) }

                          staff
                          |> Map.change
                              headCoachId
                              (Option.map (fun h ->
                                  { h with
                                      Attributes =
                                          { h.Attributes with
                                              Coaching =
                                                  { h.Attributes.Coaching with
                                                      Lineup = Some partialLu } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players partialLineupStaff with
                   | Error(IncompleteLineup _) -> true
                   | _ -> false)
                  "expected IncompleteLineup for partial lineup"
          }
          test "MissingLineup error carries club name" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[0].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let staffNoLineup =
                  staff
                  |> Map.change
                      headCoachId
                      (Option.map (fun hc ->
                          { hc with
                              Attributes =
                                  { hc.Attributes with
                                      Coaching =
                                          { hc.Attributes.Coaching with
                                              Lineup = None } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players staffNoLineup with
                   | Error(MissingLineup name) -> name = clubs[0].Name
                   | _ -> false)
                  "MissingLineup did not carry correct club name"
          }
          test "swapped home/away still simulates" {
              let clubs, players, staff = loadClubs ()
              Expect.isOk (trySimulateMatch clubs[1] clubs[0] players staff) "swap broke the simulation"
          }
          test "same club vs itself returns SameClub error" {
              let clubs, players, staff = loadClubs ()

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[0] players staff with
                   | Error(SameClub _) -> true
                   | _ -> false)
                  "expected SameClub error when home.Id = away.Id"
          }
          test "away has no lineup → MissingLineup" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[1].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let staffNoLineup =
                  staff
                  |> Map.change
                      headCoachId
                      (Option.map (fun hc ->
                          { hc with
                              Attributes =
                                  { hc.Attributes with
                                      Coaching =
                                          { hc.Attributes.Coaching with
                                              Lineup = None } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players staffNoLineup with
                   | Error(MissingLineup _) -> true
                   | _ -> false)
                  "expected MissingLineup for away club"
          }
          test "no GK in lineup → IncompleteLineup or MissingLineup, never throws" {
              let clubs, players, staff = loadClubs ()

              let headCoachId =
                  clubs[0].StaffIds
                  |> List.find (fun sid -> staff |> Map.tryFind sid |> Option.map (fun s -> s.Role) = Some HeadCoach)

              let noGkStaff =
                  match staff |> Map.tryFind headCoachId with
                  | None -> staff
                  | Some hc ->
                      match hc.Attributes.Coaching.Lineup with
                      | None -> staff
                      | Some lu ->
                          let withoutGk =
                              lu.Slots
                              |> List.map (fun s ->
                                  match s.PlayerId with
                                  | Some pid when
                                      players |> Map.tryFind pid |> Option.exists (fun p -> p.Position = GK)
                                      ->
                                      { s with PlayerId = None }
                                  | _ -> s)

                          staff
                          |> Map.change
                              headCoachId
                              (Option.map (fun h ->
                                  { h with
                                      Attributes =
                                          { h.Attributes with
                                              Coaching =
                                                  { h.Attributes.Coaching with
                                                      Lineup = Some { lu with Slots = withoutGk } } } }))

              Expect.isTrue
                  (match trySimulateMatch clubs[0] clubs[1] players noGkStaff with
                   | Error(IncompleteLineup _)
                   | Error(MissingLineup _) -> true
                   | Error(SameClub _)
                   | Ok _ -> false)
                  "expected IncompleteLineup or MissingLineup when GK missing"
          } ]

// ============================================================================
// Category 4: Replay UI contracts
// ============================================================================

let private snapshotChecks (label: string) (s: MatchState) =
    let getXY (sp: Spatial) = (sp.X, sp.Y)

    let homeActive =
        s.HomeSide.Players
        |> Array.filter (fun p -> not (Map.containsKey p.Id s.HomeSide.Sidelined))
        |> Array.length

    let awayActive =
        s.AwaySide.Players
        |> Array.filter (fun p -> not (Map.containsKey p.Id s.AwaySide.Sidelined))
        |> Array.length

    [ test "ball in bounds" { Expect.isTrue (inBounds (getXY s.Ball.Position)) $"ball at {s.Ball.Position}" }
      test "home positions in bounds" {
          Expect.isTrue (s.HomeSide.Positions |> Array.forall (getXY >> inBounds)) "home player outside 0-100"
      }
      test "away positions in bounds" {
          Expect.isTrue (s.AwaySide.Positions |> Array.forall (getXY >> inBounds)) "away player outside 0-100"
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
          Expect.equal s.HomeSide.Conditions.Length s.HomeSide.Players.Length "home conditions/players mismatch"
      }
      test "away conditions length matches players" {
          Expect.equal s.AwaySide.Conditions.Length s.AwaySide.Players.Length "away conditions/players mismatch"
      }
      test "all home conditions in [0, 100]" {
          Expect.isTrue
              (s.HomeSide.Conditions |> Array.forall (fun c -> c >= 0 && c <= 100))
              "home condition out of range"
      }
      test "all away conditions in [0, 100]" {
          Expect.isTrue
              (s.AwaySide.Conditions |> Array.forall (fun c -> c >= 0 && c <= 100))
              "away condition out of range"
      }
      test "subs used in [0, 3]" {
          Expect.isTrue
              (s.HomeSide.SubsUsed >= 0
               && s.HomeSide.SubsUsed <= 3
               && s.AwaySide.SubsUsed >= 0
               && s.AwaySide.SubsUsed <= 3)
              $"subs home={s.HomeSide.SubsUsed} away={s.AwaySide.SubsUsed}"
      }
      test "home base positions count matches players" {
          Expect.equal s.HomeSide.BasePositions.Length s.HomeSide.Players.Length "home base positions mismatch"
      }
      test "away base positions count matches players" {
          Expect.equal s.AwaySide.BasePositions.Length s.AwaySide.Players.Length "away base positions mismatch"
      } ]
    |> testList label

let replayTests =
    testList
        "Replay UI Contracts"
        [ testCase "replay has snapshots"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              Expect.isTrue (replay.Snapshots.Length > 0) "no snapshots — viewer slider will be empty"
          testCase "snapshot seconds are non-decreasing"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.Second >= a.Second))
                  "time went backwards between snapshots"
          testCase "home score never decreases"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.HomeScore >= a.HomeScore))
                  "home score decreased between snapshots"
          testCase "away score never decreases"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.pairwise
                   |> Array.forall (fun (a, b) -> b.AwayScore >= a.AwayScore))
                  "away score decreased between snapshots"
          testCase "final score >= last snapshot score"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              let last = replay.Snapshots[replay.Snapshots.Length - 1]

              Expect.isTrue
                  (replay.Final.HomeScore >= last.HomeScore
                   && replay.Final.AwayScore >= last.AwayScore)
                  "final score lower than last snapshot"
          testCase "final second = 5700 (95 min)"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              Expect.equal replay.Final.Second (95 * 60) "final.Second != 5700"
          testCase "goal events match final score"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              let final = replay.Final
              let goals = replay.Events |> List.filter (fun e -> e.Type = Goal)
              let hGoals = goals |> List.filter (fun e -> e.ClubId = final.Home.Id) |> List.length
              let aGoals = goals |> List.filter (fun e -> e.ClubId = final.Away.Id) |> List.length

              Expect.isTrue
                  (hGoals = final.HomeScore && aGoals = final.AwayScore)
                  $"goal events don't match {final.HomeScore}-{final.AwayScore}"
          testCase "all snapshots have [11, 14] home players"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.HomeSide.Players.Length >= 11 && s.HomeSide.Players.Length <= 14))
                  "snapshot with invalid home player count"
          testCase "all snapshots have [11, 14] away players"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots
                   |> Array.forall (fun s -> s.AwaySide.Players.Length >= 11 && s.AwaySide.Players.Length <= 14))
                  "snapshot with invalid away player count"
          testCase "first snapshot second > 0"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              Expect.isTrue (replay.Snapshots.Length = 0 || replay.Snapshots[0].Second > 0) "first snapshot at second 0"
          testCase "last snapshot second <= 5700"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff

              Expect.isTrue
                  (replay.Snapshots.Length = 0
                   || replay.Snapshots[replay.Snapshots.Length - 1].Second <= 95 * 60)
                  "last snapshot beyond 95 min"
          testCase "final snapshot invariants pass"
          <| fun () ->
              let clubs, players, staff = loadClubs ()
              let replay = simFull clubs[0] clubs[1] players staff
              let result = runTestsWithCLIArgs [] [||] (snapshotChecks "final" replay.Final)
              Expect.equal result 0 "one or more final snapshot invariants failed" ]

// ============================================================================
// Category 5: MatchStateOps unit tests
// ============================================================================

let matchStateOpsTests =
    let minimalState homeScore awayScore =
        { Home = Unchecked.defaultof<_>
          Away = Unchecked.defaultof<_>
          HomeCoach = Unchecked.defaultof<_>
          AwayCoach = Unchecked.defaultof<_>
          Second = 0
          HomeScore = homeScore
          AwayScore = awayScore
          Ball = defaultBall
          Possession = Home
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
              Instructions = Some TacticalInstructions.defaultInstructions }
          AwaySide =
            { Players = Array.empty
              Conditions = Array.empty
              Positions = Array.empty
              BasePositions = Array.empty
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = Balanced
              Instructions = Some TacticalInstructions.defaultInstructions }
          PenaltyShootout = None
          IsExtraTime = false
          IsKnockoutMatch = false }

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

// ============================================================================
// Category 6: Multi-match consistency
// ============================================================================

let multiMatchTests =
    testList
        "Multi-Match Consistency"
        [ test "all repeated match scores non-negative" {
              let clubs, players, staff = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1] players staff)
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h >= 0 && a >= 0)) "negative score detected"
          }
          test "matches produce at least 5 distinct scores across 50 runs" {
              let clubs, players, staff = loadClubs ()

              let distinctScores =
                  Array.init 50 (fun _ -> trySimulateMatch clubs[0] clubs[1] players staff)
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)
                  |> Array.distinct

              Expect.isTrue
                  (distinctScores.Length >= 5)
                  $"only {distinctScores.Length} distinct scores in 50 runs — RNG may be broken"
          }
          test "no match has outlier score (> 10 each)" {
              let clubs, players, staff = loadClubs ()

              let results =
                  Array.init 20 (fun _ -> trySimulateMatch clubs[0] clubs[1] players staff)
                  |> Array.choose (function
                      | Ok(h, a, _, _) -> Some(h, a)
                      | Error _ -> None)

              Expect.isTrue (results |> Array.forall (fun (h, a) -> h <= 10 && a <= 10)) "outlier score detected"
          } ]
