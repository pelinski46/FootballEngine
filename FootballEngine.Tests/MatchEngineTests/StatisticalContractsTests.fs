module FootballEngine.Tests.MatchEngineTests.StatisticalContractsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests
open FootballEngine.Tests.MatchEngineTests.Helpers

let statisticalContractsTests =
    testList
        "StatisticalContracts"
        [

          let game = Helpers.loadGame ()
          let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
          let sw = System.Diagnostics.Stopwatch.StartNew()

          let results =
              Array.Parallel.init 100 (fun i ->
                  let hi = i % clubs.Length
                  let ai = (hi + 1) % clubs.Length
                  MatchSimulator.trySimulateMatch clubs[hi] clubs[ai] game.Players game.Staff game.ProfileCache)

          sw.Stop()
          let msPerGame = sw.Elapsed.TotalMilliseconds / 100.0
          let matches = results |> Array.mapi (fun i r -> i + 1, r, int msPerGame) |> Array.toList

          testCase "avg goals/match in [1.5, 4.0] over 100 trials"
          <| fun () ->
              let totalGoals =
                  matches
                  |> List.sumBy (fun (_, r, _) ->
                      match r with
                      | Ok(h, a, _, _) -> h + a
                      | _ -> 0)

              let avg = float totalGoals / 100.0

              Expect.isGreaterThanOrEqual
                  avg
                  1.5
                  $"avg goals/match = {avg:F2} over 100 trials. Expected range [1.5, 4.0]."

              Expect.isLessThanOrEqual avg 4.0 $"avg goals/match = {avg:F2} over 100 trials. Expected range [1.5, 4.0]."

          testCase "avg shots/match in [10, 40] over 100 trials"
          <| fun () ->
              let totalShots =
                  matches
                  |> List.sumBy (fun (_, r, _) ->
                      match r with
                      | Ok(_, _, events, _) ->
                          events
                          |> List.filter (fun e ->
                              match e.Type with
                              | MatchEventType.ShotOffTarget
                              | MatchEventType.ShotBlocked
                              | MatchEventType.Goal -> true
                              | _ -> false)
                          |> List.length
                      | _ -> 0)

              let avg = float totalShots / 100.0

              Expect.isGreaterThanOrEqual
                  avg
                  10.0
                  $"avg shots/match = {avg:F2} over 100 trials. Expected range [10, 40]."

              Expect.isLessThanOrEqual avg 40.0 $"avg shots/match = {avg:F2} over 100 trials. Expected range [10, 40]."

          testCase "avg pass completion rate in [55%, 95%] over 100 trials"
          <| fun () ->
              let completed, total =
                  matches
                  |> List.fold
                      (fun (c, t) (_, r, _) ->
                          match r with
                          | Ok(_, _, events, _) ->
                              let compOnly =
                                  events
                                  |> List.filter (fun e ->
                                      match e.Type with
                                      | MatchEventType.PassCompleted _ -> true
                                      | _ -> false)
                                  |> List.length

                              let all =
                                  events
                                  |> List.filter (fun e ->
                                      match e.Type with
                                      | MatchEventType.PassCompleted _
                                      | MatchEventType.PassMisplaced _
                                      | MatchEventType.PassIntercepted _
                                      | MatchEventType.PassDeflected _ -> true
                                      | _ -> false)
                                  |> List.length

                              c + compOnly, t + all
                          | _ -> c, t)
                      (0, 0)

              if total > 0 then
                  let rate = float completed / float total * 100.0

                  Expect.isGreaterThanOrEqual
                      rate
                      55.0
                      $"pass completion rate {rate:F1} percent ({completed} of {total}) over 100 trials. Expected range [55 percent, 95 percent]."

                  Expect.isLessThanOrEqual
                      rate
                      95.0
                      $"pass completion rate {rate:F1} percent ({completed} of {total}) over 100 trials. Expected range [55 percent, 95 percent]."

          testCase "avg corners/match in [3, 20] over 100 trials"
          <| fun () ->
              let totalCorners =
                  matches
                  |> List.sumBy (fun (_, r, _) ->
                      match r with
                      | Ok(_, _, events, _) -> countEventType MatchEventType.Corner events
                      | _ -> 0)

              let avg = float totalCorners / 100.0

              Expect.isGreaterThanOrEqual
                  avg
                  3.0
                  $"avg corners/match = {avg:F2} over 100 trials. Expected range [3, 20]."

              Expect.isLessThanOrEqual avg 20.0 $"avg corners/match = {avg:F2} over 100 trials. Expected range [3, 20]."

          testCase "avg fouls/match in [10, 50] over 100 trials"
          <| fun () ->
              let totalFouls =
                  matches
                  |> List.sumBy (fun (_, r, _) ->
                      match r with
                      | Ok(_, _, events, _) -> countEventType MatchEventType.FoulCommitted events
                      | _ -> 0)

              let avg = float totalFouls / 100.0

              Expect.isGreaterThanOrEqual
                  avg
                  10.0
                  $"avg fouls/match = {avg:F2} over 100 trials. Expected range [10, 50]."

              Expect.isLessThanOrEqual avg 50.0 $"avg fouls/match = {avg:F2} over 100 trials. Expected range [10, 50]."

          testCase "shot conversion rate in [3%, 25%] over 100 trials"
          <| fun () ->
              let totalGoals, totalShots =
                  matches
                  |> List.fold
                      (fun (g, s) (_, r, _) ->
                          match r with
                          | Ok(_, _, events, _) ->
                              let goals =
                                  events
                                  |> List.filter (fun e ->
                                      match e.Type with
                                      | MatchEventType.Goal
                                      | MatchEventType.OwnGoal -> true
                                      | _ -> false)
                                  |> List.length

                              let shots =
                                  events
                                  |> List.filter (fun e ->
                                      match e.Type with
                                      | MatchEventType.ShotOffTarget
                                      | MatchEventType.ShotBlocked
                                      | MatchEventType.Goal -> true
                                      | _ -> false)
                                  |> List.length

                              g + goals, s + shots
                          | _ -> g, s)
                      (0, 0)

              if totalShots > 0 then
                  let rate = float totalGoals / float totalShots * 100.0

                  Expect.isGreaterThanOrEqual
                      rate
                      3.0
                      $"shot conversion rate {rate:F1} percent ({totalGoals} goals of {totalShots} shots) over 100 trials. Expected range [3 percent, 25 percent]."

                  Expect.isLessThanOrEqual
                      rate
                      25.0
                      $"shot conversion rate {rate:F1} percent ({totalGoals} goals of {totalShots} shots) over 100 trials. Expected range [3 percent, 25 percent]."

          testCase "home win rate > away win rate over 100 trials"
          <| fun () ->
              let homeWins, awayWins =
                  matches
                  |> List.fold
                      (fun (hw, aw) (_, r, _) ->
                          match r with
                          | Ok(h, a, _, _) ->
                              if h > a then hw + 1, aw
                              elif a > h then hw, aw + 1
                              else hw, aw
                          | _ -> hw, aw)
                      (0, 0)

              Expect.isGreaterThan
                  homeWins
                  awayWins
                  $"home wins = {homeWins}, away wins = {awayWins} over 100 trials. Expected homeWins > awayWins."

          testCase "draws are not the most common outcome over 100 trials"
          <| fun () ->
              let homeWins, awayWins, draws =
                  matches
                  |> List.fold
                      (fun (hw, aw, d) (_, r, _) ->
                          match r with
                          | Ok(h, a, _, _) ->
                              if h > a then hw + 1, aw, d
                              elif a > h then hw, aw + 1, d
                              else hw, aw, d + 1
                          | _ -> hw, aw, d)
                      (0, 0, 0)

              Expect.isLessThan
                  draws
                  (max homeWins awayWins)
                  $"home wins = {homeWins}, away wins = {awayWins}, draws = {draws} over 100 trials. Expected draws < max(homeWins, awayWins)."

          testCase "avg match simulation time < 150ms over 100 trials"
          <| fun () ->
              let totalTime = matches |> List.sumBy (fun (_, _, ms) -> ms)
              let avg = float totalTime / 100.0
              Expect.isLessThan avg 150.0 $"avg match time = {avg:F0}ms over 100 trials. Expected < 150ms." ]

         