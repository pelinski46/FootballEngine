module FootballEngine.Tests.MatchEngineTests.MatchInvariantsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests
open FootballEngine.Tests.MatchEngineTests.Helpers

let matchInvariantsTests =
    testList
        "MatchInvariants"
        [

          let runMatch seed =
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let home, away = clubs[0], clubs[1]

              let result =
                  MatchSimulator.trySimulateMatch home away game.Players game.Staff game.ProfileCache

              seed, result

          let matches = [ 1..20 ] |> List.map runMatch

          testCase "scores are non-negative and ≤ 10"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(h, a, _, _) ->
                      Expect.isGreaterThanOrEqual h 0 $"match seed {seed}: HomeScore = {h}, expected ≥ 0"
                      Expect.isGreaterThanOrEqual a 0 $"match seed {seed}: AwayScore = {a}, expected ≥ 0"
                      Expect.isLessThanOrEqual h 10 $"match seed {seed}: HomeScore = {h}, expected ≤ 10"
                      Expect.isLessThanOrEqual a 10 $"match seed {seed}: AwayScore = {a}, expected ≤ 10"
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "goal events count matches final score"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(h, a, events, _) ->
                      let goalCount =
                          events |> List.filter (fun e -> e.Type = MatchEventType.Goal) |> List.length

                      let ownGoalCount =
                          events |> List.filter (fun e -> e.Type = MatchEventType.OwnGoal) |> List.length

                      let totalGoals = goalCount + ownGoalCount
                      let totalScore = h + a

                      Expect.equal
                          totalGoals
                          totalScore
                          $"match seed {seed}: {goalCount} Goal + {ownGoalCount} OwnGoal events = {totalGoals}, but score = {totalScore} ({h}-{a})."
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "all event SubTicks are in [0, FullTimeSubTick]"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, events, _) ->
                      for ev in events do
                          Expect.isGreaterThanOrEqual
                              ev.SubTick
                              0
                              $"match seed {seed}: event at SubTick {ev.SubTick} ({ev.Type}), expected ≥ 0"

                          // En el contexto del test, usaremos el valor que el motor usa para FullTime
                          Expect.isLessThanOrEqual
                              ev.SubTick
                              342000
                              $"match seed {seed}: event at SubTick {ev.SubTick} ({ev.Type}), expected ≤ 342000"
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "events are ordered chronologically"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, events, _) ->
                      let sorted = events |> List.sortBy _.SubTick

                      Expect.equal
                          (List.map _.SubTick events)
                          (List.map _.SubTick sorted)
                          $"match seed {seed}: events not in chronological order. Found SubTick sequence: %A{events |> List.take (min 10 events.Length) |> List.map _.SubTick}."
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "FreeKick events count ≤ FoulComitted events count"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, events, _) ->
                      let fkCount = countFreeKicks events
                      let foulCount = countEventType MatchEventType.FoulCommitted events

                      Expect.isLessThanOrEqual
                          fkCount
                          foulCount
                          $"match seed {seed}: {fkCount} FreeKick events, {foulCount} FoulComitted events. FreeKick ≤ FoulComitted required."
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "at most 3 substitutions per team"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, events, final) ->
                      Expect.isLessThanOrEqual
                          final.Home.SubsUsed
                          3
                          $"match seed {seed}: Home used {final.Home.SubsUsed} substitutions, expected ≤ 3"

                      Expect.isLessThanOrEqual
                          final.Away.SubsUsed
                          3
                          $"match seed {seed}: Away used {final.Away.SubsUsed} substitutions, expected ≤ 3"
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "no player receives more than 1 red card"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, events, _) ->
                      let redCards = events |> List.filter (fun e -> e.Type = MatchEventType.RedCard)
                      let byPlayer = redCards |> List.groupBy _.PlayerId

                      for pid, cards in byPlayer do
                          Expect.isLessThanOrEqual
                              (List.length cards)
                              1
                              $"match seed {seed}: player {pid} received {List.length cards} red cards, expected ≤ 1"
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "momentum stays in [-10, 10]"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, _, final) ->
                      Expect.isGreaterThanOrEqual
                          final.Momentum
                          -10.0
                          $"match seed {seed}: final Momentum = {final.Momentum:F2}, expected ≥ -10.0"

                      Expect.isLessThanOrEqual
                          final.Momentum
                          10.0
                          $"match seed {seed}: final Momentum = {final.Momentum:F2}, expected ≤ 10.0"
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}"

          testCase "all player conditions stay in [0, 100]"
          <| fun () ->
              for seed, result in matches do
                  match result with
                  | Ok(_, _, _, final) ->
                      for slot in final.Home.Slots do
                          match slot with
                          | PlayerSlot.Active s ->
                              Expect.isGreaterThanOrEqual
                                  s.Condition
                                  0
                                  $"match seed {seed}: Home player {s.Player.Name} condition = {s.Condition}, expected ≥ 0"

                              Expect.isLessThanOrEqual
                                  s.Condition
                                  100
                                  $"match seed {seed}: Home player {s.Player.Name} condition = {s.Condition}, expected ≤ 100"
                          | _ -> ()

                      for slot in final.Away.Slots do
                          match slot with
                          | PlayerSlot.Active s ->
                              Expect.isGreaterThanOrEqual
                                  s.Condition
                                  0
                                  $"match seed {seed}: Away player {s.Player.Name} condition = {s.Condition}, expected ≥ 0"

                              Expect.isLessThanOrEqual
                                  s.Condition
                                  100
                                  $"match seed {seed}: Away player {s.Player.Name} condition = {s.Condition}, expected ≤ 100"
                          | _ -> ()
                  | Error e -> failtestf $"match seed {seed}: simulation error: %A{e}" ]
