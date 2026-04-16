module FootballEngine.Tests.MatchEngineTests.PassActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let passActionTests =
    testList
        "PassAction"
        [

          testCase "pass complete → Phase = InFlight(AttSide)"
          <| fun () ->
              let home = [| elitePasser 1 MC; makePlayer 3 ST 10 |]
              let hpos = [| 52.5, 34.0; 60.0, 34.0 |]

              let ctx, s =
                  buildState home hpos [||] [||] 52.5 34.0 (Owned HomeClub)

              s.Ball <- { s.Ball with Possessor = Some 1 }
              let target = makePlayer 3 ST 10
              let events = PassAction.resolve 1 ctx s target

              if events |> List.exists (fun e -> match e.Type with MatchEventType.PassCompleted _ -> true | _ -> false) then
                  Expect.equal
                      s.Ball.Phase
                      (InFlight HomeClub)
                      $"pass complete: Phase = %A{s.Ball.Phase}, expected InFlight HomeClub."

          testCase "pass complete → PendingOffsideSnapshot = Some"
          <| fun () ->
              let home = [| elitePasser 1 MC; makePlayer 3 ST 10 |]
              let hpos = [| 52.5, 34.0; 60.0, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 55.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned HomeClub)

              s.Ball <- { s.Ball with Possessor = Some 1 }
              let target = makePlayer 3 ST 10
              let events = PassAction.resolve 1 ctx s target

              if events |> List.exists (fun e -> match e.Type with MatchEventType.PassCompleted _ -> true | _ -> false) then
                  Expect.isSome
                      s.Ball.PendingOffsideSnapshot
                      $"pass complete: PendingOffsideSnapshot = %A{s.Ball.PendingOffsideSnapshot}, expected Some."

          testCase "pass to non-existent teammate returns []"
          <| fun () ->
              let home = [| elitePasser 1 MC |]
              let hpos = [| 52.5, 34.0 |]

              let ctx, s =
                  buildState home hpos [||] [||] 52.5 34.0 (Owned HomeClub)

              s.Ball <- { s.Ball with Possessor = Some 1 }
              let events = PassAction.resolve 1 ctx s (makePlayer 99 ST 10)
              Expect.isEmpty events $"pass to player 99 (not in state): {events.Length} events. Expected 0."

          testCase "pass misplaced → Phase = Contest(flip AttSide)"
          <| fun () ->
              let home = [| worstPasser 1 MC; makePlayer 3 ST 1 |]
              let hpos = [| 52.5, 34.0; 80.0, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 55.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned HomeClub)

              s.Ball <- { s.Ball with Possessor = Some 1 }
              let target = makePlayer 3 ST 1
              let events = PassAction.resolve 1 ctx s target

              let isMisplaced =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.PassMisplaced _ -> true
                      | _ -> false)

              if isMisplaced then
                  Expect.equal
                      s.Ball.Phase
                      (Contest AwayClub)
                      $"pass misplaced: Phase = %A{s.Ball.Phase}, expected Contest AwayClub."

          testCase "elite passer completion rate > 70% over 100 trials"
          <| fun () ->
              let completions =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let home = [| elitePasser 1 MC; makePlayer 3 ST 10 |]
                      let hpos = [| 52.5, 34.0; 56.0, 34.0 |]
                      let away = [| makePlayer 2 DC 10 |]
                      let apos = [| 70.0, 34.0 |]

                      let ctx, s =
                          buildState home hpos away apos 52.5 34.0 (Owned HomeClub)

                      s.Ball <- { s.Ball with Possessor = Some 1 }
                      let events = PassAction.resolve (1000 + i) ctx s (makePlayer 3 ST 10)

                      if events |> List.exists (fun e -> match e.Type with MatchEventType.PassCompleted _ -> true | _ -> false) then
                          1
                      else
                          0)

              Expect.isGreaterThan
                  completions
                  70
                  $"elitePasser to nearby teammate: {completions} completions / 100 trials. Expected ≥ 70."

          testCase "worst passer completion rate < 30% over 100 trials"
          <| fun () ->
              let completions =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let home = [| worstPasser 1 MC; makePlayer 3 ST 10 |]
                      let hpos = [| 52.5, 34.0; 65.0, 34.0 |]
                      let away = [| makePlayer 2 DC 10 |]
                      let apos = [| 55.0, 34.0 |]

                      let ctx, s =
                          buildState home hpos away apos 52.5 34.0 (Owned HomeClub)

                      s.Ball <- { s.Ball with Possessor = Some 1 }
                      let events = PassAction.resolve (1000 + i) ctx s (makePlayer 3 ST 10)

                      if events |> List.exists (fun e -> match e.Type with MatchEventType.PassCompleted _ -> true | _ -> false) then
                          1
                      else
                          0)

              Expect.isLessThan
                  completions
                  30
                  $"worstPasser to distant teammate: {completions} completions / 100 trials. Expected ≤ 30." ]
