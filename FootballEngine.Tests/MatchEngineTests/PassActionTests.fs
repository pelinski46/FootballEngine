module FootballEngine.Tests.MatchEngineTests.PassActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let passActionTests =
    testList
        "PassAction"
        [ test "pass complete -> Phase = InFlight" {
              let home = [| elitePasser 1 MC; makePlayer 2 ST 10 |]
              let away = [| makeGk 3 10 10 10 |]
              let target = home[1]
              let ctx, s =
                  buildState home [| 52.5, 34.0; 70.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let events = PassAction.resolve 0 ctx s clock target
              let hasLaunched = events |> List.exists (fun e -> match e.Type with MatchEventType.PassLaunched _ -> true | _ -> false)
              Expect.isTrue hasLaunched "pass should produce PassLaunched event"
          }

          test "pass to non-existent teammate returns []" {
              let home = [| elitePasser 1 ST |]
              let away = [| weakGk 2 |]
              let target = makePlayer 99 ST 10
              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let events = PassAction.resolve 0 ctx s clock target
              Expect.isEmpty events "pass to non-existent teammate should produce no events"
          }

          test "elite passer completion rate > 70% over 100 trials" {
              let home = [| elitePasser 1 MC; makePlayer 2 ST 20 |]
              let away = [| makeGk 3 1 1 1 |]
              let target = home[1]
              let clock = SimulationClock.defaultClock
              let mutable completed = 0
              for _ in 1..100 do
                  let ctx, s =
                      buildState home [| 52.5, 34.0; 70.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
                  let events = PassAction.resolve 0 ctx s clock target
                  let hasLaunched = events |> List.exists (fun e -> match e.Type with MatchEventType.PassLaunched _ -> true | _ -> false)
                  if hasLaunched then
                      completed <- completed + 1
              Expect.isGreaterThanOrEqual completed 70 "elite passer should complete >= 70 of 100"
          }

          test "worst passer completion rate < 30% over 100 trials" {
              let home = [| worstPasser 1 MC; makePlayer 2 ST 1 |]
              let away = [| makeGk 3 1 1 1 |]
              let target = home[1]
              let clock = SimulationClock.defaultClock
              let mutable completed = 0
              for _ in 1..100 do
                  let ctx, s =
                      buildState home [| 52.5, 34.0; 70.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
                  let events = PassAction.resolve 0 ctx s clock target
                  let hasLaunched = events |> List.exists (fun e -> match e.Type with MatchEventType.PassLaunched _ -> true | _ -> false)
                  if hasLaunched then
                      completed <- completed + 1
              Expect.isLessThanOrEqual completed 30 "worst passer should complete <= 30 of 100"
          } ]
