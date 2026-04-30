module FootballEngine.Tests.MatchEngineTests.CrossActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let crossActionTests =
    testList
        "CrossAction"
        [ test "successful cross produces CrossLaunched event" {
              let home = [| makePlayer 1 MR 10; makePlayer 2 ST 15 |]
              let away = [| makeGk 3 10 10 10; makePlayer 4 DC 10 |]
              let ctx, s =
                  buildState home [| 80.0, 10.0; 85.0, 34.0 |] away [| 99.0, 34.0; 85.0, 30.0 |] 80.0 10.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let result = CrossAction.resolve 0 ctx s clock
              let hasCrossLaunched =
                  result.Events
                  |> List.exists (fun e -> match e.Type with MatchEventType.CrossLaunched _ -> true | _ -> false)
              Expect.isTrue hasCrossLaunched "cross should produce CrossLaunched event"
          }

          test "cross always emits events" {
              let home = [| makePlayer 1 MR 10; makePlayer 2 ST 15 |]
              let away = [| makeGk 3 10 10 10; makePlayer 4 DC 10 |]
              let ctx, s =
                  buildState home [| 80.0, 10.0; 85.0, 34.0 |] away [| 99.0, 34.0; 85.0, 30.0 |] 80.0 10.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let result = CrossAction.resolve 0 ctx s clock
              Expect.isGreaterThan result.Events.Length 0 "cross should produce at least one event"
          }

          test "no targets fallback still produces events" {
              let home = [| makePlayer 1 MR 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 80.0, 10.0 |] away [| 99.0, 34.0 |] 80.0 10.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let result = CrossAction.resolve 0 ctx s clock
              Expect.isGreaterThan result.Events.Length 0 "cross with no targets should still produce events"
          } ]
