module FootballEngine.Tests.MatchEngineTests.GKActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let gkActionTests =
    testList
        "GKAction"
        [ test "GK with ball produces GKDistribution event" {
              let home = [| makeGk 1 10 10 10; makePlayer 2 DC 10 |]
              let away = [| makePlayer 3 ST 10 |]
              let ctx, s =
                  buildState home [| 5.0, 34.0; 15.0, 34.0 |] away [| 52.5, 34.0 |] 5.0 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let events = GKAction.resolve 0 ctx s clock
              let hasDist =
                  events |> List.exists (fun e -> match e.Type with MatchEventType.GKDistribution _ -> true | _ -> false)
              Expect.isTrue hasDist "GK should produce GKDistribution event"
          }

          test "GK without ball produces no events" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Contest HomeClub)
              let clock = SimulationClock.defaultClock
              let events = GKAction.resolve 0 ctx s clock
              Expect.isEmpty events "GK without ball should produce no events"
          }

          test "elite GK affects shot accuracy (more on-target vs off-target)" {
              let home = [| eliteAttacker 1 ST |]
              let away = [| eliteGk 2 |]
              let clock = SimulationClock.defaultClock
              let mutable offTarget = 0
              let mutable total = 0
              for _ in 1..100 do
                  let ctx, s =
                      buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
                  let events = ShotAction.resolve 0 ctx s clock
                  total <- total + 1
                  if events |> List.exists (fun e -> e.Type = MatchEventType.ShotOffTarget) then
                      offTarget <- offTarget + 1
              let onTargetRate = float (total - offTarget) / float total * 100.0
              Expect.isGreaterThanOrEqual onTargetRate 10.0 "elite GK vs elite attacker: on-target rate should be >= 10%"
          } ]
