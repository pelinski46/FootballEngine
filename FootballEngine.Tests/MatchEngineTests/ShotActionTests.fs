module FootballEngine.Tests.MatchEngineTests.ShotActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let shotActionTests =
    testList
        "ShotAction"
        [ test "shot always produces ShotLaunched event and ball in InFlight" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| weakGk 2 |]
            let ctx, s =
                buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
            let clock = SimulationClock.defaultClock
            let events = ShotAction.resolve 0 ctx s clock
            let hasLaunched = events |> List.exists (fun e -> match e.Type with MatchEventType.ShotLaunched -> true | _ -> false)
            Expect.isTrue hasLaunched "shot should produce ShotLaunched event"
            Expect.equal s.Ball.Possession InFlight "ball should be InFlight after shot"
          }

          test "shot from own half produces ShotOffTarget" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| weakGk 2 |]
            let ctx, s =
                buildState home [| 5.0, 34.0 |] away [| 99.0, 34.0 |] 5.0 34.0 (Possession.Owned(HomeClub, 1))
            let clock = SimulationClock.defaultClock
            let events = ShotAction.resolve 0 ctx s clock
            let isOffTarget = events |> List.exists (fun e -> e.Type = MatchEventType.ShotOffTarget)
            Expect.isTrue isOffTarget "shot from own half should be off target"
          }

          test "elite attacker produces ShotLaunched in attacking zone" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| weakGk 2 |]
            let mutable launched = 0
            let clock = SimulationClock.defaultClock
            for _ in 1..50 do
                let ctx, s =
                    buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
                let events = ShotAction.resolve 0 ctx s clock
                if events |> List.exists (fun e -> match e.Type with MatchEventType.ShotLaunched -> true | _ -> false) then
                    launched <- launched + 1
            Expect.isGreaterThanOrEqual launched 40 $"elite attacker should produce ShotLaunched >= 40/50, got {launched}"
          }

          test "weak attacker vs elite GK produces fewer shots on target" {
            let home = [| worstAttacker 1 ST |]
            let away = [| eliteGk 2 |]
            let mutable onTarget = 0
            let clock = SimulationClock.defaultClock
            for _ in 1..50 do
                let ctx, s =
                    buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
                let events = ShotAction.resolve 0 ctx s clock
                let isNotOffTarget = events |> List.forall (fun e -> e.Type <> MatchEventType.ShotOffTarget)
                if isNotOffTarget then onTarget <- onTarget + 1
            Expect.isLessThanOrEqual onTarget 20 $"weak attacker should have shots on target <= 20/50, got {onTarget}"
          } ]
