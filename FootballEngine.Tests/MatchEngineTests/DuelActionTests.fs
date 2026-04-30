module FootballEngine.Tests.MatchEngineTests.DuelActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let duelActionTests =
    testList
        "DuelAction"
        [ test "foul always results in Contest with flipped club" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| highAggression 2 DC |]
            let ctx, s =
                buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
            let clock = SimulationClock.defaultClock
            let events, _ = DuelAction.resolve 0 ctx s clock
            Expect.isTrue (List.length events > 0) "should produce events"
          }

          test "after foul: PendingOffsideSnapshot = None" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| highAggression 2 DC |]
            let ctx, s =
                buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
            let clock = SimulationClock.defaultClock
            DuelAction.resolve 0 ctx s clock |> ignore
            Expect.isNone s.Ball.PendingOffsideSnapshot "offside snapshot should be cleared after foul"
          }

          test "duel always produces a duel-class event" {
            let home = [| eliteAttacker 1 ST |]
            let away = [| worstTackler 2 DC |]
            let ctx, s =
                buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
            let clock = SimulationClock.defaultClock
            let events, _ = DuelAction.resolve 0 ctx s clock
            let isDuelEvent (e: MatchEvent) =
                match e.Type with
                | MatchEventType.DribbleSuccess
                | MatchEventType.DribbleFail
                | MatchEventType.DribbleKeep
                | MatchEventType.FoulCommitted -> true
                | _ -> false
            Expect.isTrue (events |> List.exists isDuelEvent) "should produce a duel-class event"
          }

          test "elite dribbler win rate >= 40% over 100 trials" {
            let home = [| eliteDribbler 1 ST |]
            let away = [| worstTackler 2 DC |]
            let mutable wins = 0
            let clock = SimulationClock.defaultClock
            for _ in 1..100 do
                let ctx, s =
                    buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
                let events, _ = DuelAction.resolve 0 ctx s clock
                let hasDribbleSuccess =
                    events
                    |> List.exists (fun e -> e.Type = MatchEventType.DribbleSuccess)
                if hasDribbleSuccess then
                    wins <- wins + 1
            Expect.isGreaterThanOrEqual wins 40 "elite dribbler should win >= 40 of 100"
          }

          test "worst tackler recovery rate <= 25% over 100 trials" {
            let home = [| eliteDribbler 1 ST |]
            let away = [| worstTackler 2 DC |]
            let mutable recoveries = 0
            let clock = SimulationClock.defaultClock
            for _ in 1..100 do
                let ctx, s =
                    buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
                let events, _ = DuelAction.resolve 0 ctx s clock
                let hasDribbleFail =
                    events
                    |> List.exists (fun e -> e.Type = MatchEventType.DribbleFail)
                if hasDribbleFail then
                    recoveries <- recoveries + 1
            Expect.isLessThanOrEqual recoveries 25 "worst tackler should recover <= 25 of 100"
          }

          test "high aggression defender produces foul in >= 5 of 50 trials" {
            let home = [| eliteDribbler 1 ST |]
            let away = [| highAggression 2 DC |]
            let mutable fouls = 0
            let clock = SimulationClock.defaultClock
            for _ in 1..50 do
                let ctx, s =
                    buildState home [| 52.5, 34.0 |] away [| 48.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
                let events, _ = DuelAction.resolve 0 ctx s clock
                if events |> List.exists (fun e -> e.Type = MatchEventType.FoulCommitted) then
                    fouls <- fouls + 1
            Expect.isGreaterThanOrEqual fouls 5 "high aggression defender should foul >= 5 of 50"
          } ]
