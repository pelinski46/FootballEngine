module FootballEngine.Tests.MatchEngineTests.KnockoutMatchTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests

let knockoutMatchTests =
    testList
        "KnockoutMatch"
        [ test "knockout match returns result" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              Stats.setSeed 1
              let result = MatchSimulator.trySimulateMatchKnockout clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              match result with
              | Ok(replay, wentToShootout) ->
                  Expect.isNotNull (box replay) "replay should exist"
                  Expect.isFalse wentToShootout "shootout flag should be false for normal result"
              | Error e -> Tests.failtestf $"Simulation error: %A{e}"
          }

          test "knockout match is deterministic" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              Stats.setSeed 99
              let result1 = MatchSimulator.trySimulateMatchKnockout clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              Stats.setSeed 99
              let result2 = MatchSimulator.trySimulateMatchKnockout clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              match result1, result2 with
              | Ok(r1, s1), Ok(r2, s2) ->
                  Expect.equal r1.Final.HomeScore r2.Final.HomeScore "home score should match"
                  Expect.equal r1.Final.AwayScore r2.Final.AwayScore "away score should match"
                  Expect.equal s1 s2 "shootout flag should match"
              | _ -> ()
          } ]
