module FootballEngine.Tests.MatchEngineTests.TacticalImpactTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests

let tacticalImpactTests =
    testList
        "TacticalImpact"
        [ test "match produces shots within expected range" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let mutable totalShots = 0
              for seed in 1..5 do
                  Stats.setSeed seed
                  let result = MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
                  match result with
                  | Ok(_, _, events, _) ->
                      let shots = events |> List.filter (fun e -> match e.Type with MatchEventType.ShotOffTarget | MatchEventType.ShotBlocked | MatchEventType.Goal -> true | _ -> false) |> List.length
                      totalShots <- totalShots + shots
                  | Error _ -> ()
              let avgShots = float totalShots / 5.0
              Expect.isGreaterThanOrEqual avgShots 5.0 $"avg shots/match = {avgShots:F1}, expected >= 5"
              Expect.isLessThanOrEqual avgShots 40.0 $"avg shots/match = {avgShots:F1}, expected <= 40"
          }

          test "tactics exist on both teams after match" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let result = MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              match result with
              | Ok(_, _, _, final) ->
                  Expect.isNotNull (box final.Home.Tactics) "home tactics should exist"
                  Expect.isNotNull (box final.Away.Tactics) "away tactics should exist"
              | Error e -> Tests.failtestf $"Simulation error: %A{e}"
          }

          test "subs within limits after match" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let result = MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              match result with
              | Ok(_, _, _, final) ->
                  Expect.isLessThanOrEqual final.Home.SubsUsed 3 "home subs <= 3"
                  Expect.isLessThanOrEqual final.Away.SubsUsed 3 "away subs <= 3"
              | Error e -> Tests.failtestf $"Simulation error: %A{e}"
          } ]
