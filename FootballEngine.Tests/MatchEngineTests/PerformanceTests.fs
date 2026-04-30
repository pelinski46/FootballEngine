module FootballEngine.Tests.MatchEngineTests.PerformanceTests

open Expecto
open FootballEngine
open FootballEngine.Tests
open System.Diagnostics

let performanceTests =
    testList
        "Performance"
        [ test "100 matches simulate in under 2 seconds" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let sw = Stopwatch.StartNew()

              for _ in 1..20 do
                  MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
                  |> ignore

              sw.Stop()
              Expect.isLessThan sw.Elapsed.TotalSeconds 2.0 "20 matches should complete in < 2s"
          }

          test "full replay with snapshots completes in reasonable time" {
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let sw = Stopwatch.StartNew()

              for _ in 1..3 do
                  MatchSimulator.trySimulateMatchFull clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
                  |> ignore

              sw.Stop()
              Expect.isLessThan sw.Elapsed.TotalSeconds 2.0 "3 full replays should complete in < 2s"
          }

          test "command ordering handles 500+ live commands without degradation" {
              let commands =
                  Array.init 600 (fun i ->
                      { CommandId = int64 (600 - i)
                        IssuedForSubTick = 10
                        Source = User
                        Command = PauseSimulation })
              let sw = Stopwatch.StartNew()
              let all = MatchCommands.orderForTick 10 commands
              sw.Stop()
              Expect.equal all.Length 600 "should order all 600 commands"
              Expect.isLessThan sw.Elapsed.TotalMilliseconds 50.0 "ordering should be fast"
          } ]
