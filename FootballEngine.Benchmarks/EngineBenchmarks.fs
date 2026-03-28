namespace FootballEngine.Benchmarks

open BenchmarkDotNet.Attributes
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Benchmarks.Helpers


[<MemoryDiagnoser>]
[<SimpleJob>]
type EngineBenchmarks() =

    let mutable game: GameState = Unchecked.defaultof<_>
    let mutable firstMatchDay: int = 0

    [<GlobalSetup>]
    member _.Setup() =
        let g = loadGame ()
        game <- g

        let firstDate =
            g.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq |> Seq.map (fun (_, f) -> f.ScheduledDate))
            |> Seq.min

        firstMatchDay <- int (firstDate.Date - g.CurrentDate.Date).TotalDays + 1


    [<Benchmark(Baseline = true, Description = "advance to first match day")>]
    member _.AdvanceToFirstMatchDay() =
        Engine.advanceDays firstMatchDay game |> ignore

    [<Benchmark(Description = "advance 1 day (no fixtures)")>]
    member _.AdvanceEmptyDay() = Engine.advanceDays 1 game |> ignore

    [<Benchmark(Description = "developAll players")>]
    member _.DevelopAllPlayers() =
        let rng = System.Random(42)
        PlayerDevelopment.developAll rng game |> ignore

    [<Benchmark(Description = "simulate all unplayed fixtures (parallel)")>]
    member _.SimulateAllFixtures() =
        let clubs, players, staff = loadClubs ()

        let allFixtures =
            game.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) ->
                comp.Fixtures
                |> Map.toSeq
                |> Seq.filter (fun (_, f) -> not f.Played)
                |> Seq.map (fun (_, f) -> f.HomeClubId, f.AwayClubId))
            |> Array.ofSeq

        allFixtures
        |> Array.Parallel.map (fun (homeId, awayId) ->
            match
                Map.tryFind homeId (clubs |> Array.map (fun c -> c.Id, c) |> Map.ofArray),
                Map.tryFind awayId (clubs |> Array.map (fun c -> c.Id, c) |> Map.ofArray)
            with
            | Some home, Some away -> MatchSimulator.trySimulateMatch home away players staff
            | _ -> Error(MatchSimulator.MissingLineup "unknown"))
        |> ignore
