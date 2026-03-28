namespace FootballEngine.Benchmarks

open BenchmarkDotNet.Attributes
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Benchmarks.Helpers

[<MemoryDiagnoser>]
[<SimpleJob>]
type MatchSimulatorBenchmarks() =

    let mutable clubs: Club[] = Array.empty
    let mutable players: Map<PlayerId, Player> = Map.empty
    let mutable staff: Map<StaffId, Staff> = Map.empty

    [<GlobalSetup>]
    member _.Setup() =
        let c, p, s = loadClubs ()
        clubs <- c
        players <- p
        staff <- s

    [<Benchmark(Baseline = true, Description = "single match (fast)")>]
    member _.SingleMatch() =
        MatchSimulator.trySimulateMatch clubs[0] clubs[1] players staff |> ignore

    [<Benchmark(Description = "single match (full/replay)")>]
    member _.SingleMatchFull() =
        MatchSimulator.trySimulateMatchFull clubs[0] clubs[1] players staff |> ignore

    [<Benchmark(Description = "20 matches parallel")>]
    member _.ParallelBatch20() =
        Array.Parallel.init 20 (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length
            MatchSimulator.trySimulateMatch clubs[hi] clubs[ai] players staff)
        |> ignore

    [<Benchmark(Description = "100 matches parallel")>]
    member _.ParallelBatch100() =
        Array.Parallel.init 100 (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 1) % clubs.Length
            MatchSimulator.trySimulateMatch clubs[hi] clubs[ai] players staff)
        |> ignore

    [<Benchmark(Description = "error path (SameClub)")>]
    member _.ErrorPath() =
        MatchSimulator.trySimulateMatch clubs[0] clubs[0] players staff |> ignore

    [<Benchmark(Description = "10 matches rotating clubs")>]
    member _.RotatingClubs() =
        Array.init 10 (fun i ->
            let hi = i % clubs.Length
            let ai = (hi + 3) % clubs.Length
            MatchSimulator.trySimulateMatch clubs[hi] clubs[ai] players staff)
        |> ignore
