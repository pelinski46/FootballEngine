namespace FootballEngine.Benchmarks

open BenchmarkDotNet.Attributes
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open FootballEngine.Benchmarks.Helpers

[<MemoryDiagnoser>]
[<SimpleJob>]
type PitchBenchmarks() =

    let mutable matchState: MatchState option = None

    [<GlobalSetup>]
    member _.Setup() =
        let clubs, players, staff = loadClubs ()

        match trySimulateMatchFull clubs[0] clubs[1] players staff with
        | Error e -> failwith $"Setup failed: %A{e}"
        | Ok replay ->
            // Usamos el snapshot de la mitad del partido — estado más representativo
            let mid = replay.Snapshots.Length / 2
            matchState <- Some replay.Snapshots[mid]

    member private _.State = matchState.Value

    [<Benchmark(Baseline = true, Description = "buildView")>]
    member this.BuildView() = Pitch.buildView this.State |> ignore

    [<Benchmark(Description = "updatePositions")>]
    member this.UpdatePositions() =
        Pitch.updatePositions this.State |> ignore

    [<Benchmark(Description = "activeIndices (home, no sidelined)")>]
    member this.ActiveIndicesNoSidelined() =
        let ts = MatchState.homeSide this.State
        MatchState.activeIndices ts.Players ts.Sidelined |> ignore

    [<Benchmark(Description = "pickDuel")>]
    member this.PickDuel() = Pitch.pickDuel this.State |> ignore

    [<Benchmark(Description = "effectiveStat x10 (attack+defense calc)")>]
    member this.EffectiveStatBatch() =
        let s = this.State
        let p = s.HomeSide.Players[0]
        let cond = s.HomeSide.Conditions[0]

        for _ in 1..10 do
            MatchStats.effectiveStat p.Technical.Passing cond p.Morale 2.0 |> ignore
            MatchStats.effectiveStat p.Mental.Vision cond p.Morale 1.5 |> ignore
            MatchStats.effectiveStat p.Mental.Composure cond p.Morale 1.0 |> ignore
            MatchStats.effectiveStat p.Technical.BallControl cond p.Morale 2.0 |> ignore
            MatchStats.effectiveStat p.Technical.Dribbling cond p.Morale 1.5 |> ignore
            MatchStats.effectiveStat p.Technical.Tackling cond p.Morale 2.0 |> ignore
            MatchStats.effectiveStat p.Mental.Positioning cond p.Morale 2.0 |> ignore
            MatchStats.effectiveStat p.Physical.Strength cond p.Morale 1.0 |> ignore
            MatchStats.effectiveStat p.Mental.Concentration cond p.Morale 1.0 |> ignore
            MatchStats.effectiveStat p.Technical.Finishing cond p.Morale 3.5 |> ignore
