module FootballEngine.Benchmarks.Program

open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =

    BenchmarkSwitcher.FromAssembly(typeof<MatchSimulatorBenchmarks>.Assembly).Run(args)
    |> ignore

    0
