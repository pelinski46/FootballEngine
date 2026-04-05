module FootballEngine.Benchmarks.Program

open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =

    BenchmarkSwitcher.FromAssembly(typeof<MatchEngineE2EBenchmarks>.Assembly).Run(args)
    |> ignore

    0
