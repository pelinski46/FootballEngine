module FootballEngine.Benchmarks.Program

open System
open System.Diagnostics
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Generation

let sw = Stopwatch()

let runOneSim (ctx: MatchContext) (state: SimState) =
    sw.Restart()

    let final, _events =
        MatchSimulator.runLoopFast ctx state SimulationClock.defaultClock

    sw.Stop()
    final.HomeScore, final.AwayScore, sw.ElapsedMilliseconds

[<EntryPoint>]
let main _args =
    printfn "=== Generating world..."
    let gameState = WorldGen.generateNewGame "ARG" "Test Manager" [ "ENG" ]

    let clubs = gameState.Clubs |> Map.toArray |> Array.map snd
    printfn "=== Generated %d clubs" clubs.Length

    if clubs.Length < 2 then
        failwith "Need at least 2 clubs"

    let homeClub = clubs.[0]
    let awayClub = clubs.[1]
    printfn "=== Match: %s vs %s" homeClub.Name awayClub.Name

    printfn "=== Warm-up (1 iteration)..."

    let ctx, state, _, _ =
        MatchSimulator.setup homeClub awayClub gameState.Players gameState.Staff false gameState.ProfileCache
        |> function
            | Ok v -> v
            | Error e -> failwithf "Setup failed: %A" e

    let hs, as_, ms = runOneSim ctx state
    printfn "  Warm-up: %d-%d (%d ms)" hs as_ ms

    printfn "=== Starting infinite loop..."
    printfn "PID: %d" (System.Diagnostics.Process.GetCurrentProcess().Id)
    printfn ""

    let mutable iter = 1

    while true do
        let ctx, state, _, _ =
            MatchSimulator.setup homeClub awayClub gameState.Players gameState.Staff false gameState.ProfileCache
            |> function
                | Ok v -> v
                | Error e -> failwithf "Setup failed: %A" e

        let hs, as_, ms = runOneSim ctx state
        let allocMB = GC.GetTotalMemory(false) / 1048576L
        printfn "Iter %4d | %d-%d | %5d ms | %6d MB" iter hs as_ ms allocMB
        iter <- iter + 1

    0
