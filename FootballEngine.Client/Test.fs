namespace FootballEngine.Test

open System
open FootballEngine
open FootballEngine.Client.AI
open FootballEngine.Client.AI.ManagerAI
open FootballEngine.Domain

module TestMatchExtended =
    type MatchStats =
        { TotalGoals: int
          TotalEvents: int
          HomeWins: int
          AwayWins: int
          Draws: int }

    let rnd = Random()


    let private getPlayerName (c: Club) (id: int) =
        c.Players
        |> List.tryFind (fun p -> p.Id = id)
        |> Option.map _.Name
        |> Option.defaultValue "Unknown Player"

    let runDetailedMatch () =
        match Db.loadGame () with
        | None -> printfn "[TEST] Error: No hay partida guardada. Generá una nueva primero."
        | Some game ->
            let clubs = game.Clubs |> Map.toList |> List.map snd

            if clubs.Length < 2 then
                printfn "[TEST] Error: No hay suficientes clubes."
            else

                let home = clubs[rnd.Next(clubs.Length)]
                let mutable away = clubs[rnd.Next(clubs.Length)]

                while away.Id = home.Id do
                    away <- clubs[rnd.Next(clubs.Length)]

                printfn "\n=== DETAILED MATCH SIMULATION ==="
                printfn $"Match: {home.Name} (Rep: {home.Reputation}) vs {away.Name} (Rep: {away.Reputation})"
                printfn "-----------------------------------"

                let sw = System.Diagnostics.Stopwatch.StartNew()
                let hScore, aScore, events = MatchEngine.simulateMatch home away
                sw.Stop()

                let sortedEvents = events |> List.sortBy _.Second

                for e in sortedEvents do
                    let teamName = if e.ClubId = home.Id then home.Name else away.Name

                    let playerName =
                        if e.ClubId = home.Id then
                            getPlayerName home e.PlayerId
                        else
                            getPlayerName away e.PlayerId

                    let minute = e.Second / 60

                    match e.Type with
                    | Goal -> printfn $"⚽ Min {minute}: GOL de {playerName} ({teamName})"
                    | YellowCard -> printfn $"🟨 Min {minute}: Amarilla para {playerName} ({teamName})"
                    | RedCard -> printfn $"🟥 Min {minute}: ROJA para {playerName} ({teamName})"
                    | Injury desc -> printfn $"🚑 Min {minute}: Lesión ({desc}) de {playerName} ({teamName})"
                    | _ -> ()

                printfn "-----------------------------------"
                printfn $"Final Score: {home.Name} {hScore} - {aScore} {away.Name}"
                printfn $"Simulation Time: {sw.Elapsed.TotalMilliseconds:F4} ms"
                printfn "===================================\n"

    let runMassSimulationParallel () =
        let iterations = 10000

        match Db.loadGame () with
        | None -> printfn "[TEST] No game loaded."
        | Some game ->
            let clubs = game.Clubs |> Map.toArray |> Array.map snd

            if clubs.Length < 2 then
                printfn "[TEST] Not enough clubs."
            else
                printfn $"[TEST] Iniciando simulación masiva (PARALLEL) de {iterations:N0} partidos..."
                let sw = System.Diagnostics.Stopwatch.StartNew()

                let results: MatchStats[] =
                    Array.Parallel.init iterations (fun _ ->
                        // Random.Shared es thread-safe
                        let home = clubs[Random.Shared.Next(clubs.Length)]
                        let away = clubs[Random.Shared.Next(clubs.Length)]

                        let homeReady =
                            ManagerAI.ensureLineup { home with CurrentLineup = None } (pickBestFormation home)

                        let awayReady =
                            ManagerAI.ensureLineup { away with CurrentLineup = None } (pickBestFormation away)

                        let hScore, aScore, events = MatchEngine.simulateMatch homeReady awayReady

                        { TotalGoals = hScore + aScore
                          TotalEvents = events.Length
                          HomeWins = if hScore > aScore then 1 else 0
                          AwayWins = if aScore > hScore then 1 else 0
                          Draws = if hScore = aScore then 1 else 0 })

                let totals =
                    results
                    |> Array.fold
                        (fun acc r ->
                            { TotalGoals = acc.TotalGoals + r.TotalGoals
                              TotalEvents = acc.TotalEvents + r.TotalEvents
                              HomeWins = acc.HomeWins + r.HomeWins
                              AwayWins = acc.AwayWins + r.AwayWins
                              Draws = acc.Draws + r.Draws })
                        { TotalGoals = 0
                          TotalEvents = 0
                          HomeWins = 0
                          AwayWins = 0
                          Draws = 0 }

                sw.Stop()

                let avgGoals = float totals.TotalGoals / float iterations
                let avgEvents = float totals.TotalEvents / float iterations
                let msPerMatch = sw.Elapsed.TotalMilliseconds / float iterations

                printfn "\n=== RESULTADOS DE SIMULACIÓN MASIVA (PARALLEL) ==="
                printfn $"Partidos Simulados: {iterations:N0}"
                printfn $"Tiempo Total:       {sw.Elapsed.TotalSeconds:F2} segundos"
                printfn $"Velocidad:          {msPerMatch:F4} ms/partido ({(1000.0 / msPerMatch):N0} matches/sec)"
                printfn "---------------------------------------"
                printfn $"Goles Totales:      {totals.TotalGoals:N0}"
                printfn $"Promedio Goles:     {avgGoals:F2}"
                printfn $"Eventos p/Partido:  {avgEvents:F1}"
                printfn "---------------------------------------"

                printfn
                    $"Victorias Local:    {totals.HomeWins:N0} ({float totals.HomeWins / float iterations * 100.0:F1}%%)"

                printfn
                    $"Victorias Visita:   {totals.AwayWins:N0} ({float totals.AwayWins / float iterations * 100.0:F1}%%)"

                printfn $"Empates:            {totals.Draws:N0} ({float totals.Draws / float iterations * 100.0:F1}%%)"
                printfn "=======================================\n"
