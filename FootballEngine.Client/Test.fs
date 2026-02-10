namespace FootballEngine.Test

open System
open FootballEngine
open FootballEngine.Domain

module TestMatchExtended =

    let rnd = Random()


    let private getPlayerName (c: Club) (id: int) =
        c.Players
        |> List.tryFind (fun p -> p.Id = id)
        |> Option.map (fun p -> p.Name)
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
                let hScore, aScore, events = Engine.simulateMatch home away
                sw.Stop()

                let sortedEvents = events |> List.sortBy (fun e -> e.Minute)

                for e in sortedEvents do
                    let teamName = if e.ClubId = home.Id then home.Name else away.Name

                    let playerName =
                        if e.ClubId = home.Id then
                            getPlayerName home e.PlayerId
                        else
                            getPlayerName away e.PlayerId

                    match e.Type with
                    | Goal -> printfn $"⚽ Min {e.Minute}: GOL de {playerName} ({teamName})"
                    | YellowCard -> printfn $"🟨 Min {e.Minute}: Amarilla para {playerName} ({teamName})"
                    | RedCard -> printfn $"🟥 Min {e.Minute}: ROJA para {playerName} ({teamName})"
                    | Injury desc -> printfn $"🚑 Min {e.Minute}: Lesión ({desc}) de {playerName} ({teamName})"
                    | _ -> ()

                printfn "-----------------------------------"
                printfn $"Final Score: {home.Name} {hScore} - {aScore} {away.Name}"
                printfn $"Simulation Time: {sw.Elapsed.TotalMilliseconds:F4} ms"
                printfn "===================================\n"

    let runMassSimulation () =
        let iterations = 100000

        match Db.loadGame () with
        | None -> printfn "[TEST] No game loaded."
        | Some game ->
            let clubs = game.Clubs |> Map.toList |> List.map snd

            if clubs.Length < 2 then
                printfn "[TEST] Not enough clubs."
            else

                let mutable totalGoals = 0
                let mutable homeWins = 0
                let mutable awayWins = 0
                let mutable draws = 0
                let mutable totalEvents = 0

                printfn $"[TEST] Iniciando simulación masiva de {iterations:N0} partidos..."
                let sw = System.Diagnostics.Stopwatch.StartNew()

                for i in 1..iterations do
                    let home = clubs[rnd.Next(clubs.Length)]
                    let away = clubs[rnd.Next(clubs.Length)]


                    let hScore, aScore, events = Engine.simulateMatch home away

                    totalGoals <- totalGoals + hScore + aScore
                    totalEvents <- totalEvents + events.Length

                    if hScore > aScore then homeWins <- homeWins + 1
                    elif aScore > hScore then awayWins <- awayWins + 1
                    else draws <- draws + 1

                    if i % (iterations / 10) = 0 then
                        printfn $"[TEST] Progreso: {i:N0} partidos..."

                sw.Stop()

                let avgGoals = float totalGoals / float iterations
                let avgEvents = float totalEvents / float iterations
                let msPerMatch = float sw.ElapsedMilliseconds / float iterations

                printfn "\n=== RESULTADOS DE SIMULACIÓN MASIVA ==="
                printfn $"Partidos Simulados: {iterations:N0}"
                printfn $"Tiempo Total:       {sw.Elapsed.TotalSeconds:F2} segundos"
                printfn $"Velocidad:          {msPerMatch:F4} ms/partido ({(1000.0 / msPerMatch):N0} matches/sec)"
                printfn "---------------------------------------"
                printfn $"Goles Totales:      {totalGoals:N0}"
                printfn $"Promedio Goles:     {avgGoals:F2} (Ideal: 2.5 - 3.0)"
                printfn $"Eventos p/Partido:  {avgEvents:F1} (Goles + Tarjetas + Lesiones)"
                printfn "---------------------------------------"
                printfn $"Victorias Local:    {homeWins:N0} ({float homeWins / float iterations * 100.0:F1}%%)"
                printfn $"Victorias Visita:   {awayWins:N0} ({float awayWins / float iterations * 100.0:F1}%%)"
                printfn $"Empates:            {draws:N0} ({float draws / float iterations * 100.0:F1}%%)"
                printfn "=======================================\n"
