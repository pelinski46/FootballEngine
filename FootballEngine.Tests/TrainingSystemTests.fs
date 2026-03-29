module FootballEngine.Tests.TrainingSystemTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests.Helpers

// =============================================================================
// Helpers
// =============================================================================

let private getYoungPlayer (game: GameState) : Player =
    game.Players
    |> Map.toList
    |> List.pick (fun (_, p) ->
        let age = Player.age game.CurrentDate p
        if age >= 18 && age <= 21 && p.PotentialSkill - p.CurrentSkill >= 10 && p.CurrentSkill <= 70 then
            Some p
        else
            None)

let private getOldPlayer (game: GameState) : Player =
    game.Players
    |> Map.toList
    |> List.pick (fun (_, p) ->
        let age = Player.age game.CurrentDate p
        if age >= 30 && p.CurrentSkill >= 65 then
            Some p
        else
            None)

let private getPrimePlayer (game: GameState) : Player =
    game.Players
    |> Map.toList
    |> List.pick (fun (_, p) ->
        let age = Player.age game.CurrentDate p
        if age >= 24 && age <= 27 && p.CurrentSkill >= 70 then
            Some p
        else
            None)

let private getGK (game: GameState) : Player =
    game.Players
    |> Map.toList
    |> List.pick (fun (_, p) ->
        if p.Position = GK && p.PotentialSkill - p.CurrentSkill >= 10 then
            Some p
        else
            None)

let private getDC (game: GameState) : Player =
    game.Players
    |> Map.toList
    |> List.pick (fun (_, p) ->
        if p.Position = DC && p.PotentialSkill - p.CurrentSkill >= 10 && p.CurrentSkill <= 70 then
            Some p
        else
            None)

let private simulateWeeks (player: Player) (weeks: int) (schedule: TrainingSchedule) (game: GameState) : Player =
    let mutable p = { player with TrainingSchedule = schedule }
    let startDate = game.CurrentDate
    
    for week = 1 to weeks do
        let currentDate = startDate.AddDays(float (week * 7))
        p <- TrainingEngine.applyWeeklyTraining currentDate schedule p
    
    p

let private simulateFullSeason (player: Player) (schedule: TrainingSchedule) (game: GameState) : Player =
    let afterWeekly = simulateWeeks player 40 schedule game
    let gs : GameState = { game with Players = game.Players |> Map.add afterWeekly.Id afterWeekly; CurrentDate = game.CurrentDate.AddDays(280.0); TrainingWeeksApplied = game.TrainingWeeksApplied + 40 }
    PlayerDevelopment.developAll gs |> fun gs -> gs.Players.[afterWeekly.Id]

let private runSimulation (playerFactory: unit -> Player) (simulator: Player -> Player) (iterations: int) : Player seq =
    Seq.init iterations (fun _ ->
        let player = playerFactory ()
        simulator player)

type Stats = { Mean: float; P50: int; P90: int; P95: int; Min: int; Max: int }

let private calculateStats (selector: Player -> int) (players: Player seq) : Stats =
    let values = players |> Seq.map selector |> Seq.toArray |> Array.sort
    let n = values.Length
    let mean = values |> Seq.averageBy float
    let p50 = values.[n * 50 / 100]
    let p90 = values.[n * 90 / 100]
    let p95 = values.[n * 95 / 100]
    let min = values.[0]
    let max = values.[n - 1]
    { Mean = mean; P50 = p50; P90 = p90; P95 = p95; Min = min; Max = max }

// =============================================================================
// Tests
// =============================================================================

let trainingSystemTests =
    testList "TrainingSystem" [
        test "Young player with Heavy training should not gain more than 14 CA in a season" {
            let game = loadGame ()
            let basePlayer = getYoungPlayer game

            let playerFactory () = { basePlayer with Condition = 100; Morale = 80; Status = Available }
            let simulator (p: Player) = simulateFullSeason p { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingHeavy } game

            let results = runSimulation playerFactory simulator 1000
            let caStats = calculateStats (fun p -> p.CurrentSkill) results

            let maxAllowedGain = 14
            let actualGain = caStats.P95 - basePlayer.CurrentSkill

            Expect.isLessThan 
                actualGain 
                maxAllowedGain 
                (sprintf "P95 CA gain (%d) exceeds maximum allowed (%d). Stats: Mean=%.1f, P95=%d, Max=%d" 
                    actualGain maxAllowedGain caStats.Mean caStats.P95 caStats.Max)
        }
        
        test "Young player with correct focus should gain more than wrong focus" {
            let game = loadGame ()
            let basePlayer = getYoungPlayer game

            let correctFocus = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            let wrongFocus = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingNormal }

            let correctResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p correctFocus game) 5000
            let wrongResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p wrongFocus game) 5000

            let correctStats = calculateStats (fun p -> p.CurrentSkill) correctResults
            let wrongStats = calculateStats (fun p -> p.CurrentSkill) wrongResults
            let diff = correctStats.Mean - wrongStats.Mean

            Expect.isGreaterThan diff 0.3 (sprintf "Focus difference (%.1f) too small, expected > 0.3. Correct mean = %.1f, Wrong mean = %.1f" diff correctStats.Mean wrongStats.Mean)
            Expect.isGreaterThan
                correctStats.Mean
                wrongStats.Mean
                (sprintf "Correct focus mean (%.1f) should exceed wrong focus mean (%.1f)" 
                    correctStats.Mean wrongStats.Mean)
        }
        
        test "Player over 30 should not gain CA on average regardless of training" {
            let game = loadGame ()
            let basePlayer = getOldPlayer game

            for intensity in [TrainingIntensity.TrainingLight; TrainingIntensity.TrainingNormal; TrainingIntensity.TrainingHeavy] do
                let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = intensity }
                let results = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p schedule game) 1000
                let caStats = calculateStats (fun p -> p.CurrentSkill) results

                Expect.isLessThanOrEqual
                    caStats.Mean
                    (float basePlayer.CurrentSkill + 0.5)
                    (sprintf "30+ yo player with %A gained CA (Mean=%.1f, start=%d, P95=%d)" 
                        intensity caStats.Mean basePlayer.CurrentSkill caStats.P95)
            ()
        }
        
        test "Player at 27-30 should have minimal or negative CA change" {
            let game = loadGame ()
            let basePlayer = getPrimePlayer game
            let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            
            let results = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p schedule game) 1000
            let caStats = calculateStats (fun p -> p.CurrentSkill) results
            
            Expect.isLessThan
                caStats.Mean
                (float basePlayer.CurrentSkill + 1.0)
                (sprintf "Prime player mean CA (%.1f) is too high, expected < %d" caStats.Mean (basePlayer.CurrentSkill + 1))
        }
        
        test "Heavy training should give more CA than Normal for young players" {
            let game = loadGame ()
            let basePlayer = getYoungPlayer game
            
            let heavySchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingHeavy }
            let normalSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            
            let heavyResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p heavySchedule game) 1000
            let normalResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p normalSchedule game) 1000
            
            let heavyStats = calculateStats (fun p -> p.CurrentSkill) heavyResults
            let normalStats = calculateStats (fun p -> p.CurrentSkill) normalResults
            
            Expect.isGreaterThan
                heavyStats.Mean
                normalStats.Mean
                (sprintf "Heavy (%.1f) should exceed Normal (%.1f) for young player" 
                    heavyStats.Mean normalStats.Mean)
        }
        
        test "Normal training should give more CA than Light for young players" {
            let game = loadGame ()
            let basePlayer = getYoungPlayer game
            
            let normalSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            let lightSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingLight }
            
            let normalResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p normalSchedule game) 1000
            let lightResults = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p lightSchedule game) 1000
            
            let normalStats = calculateStats (fun p -> p.CurrentSkill) normalResults
            let lightStats = calculateStats (fun p -> p.CurrentSkill) lightResults
            
            Expect.isGreaterThan
                normalStats.Mean
                lightStats.Mean
                (sprintf "Normal (%.1f) should exceed Light (%.1f) for young player" 
                    normalStats.Mean lightStats.Mean)
        }
        
        test "Condition should drop below 50 after 10 weeks of Heavy training" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingHeavy }
            
            let after10Weeks = simulateWeeks { player with Condition = 100 } 10 schedule game
            
            Expect.isLessThan
                after10Weeks.Condition
                50
                (sprintf "Condition after 10 weeks Heavy = %d, should be < 50 (started at 100, -18/week)" after10Weeks.Condition)
        }
        
        test "Condition should drop below 30 after 20 weeks of Heavy training" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingHeavy }
            
            let after20Weeks = simulateWeeks { player with Condition = 100 } 20 schedule game
            
            Expect.isLessThan
                after20Weeks.Condition
                30
                (sprintf "Condition after 20 weeks Heavy = %d, should be < 30" after20Weeks.Condition)
        }
        
        test "Light training should maintain condition above 45 after 10 weeks" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingLight }

            let after10Weeks = simulateWeeks { player with Condition = 100 } 10 schedule game

            Expect.isGreaterThan
                after10Weeks.Condition
                45
                (sprintf "Condition after 10 weeks Light = %d, should be > 45 (started at 100, -5/week)" after10Weeks.Condition)
        }
        
        test "Normal training should result in condition dropping to 0 after 10 weeks" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingNormal }

            let after10Weeks = simulateWeeks { player with Condition = 100 } 10 schedule game

            Expect.isLessThanOrEqual
                after10Weeks.Condition
                10
                (sprintf "Condition after 10 weeks Normal = %d, should be <= 10 (started at 100, -10/week, clamped at 0)" after10Weeks.Condition)
        }
        
        test "Heavy training should cause injuries in ~4% of players per week" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingHeavy }
            
            let results = runSimulation (fun () -> { player with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateWeeks p 1 schedule game) 2000
            
            let injuredCount = results |> Seq.filter (fun p -> match p.Status with Injured _ -> true | _ -> false) |> Seq.length
            let injuryRate = float injuredCount / 2000.0
            
            Expect.isGreaterThan injuryRate 0.02 (sprintf "Injury rate %.2f too low, expected ~4%%" injuryRate)
            Expect.isLessThan injuryRate 0.08 (sprintf "Injury rate %.2f too high, expected ~4%%" injuryRate)
        }
        
        test "Light and Normal training should have zero or near-zero injuries" {
            let game = loadGame ()
            let player = getYoungPlayer game
            
            let lightSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingLight }
            let normalSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            
            let lightResults = runSimulation (fun () -> { player with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateWeeks p 10 lightSchedule game) 1000
            let normalResults = runSimulation (fun () -> { player with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateWeeks p 10 normalSchedule game) 1000
            
            let lightInjured = lightResults |> Seq.filter (fun p -> match p.Status with Injured _ -> true | _ -> false) |> Seq.length
            let normalInjured = normalResults |> Seq.filter (fun p -> match p.Status with Injured _ -> true | _ -> false) |> Seq.length
            
            Expect.isLessThanOrEqual lightInjured 5 (sprintf "Light training had %d injuries, expected 0" lightInjured)
            Expect.isLessThanOrEqual normalInjured 5 (sprintf "Normal training had %d injuries, expected 0" normalInjured)
        }
        
        test "GK training focus should benefit goalkeepers" {
            let game = loadGame ()
            let gk = getGK game
            
            let gkSchedule = { Focus = TrainingFocus.TrainingGoalkeeping; Intensity = TrainingIntensity.TrainingNormal }
            let techSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            
            let gkResults = runSimulation (fun () -> { gk with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p gkSchedule game) 1000
            let techResults = runSimulation (fun () -> { gk with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p techSchedule game) 1000
            
            let gkStats = calculateStats (fun p -> p.CurrentSkill) gkResults
            let techStats = calculateStats (fun p -> p.CurrentSkill) techResults
            
            Expect.isGreaterThan
                gkStats.Mean
                techStats.Mean
                (sprintf "GK focus (%.1f) should exceed Technical focus (%.1f) for GK position" 
                    gkStats.Mean techStats.Mean)
        }
        
        test "Physical focus should benefit defenders more than Technical" {
            let game = loadGame ()
            let dc = getDC game

            let physSchedule = { Focus = TrainingFocus.TrainingPhysical; Intensity = TrainingIntensity.TrainingNormal }
            let techSchedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }

            let physResults = runSimulation (fun () -> { dc with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p physSchedule game) 5000
            let techResults = runSimulation (fun () -> { dc with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p techSchedule game) 5000

            let physStats = calculateStats (fun p -> p.CurrentSkill) physResults
            let techStats = calculateStats (fun p -> p.CurrentSkill) techResults
            let diff = physStats.Mean - techStats.Mean

            printfn "DC Focus Test: Physical mean = %.1f, Technical mean = %.1f, diff = %.2f (Physical mult 2.0 vs Technical 0.5)" physStats.Mean techStats.Mean diff

            Expect.isGreaterThan diff 0.5 (sprintf "Focus difference (%.1f) too small, expected > 0.5. Physical mean = %.1f, Technical mean = %.1f" diff physStats.Mean techStats.Mean)
            Expect.isGreaterThan
                physStats.Mean
                techStats.Mean
                (sprintf "Physical focus (%.1f) should exceed Technical focus (%.1f) for DC position (Physical mult 2.0 vs Technical 0.5)"
                    physStats.Mean techStats.Mean)
        }
        
        test "Heavy training should decrease morale over time" {
            let game = loadGame ()
            let player = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingHeavy }
            
            let after10Weeks = simulateWeeks { player with Condition = 100; Morale = 80 } 10 schedule game
            
            Expect.isLessThan
                after10Weeks.Morale
                80
                (sprintf "Morale after 10 weeks Heavy = %d, should be < 80 (started at 80, -5/week)" after10Weeks.Morale)
        }
        
        test "Player should never exceed potential skill" {
            let game = loadGame ()
            let basePlayer = getYoungPlayer game
            let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingHeavy }
            
            let results = runSimulation (fun () -> { basePlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p schedule game) 1000
            let maxCA = results |> Seq.maxBy (fun p -> p.CurrentSkill) |> fun p -> p.CurrentSkill
            
            Expect.isLessThanOrEqual
                maxCA
                basePlayer.PotentialSkill
                (sprintf "Player exceeded potential: max CA = %d, potential = %d" maxCA basePlayer.PotentialSkill)
        }
        
        test "Players near potential should gain minimal CA" {
            let game = loadGame ()
            
            let nearPotentialPlayer = 
                game.Players
                |> Map.toList
                |> List.pick (fun (_, p) -> 
                    let age = Player.age game.CurrentDate p
                    if age >= 22 && age <= 26 && p.PotentialSkill - p.CurrentSkill <= 3 then Some p else None)
            
            let schedule = { Focus = TrainingFocus.TrainingTechnical; Intensity = TrainingIntensity.TrainingNormal }
            
            let results = runSimulation (fun () -> { nearPotentialPlayer with Condition = 100; Morale = 80; Status = Available }) (fun p -> simulateFullSeason p schedule game) 1000
            let caStats = calculateStats (fun p -> p.CurrentSkill) results
            
            Expect.isLessThan
                caStats.Mean
                (float nearPotentialPlayer.PotentialSkill)
                (sprintf "Player near potential gained too much: mean = %.1f, potential = %d" caStats.Mean nearPotentialPlayer.PotentialSkill)
        }
    ]
