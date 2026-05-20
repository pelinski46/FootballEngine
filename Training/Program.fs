namespace Training

open System
open System.IO
open System.Text.Json
open FootballEngine
open FootballEngine.Domain
open FootballEngine.ML
open FootballEngine.Simulation

module Program =

    let private saveWeights (path: string) (weights: EngineWeights) : unit =
        let opts = JsonSerializerOptions(WriteIndented = true)
        let json = JsonSerializer.Serialize(weights, opts)
        File.WriteAllText(path, json)
        printfn "Saved trained weights to %s" path

    let private makeTestPlayer (id: PlayerId) (pos: Position) (skill: int) : Player =
        let s = Math.Clamp(skill, 1, 20)
        { Id = id; Name = $"P{id}"; Birthday = System.DateTime(1995, 1, 1); Nationality = "AR"
          Position = pos; PreferredFoot = Right; Height = 180; Weight = 75
          Physical = { Acceleration = s; Pace = s; Agility = s; Balance = s; JumpingReach = s; Stamina = s; Strength = s }
          Technical = { Finishing = s; LongShots = s; Dribbling = s; BallControl = s; Passing = s; Crossing = s; Tackling = s; Marking = s; Heading = s; FreeKick = s; Penalty = s }
          Mental = { Aggression = s; Composure = s; Vision = s; Positioning = s; Bravery = s; WorkRate = s; Concentration = s; Leadership = s }
          Goalkeeping = { Reflexes = s; Handling = s; Kicking = s; OneOnOne = s; AerialReach = s }
          Condition = 100; MatchFitness = 100; Morale = 50; Status = Available
          CurrentSkill = skill; PotentialSkill = skill; Reputation = 50
          Affiliation = Contracted(1, { Salary = 1000m; ExpiryYear = 2030 })
          TrainingSchedule = { Focus = TrainingAllRound; Intensity = TrainingLight }
          ExperienceModifiers = ExperienceModifiers.defaultModifiers }

    let private makeTestClub (id: ClubId) (name: string) (playerIds: PlayerId list) : Club =
        { Id = id; Name = name; Nationality = "AR"; Reputation = 1500
          PlayerIds = playerIds; StaffIds = []; Budget = 10_000_000m; Morale = 50
          BoardObjective = LeagueObjective MidTable
          CoordinationMemory = CoordinationMemory.defaultMemory }

    let private makeTestStaff (id: StaffId) (clubId: ClubId) : Staff =
        { Id = id; Name = $"Coach{id}"; Birthday = System.DateTime(1970, 1, 1); Nationality = "AR"
          Role = HeadCoach
          Attributes = {
              Coaching = {
                  Attacking = 15; Defending = 14; Fitness = 12; Goalkeeping = 10
                  Mental = 14; SetPieces = 13; Tactical = 15; Technical = 14
                  WorkingWithYoungsters = 10; PreferredFormation = None; Lineup = None
              }
              Scouting = { NetworkReach = 10; DataAnalysis = 10; MarketKnowledge = 10 }
              Medical = { Physiotherapy = 10; SportsScience = 10 }
              Analysis = { PerformanceAnalysis = 10; RecruitmentAnalysis = 10 }
          }
          Knowledge = {
              JudgingPlayerAbility = 14; JudgingPlayerPotential = 13
              JudgingStaffAbility = 12; Negotiating = 13; TacticalKnowledge = 15
          }
          Mental = { Adaptability = 14; Determination = 15; LevelOfDiscipline = 14; PeopleManagement = 14; Motivating = 15 }
          CurrentSkill = 15; PotentialSkill = 15
          Badge = ProLicense
          Reputation = 1500
          Contract = Some { ClubId = clubId; Salary = 50000m; ExpiryYear = 2030 }
          Status = Active
          TrophiesWon = 0; SeasonsManaged = 5 }

    let private createTestData () =
        let homePlayers =
            [| makeTestPlayer 1 GK 15
               makeTestPlayer 2 DL 12; makeTestPlayer 3 DC 13; makeTestPlayer 4 DC 13; makeTestPlayer 5 DR 12
               makeTestPlayer 6 DM 12; makeTestPlayer 7 MC 13; makeTestPlayer 8 MC 13
               makeTestPlayer 9 AML 14; makeTestPlayer 10 AMC 14; makeTestPlayer 11 ST 15 |]
        let awayPlayers =
            [| makeTestPlayer 12 GK 15
               makeTestPlayer 13 DL 12; makeTestPlayer 14 DC 13; makeTestPlayer 15 DC 13; makeTestPlayer 16 DR 12
               makeTestPlayer 17 DM 12; makeTestPlayer 18 MC 13; makeTestPlayer 19 MC 13
               makeTestPlayer 20 AML 14; makeTestPlayer 21 AMC 14; makeTestPlayer 22 ST 15 |]

        let homeClub = makeTestClub 100 "Home" (homePlayers |> Array.map _.Id |> Array.toList)
        let awayClub = makeTestClub 200 "Away" (awayPlayers |> Array.map _.Id |> Array.toList)

        let allPlayers = Array.append homePlayers awayPlayers
        let players = allPlayers |> Array.map (fun p -> p.Id, p) |> Map.ofArray

        let homeCoach = makeTestStaff 1000 homeClub.Id
        let awayCoach = makeTestStaff 1001 awayClub.Id
        let staff = [homeCoach; awayCoach] |> List.map (fun s -> s.Id, s) |> Map.ofList

        let profileMap =
            players
            |> Map.map (fun _ p -> Player.profile p EngineWeightDefaults.defaults.ProfileWeights)

        homeClub, awayClub, players, staff, profileMap

    let runTrainingLoop
        (weightsPath: string)
        (outputPath: string)
        (iterations: int)
        (matchesPerIteration: int)
        (validationMatches: int)
        (epsilon: float)
        (learningRate: float)
        : unit =
        printfn "=== Training Loop C ==="
        printfn "Iterations: %d" iterations
        printfn "Matches per iteration: %d" matchesPerIteration
        printfn "Validation matches: %d" validationMatches

        let weights =
            match WeightsLoader.load weightsPath with
            | Ok w ->
                printfn "Loaded weights from %s" weightsPath
                w
            | Error _ ->
                printfn "Using default weights (file not found: %s)" weightsPath
                EngineWeightDefaults.defaults

        let home, away, players, staff, profileMap = createTestData ()
        let targets: Training.CalibrationTargets = Training.CalibrationTargets.targetsDefault

        let errorFn (w: EngineWeights) : float =
            let results = SimulatorRunner.runBatch 10 home away players staff profileMap
            if List.isEmpty results then 999.0
            else
                let matchMetrics, _ = MetricsAggregator.aggregate results
                let error, _ = ErrorCalculator.computeError matchMetrics targets
                error

        let initialResults = SimulatorRunner.runBatch matchesPerIteration home away players staff profileMap
        let initialMetrics, _ = MetricsAggregator.aggregate initialResults
        let baseError, _ = ErrorCalculator.computeError initialMetrics targets
        printfn "Initial error: %.4f" baseError

        let rec loop iteration currentWeights currentError =
            if iteration >= iterations then
                printfn "\nTraining complete after %d iterations" iterations
                printfn "Final error: %.4f" currentError
                saveWeights outputPath currentWeights
            else
                printfn "\n--- Iteration %d/%d (error: %.4f) ---" (iteration + 1) iterations currentError

                let tuned =
                    GradientTuner.tune epsilon learningRate currentWeights (fun w ->
                        let results = SimulatorRunner.runBatch 5 home away players staff profileMap
                        if List.isEmpty results then 999.0
                        else
                            let m, _ = MetricsAggregator.aggregate results
                            let e, _ = ErrorCalculator.computeError m targets
                            e)

                let validationResults =
                    SimulatorRunner.runBatch validationMatches home away players staff profileMap

                if List.isEmpty validationResults then
                    printfn "  Validation: no results, keeping current weights"
                    loop (iteration + 1) currentWeights currentError
                else
                    let valMetrics, _ = MetricsAggregator.aggregate validationResults
                    let newError, deltas = ErrorCalculator.computeError valMetrics targets

                    let significant = Calibrator.getSignificant 0.1 deltas
                    for d in significant do
                        printfn "  %s: %.3f (target: %.2f-%.2f)" d.Metric d.Observed d.TargetMin d.TargetMax

                    if newError < currentError then
                        printfn "  Improved: %.4f -> %.4f" currentError newError
                        loop (iteration + 1) tuned newError
                    else
                        printfn "  No improvement (%.4f >= %.4f), reducing learning rate" currentError newError
                        let halfLR = learningRate * 0.5
                        if halfLR < 0.0001 then
                            printfn "  Learning rate too small, stopping"
                            saveWeights outputPath currentWeights
                        else
                            loop (iteration + 1) currentWeights currentError

        loop 0 weights baseError

    [<EntryPoint>]
    let main argv =
        let weightsPath =
            if argv.Length > 0 then argv.[0]
            else "weights.json"
        let outputPath =
            if argv.Length > 1 then argv.[1]
            else "weights_trained.json"

        runTrainingLoop
            weightsPath
            outputPath
            20
            20
            10
            0.01
            0.05

        0
