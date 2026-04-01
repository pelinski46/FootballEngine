module FootballEngine.Tests.Program

open Expecto
open Expecto.Logging
open FootballEngine.Tests.MatchTests
open FootballEngine.Tests.EngineTests
open FootballEngine.Tests.WorldTests
open FootballEngine.Tests.TrainingSystemTests
open FootballEngine.Tests.DeterminismTests
open FootballEngine.Tests.TickSchedulerTests

[<EntryPoint>]
let main argv =
    let matchTests =
        testList
            "Match"
            [ singleMatchTests
              statisticalTests |> testSequenced
              errorHandlingTests
              replayTests
              matchStateOpsTests
              multiMatchTests ]

    let engineTests =
        testList "Engine" [ batchTests; doubleSimGuardTests; standingUpdateTests; fixtureIntegrityTests ]

    let worldTests =
        testList
            "World"
            [ gameStateIntegrityTests
              playerDataTests
              seasonProgressTests
              lineupTests
              isSeasonOverTests ]

    let trainingTests = testList "Training" [ trainingSystemTests ]

    let determinismTests = testList "Determinism" [ determinismTests |> testSequenced ]

    let schedulerTests = testList "Scheduler" [ tickSchedulerTests ]

    let all =
        testList "FootballEngine" [ matchTests; engineTests; worldTests; trainingTests; determinismTests; schedulerTests ]

    runTestsWithCLIArgs [ Verbosity LogLevel.Verbose ] argv all
