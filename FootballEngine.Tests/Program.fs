module FootballEngine.Tests.Program

open Expecto
open Expecto.Logging
open FootballEngine.Tests.MatchTests
open FootballEngine.Tests.EngineTests
open FootballEngine.Tests.WorldTests
open FootballEngine.Tests.TrainingSystemTests

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

    let all =
        testList "FootballEngine" [ matchTests; engineTests; worldTests; trainingTests ]

    runTestsWithCLIArgs [ Verbosity LogLevel.Verbose ] argv all
