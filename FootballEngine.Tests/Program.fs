module FootballEngine.Tests.Program

open Expecto
open Expecto.Logging
open FootballEngine.Tests.MatchTests
open FootballEngine.Tests.EngineTests
open FootballEngine.Tests.WorldTests

[<EntryPoint>]
let main argv =
    let all =
        testList
            "FootballEngine"
            [ singleMatchTests
              statisticalTests |> testSequenced
              errorHandlingTests
              replayTests
              matchStateOpsTests
              multiMatchTests
              batchTests
              doubleSimGuardTests
              standingUpdateTests
              fixtureIntegrityTests
              gameStateIntegrityTests
              playerDataTests
              seasonProgressTests
              lineupTests
              isSeasonOverTests ]

    runTestsWithCLIArgs [ Verbosity LogLevel.Verbose ] argv all
