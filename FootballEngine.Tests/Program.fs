module FootballEngine.Tests.Program

open Expecto
open Expecto.Logging
open FootballEngine.Tests.EngineTests
open FootballEngine.Tests.MatchEngineTests.StateInvariantTests
open FootballEngine.Tests.WorldTests
open FootballEngine.Tests.TrainingSystemTests
open FootballEngine.Tests.DeterminismTests
open FootballEngine.Tests.TickSchedulerTests
open FootballEngine.Tests.MatchEngineTests.PossessionTests
open FootballEngine.Tests.MatchEngineTests.BallPhysicsTests
open FootballEngine.Tests.MatchEngineTests.OffsideTests
open FootballEngine.Tests.MatchEngineTests.DuelActionTests
open FootballEngine.Tests.MatchEngineTests.PassActionTests
open FootballEngine.Tests.MatchEngineTests.ShotActionTests
open FootballEngine.Tests.MatchEngineTests.CrossActionTests
open FootballEngine.Tests.MatchEngineTests.SetPieceTests
open FootballEngine.Tests.MatchEngineTests.MatchInvariantsTests
open FootballEngine.Tests.MatchEngineTests.StatisticalContractsTests


[<EntryPoint>]
let main argv =

    let stateinvariant = testList "stateinvariant" [ stateInvariantTests ]

    let matchEngineTests =
        testList
            "MatchEngine"
            [ possessionTests
              ballPhysicsTests
              offsideTests
              duelActionTests
              passActionTests
              shotActionTests
              crossActionTests
              setPieceTests
              matchInvariantsTests
              statisticalContractsTests ]

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
        testList
            "FootballEngine"
            [ matchEngineTests
              engineTests
              worldTests
              trainingTests
              determinismTests
              schedulerTests
              stateinvariant ]

    runTestsWithCLIArgs [ Verbosity LogLevel.Verbose ] argv all
