module FootballEngine.Tests.Program

open Expecto
open Expecto.Logging
open FootballEngine.Tests.EngineTests
open FootballEngine.Tests.WorldTests
open FootballEngine.Tests.TrainingSystemTests
open FootballEngine.Tests.MatchEngineTests.PossessionTests
open FootballEngine.Tests.MatchEngineTests.BallPhysicsTests
open FootballEngine.Tests.MatchEngineTests.BallTrajectoryTests
open FootballEngine.Tests.MatchEngineTests.OffsideTests
open FootballEngine.Tests.MatchEngineTests.DuelActionTests
open FootballEngine.Tests.MatchEngineTests.PassActionTests
open FootballEngine.Tests.MatchEngineTests.ShotActionTests
open FootballEngine.Tests.MatchEngineTests.CrossActionTests
open FootballEngine.Tests.MatchEngineTests.SetPieceTests
open FootballEngine.Tests.MatchEngineTests.SetPieceE2ETests
open FootballEngine.Tests.MatchEngineTests.GKActionTests
open FootballEngine.Tests.MatchEngineTests.ChemistryTests
open FootballEngine.Tests.MatchEngineTests.AdvantageEngineTests
open FootballEngine.Tests.MatchEngineTests.VARTests
open FootballEngine.Tests.MatchEngineTests.HandballDetectorTests
open FootballEngine.Tests.MatchEngineTests.PlayerPersonalityTests
open FootballEngine.Tests.MatchEngineTests.AdaptiveTacticsTests
open FootballEngine.Tests.MatchEngineTests.EmergentLoopsTests
open FootballEngine.Tests.MatchEngineTests.CognitiveFrameTests
open FootballEngine.Tests.MatchEngineTests.InfluenceFrameTests
open FootballEngine.Tests.MatchEngineTests.TacticalImpactTests
open FootballEngine.Tests.MatchEngineTests.StateInvariantTests
open FootballEngine.Tests.MatchEngineTests.MatchInvariantsTests
open FootballEngine.Tests.MatchEngineTests.StatisticalContractsTests
open FootballEngine.Tests.MatchEngineTests.PossessionTransitionTests
open FootballEngine.Tests.MatchEngineTests.KnockoutMatchTests
open FootballEngine.Tests.MatchEngineTests.EdgeCaseTests
open FootballEngine.Tests.MatchEngineTests.PerformanceTests
open FootballEngine.Tests.MatchEngineTests.MatchFlowTests
open FootballEngine.Tests.MatchEngineTests.DeterminismTests

[<EntryPoint>]
let main argv =

    let matchEngineTests =
        testList
            "MatchEngine"
            [ possessionTests
              ballPhysicsTests
              ballTrajectoryTests
              offsideTests
              duelActionTests
              passActionTests
              shotActionTests
              crossActionTests
              setPieceTests
              setPieceE2ETests
              gkActionTests
              chemistryTests
              advantageEngineTests
              varTests
              handballDetectorTests
              playerPersonalityTests
              adaptiveTacticsTests
              emergentLoopsTests
              cognitiveFrameTests
              influenceFrameTests
              tacticalImpactTests
              stateInvariantTests
              matchInvariantsTests
              statisticalContractsTests
              phaseTransitionTests
              knockoutMatchTests
              edgeCaseTests
              performanceTests |> testSequenced
              matchFlowTests
              determinismTests |> testSequenced ]

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
        testList "FootballEngine" [ matchEngineTests; engineTests; worldTests; trainingTests ]

    runTestsWithCLIArgs [ Verbosity LogLevel.Verbose ] argv all
