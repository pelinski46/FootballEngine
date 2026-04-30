module FootballEngine.Tests.MatchEngineTests.CognitiveFrameTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let cognitiveFrameTests =
    testList
        "CognitiveFrame"
        [ test "build produces valid cognitive frame" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10; makeGk 4 10 10 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0; 60.0, 34.0 |] away [| 70.0, 34.0; 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.isGreaterThan cFrame.SlotCount 0 "slot count should be positive"
              Expect.isGreaterThanOrEqual cFrame.BallX 0.0f "ball X should be >= 0"
          }

          test "nearest teammate is found" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0; 60.0, 34.0 |] away [| 70.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.isGreaterThan cFrame.NearestTeammateIdx[0] -1s "should find nearest teammate"
          }

          test "nearest opponent is found" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 DC 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 70.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.isGreaterThan cFrame.NearestOpponentIdx[0] -1s "should find nearest opponent"
          }

          test "ball zone is correct" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 20.0, 34.0 |] away [| 99.0, 34.0 |] 20.0 34.0 (Possession.Owned(HomeClub, 1))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.equal cFrame.BallZone PitchZone.DefensiveZone "ball at x=20 should be defensive zone"
          }

          test "match phase is BuildUp in defensive zone" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 20.0, 34.0 |] away [| 99.0, 34.0 |] 20.0 34.0 (Possession.Owned(HomeClub, 1))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.equal cFrame.Phase MatchPhase.BuildUp "defensive zone = BuildUp phase"
          }

          test "ball carrier opponent index is set when opponent has ball" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 DC 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 55.0, 34.0 |] 55.0 34.0 (Possession.Owned(AwayClub, 2))
              let cFrame = CognitiveFrameModule.build ctx s HomeClub
              Expect.isGreaterThanOrEqual cFrame.BallCarrierOppIdx 0s "should find ball carrier opponent"
          } ]
