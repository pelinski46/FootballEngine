module FootballEngine.Tests.MatchEngineTests.AdvantageEngineTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract

let advantageEngineTests =
    testList
        "AdvantageEngine"
        [ test "evaluate returns PlayOn in attacking zone" {
              let decision =
                  AdvantageEngine.evaluate
                      80.0<meter>
                      34.0<meter>
                      AttackDir.LeftToRight
                      HomeClub
                      (Possession.Owned(HomeClub, 1))
                      PitchZone.AttackingZone

              match decision with
              | PlayOn reason ->
                  Expect.stringContains reason "promising attack" "PlayOn reason should mention promising attack"
              | _ -> failwith "expected PlayOn in attacking zone"
          }

          test "evaluate returns StopPlay in defensive zone" {
              let decision =
                  AdvantageEngine.evaluate
                      10.0<meter>
                      34.0<meter>
                      AttackDir.LeftToRight
                      HomeClub
                      (Possession.Owned(HomeClub, 1))
                      PitchZone.DefensiveZone

              match decision with
              | StopPlay reason -> Expect.stringContains reason "defensive" "StopPlay reason should mention defensive"
              | _ -> failwith "expected StopPlay in defensive zone"
          }

          test "evaluate returns StopPlay if fouled team has no possession" {
              let decision =
                  AdvantageEngine.evaluate
                      50.0<meter>
                      34.0<meter>
                      AttackDir.LeftToRight
                      HomeClub
                      Possession.Loose
                      PitchZone.MidfieldZone

              match decision with
              | StopPlay reason ->
                  Expect.stringContains reason "no possession" "StopPlay reason should mention no possession"
              | _ -> failwith "expected StopPlay when fouled team lacks possession"
          }

          test "shouldCallBack returns true after window expires" {
              let result =
                  AdvantageEngine.shouldCallBack 0 10000 (Possession.Owned(HomeClub, 1)) HomeClub

              Expect.isTrue result "should callback after window expires"
          }

          test "shouldCallBack returns true when possession flips" {
              let result =
                  AdvantageEngine.shouldCallBack 0 50 (Possession.Owned(AwayClub, 1)) HomeClub

              Expect.isTrue result "should callback when possession flips"
          }

          test "shouldCallBack returns false within window with same possession" {
              let result =
                  AdvantageEngine.shouldCallBack 0 100 (Possession.Owned(HomeClub, 1)) HomeClub

              Expect.isFalse result "should not callback within window with same possession"
          } ]
