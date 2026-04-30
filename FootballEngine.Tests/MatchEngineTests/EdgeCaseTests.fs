module FootballEngine.Tests.MatchEngineTests.EdgeCaseTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.Tests
open Helpers

let edgeCaseTests =
    testList
        "EdgeCases"
        [ test "10 vs 11 (red card) does not crash" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10; makeGk 4 10 10 10 |]

              let ctx, s =
                  buildState
                      home
                      [| 52.5, 34.0; 60.0, 34.0 |]
                      away
                      [| 70.0, 34.0; 99.0, 34.0 |]
                      52.5
                      34.0
                      (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              Expect.isNotNull (box output) "engine should not crash with uneven teams"
          }

          test "ball at pitch boundaries is clamped" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]

              let ctx, s =
                  buildState home [| 0.0, 0.0 |] away [| 99.0, 34.0 |] 0.0 0.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              BallAgent.agent tick ctx s clock |> ignore
              let bx = s.Ball.Position.X
              let by = s.Ball.Position.Y
              Expect.isGreaterThanOrEqual bx 0.0<meter> "ball X >= 0"
              Expect.isLessThanOrEqual bx PhysicsContract.PitchLength "ball X <= pitch length"
              Expect.isGreaterThanOrEqual by 0.0<meter> "ball Y >= 0"
              Expect.isLessThanOrEqual by PhysicsContract.PitchWidth "ball Y <= pitch width"
          }

          test "empty team does not crash" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]

              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              Expect.isNotNull (box output) "engine should not crash"
          }

          test "stuck ball recovery works" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]

              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      StationarySinceSubTick = Some 0 }

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              Expect.isNotNull (box output) "engine should handle stuck ball"
          }

          test "GK hold time limit triggers indirect free kick" {
              let home = [| makeGk 1 10 10 10; makePlayer 2 DC 10 |]
              let away = [| makePlayer 3 ST 10 |]

              let ctx, s =
                  buildState
                      home
                      [| 5.0, 34.0; 15.0, 34.0 |]
                      away
                      [| 52.5, 34.0 |]
                      5.0
                      34.0
                      (Possession.Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      GKHoldSinceSubTick = Some 0 }

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              Expect.isNotNull (box output) "engine should handle GK hold time"
          }

          test "goal does not re-trigger on subsequent physics ticks" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]

              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))

              let shooter = home[0]

              s.Ball <-
                  { s.Ball with
                      Position =
                          { X = PhysicsContract.GoalLineHome
                            Y = 34.0<meter>
                            Z = 0.5<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                      Possession = InFlight
                      LastTouchBy = Some shooter.Id
                      Trajectory =
                          Some
                              { OriginX = 90.0<meter>
                                OriginY = 34.0<meter>
                                TargetX = 105.0<meter>
                                TargetY = 34.0<meter>
                                LaunchSubTick = 0
                                EstimatedArrivalSubTick = 10
                                KickerId = shooter.Id
                                PeakHeight = 0.0<meter>
                                ActionKind = BallActionKind.Shot(shooter.Id, 0.8) } }

              let clock = SimulationClock.defaultClock
              let mutable goalEvents = []

              for tickNum in 0..2 do
                  let tick = mkPhysicsTick tickNum
                  let output = BallAgent.agent tick ctx s clock

                  let tickGoals =
                      output.Events
                      |> List.filter (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.ShotOnTarget)

                  goalEvents <- goalEvents @ tickGoals

              let goalCount = goalEvents |> List.length
              Expect.equal goalCount 1 $"goal should trigger exactly once, got {goalCount} events: %A{goalEvents}"

              Expect.equal
                  s.Ball.Possession
                  (Possession.SetPiece(AwayClub, SetPieceKind.KickOff))
                  $"ball possession should be SetPiece(AwayClub, KickOff), got %A{s.Ball.Possession}"

              let bx = s.Ball.Position.X
              let by = s.Ball.Position.Y
              Expect.isGreaterThan bx 0.0<meter> $"ball should be moved off goal line, X = {bx}"
              Expect.isLessThan bx PhysicsContract.GoalLineHome $"ball should be moved off goal line, X = {bx}"
          } ]
