module FootballEngine.Tests.MatchEngineTests.BallTrajectoryTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open Helpers

let ballTrajectoryTests =
    testList
        "BallTrajectory"
        [ test "pass trajectory has correct action kind" {
              let home = [| makePlayer 1 MC 10; makePlayer 2 ST 10 |]
              let away = [| makeGk 3 10 10 10 |]
              let target = home[1]

              let ctx, s =
                  buildState
                      home
                      [| 52.5, 34.0; 70.0, 34.0 |]
                      away
                      [| 99.0, 34.0 |]
                      52.5
                      34.0
                      (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              PassAction.resolve 0 ctx s clock target |> ignore

              match s.Ball.Trajectory with
              | Some traj ->
                  match traj.ActionKind with
                  | BallActionKind.Pass(passerId, receiverId, _) ->
                      Expect.equal passerId 1 "passer should be player 1"
                      Expect.equal receiverId 2 "receiver should be player 2"
                  | _ -> failwith "expected Pass action kind"
              | None -> ()
          }

          test "shot trajectory has correct action kind" {
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]

              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              ShotAction.resolve 0 ctx s clock |> ignore

              match s.Ball.Trajectory with
              | Some traj ->
                  match traj.ActionKind with
                  | BallActionKind.Shot _ -> ()
                  | _ -> failwith "expected Shot action kind"
              | None -> ()
          }

          test "cross trajectory has correct action kind" {
              let home = [| makePlayer 1 MR 10; makePlayer 2 ST 15 |]
              let away = [| makeGk 3 10 10 10; makePlayer 4 DC 10 |]

              let ctx, s =
                  buildState
                      home
                      [| 80.0, 10.0; 85.0, 34.0 |]
                      away
                      [| 99.0, 34.0; 85.0, 30.0 |]
                      80.0
                      10.0
                      (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              CrossAction.resolve 0 ctx s clock |> ignore

              match s.Ball.Trajectory with
              | Some traj ->
                  match traj.ActionKind with
                  | BallActionKind.Cross _ -> ()
                  | _ -> failwith "expected Cross action kind"
              | None -> ()
          }

          test "trajectory has valid estimated arrival" {
              let home = [| makePlayer 1 MC 10; makePlayer 2 ST 10 |]
              let away = [| makeGk 3 10 10 10 |]
              let target = home[1]

              let ctx, s =
                  buildState
                      home
                      [| 52.5, 34.0; 70.0, 34.0 |]
                      away
                      [| 99.0, 34.0 |]
                      52.5
                      34.0
                      (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock
              PassAction.resolve 0 ctx s clock target |> ignore

              match s.Ball.Trajectory with
              | Some traj ->
                  Expect.isGreaterThanOrEqual
                      traj.EstimatedArrivalSubTick
                      traj.LaunchSubTick
                      "arrival should be >= launch"

                  Expect.isGreaterThan traj.PeakHeight 0.0<meter> "peak height should be > 0 for a pass"
              | None -> ()
          } ]
