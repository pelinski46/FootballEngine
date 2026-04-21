module FootballEngine.Tests.MatchEngineTests.BallPhysicsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract

let ballPhysicsTests =
    testList
        "BallPhysics"
        [

          testCase "ball always decelerates after one physics step"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 30.0<meter / second>
                        Vy = 5.0<meter / second>
                        Vz = 2.0<meter / second> }
                    Spin = Spin.zero
                    Possession = InFlight (HomeClub, 0)
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None }

              let initialSpeed =
                  let vx = float ball.Position.Vx
                  let vy = float ball.Position.Vy
                  let vz = float ball.Position.Vz
                  sqrt (vx * vx + vy * vy + vz * vz)

              let stepped = BallPhysics.update 1.0<second> ball

              let finalSpeed =
                  let vx = float stepped.Position.Vx
                  let vy = float stepped.Position.Vy
                  let vz = float stepped.Position.Vz
                  sqrt (vx * vx + vy * vy + vz * vz)

              Expect.isLessThanOrEqual
                  finalSpeed
                  initialSpeed
                  $"ball physics: speed changed from {initialSpeed:F2} to {finalSpeed:F2} m/s in one step. Ball should always decelerate."

          testCase "ball Z reaches 0 after bounce"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 1.0<meter>
                        Vx = 5.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = -3.0<meter / second> }
                    Spin = Spin.zero
                    Possession = InFlight (HomeClub, 0)
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None }

              let stepped = BallPhysics.update 1.0<second> ball

              if float stepped.Position.Z <= 0.0 then
                  Expect.isLessThanOrEqual
                      (float stepped.Position.Z)
                      0.01
                      $"ball bounce: Z = {float stepped.Position.Z:F3} after step from Z=1.0, Vz=-3.0. Expected Z ≈ 0 after bounce."

          testCase "ball with different spin produces different trajectory"
          <| fun () ->
              let baseBall =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.5<meter>
                        Vx = 15.0<meter / second>
                        Vy = 3.0<meter / second>
                        Vz = 1.0<meter / second> }
                    Spin = Spin.zero
                    Possession = InFlight (HomeClub, 0)
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None }

              let spinBall =
                  { baseBall with
                      Spin = { Top = 3.0<radianPerSecond>; Side = 2.0<radianPerSecond> } }

              let baseAfter = BallPhysics.update 1.0<second> baseBall
              let spinAfter = BallPhysics.update 1.0<second> spinBall
              let dx = float (baseAfter.Position.X - spinAfter.Position.X)
              let dy = float (baseAfter.Position.Y - spinAfter.Position.Y)
              let dist = sqrt (dx * dx + dy * dy)

              Expect.isGreaterThan
                  dist
                  0.001
                  $"ball spin: trajectories converged after 1 step (distance = {dist:F4}). Spin should affect trajectory."

          testCase "ball with ControlledBy=None still moves"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 10.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    Possession = Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None }

              let stepped = BallPhysics.update 1.0<second> ball

              let moved =
                  abs (float (stepped.Position.X - ball.Position.X)) > 0.001
                  || abs (float (stepped.Position.Y - ball.Position.Y)) > 0.001

              Expect.isTrue
                  moved
                  $"ball with ControlledBy=None and Vx=10: position changed from ({float ball.Position.X}, {float ball.Position.Y}) to ({float stepped.Position.X}, {float stepped.Position.Y}). Ball should still move."

          testCase "PendingOffsideSnapshot survives physics step unchanged"
          <| fun () ->
              let snapshot =
                  { PasserId = 1
                    ReceiverId = 2
                    ReceiverXAtPass = 80.0<meter>
                    SecondLastDefenderX = 75.0<meter>
                    BallXAtPass = 52.5<meter>
                    Dir = LeftToRight }

              let ball =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 10.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    Possession = InFlight (HomeClub, 0)
                    LastTouchBy = None
                    PendingOffsideSnapshot = Some snapshot
                    StationarySinceSubTick = None }

              let stepped = BallPhysics.update 1.0<second> ball

              Expect.equal
                  stepped.PendingOffsideSnapshot
                  (Some snapshot)
                  $"ball physics: PendingOffsideSnapshot = %A{stepped.PendingOffsideSnapshot}, expected %A{Some snapshot}. Physics step should not clear offside snapshot." ]
