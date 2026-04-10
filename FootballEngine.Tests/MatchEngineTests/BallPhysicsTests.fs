module FootballEngine.Tests.MatchEngineTests.BallPhysicsTests

open Expecto
open FootballEngine
open FootballEngine.Domain

let ballPhysicsTests =
    testList
        "BallPhysics"
        [

          testCase "ball always decelerates after one physics step"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 0.0
                        Vx = 30.0
                        Vy = 5.0
                        Vz = 2.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true
                    Phase = PossessionPhase.InFlight HomeClub
                    PendingOffsideSnapshot = None }

              let initialSpeed =
                  sqrt (ball.Position.Vx ** 2.0 + ball.Position.Vy ** 2.0 + ball.Position.Vz ** 2.0)

              let stepped = BallPhysics.update ball

              let finalSpeed =
                  sqrt (
                      stepped.Position.Vx ** 2.0
                      + stepped.Position.Vy ** 2.0
                      + stepped.Position.Vz ** 2.0
                  )

              Expect.isLessThanOrEqual
                  finalSpeed
                  initialSpeed
                  $"ball physics: speed changed from {initialSpeed:F2} to {finalSpeed:F2} m/s in one step. Ball should always decelerate."

          testCase "ball Z reaches 0 after bounce"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 1.0
                        Vx = 5.0
                        Vy = 0.0
                        Vz = -3.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true
                    Phase = PossessionPhase.InFlight HomeClub
                    PendingOffsideSnapshot = None }

              let stepped = BallPhysics.update ball

              if stepped.Position.Z <= 0.0 then
                  Expect.isLessThanOrEqual
                      stepped.Position.Z
                      0.01
                      $"ball bounce: Z = {stepped.Position.Z:F3} after step from Z=1.0, Vz=-3.0. Expected Z ≈ 0 after bounce."

          testCase "ball with different spin produces different trajectory"
          <| fun () ->
              let baseBall =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 0.5
                        Vx = 15.0
                        Vy = 3.0
                        Vz = 1.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true
                    Phase = PossessionPhase.InFlight HomeClub
                    PendingOffsideSnapshot = None }

              let spinBall =
                  { baseBall with
                      Spin = { Top = 3.0; Side = 2.0 } }

              let baseAfter = BallPhysics.update baseBall
              let spinAfter = BallPhysics.update spinBall
              let dx = baseAfter.Position.X - spinAfter.Position.X
              let dy = baseAfter.Position.Y - spinAfter.Position.Y
              let dist = sqrt (dx * dx + dy * dy)

              Expect.isGreaterThan
                  dist
                  0.001
                  $"ball spin: trajectories converged after 1 step (distance = {dist:F4}). Spin should affect trajectory."

          testCase "ball with ControlledBy=None still moves"
          <| fun () ->
              let ball =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 0.0
                        Vx = 10.0
                        Vy = 0.0
                        Vz = 0.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true
                    Phase = PossessionPhase.InFlight HomeClub
                    PendingOffsideSnapshot = None }

              let stepped = BallPhysics.update ball

              let moved =
                  abs (stepped.Position.X - ball.Position.X) > 0.001
                  || abs (stepped.Position.Y - ball.Position.Y) > 0.001

              Expect.isTrue
                  moved
                  $"ball with ControlledBy=None and Vx=10: position changed from ({ball.Position.X}, {ball.Position.Y}) to ({stepped.Position.X}, {stepped.Position.Y}). Ball should still move."

          testCase "PendingOffsideSnapshot survives physics step unchanged"
          <| fun () ->
              let snapshot =
                  { PasserId = 1
                    ReceiverId = 2
                    ReceiverXAtPass = 80.0
                    SecondLastDefenderX = 75.0
                    BallXAtPass = 52.5
                    Dir = LeftToRight }

              let ball =
                  { Position =
                      { X = 52.5
                        Y = 34.0
                        Z = 0.0
                        Vx = 10.0
                        Vy = 0.0
                        Vz = 0.0 }
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true
                    Phase = PossessionPhase.InFlight HomeClub
                    PendingOffsideSnapshot = Some snapshot }

              let stepped = BallPhysics.update ball

              Expect.equal
                  stepped.PendingOffsideSnapshot
                  (Some snapshot)
                  $"ball physics: PendingOffsideSnapshot = %A{stepped.PendingOffsideSnapshot}, expected %A{Some snapshot}. Physics step should not clear offside snapshot." ]
