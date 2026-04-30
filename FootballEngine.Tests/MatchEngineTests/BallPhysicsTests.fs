module FootballEngine.Tests.MatchEngineTests.BallPhysicsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open Helpers

let ballPhysicsTests =
    testList
        "BallPhysics"
        [ test "ball always decelerates after one physics step" {
              let config = BalanceConfig.defaultConfig.Physics
              let dt = 1.0<second>
              let initialPos =
                  { X = 52.5<meter>
                    Y = 34.0<meter>
                    Z = 0.0<meter>
                    Vx = 10.0<meter/second>
                    Vy = 0.0<meter/second>
                    Vz = 0.0<meter/second> }
              let ball: BallPhysicsState =
                  { Position = initialPos
                    Spin = Spin.zero
                    Possession = Possession.Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
              let updated = BallPhysics.update config dt ball
              let speedBefore = sqrt (initialPos.Vx * initialPos.Vx + initialPos.Vy * initialPos.Vy)
              let speedAfter = sqrt (updated.Position.Vx * updated.Position.Vx + updated.Position.Vy * updated.Position.Vy)
              Expect.isTrue (speedAfter <= speedBefore) "speed should not increase after drag"
          }

          test "ball Z reaches 0 after bounce" {
              let config = BalanceConfig.defaultConfig.Physics
              let dt = 0.025<second>
              let initialPos =
                  { X = 52.5<meter>
                    Y = 34.0<meter>
                    Z = 1.0<meter>
                    Vx = 0.0<meter/second>
                    Vy = 0.0<meter/second>
                    Vz = -3.0<meter/second> }
              let ball: BallPhysicsState =
                  { Position = initialPos
                    Spin = Spin.zero
                    Possession = Possession.Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
              let mutable current = ball
              for _ in 1..200 do
                  current <- BallPhysics.update config dt current
              Expect.isTrue (current.Position.Z >= -0.01<meter>) "ball Z should be >= 0 after bounce"
          }

          test "ball with different spin produces different trajectory" {
              let config = BalanceConfig.defaultConfig.Physics
              let dt = 0.025<second>
              let basePos =
                  { X = 52.5<meter>
                    Y = 34.0<meter>
                    Z = 0.0<meter>
                    Vx = 10.0<meter/second>
                    Vy = 0.0<meter/second>
                    Vz = 2.0<meter/second> }

              let ballNoSpin: BallPhysicsState =
                  { Position = basePos
                    Spin = Spin.zero
                    Possession = Possession.Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }

              let ballTopSpin: BallPhysicsState =
                  { ballNoSpin with
                      Spin =
                          { Top = 5.0<radianPerSecond>
                            Side = 0.0<radianPerSecond> } }

              let ballSideSpin: BallPhysicsState =
                  { ballNoSpin with
                      Spin =
                          { Top = 0.0<radianPerSecond>
                            Side = 5.0<radianPerSecond> } }

              let update50 (b: BallPhysicsState) =
                  let mutable cur = b
                  for _ in 1..50 do
                      cur <- BallPhysics.update config dt cur
                  cur

              let r1 = update50 ballNoSpin
              let r2 = update50 ballTopSpin
              let r3 = update50 ballSideSpin
              let allSame = r1.Position.X = r2.Position.X && r2.Position.X = r3.Position.X
              Expect.isFalse allSame "different spins should produce different trajectories"
          }

          test "ball with ControlledBy=None still moves" {
              let config = BalanceConfig.defaultConfig.Physics
              let dt = 1.0<second>
              let ball: BallPhysicsState =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 5.0<meter/second>
                        Vy = 3.0<meter/second>
                        Vz = 0.0<meter/second> }
                    Spin = Spin.zero
                    Possession = Possession.Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
              let updated = BallPhysics.update config dt ball
              let moved =
                  updated.Position.X <> ball.Position.X
                  || updated.Position.Y <> ball.Position.Y
              Expect.isTrue moved "ball should move unless stopped"
          }

          test "PendingOffsideSnapshot survives physics step unchanged" {
              let config = BalanceConfig.defaultConfig.Physics
              let dt = 0.025<second>
              let snap = mkSnap ()
              let ball: BallPhysicsState =
                  { Position =
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 5.0<meter/second>
                        Vy = 0.0<meter/second>
                        Vz = 0.0<meter/second> }
                    Spin = Spin.zero
                    Possession = Possession.Loose
                    LastTouchBy = None
                    PendingOffsideSnapshot = Some snap
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
              let updated = BallPhysics.update config dt ball
              Expect.equal updated.PendingOffsideSnapshot (Some snap) "offside snapshot should survive physics update"
          } ]
