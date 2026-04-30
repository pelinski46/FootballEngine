module FootballEngine.Tests.MatchEngineTests.InfluenceFrameTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open Helpers

let influenceFrameTests =
    testList
        "InfluenceFrame"
        [ test "compute produces valid influence frame" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10; makeGk 4 10 10 10 |]
              let homeRoster = PlayerRoster.build home
              let awayRoster = PlayerRoster.build away

              let homeSp =
                  home
                  |> Array.map (fun _ ->
                      { X = 52.5<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 0.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = 0.0<meter / second> })

              let awaySp =
                  away
                  |> Array.map (fun _ ->
                      { X = 70.0<meter>
                        Y = 34.0<meter>
                        Z = 0.0<meter>
                        Vx = 0.0<meter / second>
                        Vy = 0.0<meter / second>
                        Vz = 0.0<meter / second> })

              let homeFrame = TeamFrame.init homeRoster homeSp
              let awayFrame = TeamFrame.init awayRoster awaySp
              let frame = InfluenceFrame.compute homeFrame awayFrame
              Expect.isGreaterThan frame.HomeGrid.Length 0 "home grid should exist"
              Expect.equal frame.HomeGrid.Length frame.AwayGrid.Length "grids should be same size"
          }

          test "control grid reflects home dominance when home is closer" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10; makeGk 4 10 10 10 |]
              let homeRoster = PlayerRoster.build home
              let awayRoster = PlayerRoster.build away

              let homeSp =
                  [| { X = 30.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> }
                     { X = 35.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let awaySp =
                  [| { X = 90.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> }
                     { X = 95.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let homeFrame = TeamFrame.init homeRoster homeSp
              let awayFrame = TeamFrame.init awayRoster awaySp
              let frame = InfluenceFrame.compute homeFrame awayFrame

              let homeControlled =
                  frame.ControlGrid |> Array.filter (fun v -> v = 1uy) |> Array.length

              Expect.isGreaterThan homeControlled 0 "home should control some cells"
          }

          test "pass safety is in [0, 1] range" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 DC 10 |]
              let homeRoster = PlayerRoster.build home
              let awayRoster = PlayerRoster.build away

              let homeSp =
                  [| { X = 52.5<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let awaySp =
                  [| { X = 70.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let homeFrame = TeamFrame.init homeRoster homeSp
              let awayFrame = TeamFrame.init awayRoster awaySp
              let frame = InfluenceFrame.compute homeFrame awayFrame

              for i = 0 to frame.AttackerPassSafety.Length - 1 do
                  Expect.isGreaterThanOrEqual frame.AttackerPassSafety[i] 0.0f "pass safety >= 0"
                  Expect.isLessThanOrEqual frame.AttackerPassSafety[i] 1.0f "pass safety <= 1"
          }

          test "defender coverage is in [0, 1] range" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 DC 10 |]
              let homeRoster = PlayerRoster.build home
              let awayRoster = PlayerRoster.build away

              let homeSp =
                  [| { X = 52.5<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let awaySp =
                  [| { X = 70.0<meter>
                       Y = 34.0<meter>
                       Z = 0.0<meter>
                       Vx = 0.0<meter / second>
                       Vy = 0.0<meter / second>
                       Vz = 0.0<meter / second> } |]

              let homeFrame = TeamFrame.init homeRoster homeSp
              let awayFrame = TeamFrame.init awayRoster awaySp
              let frame = InfluenceFrame.compute homeFrame awayFrame

              for i = 0 to frame.DefenderCoverage.Length - 1 do
                  Expect.isGreaterThanOrEqual frame.DefenderCoverage[i] 0.0f "coverage >= 0"
                  Expect.isLessThanOrEqual frame.DefenderCoverage[i] 1.0f "coverage <= 1"
          } ]
