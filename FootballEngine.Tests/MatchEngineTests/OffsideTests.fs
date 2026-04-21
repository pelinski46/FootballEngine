module FootballEngine.Tests.MatchEngineTests.OffsideTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSpatial
open Helpers

let offsideTests =
    testList
        "Offside"
        [

          testCase "GK is never offside"
          <| fun () ->
              let home = [| makePlayer 1 GK 10 |]
              let hpos = [| 80.0, 34.0 |]
              let away =[| makePlayer 2 GK 10 |]
              let apos = [| 85.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(HomeClub, 1))

              let gkSp = spatialAt 80.0 34.0
              let actx = ActionContext.build s

              Expect.isFalse
                  (isOffside (makePlayer 1 GK 10) gkSp.X s actx.Dir)
                  $"isOffside(LeftToRight): GK at x=80, defender GK at x=85 → Offside. GK should never be offside."

          testCase "player in own half is never offside"
          <| fun () ->
              let home = [| makePlayer 1 MC 10 |]
              let hpos = [| 40.0, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 45.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(HomeClub, 1))

              let receiverSp = spatialAt 40.0 34.0
              let actx = ActionContext.build s

              Expect.isFalse
                  (isOffside (makePlayer 1 MC 10) receiverSp.X s actx.Dir)
                  $"isOffside(LeftToRight): player at x=40 (own half), defender at x=45 → Offside. Player in own half should never be offside."

          testCase "player level with second-last defender is not offside"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let hpos = [| 75.0, 34.0 |]
              let away = [| makePlayer 2 DC 10; makePlayer 3 DC 10 |]
              let apos = [| 75.0, 30.0; 70.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(HomeClub, 1))

              let receiverSp = spatialAt 75.0 34.0
              let actx = ActionContext.build s

              Expect.isFalse
                  (isOffside (makePlayer 1 ST 10) receiverSp.X s actx.Dir)
                  $"isOffside(LeftToRight): receiver at x=75, defenders at [75, 70] → Offside. Player level with second-last defender should not be offside."

          testCase "player ahead of second-last defender is offside"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let hpos = [| 80.0, 34.0 |]
              let away = [| makePlayer 2 DC 10; makePlayer 3 DC 10 |]
              let apos = [| 75.0, 30.0; 70.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(HomeClub, 1))

              let receiverSp = spatialAt 80.0 34.0
              let actx = ActionContext.build s

              Expect.isTrue
                  (isOffside (makePlayer 1 ST 10) receiverSp.X s actx.Dir)
                  $"isOffside(LeftToRight): receiver at x=80, defenders at [75, 70] → NotOffside. Expected Offside."

          testCase "RightToLeft direction mirrors LeftToRight"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let hpos = [| 25.0, 34.0 |]
              let away = [| makePlayer 2 DC 10; makePlayer 3 DC 10 |]
              let apos = [| 30.0, 30.0; 35.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(AwayClub, 1))

              s.HomeAttackDir <- RightToLeft
              let receiverSp = spatialAt 25.0 34.0
              let actx = ActionContext.build s

              Expect.isTrue
                  (isOffside (makePlayer 1 ST 10) receiverSp.X s actx.Dir)
                  $"isOffside(RightToLeft): receiver at x=25, defenders at [30, 35] → NotOffside. Expected Offside."

          testCase "only GK as defender: offside depends on ball position"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let hpos = [| 90.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 85.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 70.0 34.0 (Owned(HomeClub, 1))

              let receiverSp = spatialAt 90.0 34.0
              let actx = ActionContext.build s

              Expect.isTrue
                  (isOffside (makePlayer 1 ST 10) receiverSp.X s actx.Dir)
                  $"isOffside(LeftToRight): receiver at x=90, only GK at x=85, ball at x=70 → NotOffside. Expected Offside."

          testCase "player exactly at halfway line is not offside"
          <| fun () ->
              let home = [| makePlayer 1 MC 10 |]
              let hpos = [| 52.5, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 50.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Owned(HomeClub, 1))

              let receiverSp = spatialAt 52.5 34.0
              let actx = ActionContext.build s

              Expect.isFalse
                  (isOffside (makePlayer 1 MC 10) receiverSp.X s actx.Dir)
                  $"isOffside(LeftToRight): player at x=52.5 (halfway line), defender at x=50 → Offside. Player at own half boundary should not be offside." ]
