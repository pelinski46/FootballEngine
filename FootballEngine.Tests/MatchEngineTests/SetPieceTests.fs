module FootballEngine.Tests.MatchEngineTests.SetPieceTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open SchedulingTypes
open Helpers

let setPieceTests =
    testList
        "SetPiece"
        [

          testCase "AwardThrowIn HomeClub → Phase = SetPiece HomeClub"
          <| fun () ->
              let home = [| makePlayer 1 DC 10 |]
              let hpos = [| 10.0, 34.0 |]
              let away = [| makePlayer 2 MR 10 |]
              let apos = [| 100.0, 34.0 |]

              let ctx, s = buildState home hpos away apos 0.0 34.0 (Contest AwayClub)

              let tick =
                  { SubTick = 1
                    Priority = TickPriority.Referee
                    SequenceId = 0L
                    Kind = RefereeTick }

              let clock = SimulationClock.defaultClock
              let result = RefereeAgent.agent tick ctx s clock

              let hasThrowIn =
                  result.Actions |> List.exists (function | AwardThrowIn HomeClub -> true | _ -> false)
              Expect.isTrue hasThrowIn "ball out of bounds at Y<0.1 should produce AwardThrowIn HomeClub"

          testCase "AwardCorner AwayClub → Phase = SetPiece AwayClub"
          <| fun () ->
              let home = [| makePlayer 1 DC 10 |]
              let hpos = [| 10.0, 34.0 |]
              let away = [| makePlayer 2 MR 10 |]
              let apos = [| 100.0, 34.0 |]

              let ctx, s = buildState home hpos away apos 104.0 34.0 (Contest HomeClub)

              let tick =
                  { SubTick = 1
                    Priority = TickPriority.Referee
                    SequenceId = 0L
                    Kind = RefereeTick }

              let clock = SimulationClock.defaultClock
              let result = RefereeAgent.agent tick ctx s clock

              let hasCorner =
                  result.Actions |> List.exists (function | AwardCorner AwayClub -> true | _ -> false)
              Expect.isTrue hasCorner "ball out at X>104 in goal Y should produce AwardCorner AwayClub"

          testCase "KickOffTick (HomeClub kicking) → Phase = SetPiece HomeClub"
          <| fun () ->
              let home = [| makePlayer 1 ST 10; makePlayer 3 MC 10 |]
              let hpos = [| 52.5, 34.0; 50.0, 36.0 |]
              let away = [| makePlayer 2 ST 10 |]
              let apos = [| 55.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Possession.SetPiece(AwayClub, SetPieceKind.KickOff))

              s.HomeAttackDir <- LeftToRight
              s.LastAttackingClub <- HomeClub

              let tick =
                  { SubTick = 100
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = KickOffTick }

              let clock = SimulationClock.defaultClock
              SetPieceAgent.agent tick ctx s clock |> ignore

              Expect.equal
                  s.Ball.Possession
                  (Possession.SetPiece(HomeClub, SetPieceKind.KickOff))
                  $"KickOffTick(HomeClub kicking): Phase = %A{s.Ball.Possession}, expected SetPiece(HomeClub, KickOff)."

          testCase "FreeKick always emits FreeKick event"
          <| fun () ->
              let home = [| makePlayer 1 MC 20 |]
              let hpos = [| 40.0, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 45.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 40.0 34.0 (Possession.SetPiece(HomeClub, SetPieceKind.FreeKick))

              s.Ball <- { s.Ball with Possession = Loose }
              let clock = SimulationClock.defaultClock
              let result = SetPlayAction.resolveFreeKick 1 ctx s clock

              Expect.isTrue
                  (result.Events
                   |> List.exists (fun e ->
                       match e.Type with
                       | MatchEventType.FreeKick _ -> true
                       | _ -> false))
                  $"FreeKick produced events: %A{result.Events |> List.map _.Type}. Expected FreeKick."

          testCase "Corner always emits Corner event"
          <| fun () ->
              let home = [| makePlayer 1 MC 20; eliteAttacker 3 ST |]
              let hpos = [| 52.5, 34.0; 90.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (Possession.SetPiece(HomeClub, SetPieceKind.Corner))

              s.Ball <- { s.Ball with Possession = Loose }
              let clock = SimulationClock.defaultClock
              let result = SetPlayAction.resolveCorner 1 ctx s clock

              Expect.isTrue
                  (hasEventType MatchEventType.Corner result.Events)
                  $"Corner produced events: %A{result.Events |> List.map _.Type}. Expected Corner."

          testCase "ThrowIn emits pass-class event"
          <| fun () ->
              let home = [| makePlayer 1 DC 10; makePlayer 3 MC 10 |]
              let hpos = [| 10.0, 34.0; 20.0, 34.0 |]
              let away = [| makePlayer 2 MR 10 |]
              let apos = [| 15.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 10.0 34.0 (Possession.SetPiece(HomeClub, SetPieceKind.ThrowIn))

              s.Ball <- { s.Ball with Possession = Loose }
              let clock = SimulationClock.defaultClock
              let result = SetPlayAction.resolveThrowIn 1 ctx s HomeClub clock

              let hasPass =
                  result.Events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.PassLaunched _
                      | MatchEventType.PassCompleted _
                      | MatchEventType.PassIncomplete _ -> true
                      | _ -> false)

              Expect.isTrue
                  hasPass
                  $"ThrowIn produced events: %A{result.Events |> List.map _.Type}. Expected PassCompleted or PassIncomplete."

          testCase "elite free-kick taker scores ≥ 1 goal in 30 direct attempts"
          <| fun () ->
              let goals =
                  [ 1..30 ]
                  |> List.sumBy (fun i ->
                      let home =
                          [| makePlayer 1 MC 20
                             |> withTechnical
                                 { defaultTechnical with
                                     FreeKick = 20
                                     Finishing = 20 } |]

                      let hpos = [| 80.0, 34.0 |]
                      let away = [| weakGk 2 |]
                      let apos = [| 99.0, 34.0 |]

                      let ctx, s =
                          buildState
                              home
                              hpos
                              away
                              apos
                              80.0
                              34.0
                              (Possession.SetPiece(HomeClub, SetPieceKind.FreeKick))

                      s.Ball <- { s.Ball with Possession = Loose }
                      let clock = SimulationClock.defaultClock
                      let result = SetPlayAction.resolveFreeKick (1000 + i) ctx s clock
                      if hasGoal result.Events then 1 else 0)

              Expect.isGreaterThan
                  goals
                  0
                  $"elite free-kick taker vs weakGK: {goals} goals / 30 attempts. Expected ≥ 1." ]
