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

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (PossessionPhase.Contest AwayClub)

              RefereeAgent.resolve 1 (AwardThrowIn HomeClub) ctx s |> ignore

              Expect.equal
                  s.Ball.Phase
                  (PossessionPhase.SetPiece HomeClub)
                  $"AwardThrowIn HomeClub: Phase = %A{s.Ball.Phase}, expected SetPiece HomeClub."

              Expect.equal
                  s.Ball.Position.X
                  PhysicsContract.PenaltyAreaDepth
                  $"AwardThrowIn HomeClub: ball X = {s.Ball.Position.X}, expected {PhysicsContract.PenaltyAreaDepth}."

          testCase "AwardCorner AwayClub → Phase = SetPiece AwayClub"
          <| fun () ->
              let home = [| makePlayer 1 DC 10 |]
              let hpos = [| 10.0, 34.0 |]
              let away = [| makePlayer 2 MR 10 |]
              let apos = [| 100.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (PossessionPhase.Contest HomeClub)

              RefereeAgent.resolve 1 (AwardCorner AwayClub) ctx s |> ignore

              Expect.equal
                  s.Ball.Phase
                  (PossessionPhase.SetPiece AwayClub)
                  $"AwardCorner AwayClub: Phase = %A{s.Ball.Phase}, expected SetPiece AwayClub."

          testCase "KickOffTick (HomeClub kicking) → Phase = SetPiece HomeClub"
          <| fun () ->
              let home = [| makePlayer 1 ST 10; makePlayer 3 MC 10 |]
              let hpos = [| 52.5, 34.0; 50.0, 36.0 |]
              let away = [| makePlayer 2 ST 10 |]
              let apos = [| 55.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (PossessionPhase.SetPiece AwayClub)

              s.HomeAttackDir <- LeftToRight

              let tick =
                  { SubTick = 100
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = KickOffTick }

              SetPieceAgent.agent 100 [] [] tick ctx s |> ignore

              Expect.equal
                  s.Ball.Phase
                  (PossessionPhase.SetPiece HomeClub)
                  $"KickOffTick(HomeClub kicking): Phase = %A{s.Ball.Phase}, expected SetPiece HomeClub."

          testCase "FreeKick always emits FreeKick event"
          <| fun () ->
              let home = [| makePlayer 1 MC 20 |]
              let hpos = [| 40.0, 34.0 |]
              let away = [| makePlayer 2 DC 10 |]
              let apos = [| 45.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 40.0 34.0 (PossessionPhase.SetPiece HomeClub)

              s.Ball <- { s.Ball with ControlledBy = None }
              let events = SetPlayAction.resolveFreeKick 1 ctx s

              Expect.isTrue
                  (events |> List.exists (fun e -> match e.Type with MatchEventType.FreeKick _ -> true | _ -> false))
                  $"FreeKick produced events: %A{events |> List.map _.Type}. Expected FreeKick."

          testCase "Corner always emits Corner event"
          <| fun () ->
              let home = [| makePlayer 1 MC 20; eliteAttacker 3 ST |]
              let hpos = [| 52.5, 34.0; 90.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 52.5 34.0 (PossessionPhase.SetPiece HomeClub)

              s.Ball <- { s.Ball with ControlledBy = None }
              let events = SetPlayAction.resolveCorner 1 ctx s

              Expect.isTrue
                  (hasEventType MatchEventType.Corner events)
                  $"Corner produced events: %A{events |> List.map _.Type}. Expected Corner."

          testCase "ThrowIn emits pass-class event"
          <| fun () ->
              let home = [| makePlayer 1 DC 10; makePlayer 3 MC 10 |]
              let hpos = [| 10.0, 34.0; 20.0, 34.0 |]
              let away = [| makePlayer 2 MR 10 |]
              let apos = [| 15.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 10.0 34.0 (PossessionPhase.SetPiece HomeClub)

              s.Ball <- { s.Ball with ControlledBy = None }
              let events = SetPlayAction.resolveThrowIn 1 ctx s HomeClub

              let hasPass =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.PassCompleted _
                      | MatchEventType.PassIncomplete _ -> true
                      | _ -> false)

              Expect.isTrue
                  hasPass
                  $"ThrowIn produced events: %A{events |> List.map _.Type}. Expected PassCompleted or PassIncomplete."

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
                          buildState home hpos away apos 80.0 34.0 (PossessionPhase.SetPiece HomeClub)

                      s.Ball <- { s.Ball with ControlledBy = None }
                      let events = SetPlayAction.resolveFreeKick (1000 + i) ctx s
                      if hasGoal events then 1 else 0)

              Expect.isGreaterThan
                  goals
                  0
                  $"elite free-kick taker vs weakGK: {goals} goals / 30 attempts. Expected ≥ 1." ]
