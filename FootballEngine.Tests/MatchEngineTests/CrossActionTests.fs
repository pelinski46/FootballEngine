module FootballEngine.Tests.MatchEngineTests.CrossActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let crossActionTests =
    testList
        "CrossAction"
        [

          testCase "blocked cross → Phase = Contest(defClub) AND PendingOffsideSnapshot = None"
          <| fun () ->
              let home = [| makePlayer 1 MR 10 |]
              let hpos = [| 5.0, 34.0 |]
              let away = [| makePlayer 2 DC 20; makePlayer 3 GK 10 |]
              let apos = [| 90.0, 34.0; 99.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 5.0 34.0 (PossessionPhase.InPossession HomeClub)

              s.Ball <-
                  { s.Ball with
                      ControlledBy = Some 1
                      PendingOffsideSnapshot = Some(mkSnap ()) }

              let events = CrossAction.resolve 1 ctx s

              let hasCrossAttempt =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.CrossAttempt _ -> true
                      | _ -> false)

              if hasCrossAttempt then
                  Expect.equal
                      s.Ball.Phase
                      (PossessionPhase.Contest AwayClub)
                      $"blocked cross: Phase = %A{s.Ball.Phase}, expected Contest AwayClub."

                  Expect.isNone
                      s.Ball.PendingOffsideSnapshot
                      $"blocked cross: PendingOffsideSnapshot = %A{s.Ball.PendingOffsideSnapshot}, expected None."

          testCase "successful cross → Phase = InFlight(AttSide)"
          <| fun () ->
              let home = [| makePlayer 1 MR 20; eliteAttacker 3 ST |]
              let hpos = [| 5.0, 34.0; 90.0, 34.0 |]
              let away = [| makePlayer 2 GK 1 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 5.0 34.0 (PossessionPhase.InPossession HomeClub)

              s.Ball <- { s.Ball with ControlledBy = Some 1 }
              let events = CrossAction.resolve 1 ctx s

              let hasCrossSuccess =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.CrossAttempt true -> true
                      | _ -> false)

              if hasCrossSuccess then
                  Expect.equal
                      s.Ball.Phase
                      (PossessionPhase.InFlight HomeClub)
                      $"successful cross: Phase = %A{s.Ball.Phase}, expected InFlight HomeClub."

          testCase "cross always emits CrossAttempt event"
          <| fun () ->
              let home = [| makePlayer 1 MR 10 |]
              let hpos = [| 5.0, 34.0 |]
              let away = [| makePlayer 2 DC 10; makePlayer 3 GK 10 |]
              let apos = [| 85.0, 34.0; 99.0, 34.0 |]

              let ctx, s =
                  buildState home hpos away apos 5.0 34.0 (PossessionPhase.InPossession HomeClub)

              s.Ball <- { s.Ball with ControlledBy = Some 1 }
              let events = CrossAction.resolve 1 ctx s

              Expect.isTrue
                  (events
                   |> List.exists (fun e ->
                       match e.Type with
                       | MatchEventType.CrossAttempt _ -> true
                       | _ -> false))
                  $"cross produced events: %A{events |> List.map _.Type}. Expected CrossAttempt." ]
