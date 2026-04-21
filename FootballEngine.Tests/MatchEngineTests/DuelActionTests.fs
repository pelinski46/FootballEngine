module FootballEngine.Tests.MatchEngineTests.DuelActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open Helpers

let duelActionTests =
    testList
        "DuelAction"
        [

          testCase "foul always results in Contest with flipped club"
          <| fun () ->
              let home = [| eliteDribbler 1 MC; makePlayer 3 MC 10 |]
              let away = [| highAggression 2 MC |]

              let ctx, s =
                  buildState home [| 52.5, 34.0; 55.0, 34.0 |] away [| 53.0, 34.0 |] 52.5 34.0 (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1) }

              let events = DuelAction.resolve 1 ctx s SimulationClock.defaultClock
              let hasFoul = hasEventType MatchEventType.FoulCommitted events

              if hasFoul then
                  match s.Ball.Possession with
                  | Contest AwayClub -> ()
                  | other ->
                      failtestf
                          $"Phase after foul = %A{other}, expected Contest AwayClub. AttackingClub before foul was HomeClub."

          testCase "after foul: PendingOffsideSnapshot = None"
          <| fun () ->
              let home = [| eliteDribbler 1 MC; makePlayer 3 MC 10 |]
              let away = [| highAggression 2 MC |]

              let ctx, s =
                  buildState home [| 52.5, 34.0; 55.0, 34.0 |] away [| 53.0, 34.0 |] 52.5 34.0 (Owned(HomeClub, 1))

              let snap =
                  { PasserId = 1
                    ReceiverId = 3
                    ReceiverXAtPass = 55.0<meter>
                    SecondLastDefenderX = 50.0<meter>
                    BallXAtPass = 52.5<meter>
                    Dir = LeftToRight }

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1)
                      PendingOffsideSnapshot = Some snap }

              let events = DuelAction.resolve 1 ctx s SimulationClock.defaultClock
              let hasFoul = hasEventType MatchEventType.FoulCommitted events

              if hasFoul then
                  Expect.isNone
                      s.Ball.PendingOffsideSnapshot
                      $"after foul: PendingOffsideSnapshot = %A{s.Ball.PendingOffsideSnapshot}, expected None."

          testCase "duel always produces a duel-class event"
          <| fun () ->
              let home = [| makePlayer 1 MC 10 |]
              let away = [| makePlayer 2 MC 10 |]

              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 53.0, 34.0 |] 52.5 34.0 (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1) }

              let events = DuelAction.resolve 1 ctx s SimulationClock.defaultClock

              let isDuelClass =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.DribbleSuccess
                      | MatchEventType.DribbleFail
                      | MatchEventType.DribbleKeep
                      | MatchEventType.FoulCommitted -> true
                      | _ -> false)

              Expect.isTrue
                  isDuelClass
                  $"duel produced events: %A{events |> List.map (fun e -> e.Type)}. Expected one of DribbleSuccess/DribbleFail/DribbleKeep/FoulCommitted."

          testCase "elite dribbler win rate ≥ 40% over 100 trials"
          <| fun () ->
              let wins =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let ctx, s =
                          buildState
                              [| eliteDribbler 1 ST |]
                              [| 52.5, 34.0 |]
                              [| worstTackler 2 DC |]
                              [| 53.0, 34.0 |]
                              52.5
                              34.0
                              (Owned(HomeClub, 1))

                      s.Ball <-
                          { s.Ball with
                              Possession = Owned(HomeClub, 1) }

                      let events = DuelAction.resolve (1000 + i) ctx s SimulationClock.defaultClock

                      if hasEventType MatchEventType.DribbleSuccess events then
                          1
                      else
                          0)

              Expect.isGreaterThanOrEqual wins 40 $"eliteDribbler vs worstTackler: {wins} wins / 100 trials. Expected ≥ 40."

          testCase "worst tackler recovery rate ≤ 25% over 100 trials"
          <| fun () ->
              let recovers =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let ctx, s =
                          buildState
                              [| worstTackler 1 DC |]
                              [| 53.0, 34.0 |]
                              [| eliteDribbler 2 ST |]
                              [| 52.5, 34.0 |]
                              52.5
                              34.0
                              (Owned(AwayClub, 1))

                      s.Ball <-
                          { s.Ball with
                              Possession = Owned(HomeClub, 2) }

                      let events = DuelAction.resolve (1000 + i) ctx s SimulationClock.defaultClock

                      if hasEventType MatchEventType.DribbleFail events then
                          1
                      else
                          0)

              Expect.isLessThanOrEqual
                  recovers
                  25
                  $"worstTackler vs eliteDribbler: {recovers} recoveries / 100 trials. Expected ≤ 25."

          testCase "high aggression defender produces foul in ≥ 5 of 50 trials"
          <| fun () ->
              let fouls =
                  [ 1..50 ]
                  |> List.sumBy (fun i ->
                      let ctx, s =
                          buildState
                              [| eliteDribbler 1 ST |]
                              [| 52.5, 34.0 |]
                              [| highAggression 2 DC |]
                              [| 53.0, 34.0 |]
                              52.5
                              34.0
                              (Owned(HomeClub, 1))

                      s.Ball <-
                          { s.Ball with
                              Possession = Owned(HomeClub, 1) }

                      let events = DuelAction.resolve (1000 + i) ctx s SimulationClock.defaultClock
                      if hasFoul events then 1 else 0)

              Expect.isGreaterThan fouls 5 $"highAggression defender: {fouls} fouls / 50 trials. Expected ≥ 5." ]
