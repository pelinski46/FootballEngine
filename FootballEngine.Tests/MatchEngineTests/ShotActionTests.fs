module FootballEngine.Tests.MatchEngineTests.ShotActionTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open Helpers

let shotActionTests =
    testList
        "ShotAction"
        [

          testCase "shot always produces a shot-class event"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let hpos = [| 85.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s = buildState home hpos away apos 85.0 34.0 (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1) }

              let events = ShotAction.resolve 1 ctx s SimulationClock.defaultClock

              let isShotClass =
                  events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.ShotOffTarget
                      | MatchEventType.ShotBlocked
                      | MatchEventType.Goal
                      | MatchEventType.Save -> true
                      | _ -> false)

              Expect.isTrue
                  isShotClass
                  $"shot produced events: %A{events |> List.map _.Type}. Expected one of ShotOffTarget/ShotBlocked/Goal/Save."

          testCase "after shot: Phase = InFlight(AttSide)"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let hpos = [| 85.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s = buildState home hpos away apos 85.0 34.0 (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1) }

              let _ = ShotAction.resolve 1 ctx s SimulationClock.defaultClock

              Expect.equal
                  s.Ball.Possession
                  (InFlight(HomeClub, 1))
                  $"after shot: Phase = %A{s.Ball.Possession}, expected InFlight HomeClub."

          testCase "shot from own half (x=5) never scores"
          <| fun () ->
              let home = [| makePlayer 1 MC 10 |]
              let hpos = [| 5.0, 34.0 |]
              let away = [| makePlayer 2 GK 10 |]
              let apos = [| 99.0, 34.0 |]

              let ctx, s = buildState home hpos away apos 5.0 34.0 (Owned(HomeClub, 1))

              s.Ball <-
                  { s.Ball with
                      Possession = Owned(HomeClub, 1) }

              let events = ShotAction.resolve 1 ctx s SimulationClock.defaultClock

              Expect.isFalse
                  (events |> List.exists (fun e -> e.Type = MatchEventType.Goal))
                  $"shot from x=5: produced {events.Length} events. No goal expected from own half."

          testCase "elite attacker vs weak GK: goal rate ≥ 20% over 100 trials"
          <| fun () ->
              let goals =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let home = [| eliteAttacker 1 ST |]
                      let hpos = [| 85.0, 34.0 |]
                      let away = [| weakGk 2 |]
                      let apos = [| 99.0, 34.0 |]

                      let ctx, s = buildState home hpos away apos 85.0 34.0 (Owned(HomeClub, 1))

                      s.Ball <-
                          { s.Ball with
                              Possession = Owned(HomeClub, 1) }

                      let events = ShotAction.resolve (1000 + i) ctx s SimulationClock.defaultClock
                      if hasGoal events then 1 else 0)

              Expect.isGreaterThanOrEqual goals 20 $"eliteAttacker vs weakGK: {goals} goals / 100 shots. Expected ≥ 20."

          testCase "worst attacker vs elite GK: goal rate ≤ 5% over 100 trials"
          <| fun () ->
              let goals =
                  [ 1..100 ]
                  |> List.sumBy (fun i ->
                      let home = [| worstAttacker 1 ST |]
                      let hpos = [| 85.0, 34.0 |]
                      let away = [| eliteGk 2 |]
                      let apos = [| 99.0, 34.0 |]

                      let ctx, s = buildState home hpos away apos 85.0 34.0 (Owned(HomeClub, 1))

                      s.Ball <-
                          { s.Ball with
                              Possession = Owned(HomeClub, 1) }

                      let events = ShotAction.resolve (1000 + i) ctx s SimulationClock.defaultClock

                      if hasGoal events then 1 else 0)

              Expect.isLessThanOrEqual goals 5 $"worstAttacker vs eliteGK: {goals} goals / 100 shots. Expected ≤ 5." ]
