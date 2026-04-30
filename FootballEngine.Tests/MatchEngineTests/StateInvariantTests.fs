module FootballEngine.Tests.MatchEngineTests.StateInvariantTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Movement
open FootballEngine.PhysicsContract
open FootballEngine.SchedulingTypes
open FootballEngine.Tests
open Helpers

let stateInvariantTests =
    testList
        "StateInvariants"
        [ testCase "Full match simulation produces events"
          <| fun () ->
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let result =
                  MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
              match result with
              | Ok(homeScore, awayScore, events, finalState) ->
                  Expect.isGreaterThan
                      events.Length
                      0
                      $"No events in simulation. Score: %d{homeScore}-%d{awayScore}. Possession: %A{finalState.Ball.Possession}"
              | Error e -> Tests.failtestf $"Simulation error: %A{e}"

          testCase "After KickOff, state stepper resumes live play"
          <| fun () ->
              let home = [| makePlayer 1 ST 10; makePlayer 2 AMC 10 |]
              let away = [| makeGk 3 10 10 10 |]
              let ctx, s =
                  buildState
                      home
                      [| 52.5, 34.0; 49.0, 34.0 |]
                      away
                      [| 99.0, 34.0 |]
                      52.5
                      34.0
                      (Possession.SetPiece(HomeClub, SetPieceKind.KickOff))
              let clock = SimulationClock.defaultClock
              s.Flow <-
                  RestartDelay
                      { Kind = SetPieceKind.KickOff
                        Team = HomeClub
                        Cause = InitialKickOff
                        RemainingTicks = 0 }

              let result = MatchStepper.updateOne ctx clock [||] s

              Expect.equal result.State.Flow Live "kickoff restart should enter live flow"
              Expect.isGreaterThan result.Events.Length 0 "no events after KickOff"

          testCase "PlayerAgent produces events on RefereeTick with attacker in box"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]
              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let tick =
                  { SubTick = 0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = RefereeTick }
              let output = PlayerAgent.agent tick ctx s clock
              Expect.isGreaterThan output.Events.Length 0 "PlayerAgent should produce events when attacker is in box"

          testCase "ShotAction.resolve generates events when attacker is near goal"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]
              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let events = ShotAction.resolve 0 ctx s clock
              Expect.isGreaterThan events.Length 0 "ShotAction.resolve should generate events"

          testCase "BallAgent handles possession correctly when player is near ball"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 52.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose
              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              match s.Ball.Possession with
              | Possession.Owned(HomeClub, 1) ->
                  Expect.isGreaterThan output.Events.Length 0 "BallAgent changed possession but produced no events"
              | _ -> ()

          testCase "SetPiece must resolve within 6 physics ticks"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState
                      home
                      [| 52.5, 34.0 |]
                      away
                      [| 99.0, 34.0 |]
                      52.5
                      34.0
                      (Possession.SetPiece(HomeClub, SetPieceKind.KickOff))
              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              for _ in 1..6 do
                  BallAgent.agent tick ctx s clock |> ignore
              match s.Ball.Possession with
              | Possession.SetPiece _ ->
                  Tests.failtest "Engine is stuck: SetPiece persists after 6 physics ticks."
              | _ -> ()

          testCase "Striker in small box should attempt shot"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]
              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))
              let clock = SimulationClock.defaultClock
              let tick =
                  { SubTick = 0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = RefereeTick }
              let output = PlayerAgent.agent tick ctx s clock
              let isShot (e: MatchEvent) =
                  match e.Type with
                  | MatchEventType.Goal
                  | MatchEventType.ShotOffTarget
                  | MatchEventType.ShotBlocked -> true
                  | _ -> false
              let hasShot = List.exists isShot output.Events
              let eventTypes = List.map (fun (e: MatchEvent) -> e.Type) output.Events
              Expect.isTrue hasShot $"Striker did not shoot. Events: %A{eventTypes}"

          testCase "Player should recover ball if very close (Sticky Ball)"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 52.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose
              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              BallAgent.agent tick ctx s clock |> ignore
              match s.Ball.Possession with
              | Possession.Owned(HomeClub, 1) -> ()
              | _ ->
                  Tests.failtestf
                      $"Sticky Ball failed: Player at 0.5m did not pick up ball. State: %A{s.Ball.Possession}"

          testCase "Engine handles physics tick without crashing when no events occur"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose
              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock
              Expect.isTrue (output.Events.Length >= 0) "physics tick should not crash"

          testCase "Players must change position after multiple steering ticks"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 0.0, 0.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose
              let initialPos = s.Home.Frame.PosX[0], s.Home.Frame.PosY[0]
              MovementEngine.updatePhysics ctx s HomeClub 0 0.1<second>
              let newPos = s.Home.Frame.PosX[0], s.Home.Frame.PosY[0]
              Expect.isTrue (initialPos <> newPos) "Players did not move: SteeringPipeline inactive" ]
