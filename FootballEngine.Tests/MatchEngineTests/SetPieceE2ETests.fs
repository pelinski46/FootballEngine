module FootballEngine.Tests.MatchEngineTests.SetPieceE2ETests

open Expecto
open FootballEngine
open FootballEngine.Domain
open SchedulingTypes
open Helpers

let setPieceE2ETests =
    testList
        "SetPieceE2E"
        [ test "penalty produces goal or save" {
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]
              let clock = SimulationClock.defaultClock
              let mutable goalsOrSaves = 0
              for _ in 1..30 do
                  let ctx, s =
                      buildState home [| 88.0, 34.0 |] away [| 99.0, 34.0 |] 88.0 34.0 (Possession.SetPiece(HomeClub, SetPieceKind.Penalty))
                  let scored = SetPlayAction.resolvePenalty 0 ctx s home[0] HomeClub clock
                  if scored then goalsOrSaves <- goalsOrSaves + 1
              Expect.isGreaterThanOrEqual goalsOrSaves 1 "at least one penalty should produce goal or save"
          }

          test "corner emits Corner event" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makeGk 3 10 10 10; makePlayer 4 DC 10 |]
              let ctx, s =
                  buildState home [| 99.0, 5.0; 85.0, 34.0 |] away [| 90.0, 34.0; 95.0, 30.0 |] 104.0 5.0 (Possession.SetPiece(HomeClub, SetPieceKind.Corner))
              let clock = SimulationClock.defaultClock
              let tick : ScheduledTick =
                  { SubTick = 0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = SetPieceTick(SetPieceKind.Corner, HomeClub) }
              let result = SetPieceAgent.agent tick ctx s clock
              let hasCorner = result.Events |> List.exists (fun e -> e.Type = MatchEventType.Corner)
              Expect.isTrue hasCorner "corner should emit Corner event"
          }

          test "free kick emits FreeKick event" {
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makeGk 2 10 10 10 |]
              let ctx, s =
                  buildState home [| 75.0, 34.0 |] away [| 99.0, 34.0 |] 75.0 34.0 (Possession.SetPiece(HomeClub, SetPieceKind.FreeKick))
              let clock = SimulationClock.defaultClock
              let tick : ScheduledTick =
                  { SubTick = 0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = SetPieceTick(SetPieceKind.FreeKick, HomeClub) }
              let result = SetPieceAgent.agent tick ctx s clock
              let hasFK =
                  result.Events |> List.exists (fun e -> match e.Type with MatchEventType.FreeKick _ -> true | _ -> false)
              Expect.isTrue hasFK "free kick should emit FreeKick event"
          }

          test "throw-in emits pass-class event" {
              let home = [| makePlayer 1 ST 10; makePlayer 2 MC 10 |]
              let away = [| makePlayer 3 DC 10 |]
              let ctx, s =
                  buildState home [| 52.5, 0.5; 55.0, 10.0 |] away [| 55.0, 5.0 |] 52.5 0.5 (Possession.SetPiece(HomeClub, SetPieceKind.ThrowIn))
              let clock = SimulationClock.defaultClock
              let tick : ScheduledTick =
                  { SubTick = 0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = SetPieceTick(SetPieceKind.ThrowIn, HomeClub) }
              let result = SetPieceAgent.agent tick ctx s clock
              let hasPass =
                  result.Events
                  |> List.exists (fun e ->
                      match e.Type with
                      | MatchEventType.PassLaunched _
                      | MatchEventType.PassCompleted _
                      | MatchEventType.PassIncomplete _ -> true
                      | _ -> false)
              Expect.isTrue hasPass "throw-in should emit pass-class event"
          } ]
