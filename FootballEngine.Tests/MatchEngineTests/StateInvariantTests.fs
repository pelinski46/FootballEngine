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
        [ testCase "Diagnóstico: Simulación de partido corto produce eventos"
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
                      (sprintf
                          "No hay eventos en la simulación. Puntaje final: %d-%d. Estado final de posesión: %A. Eventos: %A"
                          homeScore
                          awayScore
                          finalState.Ball.Possession
                          events)
              | Error e -> Tests.failtestf "Error en simulación: %A" e

          testCase "Diagnóstico: Después de KickOff, se genera al menos un DuelTick"
          <| fun () ->
              let home = [| makePlayer 1 ST 10; makePlayer 2 AMC 10 |]
              let away = [| makePlayer 3 GK 10 |]

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

              let scheduler = TickScheduler(SimulationClock.fullTime clock)

              [ { SubTick = 0
                  Priority = TickPriority.SetPiece
                  SequenceId = -1L
                  Kind = KickOffTick } ]
              |> List.iter scheduler.Insert

              let mutable events = []
              let mutable ticksProcessed = 0

              while scheduler.Count > 0 && ticksProcessed < 10 do
                  match scheduler.Dequeue() with
                  | ValueSome tick ->
                      ticksProcessed <- ticksProcessed + 1
                      let output = SetPieceAgent.agent tick ctx s clock
                      events <- events @ output.Events

                      match output.Continuation with
                      | EndChain ->
                          scheduler.Insert
                              { SubTick = tick.SubTick + 1000
                                Priority = TickPriority.Duel
                                SequenceId = int64 ticksProcessed
                                Kind = DuelTick 0 }
                      | _ -> ()
                  | ValueNone -> ()

              Expect.isGreaterThan ticksProcessed 1 "Solo se procesó el KickOff, no se generaron ticks siguientes."
              Expect.isGreaterThan events.Length 0 (sprintf "No hay eventos después de KickOff. Eventos: %A" events)

          testCase "Diagnóstico: PlayerAgent genera eventos en DuelTick"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]

              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock

              let tick =
                  { SubTick = 0
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 }

              let output = PlayerAgent.agent tick ctx s clock

              Tests.failtestf "output.Events = %A" output.Events

          testCase "Diagnóstico: ShotAction.resolve genera eventos directamente"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]

              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock

              // Llamar ShotAction.resolve directamente con el mismo setup
              let events = ShotAction.resolve 0 ctx s clock

              Expect.isGreaterThan events.Length 0 (sprintf "ShotAction.resolve no generó eventos. Eventos: %A" events)

          testCase "Diagnóstico: BallAgent maneja posesión correctamente"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 GK 10 |]

              let ctx, s =
                  buildState home [| 52.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock

              match s.Ball.Possession with
              | Possession.Owned(HomeClub, 1) ->
                  Expect.isGreaterThan output.Events.Length 0 "BallAgent cambió posesión pero no generó eventos."
              | _ ->
                  Expect.isTrue
                      (output.Continuation <> EndChain)
                      (sprintf
                          "BallAgent no cambió posesión y terminó cadena. Posesión: %A. Eventos: %A"
                          s.Ball.Possession
                          output.Events)

          testCase "Un SetPiece nunca debe persistir por más de 5 ticks físicos"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 GK 10 |]

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

              // Simulamos 6 ticks consecutivos
              for i in 1..6 do
                  BallAgent.agent tick ctx s clock |> ignore

              // Si después de 6 ticks sigue en SetPiece, la lógica de transición está rota
              match s.Ball.Possession with
              | Possession.SetPiece _ ->
                  Tests.failtest "El motor está bloqueado: SetPiece persiste tras 6 ticks físicos."
              | _ -> ()

          testCase "ST en área pequeña debe intentar disparo"
          <| fun () ->
              let home = [| eliteAttacker 1 ST |]
              let away = [| weakGk 2 |]

              let ctx, s =
                  buildState home [| 90.0, 34.0 |] away [| 99.0, 34.0 |] 90.0 34.0 (Possession.Owned(HomeClub, 1))

              let clock = SimulationClock.defaultClock

              let tick =
                  { SubTick = 0
                    Priority = SchedulingTypes.TickPriority.Duel
                    SequenceId = 0L
                    Kind = SchedulingTypes.TickKind.DecisionTick(0, Some 1) }

              let output = PlayerAgent.agent tick ctx s clock

              let isShot (e: MatchEvent) =
                  match e.Type with
                  | MatchEventType.Goal
                  | MatchEventType.ShotOffTarget
                  | MatchEventType.ShotBlocked -> true
                  | _ -> false

              let hasShot = List.exists isShot output.Events
              let eventTypes = List.map (fun (e: MatchEvent) -> e.Type) output.Events
              Expect.isTrue hasShot (sprintf "IA muy conservadora: El delantero no disparó. Eventos: %A" eventTypes)

          testCase "Jugador debe recuperar posesión si está cerca (Sticky Ball)"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 GK 10 |]

              let ctx, s =
                  buildState home [| 52.0, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              BallAgent.agent tick ctx s clock |> ignore

              match s.Ball.Possession with
              | Possession.Owned(HomeClub, 1) -> ()
              | _ ->
                  Tests.failtestf
                      "Sticky Ball falló: El jugador a 0.5m no tomó la posesión. Estado: %A"
                      s.Ball.Possession

          testCase "El motor debe manejar la falta de eventos en un tick físico"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 GK 10 |]

              let ctx, s =
                  buildState home [| 52.5, 34.0 |] away [| 99.0, 34.0 |] 52.5 34.0 Possession.Loose

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0
              let output = BallAgent.agent tick ctx s clock

              Expect.isTrue
                  (output.Continuation <> EndChain)
                  "El motor se detuvo abruptamente (EndChain sin transición)."

          testCase "Los jugadores deben cambiar de posición tras varios ticks (Steering activo)"
          <| fun () ->
              let home = [| makePlayer 1 ST 10 |]
              let away = [| makePlayer 2 GK 10 |]

              let ctx, s =
                  buildState home [| 0.0, 0.0 |] away [| 99.0, 34.0 |] 52.5 34.0 (Possession.Loose)

              let clock = SimulationClock.defaultClock
              let tick = mkPhysicsTick 0

              let initialPos =
                  s.Home.Slots[0]
                  |> function
                      | PlayerSlot.Active s -> s.Pos
                      | _ -> failwith "No active slot"

              MovementEngine.updatePhysics 0 s HomeClub 0.1<second>

              let newPos =
                  s.Home.Slots[0]
                  |> function
                      | PlayerSlot.Active s -> s.Pos
                      | _ -> failwith "No active slot"

              Expect.isTrue (initialPos <> newPos) "Los jugadores no se mueven: SteeringPipeline inactivo." ]
