module FootballEngine.Tests.MatchEngineTests.PossessionTransitionTests

open Expecto
open FootballEngine
open FootballEngine.Tests

let phaseTransitionTests =
    testList
        "PhaseTransitions"
        [

          let runMatchWithSnapshots () =
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2
              let home, away = clubs[0], clubs[1]
              let profileMap = Map.empty // Necesario para la nueva firma
              MatchSimulator.trySimulateMatchFull home away game.Players game.Staff profileMap

          let matches = [ 1..20 ] |> List.map (fun _ -> runMatchWithSnapshots ())

          testCase "Phase transitions follow allowed graph"
          <| fun () ->
              let allowed (from': Possession) (to': Possession) =
                  match from', to' with
                  | Owned _, (Owned _ | InFlight _ | Contest _ | Transition _ | SetPiece _) -> true
                  | Transition _, (Owned _ | Contest _ | InFlight _ | Transition _) -> true
                  | Contest _, (Owned _ | Contest _ | SetPiece _ | InFlight _) -> true
                  | InFlight _, (Owned _ | Contest _ | SetPiece _ | InFlight _ | Transition _) -> true
                  | SetPiece _, (InFlight _ | Contest _ | Owned _ | SetPiece _) -> true
                  | _ -> false

              for idx, r in List.indexed matches do
                  match r with
                  | Ok replay ->
                      for i = 0 to replay.Snapshots.Length - 2 do
                          let from = replay.Snapshots[i].Possession
                          let to' = replay.Snapshots[i + 1].Possession

                          if not (allowed from to') then
                              failtest
                                  $"match {idx}, snapshot {i}→{i + 1}: Phase transition %A{from} → %A{to'} is not in the allowed graph."
                  | Error e -> failtestf $"match {idx}: simulation error: %A{e}" ]
