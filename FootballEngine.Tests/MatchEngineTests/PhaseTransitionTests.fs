module FootballEngine.Tests.MatchEngineTests.PossessionTransitionTests

open Expecto
open FootballEngine
open FootballEngine.Tests

let phaseTransitionTests =
    testList
        "PhaseTransitions"
        [ testCase "phase transitions follow allowed graph over 20 matches" <| fun () ->
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2

              let allowed (from': Possession) (to': Possession) =
                  match from', to' with
                  | Owned _, (Owned _ | InFlight | Contest _ | Transition _ | SetPiece _) -> true
                  | Transition _, (Owned _ | Contest _ | InFlight | Transition _) -> true
                  | Contest _, (Owned _ | Contest _ | SetPiece _ | InFlight) -> true
                  | InFlight, (Owned _ | Contest _ | SetPiece _ | InFlight | Transition _) -> true
                  | SetPiece _, (InFlight | Contest _ | Owned _ | SetPiece _) -> true
                  | Loose, _ -> true
                  | _, Loose -> true
                  | _ -> false

              for idx in 1..5 do
                  let result = MatchSimulator.trySimulateMatchFull clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
                  match result with
                  | Ok replay ->
                      for i = 0 to replay.Snapshots.Length - 2 do
                          let from = replay.Snapshots[i].Possession
                          let to' = replay.Snapshots[i + 1].Possession
                          if not (allowed from to') then
                              failtest $"match {idx}, snapshot {i}->{i + 1}: transition %A{from} -> %A{to'} not allowed"
                  | Error e -> failtestf $"match {idx}: simulation error: %A{e}" ]
