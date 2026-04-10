module FootballEngine.Tests.MatchEngineTests.PhaseTransitionTests

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
              MatchSimulator.trySimulateMatchFull home away game.Players game.Staff

          let matches = [ 1..20 ] |> List.map (fun _ -> runMatchWithSnapshots ())

          testCase "never ControlledBy = Some with Phase = SetPiece"
          <| fun () ->
              for idx, r in List.indexed matches do
                  match r with
                  | Ok replay ->
                      for i, snap in replay.Snapshots |> Array.indexed do
                          match snap.BallControlledBy, snap.Phase with
                          | Some pid, PossessionPhase.SetPiece c ->
                              failtest
                                  $"match {idx}, snapshot {i}: ControlledBy = Some({pid}), Phase = SetPiece %A{c}. Set pieces must have ControlledBy = None."
                          | _ -> ()
                  | Error e -> failtestf $"match {idx}: simulation error: %A{e}"

          testCase "AttackingClub always matches club embedded in Phase"
          <| fun () ->
              for idx, r in List.indexed matches do
                  match r with
                  | Ok replay ->
                      for i, snap in replay.Snapshots |> Array.indexed do
                          let clubFromPhase =
                              match snap.Phase with
                              | PossessionPhase.InPossession c
                              | PossessionPhase.Transition c
                              | PossessionPhase.Contest c
                              | PossessionPhase.InFlight c
                              | PossessionPhase.SetPiece c -> c

                          Expect.equal
                              snap.AttackingClub
                              clubFromPhase
                              $"match {idx}, snapshot {i}: AttackingClub = %A{snap.AttackingClub}, but Phase = %A{snap.Phase} (club = %A{clubFromPhase})."
                  | Error e -> failtestf $"match {idx}: simulation error: %A{e}"

          testCase "Phase transitions follow allowed graph"
          <| fun () ->
              let allowed (from': PossessionPhase) (to': PossessionPhase) =
                  match from', to' with
                  | PossessionPhase.InPossession _,
                    (PossessionPhase.InPossession _ | PossessionPhase.InFlight _ | PossessionPhase.Contest _ | PossessionPhase.Transition _ | PossessionPhase.SetPiece _) ->
                      true
                  | PossessionPhase.Transition _,
                    (PossessionPhase.InPossession _ | PossessionPhase.Contest _ | PossessionPhase.InFlight _ | PossessionPhase.Transition _) -> true
                  | PossessionPhase.Contest _,
                    (PossessionPhase.InPossession _ | PossessionPhase.Contest _ | PossessionPhase.SetPiece _ | PossessionPhase.InFlight _) ->
                      true
                  | PossessionPhase.InFlight _,
                    (PossessionPhase.InPossession _ | PossessionPhase.Contest _ | PossessionPhase.SetPiece _ | PossessionPhase.InFlight _ | PossessionPhase.Transition _) ->
                      true
                  | PossessionPhase.SetPiece _,
                    (PossessionPhase.InFlight _ | PossessionPhase.Contest _ | PossessionPhase.InPossession _ | PossessionPhase.SetPiece _) -> true
                  | _ -> false

              for idx, r in List.indexed matches do
                  match r with
                  | Ok replay ->
                      for i = 0 to replay.Snapshots.Length - 2 do
                          let from = replay.Snapshots[i].Phase
                          let to' = replay.Snapshots[i + 1].Phase

                          if not (allowed from to') then
                              failtest
                                  $"match {idx}, snapshot {i}→{i + 1}: Phase transition %A{from} → %A{to'} is not in the allowed graph."
                  | Error e -> failtestf $"match {idx}: simulation error: %A{e}" ]
