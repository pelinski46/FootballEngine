module FootballEngine.Tests.MatchEngineTests.DeterminismTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.MatchSimulator
open FootballEngine.Stats
open FootballEngine.Tests.Helpers

let private sim home away players staff profileCache =
    trySimulateMatch home away players staff profileCache |> getOk

let private simFull home away players staff profileCache =
    trySimulateMatchFull home away players staff profileCache |> getOk

let determinismTests =
    testList
        "Determinism"
        [ test "same seed produces identical full replay" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]
              setSeed 42
              let h1, a1, evs1, _ = sim home away players staff gs.ProfileCache
              setSeed 42
              let h2, a2, evs2, _ = sim home away players staff gs.ProfileCache

              Expect.equal (h1, a1) (h2, a2) "same seed must produce identical scores"
              Expect.equal evs1.Length evs2.Length "same seed must produce same event count"

              evs1 |> List.iteri (fun i e1 ->
                  let e2 = evs2[i]
                  Expect.equal e1.SubTick e2.SubTick $"event[{i}].SubTick mismatch"
                  Expect.equal e1.PlayerId e2.PlayerId $"event[{i}].PlayerId mismatch"
                  Expect.equal e1.ClubId e2.ClubId $"event[{i}].ClubId mismatch"
                  Expect.equal e1.Type e2.Type $"event[{i}].Type mismatch")

              let goals1 = evs1 |> List.filter (fun e -> e.Type = MatchEventType.Goal)
              let goals2 = evs2 |> List.filter (fun e -> e.Type = MatchEventType.Goal)
              Expect.equal goals1.Length goals2.Length "goal count must match"

              let fouls1 = evs1 |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted) |> List.length
              let fouls2 = evs2 |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted) |> List.length
              Expect.equal fouls1 fouls2 "foul count must match"

              let corners1 = evs1 |> List.filter (fun e -> e.Type = MatchEventType.Corner) |> List.length
              let corners2 = evs2 |> List.filter (fun e -> e.Type = MatchEventType.Corner) |> List.length
              Expect.equal corners1 corners2 "corner count must match"

              setSeed 42
              let replay1 = simFull home away players staff gs.ProfileCache
              setSeed 42
              let replay2 = simFull home away players staff gs.ProfileCache
              Expect.equal replay1.Snapshots.Length replay2.Snapshots.Length "snapshot count mismatch"
              Expect.equal replay1.Events.Length replay2.Events.Length "replay event count mismatch"
          }

          test "different seeds produce different outcomes" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]
              let pairs = [ (1, 2); (100, 200); (7, 13); (42, 99) ]
              let anyDiffers =
                  pairs
                  |> List.exists (fun (seed1, seed2) ->
                      setSeed seed1
                      let _, _, evs1, _ = sim home away players staff gs.ProfileCache
                      setSeed seed2
                      let _, _, evs2, _ = sim home away players staff gs.ProfileCache
                      let sig1 = evs1 |> List.map (fun e -> e.SubTick, e.Type)
                      let sig2 = evs2 |> List.map (fun e -> e.SubTick, e.Type)
                      sig1 <> sig2)
              Expect.isTrue anyDiffers "at least one seed pair should produce different sequences"
          } ]
