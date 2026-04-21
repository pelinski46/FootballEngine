module FootballEngine.Tests.DeterminismTests

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
        [ test "same seed produces same score" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let h1, a1, _, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let h2, a2, _, _ = sim home away players staff gs.ProfileCache

              Expect.equal (h1, a1) (h2, a2) "same seed must produce identical scores"
          }

          test "same seed produces same event count" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let _, _, evs1, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let _, _, evs2, _ = sim home away players staff gs.ProfileCache

              Expect.equal evs1.Length evs2.Length "same seed must produce same event count"
          }

          test "same seed produces identical event sequence" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let _, _, evs1, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let _, _, evs2, _ = sim home away players staff gs.ProfileCache

              Expect.equal evs1.Length evs2.Length "event count mismatch"

              evs1
              |> List.iteri (fun i e1 ->
                  let e2 = evs2[i]
                  Expect.equal e1.SubTick e2.SubTick $"event[{i}].SubTick mismatch"
                  Expect.equal e1.PlayerId e2.PlayerId $"event[{i}].PlayerId mismatch"
                  Expect.equal e1.ClubId e2.ClubId $"event[{i}].ClubId mismatch"
                  Expect.equal e1.Type e2.Type $"event[{i}].Type mismatch")
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

              Expect.isTrue anyDiffers "at least one seed pair should produce different event sequences"
          }

          test "same seed produces same snapshots" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let replay1 = simFull home away players staff gs.ProfileCache

              setSeed 42
              let replay2 = simFull home away players staff gs.ProfileCache

              Expect.equal replay1.Snapshots.Length replay2.Snapshots.Length "snapshot count mismatch"
              Expect.equal replay1.Events.Length replay2.Events.Length "event count mismatch"

              replay1.Snapshots
              |> Array.iteri (fun i s1 ->
                  let s2 = replay2.Snapshots[i]
                  Expect.equal s1.SubTick s2.SubTick $"snapshot[{i}].SubTick mismatch"
                  Expect.equal s1.HomeScore s2.HomeScore $"snapshot[{i}].HomeScore mismatch"
                  Expect.equal s1.AwayScore s2.AwayScore $"snapshot[{i}].AwayScore mismatch")
          }

          test "same seed produces same goal events" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let _, _, evs1, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let _, _, evs2, _ = sim home away players staff gs.ProfileCache

              let goals1 = evs1 |> List.filter (fun e -> e.Type = Goal)
              let goals2 = evs2 |> List.filter (fun e -> e.Type = Goal)

              Expect.equal goals1.Length goals2.Length "goal count mismatch"

              goals1
              |> List.iteri (fun i g1 ->
                  let g2 = goals2[i]
                  Expect.equal g1.PlayerId g2.PlayerId $"goal[{i}].PlayerId mismatch"
                  Expect.equal g1.SubTick g2.SubTick $"goal[{i}].SubTick mismatch")
          }

          test "same seed produces same foul count" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let _, _, evs1, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let _, _, evs2, _ = sim home away players staff gs.ProfileCache

              let fouls1 = evs1 |> List.filter (fun e -> e.Type = FoulCommitted) |> List.length
              let fouls2 = evs2 |> List.filter (fun e -> e.Type = FoulCommitted) |> List.length

              Expect.equal fouls1 fouls2 "foul count must be deterministic"
          }

          test "same seed produces same corner count" {
              let gs, clubs, players, staff = loadClubs ()
              let home, away = clubs.[0], clubs.[1]

              setSeed 42
              let _, _, evs1, _ = sim home away players staff gs.ProfileCache

              setSeed 42
              let _, _, evs2, _ = sim home away players staff gs.ProfileCache

              let corners1 = evs1 |> List.filter (fun e -> e.Type = Corner) |> List.length
              let corners2 = evs2 |> List.filter (fun e -> e.Type = Corner) |> List.length

              Expect.equal corners1 corners2 "corner count must be deterministic"
          } ]
