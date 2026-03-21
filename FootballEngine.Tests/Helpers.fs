module FootballEngine.Tests.Helpers

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Lineup

let loadGame () =
    match Db.loadGame().GetAwaiter().GetResult() with
    | None -> failtest "No saved game — run generateNewGame first."
    | Some game -> game

let makeReadyClub (c: Club) =
    autoLineup { c with CurrentLineup = None } (bestFormation c)

let loadClubs () =
    let game = loadGame ()
    let clubs = game.Clubs |> Map.toArray |> Array.map (snd >> makeReadyClub)

    if clubs.Length < 2 then
        failtest "Need at least 2 clubs."

    clubs

let inBounds (x: float, y: float) =
    x >= 0.0 && x <= 100.0 && y >= 0.0 && y <= 100.0

let allPositionsInBounds (positions: Map<PlayerId, float * float>) =
    positions |> Map.forall (fun _ pos -> inBounds pos)

let isSeasonOver (state: GameState) =
    state.Competitions
    |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.Played))

let getOk result =
    match result with
    | Ok v -> v
    | Error e -> failtestf $"Expected Ok but got Error: %A{e}"

let emptyStanding clubId =
    { ClubId = clubId
      Played = 0
      Won = 0
      Drawn = 0
      Lost = 0
      GoalsFor = 0
      GoalsAgainst = 0
      Points = 0 }
