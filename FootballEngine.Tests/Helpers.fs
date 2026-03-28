module FootballEngine.Tests.Helpers

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Lineup

let loadGame () =
    match Db.loadGame().GetAwaiter().GetResult() with
    | None -> failtest "No saved game — run generateNewGame first."
    | Some game -> game

let clubPlayers (game: GameState) (club: Club) : Player list =
    club.PlayerIds |> List.choose (fun pid -> game.Players |> Map.tryFind pid)

let makeReadyClubAndStaff (game: GameState) (club: Club) (staff: Map<StaffId, Staff>) : Club * Map<StaffId, Staff> =
    let players = clubPlayers game club
    let formation = bestFormation players
    
    // Find the head coach for this club
    let headCoachId = 
        club.StaffIds 
        |> List.tryFind (fun sid -> staff |> Map.tryFind sid |> Option.map _.Role = Some HeadCoach)
    
    let updatedStaff =
        match headCoachId with
        | Some hcId ->
            let coach = staff.[hcId]
            let lineup = autoLineup coach players formation
            staff |> Map.add hcId lineup
        | None -> staff
    
    club, updatedStaff


let loadClubs () =
    let game = loadGame ()
    let initialStaff = game.Staff
    
    // Process each club and accumulate staff updates
    let clubs, finalStaff =
        game.Clubs 
        |> Map.toArray 
        |> Array.map snd
        |> Array.fold (fun (accClubs, accStaff) club ->
            let updatedClub, updatedStaff = makeReadyClubAndStaff game club accStaff
            (Array.append accClubs [|updatedClub|]), updatedStaff
        ) ([||], initialStaff)
    
    if clubs.Length < 2 then
        failtest "Need at least 2 clubs."

    clubs, game.Players, finalStaff

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

let playerClubId (p: Player) =
    match p.Affiliation with
    | Contracted(clubId, _) -> clubId
    | YouthProspect clubId -> clubId
    | _ -> -1

let playerSalary (p: Player) =
    match p.Affiliation with
    | Contracted(_, contract) -> contract.Salary
    | _ -> 0m

let playerValue (p: Player) = Player.playerValue p.CurrentSkill
