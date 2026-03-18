namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open AppTypes
open AppMsgs

module UpdateTransfer =

    let private buildClubNameCache (gs: GameState) : Map<PlayerId, string> =
        gs.Players
        |> Map.toSeq
        |> Seq.choose (fun (_, p) -> gs.Clubs |> Map.tryFind p.ClubId |> Option.map (fun c -> p.Id, c.Name))
        |> Map.ofSeq

    let private matchesFilter (filter: TransferFilter) (p: Player) =
        match filter with
        | AllPositions -> true
        | Goalkeepers -> p.Position = GK
        | Defenders -> p.Position = DC || p.Position = DL || p.Position = DR
        | Midfielders -> p.Position = DM || p.Position = MC || p.Position = AMC
        | Attackers -> p.Position = AML || p.Position = AMR || p.Position = ST

    let private applySort (sortBy: SortField) (asc: bool) (players: Player list) =
        match sortBy, asc with
        | ByName, true -> players |> List.sortBy (fun p -> p.Name)
        | ByName, false -> players |> List.sortByDescending (fun p -> p.Name)
        | ByCA, true -> players |> List.sortBy (fun p -> p.CurrentSkill)
        | ByCA, false -> players |> List.sortByDescending (fun p -> p.CurrentSkill)
        | ByValue, true -> players |> List.sortBy (fun p -> p.Value)
        | ByValue, false -> players |> List.sortByDescending (fun p -> p.Value)
        | ByAge, true -> players |> List.sortByDescending (fun p -> p.Birthday)
        | ByAge, false -> players |> List.sortBy (fun p -> p.Birthday)
        | ByPosition, true -> players |> List.sortBy (fun p -> p.Position)
        | ByPosition, false -> players |> List.sortByDescending (fun p -> p.Position)

    let private applyFilters (t: TransferState) =
        let query = t.SearchQuery.Trim().ToLowerInvariant()

        t.CachedPlayers
        |> List.filter (fun p ->
            matchesFilter t.PositionFilter p
            && (query = "" || p.Name.ToLowerInvariant().Contains(query)))
        |> applySort t.SortBy t.SortAsc

    let private withFilteredPlayers (t: TransferState) =
        { t with
            FilteredPlayers = applyFilters t
            Page = 0 }

    let private buildCacheAsync (gs: GameState) =
        Task.Run(fun () ->
            let clubNameCache = buildClubNameCache gs

            let players =
                gs.Players
                |> Map.toList
                |> List.map snd
                |> List.filter (fun p -> p.ClubId <> gs.UserClubId)

            players, clubNameCache)


    let handle (msg: TransferMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Load ->
            { state with
                State.Transfer.IsLoading = true },
            Cmd.OfTask.perform (fun () -> buildCacheAsync state.GameState) () (fun (players, cache) ->
                TransferMsg(Loaded(players, cache)))

        | Loaded(players, cache) ->
            let updated =
                { state.Transfer with
                    CachedPlayers = players
                    ClubNameCache = cache
                    IsLoading = false }
                |> withFilteredPlayers

            { state with Transfer = updated }, Cmd.none

        | TabChange tab ->
            { state with
                Transfer =
                    { state.Transfer with
                        ActiveTab = tab
                        Page = 0 } },
            Cmd.none

        | Search query ->
            let updated =
                { state.Transfer with
                    SearchQuery = query }
                |> withFilteredPlayers

            { state with Transfer = updated }, Cmd.none

        | FilterChange f ->
            let updated =
                { state.Transfer with
                    PositionFilter = f }
                |> withFilteredPlayers

            { state with Transfer = updated }, Cmd.none

        | SortChange s ->
            let asc =
                if state.Transfer.SortBy = s then
                    not state.Transfer.SortAsc
                else
                    false

            let updated =
                { state.Transfer with
                    SortBy = s
                    SortAsc = asc }
                |> withFilteredPlayers

            { state with Transfer = updated }, Cmd.none

        | PlayerSelect pid ->
            { state with
                State.Transfer.SelectedPlayerId = Some pid },
            Cmd.none

        | WatchToggle pid ->
            let updated =
                if List.contains pid state.Transfer.WatchlistIds then
                    state.Transfer.WatchlistIds |> List.filter ((<>) pid)
                else
                    pid :: state.Transfer.WatchlistIds

            { state with
                State.Transfer.WatchlistIds = updated },
            Cmd.none

        | PageChange p -> { state with State.Transfer.Page = p }, Cmd.none
