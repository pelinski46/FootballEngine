namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open AppTypes
open AppMsgs

module UpdateTransfer =

    let private buildCache (gs: GameState) =
        Task.Run(fun () ->
            let clubNameByPlayerId =
                gs.Clubs
                |> Map.toSeq
                |> Seq.collect (fun (_, c) -> c.Players |> List.map (fun p -> p.Id, c.Name))
                |> Map.ofSeq

            let players =
                gs.Players
                |> Map.toList
                |> List.map snd
                |> List.filter (fun p -> p.ClubId <> gs.UserClubId)
                |> List.sortByDescending _.CurrentSkill

            players, clubNameByPlayerId)

    let handle (msg: TransferMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Load ->
            { state with Transfer.IsLoading = true },
            Cmd.OfTask.perform (fun () -> buildCache state.GameState) () (fun (players, cache) ->
                TransferMsg(Loaded(players, cache)))

        | Loaded(players, cache) ->
            { state with
                Transfer =
                    { state.Transfer with
                        CachedPlayers = players
                        ClubNameCache = cache
                        IsLoading = false } },
            Cmd.none

        | TabChange tab ->
            { state with
                Transfer =
                    { state.Transfer with
                        ActiveTab = tab
                        Page = 0 } },
            Cmd.none

        | Search query ->
            { state with
                Transfer =
                    { state.Transfer with
                        SearchQuery = query
                        Page = 0 } },
            Cmd.none

        | FilterChange f ->
            { state with
                Transfer =
                    { state.Transfer with
                        PositionFilter = f
                        Page = 0 } },
            Cmd.none

        | SortChange s ->
            let asc =
                if state.Transfer.SortBy = s then
                    not state.Transfer.SortAsc
                else
                    false

            { state with
                Transfer =
                    { state.Transfer with
                        SortBy = s
                        SortAsc = asc
                        Page = 0 } },
            Cmd.none

        | PlayerSelect pid ->
            { state with
                Transfer.SelectedPlayerId = Some pid },
            Cmd.none

        | WatchToggle pid ->
            let updated =
                if List.contains pid state.Transfer.WatchlistIds then
                    state.Transfer.WatchlistIds |> List.filter ((<>) pid)
                else
                    pid :: state.Transfer.WatchlistIds

            { state with
                Transfer.WatchlistIds = updated },
            Cmd.none

        | PageChange p -> { state with Transfer.Page = p }, Cmd.none
