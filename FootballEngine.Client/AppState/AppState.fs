namespace FootballEngine

open System
open Elmish
open FootballEngine.AppMsgs
open FootballEngine.Domain
open AppTypes


module AppState =

    let private emptyGameState () : GameState =
        { CurrentDate = DateTime.Now
          Season = DateTime.Now.Year
          Clubs = Map.empty
          Players = Map.empty
          Competitions = Map.empty
          Countries = Map.empty
          UserClubId = 0
          ManagerName = ""
          PrimaryCountry = "" }

    let private initialState (gs: GameState) : State =
        { GameState = gs
          CurrentPage = if gs.Clubs.IsEmpty then Setup else Home
          IsProcessing = true
          LogMessages = [ "Football Engine 2026 Initialized" ]
          Notifications = []
          NextNotificationId = 1
          SelectedPlayer = None
          SelectedTactics = F433
          SelectedLeagueId = 1
          DraggedPlayer = None
          PlayerSortBy = "position"
          Setup = initSetupState
          Transfer = initTransferState
          MatchLab = initMatchLabState }

    let private addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let private buildNewLineupSlots (formation: Formation) (existing: ClubLineup option) =
        let newFormationSlots = FormationData.getFormation formation

        match existing with
        | None ->
            newFormationSlots
            |> List.map (fun fs ->
                { Index = fs.Index
                  Role = fs.Role
                  X = fs.X
                  Y = fs.Y
                  PlayerId = None })
        | Some lineup ->
            let mutable remaining =
                lineup.Slots
                |> List.choose (fun s -> s.PlayerId |> Option.map (fun pid -> s.Role, pid))

            newFormationSlots
            |> List.map (fun fs ->
                match remaining |> List.tryFind (fun (role, _) -> role = fs.Role) with
                | Some(role, pid) ->
                    remaining <- remaining |> List.filter (fun x -> x <> (role, pid))

                    { Index = fs.Index
                      Role = fs.Role
                      X = fs.X
                      Y = fs.Y
                      PlayerId = Some pid }
                | None ->
                    { Index = fs.Index
                      Role = fs.Role
                      X = fs.X
                      Y = fs.Y
                      PlayerId = None })

    /// Reset the transfer cache so it's rebuilt on next visit.
    /// Called after any SimMsg that mutates GameState (days/season advance).
    let private invalidateTransferCache (state: State) =
        { state with
            Transfer =
                { state.Transfer with
                    CachedPlayers = []
                    FilteredPlayers = [] } }

    let init () =
        let loadCmd = Cmd.OfTask.perform (fun () -> Db.loadGame ()) () GameLoaded
        initialState (emptyGameState ()), loadCmd

    let update (msg: Msg) (state: State) : State * Cmd<Msg> =
        match msg with
        | SetupMsg m -> UpdateSetup.handle m state

        | SimMsg m ->
            let nextState, cmd = UpdateSim.handle m state
            // Invalidate transfer cache after messages that produce a new GameState.
            // AdvanceDay / AdvanceSeason are async — the Done variants carry the new state.
            let invalidated =
                match m with
                | AdvanceDayDone _
                | SeasonAdvanceDone _
                | SimulateAllToday
                | SimulateNextFixture
                | SimulateMatch -> invalidateTransferCache nextState
                | _ -> nextState

            invalidated, cmd

        | TransferMsg m -> UpdateTransfer.handle m state
        | MatchLabMsg m -> UpdateMatchLab.handle m state

        | NotificationMsg nm ->
            match nm with
            | DismissNotification id ->
                { state with
                    Notifications = state.Notifications |> List.filter (fun n -> n.Id <> id) },
                Cmd.none
            | DismissAll -> { state with Notifications = [] }, Cmd.none
            | PushNotification note ->
                { state with
                    Notifications = note :: state.Notifications |> List.truncate 20
                    NextNotificationId = state.NextNotificationId + 1 },
                Cmd.none

        | GameLoaded result ->
            let gs = result |> Option.defaultValue (emptyGameState ())

            { state with
                GameState = gs
                CurrentPage = if gs.Clubs.IsEmpty then Setup else Home
                IsProcessing = false },
            Cmd.none

        | ChangePage page ->
            let cmd =
                if page = Transfers && state.Transfer.CachedPlayers.IsEmpty then
                    Cmd.ofMsg (TransferMsg Load)
                else
                    Cmd.none

            { state with CurrentPage = page }, cmd

        | SelectPlayer pId ->
            { state with
                SelectedPlayer = state.GameState.Players.TryFind pId },
            Cmd.none

        | DropPlayerInSlot(targetIdx, pId) ->
            let team = state.GameState.Clubs[state.GameState.UserClubId]

            let lineup =
                team.CurrentLineup
                |> Option.defaultValue
                    { Formation = state.SelectedTactics
                      TeamTactics = "Balanced"
                      Slots =
                        FormationData.getFormation state.SelectedTactics
                        |> List.map (fun fs ->
                            { Index = fs.Index
                              Role = fs.Role
                              X = fs.X
                              Y = fs.Y
                              PlayerId = None })
                        |> List.sortBy _.Index }

            let updatedTeam =
                { team with
                    CurrentLineup = Some(Lineup.swapPlayer targetIdx pId lineup) }

            let newGs =
                { state.GameState with
                    Clubs = state.GameState.Clubs.Add(team.Id, updatedTeam) }

            let saveCmd =
                Cmd.OfTask.attempt Db.saveGameAsync newGs (fun _ -> SetProcessing false)

            { state with
                GameState = newGs
                DraggedPlayer = None }
            |> addLog $"🔄 Swap made: Slot {targetIdx}",
            saveCmd

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }, Cmd.none

        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId },
            Cmd.none

        | SetTactics formation ->
            let userClub = state.GameState.Clubs[state.GameState.UserClubId]
            let newSlots = buildNewLineupSlots formation userClub.CurrentLineup

            let updatedClub =
                { userClub with
                    CurrentLineup =
                        Some
                            { Formation = formation
                              TeamTactics = "Balanced"
                              Slots = newSlots } }

            let newGs =
                { state.GameState with
                    Clubs = state.GameState.Clubs |> Map.add updatedClub.Id updatedClub }

            let saveCmd =
                Cmd.OfTask.attempt Db.saveGameAsync newGs (fun _ -> SetProcessing false)

            { state with
                GameState = newGs
                SelectedTactics = formation },
            saveCmd

        | SetProcessing b -> { state with IsProcessing = b }, Cmd.none
