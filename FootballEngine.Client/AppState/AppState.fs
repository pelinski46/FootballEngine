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
          Staff = Map.empty
          Competitions = Map.empty
          Countries = Map.empty
          UserClubId = 0
          UserStaffId = 0
          PrimaryCountry = "" }

    let private initialState (gs: GameState) : State =
        { GameState = gs
          CurrentPage = Loading
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
          ActiveMatchReplay = None
          ActiveMatchSnapshot = 0 }

    let private addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let private headCoach (clubId: ClubId) (gs: GameState) : Staff option =
        gs.Staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map (fun c -> c.ClubId) = Some clubId)

    let private getCurrentLineup (gs: GameState) (clubId: ClubId) : Lineup option =
        headCoach clubId gs
        |> Option.bind (fun coach -> coach.Attributes.Coaching.Lineup)

    let private setCurrentLineup (gs: GameState) (clubId: ClubId) (lineup: Lineup option) : GameState =
        match headCoach clubId gs with
        | Some coach ->
            let updatedCoach =
                { coach with
                    Attributes =
                        { coach.Attributes with
                            Coaching =
                                { coach.Attributes.Coaching with
                                    Lineup = lineup } } }

            { gs with
                Staff = gs.Staff |> Map.add coach.Id updatedCoach }
        | None -> gs

    let private buildNewLineupSlots (formation: Formation) (existing: Lineup option) =
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

            let invalidated =
                match m with
                | AdvanceDone _
                | SeasonAdvanceDone _
                | UserMatchDone _ -> invalidateTransferCache nextState
                | _ -> nextState

            invalidated, cmd

        | TransferMsg m -> UpdateTransfer.handle m state

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
            let leagueId = SimHelpers.primaryLeagueId gs

            { state with
                GameState = gs
                CurrentPage = if gs.Clubs.IsEmpty then Setup else Home
                SelectedLeagueId = leagueId
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
            let clubId = state.GameState.UserClubId
            let currentLineup = getCurrentLineup state.GameState clubId

            let defaultLineup =
                { Formation = state.SelectedTactics
                  Tactics = Balanced
                  Slots =
                    FormationData.getFormation state.SelectedTactics
                    |> List.map (fun fs ->
                        { Index = fs.Index
                          Role = fs.Role
                          X = fs.X
                          Y = fs.Y
                          PlayerId = None })
                    |> List.sortBy _.Index }

            let lineup = currentLineup |> Option.defaultValue defaultLineup

            let swapPlayer (idx: int) (pid: PlayerId) (lineup: Lineup) : Lineup =
                let updatedSlots =
                    lineup.Slots
                    |> List.map (fun slot ->
                        if slot.Index = idx then
                            { slot with PlayerId = Some pid }
                        else if slot.PlayerId = Some pid then
                            { slot with PlayerId = None }
                        else
                            slot)

                { lineup with Slots = updatedSlots }

            let newLineup = swapPlayer targetIdx pId lineup
            let newGs = setCurrentLineup state.GameState clubId (Some newLineup)

            { state with
                GameState = newGs
                DraggedPlayer = None }
            |> addLog $"🔄 Swap made: Slot {targetIdx}",
            SimHelpers.saveCmd newGs

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }, Cmd.none

        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId },
            Cmd.none

        | SetTactics formation ->
            let clubId = state.GameState.UserClubId
            let currentLineup = getCurrentLineup state.GameState clubId
            let newSlots = buildNewLineupSlots formation currentLineup

            let newLineup =
                { Formation = formation
                  Tactics = Balanced
                  Slots = newSlots }

            let newGs = setCurrentLineup state.GameState clubId (Some newLineup)

            { state with
                GameState = newGs
                SelectedTactics = formation },
            SimHelpers.saveCmd newGs

        | SetProcessing b -> { state with IsProcessing = b }, Cmd.none
        | NoOp -> state, Cmd.none

        | StepActiveMatch delta ->
            let total =
                state.ActiveMatchReplay
                |> Option.map (fun r -> r.Snapshots.Length - 1)
                |> Option.defaultValue 0

            { state with
                ActiveMatchSnapshot = max 0 (min total (state.ActiveMatchSnapshot + delta)) },
            Cmd.none

        | CloseActiveMatch ->
            { state with
                ActiveMatchReplay = None
                ActiveMatchSnapshot = 0
                CurrentPage = Home },
            Cmd.none
