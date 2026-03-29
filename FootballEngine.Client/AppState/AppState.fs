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
          TrainingWeeksApplied = 0
          Clubs = Map.empty
          Players = Map.empty
          Staff = Map.empty
          Competitions = Map.empty
          Countries = Map.empty
          UserClubId = 0
          UserStaffId = 0
          PrimaryCountry = ""
          Inbox = []
          NextInboxId = 1 }

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
          ActiveMatchSnapshot = 0
          Inbox = initInboxState
          PrevUserClubSkills = None
          PrevUserClubStatus = None }

    let private addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let private headCoach (clubId: ClubId) (gs: GameState) : Staff option =
        GameState.getStaff clubId gs |> List.tryFind (fun s -> s.Role = HeadCoach)

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

        | InboxMsg im ->
            match im with
            | SelectMessage messageId ->
                let gs = GameState.markMessageAsRead messageId state.GameState

                { state with
                    GameState = gs
                    State.Inbox.SelectedMessageId = Some messageId },
                SimHelpers.saveCmd gs

            | MarkAsRead messageId ->
                let gs = GameState.markMessageAsRead messageId state.GameState
                { state with GameState = gs }, SimHelpers.saveCmd gs

            | MarkActionTaken messageId ->
                let gs = GameState.markMessageActionTaken messageId state.GameState
                { state with GameState = gs }, SimHelpers.saveCmd gs

        | GameLoaded result ->
            let gs = result |> Option.defaultValue (emptyGameState ())
            let leagueId = SimHelpers.primaryLeagueId gs

            { state with
                GameState = gs
                CurrentPage = if gs.Clubs.IsEmpty then Setup else HomePage
                SelectedLeagueId = leagueId
                IsProcessing = false },
            Cmd.none

        | ChangePage page ->
            let cmd =
                if page = Transfers && state.Transfer.CachedPlayers.IsEmpty then
                    Cmd.ofMsg (TransferMsg Load)
                else
                    Cmd.none

            let saveCmd =
                if state.CurrentPage = Tactics && page <> Tactics then
                    SimHelpers.saveCmd state.GameState
                elif state.CurrentPage = Training && page <> Training then
                    SimHelpers.saveCmd state.GameState
                else
                    Cmd.none

            { state with CurrentPage = page }, Cmd.batch [ cmd; saveCmd ]

        | SelectPlayer pId -> { state with SelectedPlayer = Some pId }, Cmd.none

        | DropPlayerInSlot(targetIdx, pId) ->
            let clubId = state.GameState.UserClubId

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

            let newGs =
                GameState.updateLineup
                    clubId
                    (fun currentLineupOpt ->
                        let defaultLineup =
                            { Formation = state.SelectedTactics
                              Tactics = Balanced
                              Instructions = Some TacticalInstructions.defaultInstructions
                              Slots =
                                FormationData.getFormation state.SelectedTactics
                                |> List.map (fun fs ->
                                    { Index = fs.Index
                                      Role = fs.Role
                                      X = fs.X
                                      Y = fs.Y
                                      PlayerId = None })
                                |> List.sortBy _.Index }

                        let current = currentLineupOpt |> Option.defaultValue defaultLineup
                        Some(swapPlayer targetIdx pId current))
                    state.GameState

            { state with
                GameState = newGs
                DraggedPlayer = None }
            |> addLog $"Swap made: Slot {targetIdx}",
            Cmd.none

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }, Cmd.none

        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId },
            Cmd.none

        | SetTactics formation ->
            let clubId = state.GameState.UserClubId

            let newGs =
                GameState.updateLineup
                    clubId
                    (fun currentLineupOpt ->
                        let lineup =
                            match currentLineupOpt with
                            | Some l -> l
                            | None ->
                                { Formation = formation
                                  Tactics = Balanced
                                  Instructions = Some TacticalInstructions.defaultInstructions
                                  Slots = [] }

                        Some
                            { Formation = formation
                              Tactics = lineup.Tactics
                              Instructions = lineup.Instructions
                              Slots = buildNewLineupSlots formation currentLineupOpt })
                    state.GameState

            { state with
                GameState = newGs
                SelectedTactics = formation },
            Cmd.none

        | SetTeamTactics tactics ->
            let clubId = state.GameState.UserClubId

            let newGs =
                GameState.updateLineup
                    clubId
                    (Option.map (fun lineup -> { lineup with Tactics = tactics }))
                    state.GameState

            { state with GameState = newGs }, Cmd.none

        | SetMentality value ->
            let clubId = state.GameState.UserClubId

            let newGs =
                GameState.updateLineup
                    clubId
                    (Option.map (fun lineup ->
                        let instructions =
                            lineup.Instructions
                            |> Option.defaultValue TacticalInstructions.defaultInstructions

                        { lineup with
                            Instructions = Some { instructions with Mentality = value } }))
                    state.GameState

            { state with GameState = newGs }, Cmd.none

        | SetDefensiveLine value ->
            let clubId = state.GameState.UserClubId

            let newGs =
                GameState.updateLineup
                    clubId
                    (Option.map (fun lineup ->
                        let instructions =
                            lineup.Instructions
                            |> Option.defaultValue TacticalInstructions.defaultInstructions

                        { lineup with
                            Instructions =
                                Some
                                    { instructions with
                                        DefensiveLine = value } }))
                    state.GameState

            { state with GameState = newGs }, Cmd.none

        | SetPressingIntensity value ->
            let clubId = state.GameState.UserClubId

            let newGs =
                GameState.updateLineup
                    clubId
                    (Option.map (fun lineup ->
                        let instructions =
                            lineup.Instructions
                            |> Option.defaultValue TacticalInstructions.defaultInstructions

                        { lineup with
                            Instructions =
                                Some
                                    { instructions with
                                        PressingIntensity = value } }))
                    state.GameState

            { state with GameState = newGs }, Cmd.none

        | SetPlayerTrainingSchedule(playerId, schedule) ->
            { state with
                GameState =
                    { state.GameState with
                        Players =
                            state.GameState.Players
                            |> Map.add
                                playerId
                                { state.GameState.Players[playerId] with
                                    TrainingSchedule = schedule } } },
            Cmd.none

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
                CurrentPage = HomePage },
            Cmd.none
