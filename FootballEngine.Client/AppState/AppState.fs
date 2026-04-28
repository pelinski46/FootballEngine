namespace FootballEngine

open Elmish
open FootballEngine.AppMsgs
open FootballEngine.Domain
open AppTypes
open FootballEngine.World
open FootballEngine.Data


module AppState =

    let private initialState () : State =
        { Mode = Initializing
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
          ModEditor = ModEditorTypes.initModEditorState
          ActiveMatchReplay = None
          ActiveMatchSnapshot = 0
          IsPlaying = false
          PlaybackSpeed = 20
          InterpolationT = 0.0
          Inbox = initInboxState
          PrevUserClubSkills = None
          PrevUserClubStatus = None
          RenderAccumulator = 0.0
          WorldClock = WorldClockOps.init 1
          ModLoadErrors = [] }

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
        initialState (), loadCmd

    let update (msg: Msg) (state: State) : State * Cmd<Msg> =
        let withGame (f: GameState -> State * Cmd<Msg>) =
            match state.Mode with
            | InGame(gs, _) -> f gs
            | _ -> state, Cmd.none

        match msg with
        | SetupMsg m -> UpdateSetup.handle m state

        | ModEditorMsg m ->
            let nextEditorState, cmd = UpdateModEditor.update m state.ModEditor

            { state with
                ModEditor = nextEditorState },
            Cmd.map ModEditorMsg cmd

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
                withGame (fun gs ->
                    let newGs = GameState.markMessageAsRead messageId gs

                    { state with
                        Mode = InGame(newGs, managerEmployment newGs)
                        Inbox = { SelectedMessageId = Some messageId } },
                    SimHelpers.saveCmd newGs state.WorldClock)

            | MarkAsRead messageId ->
                withGame (fun gs ->
                    let newGs = GameState.markMessageAsRead messageId gs

                    { state with
                        Mode = InGame(newGs, managerEmployment newGs) },
                    SimHelpers.saveCmd newGs state.WorldClock)

            | MarkActionTaken messageId ->
                withGame (fun gs ->
                    let newGs = GameState.markMessageActionTaken messageId gs

                    { state with
                        Mode = InGame(newGs, managerEmployment newGs) },
                    SimHelpers.saveCmd newGs state.WorldClock)

        | GameLoaded result ->
            match result with
            | Some(gs, clock) ->
                let leagueId = SimHelpers.primaryLeagueId gs

                match ModLoader.loadAll ModPaths.builtinsDir ModPaths.modsDir with
                | Ok data ->
                    DataRegistry.setLoadedData data
                    { state with
                        Mode = InGame(gs, managerEmployment gs)
                        CurrentPage = HomePage
                        SelectedLeagueId = leagueId
                        IsProcessing = false
                        WorldClock = clock
                        ModLoadErrors = data.Errors |> List.map string },
                    Cmd.none
                | Error errs ->
                    { state with
                        Mode = InGame(gs, managerEmployment gs)
                        CurrentPage = HomePage
                        SelectedLeagueId = leagueId
                        IsProcessing = false
                        WorldClock = clock
                        ModLoadErrors = errs |> List.map string },
                    Cmd.none
            | None ->
                { state with
                    Mode = NoSave
                    CurrentPage = Setup
                    IsProcessing = false },
                Cmd.none

        | ChangePage page ->
            let cmd =
                if page = Transfers && state.Transfer.CachedPlayers.IsEmpty then
                    Cmd.ofMsg (TransferMsg Load)
                else
                    Cmd.none

            let saveCmdVal =
                match state.Mode with
                | InGame(gs, _) ->
                    if state.CurrentPage = Tactics && page <> Tactics then
                        SimHelpers.saveCmd gs state.WorldClock
                    elif state.CurrentPage = Training && page <> Training then
                        SimHelpers.saveCmd gs state.WorldClock
                    else
                        Cmd.none
                | _ -> Cmd.none

            { state with CurrentPage = page }, Cmd.batch [ cmd; saveCmdVal ]

        | SelectPlayer pId -> { state with SelectedPlayer = Some pId }, Cmd.none

        | DropPlayerInSlot(targetIdx, pId) ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs)
                    DraggedPlayer = None }
                |> addLog $"Swap made: Slot {targetIdx}",
                Cmd.none)

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }, Cmd.none

        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId },
            Cmd.none

        | SetTactics formation ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs)
                    SelectedTactics = formation },
                Cmd.none)

        | SetTeamTactics tactics ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

                let newGs =
                    GameState.updateLineup clubId (Option.map (fun lineup -> { lineup with Tactics = tactics })) gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetMentality value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

                let newGs =
                    GameState.updateLineup
                        clubId
                        (Option.map (fun lineup ->
                            let instructions =
                                lineup.Instructions
                                |> Option.defaultValue TacticalInstructions.defaultInstructions

                            { lineup with
                                Instructions = Some { instructions with Mentality = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetDefensiveLine value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetPressingIntensity value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetWidth value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

                let newGs =
                    GameState.updateLineup
                        clubId
                        (Option.map (fun lineup ->
                            let instructions =
                                lineup.Instructions
                                |> Option.defaultValue TacticalInstructions.defaultInstructions

                            { lineup with
                                Instructions = Some { instructions with Width = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetTempo value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

                let newGs =
                    GameState.updateLineup
                        clubId
                        (Option.map (fun lineup ->
                            let instructions =
                                lineup.Instructions
                                |> Option.defaultValue TacticalInstructions.defaultInstructions

                            { lineup with
                                Instructions = Some { instructions with Tempo = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetDirectness value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

                let newGs =
                    GameState.updateLineup
                        clubId
                        (Option.map (fun lineup ->
                            let instructions =
                                lineup.Instructions
                                |> Option.defaultValue TacticalInstructions.defaultInstructions

                            { lineup with
                                Instructions = Some { instructions with Directness = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetPressTriggerZone value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                                            PressTriggerZone = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetDefensiveShape value ->
            withGame (fun gs ->
                let clubId = gs.UserClubId

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
                                            DefensiveShape = value } }))
                        gs

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetPlayerTrainingSchedule(playerId, schedule) ->
            withGame (fun gs ->
                let newGs =
                    { gs with
                        Players =
                            gs.Players
                            |> Map.add
                                playerId
                                { gs.Players[playerId] with
                                    TrainingSchedule = schedule } }

                { state with
                    Mode = InGame(newGs, managerEmployment newGs) },
                Cmd.none)

        | SetProcessing b -> { state with IsProcessing = b }, Cmd.none
        | NoOp -> state, Cmd.none

        | StepActiveMatch delta ->
            let total =
                state.ActiveMatchReplay
                |> Option.map (fun r -> r.Snapshots.Length - 1)
                |> Option.defaultValue 0

            { state with
                ActiveMatchSnapshot = max 0 (min total (state.ActiveMatchSnapshot + delta))
                RenderAccumulator = 0.0
                InterpolationT = 0.0 },
            Cmd.none

        | CloseActiveMatch ->
            { state with
                ActiveMatchReplay = None
                ActiveMatchSnapshot = 0
                CurrentPage = HomePage
                IsPlaying = false
                RenderAccumulator = 0.0
                InterpolationT = 0.0 },
            Cmd.none

        | TogglePlayback ->
            { state with
                IsPlaying = not state.IsPlaying },
            Cmd.none

        | SetPlaybackSpeed speed -> { state with PlaybackSpeed = speed }, Cmd.none

        | TickInterpolation ->
            if not state.IsPlaying then
                state, Cmd.none
            else
                let dt = 16.67 // 60 FPS
                let newAccumulator = state.RenderAccumulator + dt
                let snapInterval = 125.0 / float state.PlaybackSpeed // ms between snapshots (each snapshot = 125ms game time = 5 subticks at 40Hz)

                if newAccumulator >= snapInterval then
                    let total =
                        state.ActiveMatchReplay
                        |> Option.map (fun r -> r.Snapshots.Length - 1)
                        |> Option.defaultValue 0

                    let newSnap = min total (state.ActiveMatchSnapshot + 1)
                    let shouldStop = newSnap >= total

                    { state with
                        RenderAccumulator = newAccumulator - snapInterval
                        InterpolationT = 0.0
                        ActiveMatchSnapshot = newSnap
                        IsPlaying = not shouldStop },
                    Cmd.none
                else
                    { state with
                        RenderAccumulator = newAccumulator
                        InterpolationT = newAccumulator / snapInterval },
                    Cmd.none

        | ReloadMods ->
            match ModLoader.loadAll ModPaths.builtinsDir ModPaths.modsDir with
            | Ok data ->
                DataRegistry.setLoadedData data
                { state with
                    ModLoadErrors = data.Errors |> List.map string },
                Cmd.none
            | Error errs ->
                { state with
                    ModLoadErrors = errs |> List.map string },
                Cmd.none
