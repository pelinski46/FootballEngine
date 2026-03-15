namespace FootballEngine

open System
open System.Threading.Tasks
open Elmish
open FootballEngine
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FootballEngine.Engine
open FootballEngine.MatchContext

module AppState =

    type Page =
        | Setup
        | Home
        | Inbox
        | Squad
        | Tactics
        | Training
        | Scouting
        | Transfers
        | Finances
        | Settings
        | MatchLab

    type SetupStep =
        | MainMenu
        | CountrySelection
        | ClubSelection
        | ManagerNaming

    type TransferTab =
        | MarketSearch
        | MyWatchlist
        | IncomingOffers
        | OutgoingOffers
        | TransferHistory

    type TransferFilter =
        | AllPositions
        | Goalkeepers
        | Defenders
        | Midfielders
        | Attackers

    type SortField =
        | ByName
        | ByCA
        | ByValue
        | ByAge
        | ByPosition

    type TransferState =
        { ActiveTab: TransferTab
          SearchQuery: string
          PositionFilter: TransferFilter
          SortBy: SortField
          SortAsc: bool
          SelectedPlayerId: PlayerId option
          WatchlistIds: PlayerId list
          CachedPlayers: Player list
          ClubNameCache: Map<PlayerId, string>
          IsLoading: bool
          Page: int }

    let initTransferState =
        { ActiveTab = MarketSearch
          SearchQuery = ""
          PositionFilter = AllPositions
          SortBy = ByCA
          SortAsc = false
          SelectedPlayerId = None
          WatchlistIds = []
          CachedPlayers = []
          ClubNameCache = Map.empty
          IsLoading = false
          Page = 0 }

    type Msg =
        | SelectPrimaryCountry of CountryCode
        | ToggleSecondaryCountry of CountryCode
        | UpdateManagerName of string
        | StartNewGame

        | GoToSetupStep of SetupStep
        | ConfirmClub of ClubId
        | ChangePage of Page

        | SetProcessing of bool
        | AdvanceDay
        | AdvanceDayDone of GameState * (MatchId * MatchFixture) list

        | SimulateMatch
        | SimulateNextFixture
        | SimulateAllToday

        | StepMatchLabSnapshot of int
        | SelectPlayer of PlayerId
        | DropPlayerInSlot of slotIndex: int * playerId: int
        | SortPlayersBy of string

        | SelectMatchLabHome of ClubId
        | SelectMatchLabAway of ClubId
        | RunMatchLab
        | SaveGame
        | ChangeLeague of LeagueId
        | SetTactics of Formation

        | GameLoaded of GameState option

        | LoadTransfers
        | TransfersLoaded of players: Player list * clubNames: Map<PlayerId, string>
        | TransferTabChange of TransferTab
        | TransferSearch of string
        | TransferFilterChange of TransferFilter
        | TransferSortChange of SortField
        | TransferPlayerSelect of PlayerId
        | TransferWatchToggle of PlayerId
        | TransferPageChange of int

    type State =
        { GameState: GameState
          SelectedPlayer: Player option
          CurrentPage: Page
          LogMessages: string list
          SelectedTactics: Formation
          SelectedLeagueId: LeagueId
          DraggedPlayer: PlayerId option
          PlayerSortBy: string
          IsProcessing: bool
          SetupSelectedCountry: CountryCode option
          SetupSecondaryCountries: CountryCode list
          SetupManagerName: string
          SetupStep: SetupStep
          MatchLabHome: ClubId option
          MatchLabAway: ClubId option
          MatchLabResult: MatchReplay option
          MatchLabSnapshot: int
          TransferState: TransferState }

    let private emptyGameState () =
        { CurrentDate = DateTime.Now
          Season = DateTime.Now.Year
          Clubs = Map.empty
          Players = Map.empty
          Competitions = Map.empty
          KnockoutTies = Map.empty
          Countries = Map.empty
          Fixtures = Map.empty
          UserClubId = 0
          ManagerName = ""
          PrimaryCountry = "" }

    let private initialState gs =
        { GameState = gs
          SelectedPlayer = None
          CurrentPage = if gs.Clubs.IsEmpty then Setup else Home
          LogMessages = [ "Football Engine 2026 Initialized" ]
          SelectedTactics = F433
          SelectedLeagueId = 1
          DraggedPlayer = None
          PlayerSortBy = "position"
          IsProcessing = true
          SetupSelectedCountry = None
          SetupSecondaryCountries = []
          SetupManagerName = ""
          SetupStep = MainMenu
          MatchLabHome = None
          MatchLabAway = None
          MatchLabResult = None
          MatchLabSnapshot = 0
          TransferState = initTransferState }

    let private addLog msg state =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let getTodayFixturesWithId (gameState: GameState) =
        gameState.Fixtures
        |> Map.filter (fun _ f -> f.ScheduledDate.Date = gameState.CurrentDate.Date && not f.Played)
        |> Map.toList

    let getUserNextFixture (gameState: GameState) =
        gameState.Fixtures
        |> Map.tryPick (fun id f ->
            if
                not f.Played
                && (f.HomeClubId = gameState.UserClubId || f.AwayClubId = gameState.UserClubId)
            then
                Some(id, f)
            else
                None)

    let private saveAsync (gs: GameState) =
        Task.Run(fun () -> Db.saveGame gs) |> ignore

    let private buildTransferCache (gs: GameState) =
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

    let init () =
        let loadCmd =
            Cmd.OfTask.perform (fun () -> Task.Run(fun () -> Db.loadGame ())) () GameLoaded

        initialState (emptyGameState ()), loadCmd

    let rec update (msg: Msg) (state: State) =
        match msg with
        | GameLoaded result ->
            let gs = result |> Option.defaultValue (emptyGameState ())

            { state with
                GameState = gs
                CurrentPage = if gs.Clubs.IsEmpty then Setup else Home
                IsProcessing = false },
            Cmd.none

        | GoToSetupStep step -> { state with SetupStep = step }, Cmd.none

        | SelectPrimaryCountry code ->
            { state with
                SetupSelectedCountry = Some code
                SetupSecondaryCountries = state.SetupSecondaryCountries |> List.filter ((<>) code) },
            Cmd.none

        | ToggleSecondaryCountry code ->
            if state.SetupSelectedCountry = Some code then
                state, Cmd.none
            else
                { state with
                    SetupSecondaryCountries =
                        if List.contains code state.SetupSecondaryCountries then
                            state.SetupSecondaryCountries |> List.filter ((<>) code)
                        else
                            code :: state.SetupSecondaryCountries },
                Cmd.none

        | UpdateManagerName name -> { state with SetupManagerName = name }, Cmd.none

        | StartNewGame ->
            match state.SetupSelectedCountry with
            | None -> state, Cmd.none
            | Some primary ->
                let newGs =
                    generateNewGame (Random()) primary state.SetupManagerName state.SetupSecondaryCountries

                Db.saveGame newGs

                { state with
                    GameState = newGs
                    SetupStep = ClubSelection },
                Cmd.none

        | ChangePage page ->
            let cmd =
                if page = Transfers && state.TransferState.CachedPlayers.IsEmpty then
                    Cmd.ofMsg LoadTransfers
                else
                    Cmd.none

            { state with CurrentPage = page }, cmd

        | SetProcessing processing -> { state with IsProcessing = processing }, Cmd.none

        | AdvanceDay ->
            { state with IsProcessing = true },
            Cmd.OfTask.perform
                (fun () ->
                    Task.Run(fun () ->
                        let gs =
                            { state.GameState with
                                CurrentDate = state.GameState.CurrentDate.AddDays(1.0) }

                        gs, getTodayFixturesWithId gs))
                ()
                AdvanceDayDone

        | AdvanceDayDone(gs, fixtures) ->
            let result = simulateFixtures gs fixtures
            saveAsync result.GameState

            let errorLogs =
                result.Errors |> List.map (fun (id, e) -> $"⚠️ Fixture {id} skipped: {e}")

            let allLogs =
                if result.Logs.IsEmpty && errorLogs.IsEmpty then
                    []
                else
                    $"📊 {result.Logs.Length} matches played" :: (result.Logs @ errorLogs)

            { state with
                GameState = result.GameState
                IsProcessing = false
                LogMessages = allLogs @ state.LogMessages |> List.truncate 30 },
            Cmd.none

        | SimulateAllToday ->
            let fixtures = getTodayFixturesWithId state.GameState

            if fixtures.IsEmpty then
                state |> addLog "⚠️ No matches scheduled for today", Cmd.none
            else
                let result = simulateFixtures state.GameState fixtures
                Db.saveGame result.GameState

                let errorLogs =
                    result.Errors |> List.map (fun (id, e) -> $"⚠️ Fixture {id} skipped: {e}")

                { state with
                    GameState = result.GameState }
                |> addLog $"📊 {result.Logs.Length} matches simulated"
                |> fun s ->
                    { s with
                        LogMessages = (result.Logs @ errorLogs) @ s.LogMessages |> List.truncate 30 },
                    Cmd.none

        | SimulateNextFixture ->
            match getUserNextFixture state.GameState with
            | None -> state |> addLog "⚠️ No next fixture found", Cmd.none
            | Some(id, fixture) ->
                match simulateFixture fixture state.GameState.Clubs with
                | Error e -> state |> addLog $"⚠️ Could not simulate fixture: {e}", Cmd.none
                | Ok(updatedFixture, updatedHome, updatedAway) ->
                    let h = updatedFixture.HomeScore |> Option.defaultValue 0
                    let a = updatedFixture.AwayScore |> Option.defaultValue 0

                    let newGs =
                        { state.GameState with
                            Fixtures = state.GameState.Fixtures |> Map.add id updatedFixture
                            Clubs =
                                state.GameState.Clubs
                                |> Map.add updatedHome.Id updatedHome
                                |> Map.add updatedAway.Id updatedAway }

                    Db.saveGame newGs

                    { state with GameState = newGs }
                    |> addLog $"🏁 {updatedHome.Name} {h}-{a} {updatedAway.Name}",
                    Cmd.none

        | SimulateMatch -> update SimulateNextFixture state

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

            Db.saveGame newGs

            { state with
                GameState = newGs
                DraggedPlayer = None }
            |> addLog $"🔄 Swap made: Slot {targetIdx}",
            Cmd.none

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }, Cmd.none

        | SaveGame ->
            Db.saveGame state.GameState
            state |> addLog "💾 Game Saved", Cmd.none

        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId },
            Cmd.none

        | SetTactics formation ->
            let userClub = state.GameState.Clubs[state.GameState.UserClubId]

            let newSlots =
                match userClub.CurrentLineup with
                | None ->
                    FormationData.getFormation formation
                    |> List.map (fun s ->
                        { Index = s.Index
                          Role = s.Role
                          X = s.X
                          Y = s.Y
                          PlayerId = None })
                | Some lineup ->
                    let newFormationSlots = FormationData.getFormation formation

                    let assignedPairs =
                        lineup.Slots
                        |> List.choose (fun s -> s.PlayerId |> Option.map (fun pid -> s.Role, pid))

                    let mutable remaining = assignedPairs

                    newFormationSlots
                    |> List.map (fun fs ->
                        let found = remaining |> List.tryFind (fun (role, _) -> role = fs.Role)

                        match found with
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

            saveAsync newGs

            { state with
                GameState = newGs
                SelectedTactics = formation },
            Cmd.none

        | ConfirmClub clubId ->
            let newGs =
                { state.GameState with
                    UserClubId = clubId }

            Db.saveGame newGs

            { state with
                GameState = newGs
                CurrentPage = Home
                LogMessages = [ $"Career started by {state.SetupManagerName}" ] },
            Cmd.none

        | SelectMatchLabHome id -> { state with MatchLabHome = Some id }, Cmd.none
        | SelectMatchLabAway id -> { state with MatchLabAway = Some id }, Cmd.none

        | RunMatchLab ->
            match state.MatchLabHome, state.MatchLabAway with
            | Some hId, Some aId ->
                match MatchSimulator.trySimulateMatchFull state.GameState.Clubs[hId] state.GameState.Clubs[aId] with
                | Ok replay ->
                    { state with
                        MatchLabResult = Some replay
                        MatchLabSnapshot = 0 },
                    Cmd.none
                | Error e -> state |> addLog $"⚠️ {e}", Cmd.none
            | _ -> state |> addLog "⚠️ Select both clubs", Cmd.none

        | StepMatchLabSnapshot delta ->
            let total =
                state.MatchLabResult
                |> Option.map (fun r -> r.Snapshots.Length - 1)
                |> Option.defaultValue 0

            { state with
                MatchLabSnapshot = max 0 (min total (state.MatchLabSnapshot + delta)) },
            Cmd.none

        | LoadTransfers ->
            { state with
                TransferState =
                    { state.TransferState with
                        IsLoading = true } },
            Cmd.OfTask.perform (fun () -> buildTransferCache state.GameState) () (fun (players, cache) ->
                TransfersLoaded(players, cache))

        | TransfersLoaded(players, cache) ->
            { state with
                TransferState =
                    { state.TransferState with
                        CachedPlayers = players
                        ClubNameCache = cache
                        IsLoading = false } },
            Cmd.none

        | TransferTabChange tab ->
            { state with
                TransferState =
                    { state.TransferState with
                        ActiveTab = tab
                        Page = 0 } },
            Cmd.none

        | TransferSearch query ->
            { state with
                TransferState =
                    { state.TransferState with
                        SearchQuery = query
                        Page = 0 } },
            Cmd.none

        | TransferFilterChange f ->
            { state with
                TransferState =
                    { state.TransferState with
                        PositionFilter = f
                        Page = 0 } },
            Cmd.none

        | TransferSortChange s ->
            let asc =
                if state.TransferState.SortBy = s then
                    not state.TransferState.SortAsc
                else
                    false

            { state with
                TransferState =
                    { state.TransferState with
                        SortBy = s
                        SortAsc = asc
                        Page = 0 } },
            Cmd.none

        | TransferPlayerSelect pid ->
            { state with
                TransferState =
                    { state.TransferState with
                        SelectedPlayerId = Some pid } },
            Cmd.none

        | TransferWatchToggle pid ->
            let ids = state.TransferState.WatchlistIds

            let updated =
                if List.contains pid ids then
                    List.filter ((<>) pid) ids
                else
                    pid :: ids

            { state with
                TransferState =
                    { state.TransferState with
                        WatchlistIds = updated } },
            Cmd.none

        | TransferPageChange p ->
            { state with
                TransferState = { state.TransferState with Page = p } },
            Cmd.none
