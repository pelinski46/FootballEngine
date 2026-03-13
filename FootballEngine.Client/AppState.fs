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


        | SelectPlayer of PlayerId
        | DropPlayerInSlot of slotIndex: int * playerId: int
        | SortPlayersBy of string

        | SelectMatchLabHome of ClubId
        | SelectMatchLabAway of ClubId
        | RunMatchLab
        | SetMatchLabSnapshot of int
        | SaveGame
        | ChangeLeague of LeagueId
        | SetTactics of Formation

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
          MatchLabSnapshot: int }


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

    let private updateDailyFitness (gameState: GameState) =
        { gameState with
            Players =
                gameState.Players
                |> Map.map (fun _ p ->
                    { p with
                        MatchFitness = Math.Clamp(p.MatchFitness + 5, 0, 100) }) }


    let private saveAsync (gs: GameState) =
        Task.Run(fun () -> Db.saveGame gs) |> ignore


    // === INIT ===

    let init () =
        let gameState =
            Db.loadGame ()
            |> Option.defaultValue
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

        { GameState = gameState
          SelectedPlayer = None
          CurrentPage = if gameState.Clubs.IsEmpty then Setup else Home
          LogMessages = [ "Football Engine 2026 Initialized" ]
          SelectedTactics = F433
          SelectedLeagueId = 1
          DraggedPlayer = None
          PlayerSortBy = "position"
          IsProcessing = false
          SetupSelectedCountry = None
          SetupSecondaryCountries = []
          SetupManagerName = ""
          SetupStep = MainMenu
          MatchLabHome = None
          MatchLabAway = None
          MatchLabResult = None
          MatchLabSnapshot = 0 },
        Cmd.none


    let rec update (msg: Msg) (state: State) =
        match msg with
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
                let seedRnd = Random()

                let newGs =
                    generateNewGame seedRnd primary state.SetupManagerName state.SetupSecondaryCountries

                Db.saveGame newGs

                { state with
                    GameState = newGs
                    SetupStep = ClubSelection },
                Cmd.none

        | ChangePage page -> { state with CurrentPage = page }, Cmd.none
        | SetProcessing processing -> { state with IsProcessing = processing }, Cmd.none
        | AdvanceDay ->
            { state with IsProcessing = true },
            Cmd.OfTask.perform
                (fun () ->
                    Task.Run(fun () ->
                        let gs =
                            { state.GameState with
                                CurrentDate = state.GameState.CurrentDate.AddDays(1.0) }

                        let fixtures = getTodayFixturesWithId gs
                        // Return the GameState + fixture list; Engine.simulateFixtures
                        // runs in AdvanceDayDone so error handling stays in one place.
                        gs, fixtures))
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

            let updatedLineup = Lineup.swapPlayer targetIdx pId lineup

            let updatedTeam =
                { team with
                    CurrentLineup = Some updatedLineup }

            let newGameState =
                { state.GameState with
                    Clubs = state.GameState.Clubs.Add(team.Id, updatedTeam) }

            Db.saveGame newGameState

            { state with
                GameState = newGameState
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

        | SetMatchLabSnapshot i -> { state with MatchLabSnapshot = i }, Cmd.none
