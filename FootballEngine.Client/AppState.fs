namespace FootballEngine

open System
open FootballEngine.Domain

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


    type SetupStep =
        | MainMenu
        | CountrySelection
        | ManagerNaming

    type Msg =

        | SelectPrimaryCountry of CountryCode
        | ToggleSecondaryCountry of CountryCode
        | UpdateManagerName of string
        | StartNewGame

        | GoToSetupStep of SetupStep

        | ChangePage of Page

        | AdvanceDay
        | AdvanceWeek

        | SimulateMatch
        | SimulateNextFixture
        | SimulateAllToday


        | SelectPlayer of PlayerId
        | DropPlayerInSlot of slotIndex: int * playerId: int
        | DragStartPlayer of PlayerId
        | SortPlayersBy of string


        | SaveGame
        | ChangeLeague of LeagueId
        | SetTactics of string

    type State =
        { GameState: GameState
          SelectedPlayer: Player option
          CurrentPage: Page
          LogMessages: string list
          SelectedTactics: string
          SelectedLeagueId: LeagueId
          DraggedPlayer: PlayerId option
          PlayerSortBy: string

          SetupSelectedCountry: CountryCode option
          SetupSecondaryCountries: CountryCode list
          SetupManagerName: string

          SetupStep: SetupStep }


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

    let private runSimulation gs fixtureId fixture =
        let updatedFixture, updatedHome, updatedAway =
            Engine.simulateFixture fixture gs.Clubs

        let hScore = updatedFixture.HomeScore |> Option.defaultValue 0
        let aScore = updatedFixture.AwayScore |> Option.defaultValue 0

        let newGs =
            { gs with
                Fixtures = gs.Fixtures |> Map.add fixtureId updatedFixture
                Clubs =
                    gs.Clubs
                    |> Map.add updatedHome.Id updatedHome
                    |> Map.add updatedAway.Id updatedAway }

        (newGs, $"⚽ {updatedHome.Name} {hScore}-{aScore} {updatedAway.Name}")

    let private simulateBatch gameState fixtures =
        fixtures
        |> List.fold
            (fun (gs, logs) (id, fixture) ->
                let newGs, log = runSimulation gs id fixture
                (newGs, log :: logs))
            (gameState, [])

    // === INIT ===

    let init () : State =
        let gameState =
            Db.loadGame ()
            |> Option.defaultValue
                { CurrentDate = DateTime.Now
                  Clubs = Map.empty
                  Players = Map.empty
                  Leagues = Map.empty
                  Fixtures = Map.empty
                  UserClubId = 0
                  ManagerName = ""
                  PrimaryCountry = "" }

        { GameState = gameState
          SelectedPlayer = None
          CurrentPage = if gameState.Clubs.IsEmpty then Setup else Home
          LogMessages = [ "Football Engine 2026 Initialized" ]
          SelectedTactics = "4-3-3"
          SelectedLeagueId = 1
          DraggedPlayer = None
          PlayerSortBy = "position"
          SetupSelectedCountry = None
          SetupSecondaryCountries = []
          SetupManagerName = ""
          SetupStep = MainMenu }


    let rec update (msg: Msg) (state: State) : State =
        match msg with
        | GoToSetupStep step -> { state with SetupStep = step }
        | SelectPrimaryCountry code ->
            { state with
                SetupSelectedCountry = Some code
                SetupSecondaryCountries = state.SetupSecondaryCountries |> List.filter ((<>) code) }

        | ToggleSecondaryCountry code ->
            if state.SetupSelectedCountry = Some code then
                state
            else
                { state with
                    SetupSecondaryCountries =
                        if List.contains code state.SetupSecondaryCountries then
                            state.SetupSecondaryCountries |> List.filter ((<>) code)
                        else
                            code :: state.SetupSecondaryCountries }

        | UpdateManagerName name -> { state with SetupManagerName = name }
        | StartNewGame ->
            match state.SetupSelectedCountry with
            | None -> state
            | Some primary ->
                let seedRnd = Random()

                let newGs =
                    Engine.generateNewGame seedRnd primary state.SetupManagerName state.SetupSecondaryCountries

                Db.saveGame newGs

                { state with
                    GameState = newGs
                    CurrentPage = Home
                    LogMessages = [ $"Career started by {state.SetupManagerName} in {primary}" ] }

        | ChangePage page -> { state with CurrentPage = page }
        | AdvanceDay ->
            let gs =
                { state.GameState with
                    CurrentDate = state.GameState.CurrentDate.AddDays(1.0) }

            let gs = updateDailyFitness gs
            let fixtures = getTodayFixturesWithId gs
            let finalGs, logs = simulateBatch gs fixtures

            Db.saveGame finalGs

            let dayLog = $"📅 {{finalGs.CurrentDate:yyyy-MM-dd}}"

            let allLogs =
                if logs.IsEmpty then
                    [ dayLog ]
                else
                    dayLog :: $"📊 {fixtures.Length} matches played" :: logs

            { state with
                GameState = finalGs
                LogMessages = allLogs @ state.LogMessages |> List.truncate 30 }

        | AdvanceWeek -> [ 1..7 ] |> List.fold (fun s _ -> update AdvanceDay s) state
        | SimulateNextFixture ->
            match getUserNextFixture state.GameState with
            | None -> state |> addLog "⚠️ No next fixture found"
            | Some(id, fixture) ->
                let newGs, log = runSimulation state.GameState id fixture
                Db.saveGame newGs
                { state with GameState = newGs } |> addLog $"🏁 {log}"
        | SimulateAllToday ->
            let fixtures = getTodayFixturesWithId state.GameState

            if fixtures.IsEmpty then
                state |> addLog "⚠️ No matches scheduled for today"
            else
                let finalGs, logs = simulateBatch state.GameState fixtures
                Db.saveGame finalGs

                { state with GameState = finalGs }
                |> addLog $"📊 {fixtures.Length} matches simulated"
                |> fun s ->
                    { s with
                        LogMessages = logs @ s.LogMessages |> List.truncate 30 }
        | SimulateMatch -> update SimulateNextFixture state
        | SelectPlayer pId ->
            { state with
                SelectedPlayer = state.GameState.Players.TryFind pId }
        | DragStartPlayer pId -> { state with DraggedPlayer = Some pId }
        | DropPlayerInSlot(targetIdx, pId) ->
            let team = state.GameState.Clubs[state.GameState.UserClubId]

            let createCompleteSlots (formationName: string) : LineupSlot list =
                let formationSlots = FormationData.getFormation formationName

                formationSlots
                |> List.map (fun fs ->
                    { Index = fs.Index
                      Role = fs.Role
                      X = fs.X
                      Y = fs.Y
                      PlayerId = None })
                |> List.sortBy (fun s -> s.Index)

            let lineup =
                team.CurrentLineup
                |> Option.defaultValue
                    { FormationName = state.SelectedTactics
                      TeamTactics = "Balanced"
                      Slots = createCompleteSlots state.SelectedTactics }

            let slotsMap = lineup.Slots |> List.map (fun s -> s.Index, s.PlayerId) |> Map.ofList



            let sourceIdx =
                slotsMap
                |> Map.tryPick (fun idx idOpt -> if idOpt = Some pId then Some idx else None)

            let occupant = slotsMap |> Map.tryFind targetIdx |> Option.flatten

            let newSlotsMap =
                match sourceIdx with
                | Some src ->
                    // Intercambiar: el jugador va a target, el ocupante va a source
                    slotsMap |> Map.add targetIdx (Some pId) |> Map.add src occupant
                | None ->
                    // El jugador viene del banquillo, lo colocamos en target, y el ocupante (si existe) queda libre
                    slotsMap
                    |> Map.add targetIdx (Some pId)
                    |> Map.map (fun idx idOpt -> if idx <> targetIdx && idOpt = Some pId then None else idOpt)

            let newSlots =
                newSlotsMap
                |> Map.toList
                |> List.map (fun (idx, pidOpt) ->
                    // El slot original siempre existe porque el lineup tiene todos los índices
                    let originalSlot = lineup.Slots |> List.find (fun s -> s.Index = idx)

                    { Index = idx
                      Role = originalSlot.Role
                      X = originalSlot.X
                      Y = originalSlot.Y
                      PlayerId = pidOpt })
                |> List.sortBy (fun s -> s.Index)

            let updatedTeam =
                { team with
                    CurrentLineup = Some { lineup with Slots = newSlots } }

            let newGameState =
                { state.GameState with
                    Clubs = state.GameState.Clubs.Add(team.Id, updatedTeam) }

            Db.saveGame newGameState

            { state with
                GameState = newGameState
                DraggedPlayer = None }
            |> addLog $"🔄 Swap made: Slot {targetIdx}"

        | SortPlayersBy sortBy -> { state with PlayerSortBy = sortBy }
        | SaveGame ->
            Db.saveGame state.GameState
            state |> addLog "💾 Game Saved"
        | ChangeLeague leagueId ->
            { state with
                SelectedLeagueId = leagueId }
        | SetTactics tactics -> { state with SelectedTactics = tactics }
