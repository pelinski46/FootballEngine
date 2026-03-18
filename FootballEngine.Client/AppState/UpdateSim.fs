namespace FootballEngine

open System
open System.Threading.Tasks
open Elmish
open FootballEngine
open FootballEngine.Db
open FootballEngine.Domain
open FootballEngine.Engine
open AppTypes
open AppMsgs

module UpdateSim =

    let private addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let private pushNotification kind title body (state: State) =
        let note =
            { Id = state.NextNotificationId
              Kind = kind
              Title = title
              Body = body
              IsRead = false }

        { state with
            Notifications = note :: state.Notifications |> List.truncate 20
            NextNotificationId = state.NextNotificationId + 1 }

    let allFixtures (gs: GameState) =
        gs.Competitions
        |> Map.toSeq
        |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
        |> List.ofSeq

    let getTodayFixtures (gs: GameState) =
        allFixtures gs
        |> List.filter (fun (_, f) -> f.ScheduledDate.Date = gs.CurrentDate.Date && not f.Played)

    let getUserNextFixture (gs: GameState) =
        allFixtures gs
        |> List.tryPick (fun (id, f) ->
            if not f.Played && (f.HomeClubId = gs.UserClubId || f.AwayClubId = gs.UserClubId) then
                Some(id, f)
            else
                None)

    let private updateFixtureInState (gs: GameState) (fixtureId: MatchId) (updated: MatchFixture) =
        { gs with
            Competitions =
                gs.Competitions
                |> Map.map (fun _ comp ->
                    if Map.containsKey fixtureId comp.Fixtures then
                        { comp with
                            Fixtures = comp.Fixtures |> Map.add fixtureId updated }
                    else
                        comp) }

    let isSeasonComplete (gs: GameState) =
        let hasUnplayed (comp: Competition) =
            comp.Fixtures |> Map.exists (fun _ f -> not f.Played)

        gs.Competitions
        |> Map.tryFindKey (fun _ comp ->
            match comp.Type, comp.Country with
            | NationalLeague(LeagueLevel 0, _), Some c when c = gs.PrimaryCountry -> true
            | _ -> false)
        |> Option.map (fun id ->
            not (hasUnplayed gs.Competitions[id])
            && gs.Competitions
               |> Map.forall (fun _ comp ->
                   match comp.Type with
                   | NationalLeague _ -> not (hasUnplayed comp)
                   | _ -> true))
        |> Option.defaultValue false

    let private userFinishingLine (prevGs: GameState) (selectedLeagueId: CompetitionId) (userClubId: ClubId) =
        prevGs.Competitions
        |> Map.tryFind selectedLeagueId
        |> Option.bind (fun comp ->
            let ranked =
                comp.Standings
                |> Map.toList
                |> List.sortByDescending (fun (_, s) -> s.Points, s.Won)

            ranked
            |> List.tryFindIndex (fun (id, _) -> id = userClubId)
            |> Option.map (fun idx -> $"Season ended - finished {idx + 1}/{ranked.Length}"))
        |> Option.toList

    let private userMatchNotification (gs: GameState) (fixture: MatchFixture) (hScore: int) (aScore: int) =
        let involvesUser =
            fixture.HomeClubId = gs.UserClubId || fixture.AwayClubId = gs.UserClubId

        if not involvesUser then
            None
        else
            let clubName id =
                gs.Clubs |> Map.tryFind id |> Option.map _.Name |> Option.defaultValue "?"

            let homeName = clubName fixture.HomeClubId
            let awayName = clubName fixture.AwayClubId
            let isHome = fixture.HomeClubId = gs.UserClubId
            let userScore = if isHome then hScore else aScore
            let oppScore = if isHome then aScore else hScore

            let outcome, emoji =
                if userScore > oppScore then "WIN", "🏆"
                elif userScore < oppScore then "LOSS", "😞"
                else "DRAW", "🤝"

            let title = $"{emoji} {outcome}  {homeName} {hScore}–{aScore} {awayName}"

            let goals =
                fixture.Events
                |> List.choose (fun ev ->
                    if ev.Type = Goal || ev.Type = OwnGoal then
                        gs.Players
                        |> Map.tryFind ev.PlayerId
                        |> Option.map (fun p ->
                            let min = ev.Second / 60
                            let suffix = if ev.Type = OwnGoal then " (OG)" else ""
                            $"{p.Name}{suffix} {min}'")
                    else
                        None)

            let body =
                if goals.IsEmpty then
                    "No goal events recorded."
                else
                    String.concat " · " goals

            Some(title, body)

    let private applyUserMatchNotif (gs: GameState) (fixtures: (MatchId * MatchFixture) list) (state: State) =
        fixtures
        |> List.fold
            (fun acc (_, fixture) ->
                match fixture.HomeScore, fixture.AwayScore with
                | Some h, Some a ->
                    match userMatchNotification gs fixture h a with
                    | Some(title, body) -> acc |> pushNotification MatchResult title body
                    | None -> acc
                | _ -> acc)
            state

    // Save helper that never touches IsProcessing on success — only resets on failure.
    // This prevents a failed save from leaving the button stuck in "SIMULATING...".
    let private saveCmd gs =
        Cmd.OfTask.attempt saveGameAsync gs (fun _ -> SetProcessing false)

    let rec handle (msg: SimMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | AdvanceDay ->
            // Guard: ignore if already processing — prevents double-dispatch from rapid clicks.
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () ->
                        Task.Run(fun () ->
                            let gs =
                                { state.GameState with
                                    CurrentDate = state.GameState.CurrentDate.AddDays(1.0) }

                            let fixtures = getTodayFixtures gs
                            let result = simulateFixtures gs fixtures
                            result, fixtures))
                    ()
                    (SimMsg << AdvanceDayDone)

        | AdvanceDayDone(result, fixtures) ->
            let errorLogs =
                result.Errors |> List.map (fun (id, e) -> $"Fixture {id} skipped: {e}")

            let logs =
                if result.Logs.IsEmpty && result.Errors.IsEmpty then
                    []
                else
                    $"{result.Logs.Length} matches played" :: result.Logs @ errorLogs

            let nextState =
                { state with
                    GameState = result.GameState
                    // Only clear IsProcessing when there is NO season advance pending.
                    // AdvanceSeason will own IsProcessing from here until SeasonAdvanceDone.
                    IsProcessing = isSeasonComplete result.GameState
                    LogMessages = logs @ state.LogMessages |> List.truncate 30 }
                |> applyUserMatchNotif result.GameState fixtures

            // If the season just finished, chain directly into AdvanceSeason without saving
            // first — avoids two concurrent writes and a second IsProcessing flip.
            if isSeasonComplete result.GameState then
                nextState, Cmd.batch [ Cmd.ofMsg (SimMsg AdvanceSeason); saveCmd result.GameState ]
            else
                { nextState with IsProcessing = false }, saveCmd result.GameState

        | SimulateAllToday ->
            let fixtures = getTodayFixtures state.GameState

            if fixtures.IsEmpty then
                state |> addLog "No matches scheduled for today", Cmd.none
            else
                let result = simulateFixtures state.GameState fixtures

                let errorLogs =
                    result.Errors |> List.map (fun (id, e) -> $"Fixture {id} skipped: {e}")

                let newLogs =
                    ($"{result.Logs.Length} matches simulated" :: result.Logs @ errorLogs)
                    @ state.LogMessages
                    |> List.truncate 30

                { state with
                    GameState = result.GameState
                    LogMessages = newLogs }
                |> applyUserMatchNotif result.GameState fixtures,
                saveCmd result.GameState

        | SimulateNextFixture ->
            match getUserNextFixture state.GameState with
            | None -> state |> addLog "No next fixture found", Cmd.none
            | Some(id, fixture) ->
                match simulateFixture fixture state.GameState.Clubs with
                | Error e -> state |> addLog $"Could not simulate fixture: {e}", Cmd.none
                | Ok(updatedFixture, hScore, aScore) ->
                    let home = state.GameState.Clubs[fixture.HomeClubId]
                    let away = state.GameState.Clubs[fixture.AwayClubId]
                    let newGs = updateFixtureInState state.GameState id updatedFixture

                    { state with GameState = newGs }
                    |> addLog $"{home.Name} {hScore}-{aScore} {away.Name}"
                    |> applyUserMatchNotif newGs [ id, updatedFixture ],
                    saveCmd newGs

        | SimulateMatch -> handle SimulateNextFixture state

        | AdvanceSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () ->
                        Task.Run(fun () ->
                            let rng = Random()
                            let summary = computeSeasonSummary state.GameState
                            let newGs = state.GameState |> advanceSeason rng |> regenerateSeasonFixtures
                            summary, newGs))
                    ()
                    (SimMsg << SeasonAdvanceDone)

        | SeasonAdvanceDone(summary, newGs) ->
            let userLine =
                userFinishingLine state.GameState state.SelectedLeagueId newGs.UserClubId

            let messages = $"Season {newGs.Season} begins" :: userLine @ summary
            let seasonBody = userLine |> String.concat " · "

            { state with
                GameState = newGs
                IsProcessing = false
                LogMessages = messages @ state.LogMessages |> List.truncate 50 }
            |> pushNotification SeasonEnd $"⚽ Season {newGs.Season - 1} Complete" seasonBody,
            saveCmd newGs

        | SaveGame -> state, saveCmd state.GameState
