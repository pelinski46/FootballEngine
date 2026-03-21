namespace FootballEngine

open System
open System.Threading.Tasks
open Elmish
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Engine
open AppTypes
open AppMsgs

module UpdateSim =

    let private addLog = AppTypes.addLog
    let private pushNotification = AppTypes.pushNotification
    let private saveCmd = SimHelpers.saveCmd
    let private primaryLeagueId = SimHelpers.primaryLeagueId

    // ── GameState queries ────────────────────────────────────────────────────

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

    // ── Notifications ────────────────────────────────────────────────────────

    let private userMatchNotif (gs: GameState) (fixture: MatchFixture) (state: State) =
        match fixture.HomeScore, fixture.AwayScore with
        | Some h, Some a when fixture.HomeClubId = gs.UserClubId || fixture.AwayClubId = gs.UserClubId ->
            let isHome = fixture.HomeClubId = gs.UserClubId
            let userScore, oppScore = if isHome then h, a else a, h

            let clubName id =
                gs.Clubs |> Map.tryFind id |> Option.map _.Name |> Option.defaultValue "?"

            let outcome, emoji =
                if userScore > oppScore then "WIN", "🏆"
                elif userScore < oppScore then "LOSS", "😞"
                else "DRAW", "🤝"

            let goals =
                fixture.Events
                |> List.choose (fun ev ->
                    if ev.Type = Goal || ev.Type = OwnGoal then
                        gs.Players
                        |> Map.tryFind ev.PlayerId
                        |> Option.map (fun p ->
                            let suffix = if ev.Type = OwnGoal then " (OG)" else ""
                            $"{p.Name}{suffix} {ev.Second / 60}'")
                    else
                        None)

            let body =
                if goals.IsEmpty then
                    "No goal events recorded."
                else
                    String.concat " · " goals

            let title =
                $"{emoji} {outcome}  {clubName fixture.HomeClubId} {h}–{a} {clubName fixture.AwayClubId}"

            state |> pushNotification MatchResult title body
        | _ -> state

    let private applyMatchNotifs (gs: GameState) (fixtures: (MatchId * MatchFixture) list) (state: State) =
        fixtures |> List.fold (fun acc (_, f) -> userMatchNotif gs f acc) state

    // ── Season summary helpers ───────────────────────────────────────────────

    let private userFinishingLine (gs: GameState) (leagueId: CompetitionId) (userClubId: ClubId) =
        gs.Competitions
        |> Map.tryFind leagueId
        |> Option.bind (fun comp ->
            let ranked =
                comp.Standings
                |> Map.toList
                |> List.sortByDescending (fun (_, s) -> s.Points, s.Won)

            ranked
            |> List.tryFindIndex (fun (id, _) -> id = userClubId)
            |> Option.map (fun idx -> $"Season ended - finished {idx + 1}/{ranked.Length}"))
        |> Option.toList

    let private leagueChangeNotif (prevGs: GameState) (newGs: GameState) (userClubId: ClubId) =
        let levelOf (gs: GameState) =
            gs.Competitions
            |> Map.toList
            |> List.tryPick (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague(LeagueLevel lvl, _), Some _ when List.contains userClubId comp.ClubIds ->
                    Some(lvl, comp.Name)
                | _ -> None)

        match levelOf prevGs, levelOf newGs with
        | Some(old, _), Some(nw, league) when nw < old -> Some("🎉 Promoted!", $"Promoted to {league}")
        | Some(old, _), Some(nw, league) when nw > old -> Some("😰 Relegated", $"Relegated to {league}")
        | _ -> None

    // ── Handler ──────────────────────────────────────────────────────────────

    let rec handle (msg: SimMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | AdvanceDay ->
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
                            simulateFixtures gs fixtures, fixtures))
                    ()
                    (SimMsg << AdvanceDayDone)

        | AdvanceDayDone(result, fixtures) ->
            if not result.Errors.IsEmpty then
                let msg = result.Errors |> List.map (fun (_, e) -> string e) |> String.concat ", "

                { state with IsProcessing = false }
                |> pushNotification MatchResult "Simulation Error" msg,
                Cmd.none
            else
                let logs =
                    if result.Logs.IsEmpty then
                        []
                    else
                        $"{result.Logs.Length} matches played" :: result.Logs

                let seasonComplete = isSeasonComplete result.GameState

                let nextState =
                    { state with
                        GameState = result.GameState
                        IsProcessing = false
                        LogMessages = logs @ state.LogMessages |> List.truncate 30 }
                    |> applyMatchNotifs result.GameState fixtures

                if seasonComplete then
                    nextState, Cmd.batch [ Cmd.ofMsg (SimMsg AdvanceSeason); saveCmd result.GameState ]
                else
                    nextState, saveCmd result.GameState

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
                |> applyMatchNotifs result.GameState fixtures,
                saveCmd result.GameState

        | SimulateNextFixture ->
            match getUserNextFixture state.GameState with
            | None -> state |> addLog "No next fixture found", Cmd.none
            | Some(id, fixture) ->
                match simulateFixture fixture state.GameState.Clubs with
                | Error e -> state |> addLog $"Could not simulate fixture: {e}", Cmd.none
                | Ok(updatedFixture, h, a) ->
                    let home = state.GameState.Clubs[fixture.HomeClubId]
                    let away = state.GameState.Clubs[fixture.AwayClubId]

                    let newGs =
                        { state.GameState with
                            Competitions =
                                state.GameState.Competitions
                                |> Map.map (fun _ comp ->
                                    if comp.Fixtures.ContainsKey id then
                                        { comp with
                                            Fixtures = comp.Fixtures |> Map.add id updatedFixture }
                                    else
                                        comp) }

                    { state with GameState = newGs }
                    |> addLog $"{home.Name} {h}-{a} {away.Name}"
                    |> applyMatchNotifs newGs [ id, updatedFixture ],
                    saveCmd newGs

        | SimulateMatch -> handle SimulateNextFixture state

        | SimulateSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.either
                    (fun () ->
                        Task.Run(fun () ->
                            let gs = state.GameState

                            let allUnplayed =
                                gs.Competitions
                                |> Map.toSeq
                                |> Seq.collect (fun (_, comp) ->
                                    comp.Fixtures |> Map.toSeq |> Seq.filter (fun (_, f) -> not f.Played))
                                |> List.ofSeq

                            if allUnplayed.IsEmpty then
                                failwith "No unplayed fixtures — season already complete"

                            let result = simulateFixtures gs allUnplayed

                            if not result.Errors.IsEmpty then
                                result.Errors
                                |> List.map (fun (_, e) -> string e)
                                |> String.concat ", "
                                |> failwith

                            let lastMatchDate =
                                allUnplayed |> List.map (fun (_, f) -> f.ScheduledDate) |> List.max

                            let finalGs =
                                { result.GameState with
                                    CurrentDate = lastMatchDate }

                            let newGs =
                                finalGs
                                |> advanceSeason (Random())
                                |> regenerateSeasonFixtures
                                |> World.applyLeagueConsequences

                            computeSeasonSummary finalGs, finalGs, newGs))
                    ()
                    (SimMsg << SeasonAdvanceDone)
                    (fun ex -> SimMsg(SimulateSeasonFailed ex.Message))

        | SimulateSeasonFailed msg ->
            { state with IsProcessing = false }
            |> pushNotification Info "Sim Season failed" msg,
            Cmd.none

        | AdvanceSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () ->
                        Task.Run(fun () ->
                            let newGs =
                                state.GameState
                                |> advanceSeason (Random())
                                |> regenerateSeasonFixtures
                                |> World.applyLeagueConsequences

                            computeSeasonSummary state.GameState, state.GameState, newGs))
                    ()
                    (SimMsg << SeasonAdvanceDone)

        | SeasonAdvanceDone(summary, seasonFinalGs, newGs) ->
            let userLine =
                userFinishingLine seasonFinalGs state.SelectedLeagueId newGs.UserClubId

            let messages = $"Season {newGs.Season} begins" :: userLine @ summary
            let seasonBody = userLine |> String.concat " · "

            let nextState =
                { state with
                    GameState = newGs
                    IsProcessing = false
                    SelectedLeagueId = primaryLeagueId newGs
                    LogMessages = messages @ state.LogMessages |> List.truncate 50 }
                |> pushNotification SeasonEnd $"⚽ Season {newGs.Season - 1} Complete" seasonBody

            match leagueChangeNotif state.GameState newGs newGs.UserClubId with
            | Some(title, body) -> nextState |> pushNotification SeasonEnd title body
            | None -> nextState
            |> fun s -> s, saveCmd newGs

        | SaveGame -> state, saveCmd state.GameState
