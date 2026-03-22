namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open FootballEngine.Engine
open AppTypes
open AppMsgs

module UpdateSim =

    let private addLog = AppTypes.addLog
    let private pushNotification = AppTypes.pushNotification
    let private saveCmd = SimHelpers.saveCmd
    let private primaryLeagueId = SimHelpers.primaryLeagueId

    let getUserNextFixture (gs: GameState) =
        gs.Competitions
        |> Map.toSeq
        |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
        |> Seq.tryPick (fun (id, f) ->
            if not f.Played && (f.HomeClubId = gs.UserClubId || f.AwayClubId = gs.UserClubId) then
                Some(id, f)
            else
                None)

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

    let private applyDayResult (result: DayResult) (state: State) =
        let logs =
            if result.Logs.IsEmpty then
                []
            else
                $"{result.PlayedFixtures.Length} matches played" :: result.Logs

        { state with
            GameState = result.GameState
            IsProcessing = false
            LogMessages = logs @ state.LogMessages |> List.truncate 30 }
        |> applyMatchNotifs result.GameState result.PlayedFixtures

    let private applySeasonResult (result: SeasonResult) (state: State) =
        let userLine =
            userFinishingLine result.SeasonFinalGs state.SelectedLeagueId result.NewGs.UserClubId

        let messages = $"Season {result.NewGs.Season} begins" :: userLine @ result.Summary
        let seasonBody = userLine |> String.concat " · "

        let nextState =
            { state with
                GameState = result.NewGs
                IsProcessing = false
                SelectedLeagueId = primaryLeagueId result.NewGs
                LogMessages = messages @ state.LogMessages |> List.truncate 50 }
            |> pushNotification SeasonEnd $"⚽ Season {result.NewGs.Season - 1} Complete" seasonBody

        match leagueChangeNotif state.GameState result.NewGs result.NewGs.UserClubId with
        | Some(title, body) -> nextState |> pushNotification SeasonEnd title body
        | None -> nextState

    let rec handle (msg: SimMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Advance days ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> advanceDays days state.GameState))
                    ()
                    (SimMsg << AdvanceDone)

        | AdvanceDone result ->
            let nextState = applyDayResult result state

            if result.SeasonComplete then
                nextState, Cmd.batch [ Cmd.ofMsg (SimMsg AdvanceSeason); saveCmd result.GameState ]
            else
                nextState, saveCmd result.GameState

        | SimulateNextFixture ->
            match getUserNextFixture state.GameState with
            | None -> state |> addLog "No next fixture found", Cmd.none
            | Some(id, fixture) ->
                match simulateFixture fixture state.GameState with
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

        | SimulateSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> simulateAndAdvanceSeason state.GameState))
                    ()
                    (SimMsg << SeasonAdvanceDone)

        | SeasonAdvanceDone(Ok result) -> applySeasonResult result state, saveCmd result.NewGs

        | SeasonAdvanceDone(Error NoFixturesToPlay) ->
            { state with IsProcessing = false } |> addLog "Season already complete", Cmd.none

        | SeasonAdvanceDone(Error(SimulationErrors msg)) ->
            { state with IsProcessing = false }
            |> pushNotification Info "Sim Season failed" msg,
            Cmd.none

        | AdvanceSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> advanceSeason state.GameState))
                    ()
                    (SimMsg << SeasonAdvanceDone << Ok)

        | SaveGame -> state, saveCmd state.GameState
