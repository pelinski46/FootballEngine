namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open FootballEngine.Engine
open FootballEngine.Icons
open AppTypes
open AppMsgs

module UpdateSim =

    let private addLog = AppTypes.addLog
    let private pushNotification = AppTypes.pushNotification
    let private saveCmd = SimHelpers.saveCmd
    let private primaryLeagueId = SimHelpers.primaryLeagueId

    let private userMatchNotif (gs: GameState) (fixture: MatchFixture) (state: State) =
        match fixture.HomeScore, fixture.AwayScore with
        | Some h, Some a when MatchFixture.involves gs.UserClubId fixture ->
            let isHome = fixture.HomeClubId = gs.UserClubId
            let userScore, oppScore = if isHome then h, a else a, h

            let clubName id =
                gs.Clubs |> Map.tryFind id |> Option.map _.Name |> Option.defaultValue "?"

            let outcome, icon =
                if userScore > oppScore then
                    "WIN", NotificationIcons.win
                elif userScore < oppScore then
                    "LOSS", NotificationIcons.loss
                else
                    "DRAW", NotificationIcons.draw

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
                $"{outcome}  {clubName fixture.HomeClubId} {h}–{a} {clubName fixture.AwayClubId}"

            state |> pushNotification icon title body
        | _ -> state

    let private applyMatchNotifs (gs: GameState) (fixtures: (MatchId * MatchFixture) list) (state: State) =
        fixtures |> List.fold (fun acc (_, f) -> userMatchNotif gs f acc) state

    let private userFinishingLine (gs: GameState) (leagueId: CompetitionId) (userClubId: ClubId) =
        gs.Competitions
        |> Map.tryFind leagueId
        |> Option.bind (fun comp ->
            let ranked = Competition.rankedStandings comp

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
        | Some(old, _), Some(nw, league) when nw < old -> Some("Promoted!", $"Promoted to {league}")
        | Some(old, _), Some(nw, league) when nw > old -> Some("Relegated", $"Relegated to {league}")
        | _ -> None

    let private applyDayResult (result: DayResult) (state: State) =
        let logs =
            if result.Logs.IsEmpty then
                []
            else
                $"{result.PlayedFixtures.Length} matches played" :: result.Logs

        let nextState =
            { state with
                GameState = result.GameState
                IsProcessing = false
                LogMessages = logs @ state.LogMessages |> List.truncate 30 }
            |> applyMatchNotifs result.GameState result.PlayedFixtures

        match state.PrevUserClubSkills, state.PrevUserClubStatus with
        | Some prevSkills, Some prevStatus ->
            let userClubPlayers =
                result.GameState.Players
                |> Map.toList
                |> List.choose (fun (_, p) ->
                    match p.Affiliation with
                    | Contracted(clubId, _) when clubId = result.GameState.UserClubId -> Some p
                    | _ -> None)

            let inboxMsgs =
                InboxMessages.generateWeeklyDevelopmentMessages
                    result.GameState.CurrentDate
                    userClubPlayers
                    prevSkills
                    prevStatus

            let gs =
                inboxMsgs
                |> List.fold (fun acc msg -> GameState.addInboxMessage msg acc) result.GameState

            { nextState with
                GameState = gs
                PrevUserClubSkills = None
                PrevUserClubStatus = None }
        | _ -> nextState

    let private applySeasonResult (result: SeasonResult) (state: State) =
        let userLine =
            userFinishingLine result.SeasonFinalGs state.SelectedLeagueId result.NewGs.UserClubId

        let messages = $"Season {result.NewGs.Season} begins" :: userLine @ result.Summary
        let seasonBody = userLine |> String.concat " · "

        let seasonStartMsg =
            InboxMessages.generateSeasonStartMessage result.NewGs.CurrentDate result.NewGs.Season

        let gsAfterSeasonStart = GameState.addInboxMessage seasonStartMsg result.NewGs

        let stateWithInbox =
            match state.PrevUserClubSkills with
            | Some prevSkills ->
                let userClubPlayers =
                    result.NewGs.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = result.NewGs.UserClubId -> Some p
                        | _ -> None)

                let improved =
                    userClubPlayers
                    |> List.choose (fun p ->
                        match prevSkills.TryFind p.Id with
                        | Some prev when p.CurrentSkill > prev ->
                            Some(p.Name, string p.Position, p.CurrentSkill - prev)
                        | _ -> None)

                let seasonSummaryMsg =
                    if not improved.IsEmpty then
                        let improvements =
                            improved
                            |> List.map (fun (n, p, g) -> $"• {n} ({p}): +{g}")
                            |> String.concat "\n"

                        Some(
                            Inbox.create
                                result.NewGs.CurrentDate
                                "Youth Development System"
                                $"Season {result.NewGs.Season - 1} Development Summary"
                                ($"The following players showed improvement over the season:\n\n{improvements}")
                                InboxMessageCategory.Development
                                false
                        )
                    else
                        None

                let gs =
                    match seasonSummaryMsg with
                    | Some msg -> GameState.addInboxMessage msg gsAfterSeasonStart
                    | None -> gsAfterSeasonStart

                { state with
                    GameState = gs
                    PrevUserClubSkills = None
                    PrevUserClubStatus = None }
            | None ->
                { state with
                    GameState = gsAfterSeasonStart
                    PrevUserClubSkills = None
                    PrevUserClubStatus = None }

        let nextState =
            { stateWithInbox with
                IsProcessing = false
                SelectedLeagueId = primaryLeagueId result.NewGs
                LogMessages = messages @ stateWithInbox.LogMessages |> List.truncate 50 }
            |> pushNotification NotificationIcons.seasonComplete $"Season {result.NewGs.Season - 1} Complete" seasonBody

        match leagueChangeNotif state.GameState result.NewGs result.NewGs.UserClubId with
        | Some(title, body) when title = "Promoted!" ->
            nextState |> pushNotification NotificationIcons.promotion title body
        | Some(title, body) -> nextState |> pushNotification NotificationIcons.relegation title body
        | None -> nextState

    let rec handle (msg: SimMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Advance days ->
            if state.IsProcessing then
                state, Cmd.none
            else
                let prevSkills =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.CurrentSkill)
                        | _ -> None)
                    |> Map.ofList

                let prevStatus =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.Status)
                        | _ -> None)
                    |> Map.ofList

                { state with
                    IsProcessing = true
                    PrevUserClubSkills = Some prevSkills
                    PrevUserClubStatus = Some prevStatus },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> advanceDays days state.GameState))
                    ()
                    (SimMsg << AdvanceDone)

        | AdvanceDone result ->
            let nextState = applyDayResult result state

            let baseCmd =
                if result.SeasonComplete then
                    Cmd.batch [ Cmd.ofMsg (SimMsg AdvanceSeason); saveCmd result.GameState ]
                else
                    saveCmd result.GameState

            if hasUserFixtureToday result.GameState then
                nextState, Cmd.batch [ baseCmd; Cmd.ofMsg (SimMsg SimulateUserFixture) ]
            else
                nextState, baseCmd

        | SimulateUserFixture ->
            if state.IsProcessing then
                state, Cmd.none
            else
                { state with IsProcessing = true },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> simulateUserFixtureForDay state.GameState))
                    ()
                    (SimMsg << UserMatchDone)

        | UserMatchDone(Ok matchDay) ->
            let nextState = applyDayResult matchDay.DayResult state

            let withMatch =
                { nextState with
                    ActiveMatchReplay = Some matchDay.UserMatchReplay
                    ActiveMatchSnapshot = 0
                    CurrentPage = Match }

            if matchDay.DayResult.SeasonComplete then
                withMatch, Cmd.batch [ Cmd.ofMsg (SimMsg AdvanceSeason); saveCmd matchDay.DayResult.GameState ]
            else
                withMatch, saveCmd matchDay.DayResult.GameState

        | UserMatchDone(Error _) -> { state with IsProcessing = false }, Cmd.none

        | SimulateSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                let prevSkills =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.CurrentSkill)
                        | _ -> None)
                    |> Map.ofList

                let prevStatus =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.Status)
                        | _ -> None)
                    |> Map.ofList

                { state with
                    IsProcessing = true
                    PrevUserClubSkills = Some prevSkills
                    PrevUserClubStatus = Some prevStatus },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> simulateAndAdvanceSeason state.GameState))
                    ()
                    (SimMsg << SeasonAdvanceDone)

        | SeasonAdvanceDone(Ok result) ->
            let nextState = applySeasonResult result state
            nextState, saveCmd nextState.GameState

        | SeasonAdvanceDone(Error NoFixturesToPlay) ->
            { state with IsProcessing = false } |> addLog "Season already complete", Cmd.none

        | SeasonAdvanceDone(Error(SimulationErrors msg)) ->
            { state with IsProcessing = false }
            |> pushNotification NotificationIcons.error "Sim Season failed" msg,
            Cmd.none

        | AdvanceSeason ->
            if state.IsProcessing then
                state, Cmd.none
            else
                let prevSkills =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.CurrentSkill)
                        | _ -> None)
                    |> Map.ofList

                let prevStatus =
                    state.GameState.Players
                    |> Map.toList
                    |> List.choose (fun (_, p) ->
                        match p.Affiliation with
                        | Contracted(clubId, _) when clubId = state.GameState.UserClubId -> Some(p.Id, p.Status)
                        | _ -> None)
                    |> Map.ofList

                { state with
                    IsProcessing = true
                    PrevUserClubSkills = Some prevSkills
                    PrevUserClubStatus = Some prevStatus },
                Cmd.OfTask.perform
                    (fun () -> Task.Run(fun () -> advanceSeason state.GameState))
                    ()
                    (SimMsg << SeasonAdvanceDone << Ok)

        | SaveGame -> state, saveCmd state.GameState
