namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open FootballEngine.Engine
open AppTypes
open AppMsgs

module UpdateSim =

    let private addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let private saveAsync (gs: GameState) =
        Task.Run(fun () -> Db.saveGame gs) |> ignore

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

                            gs, getTodayFixtures gs))
                    ()
                    (SimMsg << AdvanceDayDone)

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
            let fixtures = getTodayFixtures state.GameState

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
                | Ok(updatedFixture, hScore, aScore) ->
                    let home = state.GameState.Clubs[fixture.HomeClubId]
                    let away = state.GameState.Clubs[fixture.AwayClubId]
                    let newGs = updateFixtureInState state.GameState id updatedFixture
                    Db.saveGame newGs

                    { state with GameState = newGs }
                    |> addLog $"🏁 {home.Name} {hScore}-{aScore} {away.Name}",
                    Cmd.none

        | SimulateMatch -> handle SimulateNextFixture state
