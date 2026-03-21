namespace FootballEngine

open Elmish
open FootballEngine.Domain
open AppTypes
open AppMsgs
open FootballEngine.MatchSimulator

module UpdateMatchLab =

    let private addLog = AppTypes.addLog

    let handle (msg: MatchLabMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | SelectHome id ->
            { state with
                State.MatchLab.HomeClubId = Some id },
            Cmd.none

        | SelectAway id ->
            { state with
                State.MatchLab.AwayClubId = Some id },
            Cmd.none

        | Run ->
            match state.MatchLab.HomeClubId, state.MatchLab.AwayClubId with
            | Some hId, Some aId ->
                match trySimulateMatchFull state.GameState.Clubs[hId] state.GameState.Clubs[aId] with
                | Ok replay ->
                    { state with
                        MatchLab =
                            { state.MatchLab with
                                Result = Some replay
                                Snapshot = 0 } },
                    Cmd.none
                | Error e -> state |> addLog $"⚠️ {e}", Cmd.none
            | _ -> state |> addLog "⚠️ Select both clubs", Cmd.none

        | Step delta ->
            let total =
                state.MatchLab.Result
                |> Option.map (fun r -> r.Snapshots.Length - 1)
                |> Option.defaultValue 0

            { state with
                State.MatchLab.Snapshot = max 0 (min total (state.MatchLab.Snapshot + delta)) },
            Cmd.none
