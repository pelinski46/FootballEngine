namespace FootballEngine

open Elmish
open FootballEngine.Domain
open AppTypes
open AppMsgs
open FootballEngine.GameGenerator

module UpdateSetup =

    let private saveCmd = SimHelpers.saveCmd

    let handle (msg: SetupMsg) (state: State) : State * Cmd<Msg> =
        match msg with
        | GoToStep step -> { state with State.Setup.Step = step }, Cmd.none

        | SelectPrimaryCountry code ->
            { state with
                Setup =
                    { state.Setup with
                        SelectedCountry = Some code
                        SecondaryCountries = state.Setup.SecondaryCountries |> List.filter ((<>) code) } },
            Cmd.none

        | ToggleSecondaryCountry code ->
            if state.Setup.SelectedCountry = Some code then
                state, Cmd.none
            else
                let updated =
                    if List.contains code state.Setup.SecondaryCountries then
                        state.Setup.SecondaryCountries |> List.filter ((<>) code)
                    else
                        code :: state.Setup.SecondaryCountries

                { state with
                    State.Setup.SecondaryCountries = updated },
                Cmd.none

        | UpdateManagerName name ->
            { state with
                State.Setup.ManagerName = name },
            Cmd.none

        | StartNewGame ->
            match state.Setup.SelectedCountry with
            | None -> state, Cmd.none
            | Some primary ->
                let newGs =
                    generateNewGame (System.Random()) primary state.Setup.ManagerName state.Setup.SecondaryCountries

                { state with
                    GameState = newGs
                    State.Setup.Step = ClubSelection },
                saveCmd newGs

        | ConfirmClub clubId ->
            let newGs =
                { state.GameState with
                    UserClubId = clubId }

            { state with
                GameState = newGs
                CurrentPage = Home
                LogMessages = [ $"Career started by {state.Setup.ManagerName}" ] },
            saveCmd newGs
