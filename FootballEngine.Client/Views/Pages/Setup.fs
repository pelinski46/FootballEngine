namespace FootballEngine.Pages.Setup

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppState
open FootballEngine.Components
open FootballEngine.Domain

module Setup =

    let setupView (state: State) dispatch =


        let setupContainer (content: IView) =
            Grid.create
                [ Grid.background "#020617"
                  Grid.children
                      [ ScrollViewer.create
                            [ ScrollViewer.content (
                                  StackPanel.create
                                      [ StackPanel.verticalAlignment VerticalAlignment.Center
                                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                                        StackPanel.maxWidth 900.0
                                        StackPanel.minWidth 600.0
                                        StackPanel.margin 40.0
                                        StackPanel.spacing 40.0
                                        StackPanel.children
                                            [
                                              // Logo / Título
                                              StackPanel.create
                                                  [ StackPanel.children
                                                        [ TextBlock.create
                                                              [ TextBlock.text "FOOTBALL"
                                                                TextBlock.fontSize 50.0
                                                                TextBlock.fontWeight FontWeight.Black
                                                                TextBlock.foreground "White"
                                                                TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                                          TextBlock.create
                                                              [ TextBlock.text "ENGINE 2026"
                                                                TextBlock.fontSize 20.0
                                                                TextBlock.foreground Theme.Accent
                                                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                                TextBlock.margin (0.0, -15.0, 0.0, 0.0) ] ] ]
                                              content ] ]
                              ) ] ] ]

        match state.SetupStep with
        | MainMenu ->
            setupContainer (
                UniformGrid.create
                    [ UniformGrid.columns 2
                      UniformGrid.children
                          [ UI.menuButton "NEW CAREER" "⚽" "Start a new journey as a manager" (fun _ ->
                                dispatch (GoToSetupStep CountrySelection))
                            UI.menuButton "LOAD GAME" "💾" "Continue your existing career" (fun _ -> dispatch SaveGame) ] ] // TODO missing LoadGame
            )

        | CountrySelection ->
            setupContainer (
                StackPanel.create
                    [ StackPanel.spacing 20.0
                      StackPanel.children
                          [ TextBlock.create
                                [ TextBlock.text "SELECT COUNTRIES & LEAGUES"
                                  TextBlock.fontSize 24.0
                                  TextBlock.fontWeight FontWeight.Bold
                                  TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                            TextBlock.create
                                [ TextBlock.text "Choose one main country to play and others to simulate players."
                                  TextBlock.foreground Theme.TextMuted
                                  TextBlock.horizontalAlignment HorizontalAlignment.Center ]

                            // Lista de Países
                            StackPanel.create
                                [ StackPanel.spacing 10.0
                                  StackPanel.children
                                      [ for code, name, flag in
                                            [ ("ARG", "Argentina", "🇦🇷")
                                              ("BRA", "Brazil", "🇧🇷")
                                              ("ENG", "England", "🏴󠁧󠁢󠁥󠁮󠁧󠁿")
                                              ("ESP", "Spain", "🇪🇸") ] do
                                            UI.countrySelectionCard
                                                name
                                                flag
                                                (state.SetupSelectedCountry = Some code)
                                                (state.SetupSecondaryCountries |> List.contains code)
                                                (fun _ -> dispatch (SelectPrimaryCountry code))
                                                (fun _ -> dispatch (ToggleSecondaryCountry code)) ] ]

                            // Navegación
                            DockPanel.create
                                [ DockPanel.children
                                      [ Button.create
                                            [ Button.content "Back"
                                              Button.onClick (fun _ -> dispatch (GoToSetupStep MainMenu)) ]
                                        if state.SetupSelectedCountry.IsSome then
                                            Button.create
                                                [ Button.dock Dock.Right
                                                  Button.content "Manager Details →"
                                                  Button.background Theme.Accent
                                                  Button.foreground Theme.BgSidebar
                                                  Button.fontWeight FontWeight.Bold
                                                  Button.padding (20.0, 10.0)
                                                  Button.onClick (fun _ -> dispatch (GoToSetupStep ManagerNaming)) ] ] ] ] ]
            )
        | ClubSelection ->
            let clubs =
                state.GameState.Leagues
                |> Map.tryPick (fun _ l ->
                    if l.Nationality = state.SetupSelectedCountry.Value && l.Level = First then
                        Some l
                    else
                        None)
                |> Option.map (fun l -> l.ClubIds |> List.map (fun id -> state.GameState.Clubs[id]))
                |> Option.defaultValue []
                |> List.sortByDescending _.Reputation

            setupContainer (
                StackPanel.create
                    [ StackPanel.spacing 20.0
                      StackPanel.children
                          [ TextBlock.create
                                [ TextBlock.text "CHOOSE YOUR CLUB"
                                  TextBlock.fontSize 24.0
                                  TextBlock.fontWeight FontWeight.Bold
                                  TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                            for club in clubs do
                                Button.create
                                    [ Button.horizontalAlignment HorizontalAlignment.Stretch
                                      Button.padding (20.0, 12.0)
                                      Button.onClick (fun _ -> dispatch (ConfirmClub club.Id))
                                      Button.content (
                                          DockPanel.create
                                              [ DockPanel.children
                                                    [ TextBlock.create
                                                          [ TextBlock.text club.Name
                                                            TextBlock.fontWeight FontWeight.Bold ]
                                                      TextBlock.create
                                                          [ TextBlock.dock Dock.Right
                                                            TextBlock.text $"⭐ {club.Reputation}"
                                                            TextBlock.foreground Theme.TextMuted ] ] ]
                                      ) ] ] ]
            )
        | ManagerNaming ->
            setupContainer (
                StackPanel.create
                    [ StackPanel.spacing 30.0
                      StackPanel.children
                          [ TextBlock.create
                                [ TextBlock.text "MANAGER DETAILS"
                                  TextBlock.fontSize 24.0
                                  TextBlock.fontWeight FontWeight.Bold ]

                            UI.sectionContainer
                                "ENTER YOUR NAME"
                                (TextBox.create
                                    [ TextBox.text state.SetupManagerName
                                      TextBox.onTextChanged (fun n -> dispatch (UpdateManagerName n))
                                      TextBox.padding 15.0
                                      TextBox.fontSize 18.0
                                      TextBox.background "#1e293b"
                                      TextBox.borderThickness 0.0 ])

                            UI.statCard "CHOSEN NATION" state.SetupSelectedCountry.Value "🌍" "Your career starts here"


                            DockPanel.create
                                [ DockPanel.children
                                      [ Button.create
                                            [ Button.content "← Back"
                                              Button.onClick (fun _ -> dispatch (GoToSetupStep CountrySelection)) ]
                                        if state.SetupManagerName.Length > 2 then
                                            Button.create
                                                [ Button.dock Dock.Right
                                                  Button.content "CONFIRM & CREATE GAME"
                                                  Button.background Theme.Accent
                                                  Button.foreground Theme.BgSidebar
                                                  Button.fontWeight FontWeight.Bold
                                                  Button.padding (30.0, 15.0)
                                                  Button.onClick (fun _ -> dispatch StartNewGame) ] ] ] ] ]
            )
