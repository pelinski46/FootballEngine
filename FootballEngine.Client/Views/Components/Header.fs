namespace FootballEngine.Components

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppState
open FootballEngine.Icons


module Header =

    let header (state: State) dispatch =

        let isProcessing = state.IsProcessing

        let clubName =
            state.GameState.Clubs
            |> Map.tryFind state.GameState.UserClubId
            |> Option.map _.Name
            |> Option.defaultValue "Football Engine"

        Border.create
            [ Border.height 64.0
              Border.background Theme.BgSidebar
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.borderBrush Theme.Border
              Border.padding (24.0, 0.0)
              Border.child (
                  DockPanel.create
                      [ DockPanel.lastChildFill false
                        DockPanel.children
                            [ // ── Left: club name ─────────────────────────────
                              StackPanel.create
                                  [ StackPanel.dock Dock.Left
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.spacing 8.0
                                    StackPanel.children
                                        [ Icons.iconMd Club.stadium Theme.Accent
                                          TextBlock.create
                                              [ TextBlock.text clubName
                                                TextBlock.fontSize 14.0
                                                TextBlock.fontWeight FontWeight.Bold
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

                              // ── Right: date + continue ───────────────────────
                              StackPanel.create
                                  [ StackPanel.dock Dock.Right
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.spacing 12.0
                                    StackPanel.children
                                        [ // Date pill
                                          Border.create
                                              [ Border.background Theme.BgCard
                                                Border.cornerRadius 8.0
                                                Border.padding (12.0, 6.0)
                                                Border.child (
                                                    StackPanel.create
                                                        [ StackPanel.orientation Orientation.Horizontal
                                                          StackPanel.spacing 6.0
                                                          StackPanel.children
                                                              [ Icons.iconSm UI.calendar Theme.TextMuted
                                                                TextBlock.create
                                                                    [ TextBlock.text (
                                                                          state.GameState.CurrentDate
                                                                              .ToString("dd MMM yyyy")
                                                                              .ToUpper()
                                                                      )
                                                                      TextBlock.fontSize 12.0
                                                                      TextBlock.fontWeight FontWeight.SemiBold
                                                                      TextBlock.foreground Theme.TextSub
                                                                      TextBlock.verticalAlignment
                                                                          VerticalAlignment.Center ] ] ]
                                                ) ]

                                          // Continue button
                                          Button.create
                                              [ Button.isEnabled (not isProcessing)
                                                Button.background (if isProcessing then Theme.BgCard else Theme.Accent)
                                                Button.borderThickness 0.0
                                                Button.fontWeight FontWeight.Bold
                                                Button.padding (20.0, 8.0)
                                                Button.cornerRadius 8.0
                                                Button.cursor Avalonia.Input.Cursor.Default
                                                Button.onClick (fun e ->
                                                    e.Handled <- true
                                                    dispatch AdvanceDay)
                                                Button.content (
                                                    StackPanel.create
                                                        [ StackPanel.orientation Orientation.Horizontal
                                                          StackPanel.spacing 6.0
                                                          StackPanel.children
                                                              [ Icons.iconSm
                                                                    (if isProcessing then UI.refresh else Nav.next)
                                                                    (if isProcessing then
                                                                         Theme.TextMuted
                                                                     else
                                                                         Theme.BgSidebar)
                                                                TextBlock.create
                                                                    [ TextBlock.text (
                                                                          if isProcessing then
                                                                              "SIMULATING..."
                                                                          else
                                                                              "CONTINUE"
                                                                      )
                                                                      TextBlock.fontSize 12.0
                                                                      TextBlock.fontWeight FontWeight.Bold
                                                                      TextBlock.foreground (
                                                                          if isProcessing then
                                                                              Theme.TextMuted
                                                                          else
                                                                              Theme.BgSidebar
                                                                      )
                                                                      TextBlock.verticalAlignment
                                                                          VerticalAlignment.Center ] ] ]
                                                ) ]
                                          |> View.withKey (if isProcessing then "btn-processing" else "btn-ready") ] ] ] ]
              ) ]
