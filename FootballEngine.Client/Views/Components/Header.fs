namespace FootballEngine.Components

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppState


module Header =
    let header (state: State) dispatch =
        Border.create
            [ Border.height 70.0
              Border.background Theme.BgSidebar
              Border.padding (25.0, 0.0)
              Border.child (
                  DockPanel.create
                      [ DockPanel.lastChildFill false
                        DockPanel.children
                            [ StackPanel.create
                                  [ StackPanel.orientation Orientation.Horizontal
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.dock Dock.Right
                                    StackPanel.spacing 20.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text (
                                                    state.GameState.CurrentDate.ToString("dd MMMM yyyy").ToUpper()
                                                )
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.fontWeight FontWeight.Bold
                                                TextBlock.lineSpacing 1.5 ]
                                          Button.create
                                              [ Button.content "CONTINUE"
                                                Button.background Theme.Accent
                                                Button.foreground Theme.BgSidebar
                                                Button.fontWeight FontWeight.Bold
                                                Button.padding (30.0, 8.0)
                                                Button.cornerRadius 6.0
                                                Button.onClick (fun _ -> dispatch AdvanceDay) ] ] ]


                              ] ]
              ) ]
