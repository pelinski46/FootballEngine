namespace FootballEngine.Pages.Loading

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open FootballEngine
open FootballEngine.Icons

module LoadingPage =
    let loadingView () : IView =
        Grid.create
            [ Grid.background Theme.BgMain
              Grid.rowDefinitions "*, auto, auto, auto, *"
              Grid.columnDefinitions "*"
              Grid.children
                  [ Icons.icon IconName.tactics 96.0 Theme.Accent
                    |> fun ico ->
                        Border.create
                            [ Border.row 1
                              Border.horizontalAlignment HorizontalAlignment.Center
                              Border.child ico ]

                    StackPanel.create
                        [ StackPanel.row 2
                          StackPanel.horizontalAlignment HorizontalAlignment.Center
                          StackPanel.margin (0, 24, 0, 8)
                          StackPanel.children
                              [ TextBlock.create
                                    [ TextBlock.text "Football Engine 2026"
                                      TextBlock.fontSize 32.0
                                      TextBlock.fontWeight Avalonia.Media.FontWeight.Bold
                                      TextBlock.foreground Theme.TextMain
                                      TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                TextBlock.create
                                    [ TextBlock.text "Loading your save..."
                                      TextBlock.fontSize 14.0
                                      TextBlock.foreground Theme.TextMuted
                                      TextBlock.horizontalAlignment HorizontalAlignment.Center
                                      TextBlock.margin (0, 6, 0, 0) ] ] ]

                    Border.create
                        [ Border.row 3
                          Border.width 220.0
                          Border.height 3.0
                          Border.cornerRadius 2.0
                          Border.background Theme.BgCard
                          Border.horizontalAlignment HorizontalAlignment.Center
                          Border.margin (0, 0, 0, 0)
                          Border.child (
                              Border.create
                                  [ Border.width 80.0
                                    Border.height 3.0
                                    Border.cornerRadius 2.0
                                    Border.background Theme.Accent
                                    Border.horizontalAlignment HorizontalAlignment.Left ]
                          ) ] ] ]
