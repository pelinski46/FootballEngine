namespace FootballEngine.Components


open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine.AppState
open FootballEngine

module Navigation =
    type NavItem =
        { Page: Page
          Label: string
          Icon: string }

    let menuItems =
        [ { Page = Home
            Label = "Home"
            Icon = "🏠" }
          { Page = Squad
            Label = "Squad"
            Icon = "👥" }
          { Page = Tactics
            Label = "Tactics"
            Icon = "🎯" }
          { Page = Training
            Label = "Training"
            Icon = "🏃" }
          { Page = Scouting
            Label = "Scouting"
            Icon = "🔍" }
          { Page = Transfers
            Label = "Transfers"
            Icon = "🤝" }
          { Page = Finances
            Label = "Finances"
            Icon = "💰" }
          { Page = Settings
            Label = "Settings"
            Icon = "⚙️" } ]

module Sidebar =
    let sidebar (state: State) dispatch =
        let width = 240.0

        Border.create
            [ Border.width width
              Border.background Theme.BgSidebar
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 1.0, 0.0)
              Border.child (
                  DockPanel.create
                      [ DockPanel.children
                            [

                              Border.create
                                  [ DockPanel.dock Dock.Top
                                    Border.height 80.0
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text ("FOOTBALL ENGINE")
                                              TextBlock.verticalAlignment VerticalAlignment.Center
                                              TextBlock.horizontalAlignment HorizontalAlignment.Center
                                              TextBlock.fontSize 18.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground Theme.Accent ]
                                    ) ]


                              StackPanel.create
                                  [ StackPanel.children
                                        [ for item in Navigation.menuItems do
                                              Button.create
                                                  [ yield! UI.sidebarButton (state.CurrentPage = item.Page)
                                                    Button.onClick (fun _ -> dispatch (ChangePage item.Page))
                                                    Button.content (
                                                        StackPanel.create
                                                            [ StackPanel.orientation Orientation.Horizontal
                                                              StackPanel.spacing 15.0
                                                              StackPanel.children
                                                                  [ TextBlock.create
                                                                        [ TextBlock.text item.Icon
                                                                          TextBlock.fontSize 16.0 ]

                                                                    TextBlock.create
                                                                        [ TextBlock.text item.Label
                                                                          TextBlock.fontWeight FontWeight.Medium ] ] ]
                                                    ) ] ] ] ] ]
              ) ]
