namespace FootballEngine.Components

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Icons
open Material.Icons

module Navigation =
    type NavItem =
        { Page: Page
          Label: string
          Icon: MaterialIconKind }

    let menuItems =
        [ { Page = Home
            Label = "Home"
            Icon = IconName.home }
          { Page = Squad
            Label = "Squad"
            Icon = IconName.squad }
          { Page = Tactics
            Label = "Tactics"
            Icon = IconName.tactics }
          { Page = Training
            Label = "Training"
            Icon = PlayerIcon.stamina }
          { Page = Scouting
            Label = "Scouting"
            Icon = Club.scouting }
          { Page = Transfers
            Label = "Transfers"
            Icon = Club.transfer }
          { Page = Finances
            Label = "Finances"
            Icon = Club.finances }
          { Page = Settings
            Label = "Settings"
            Icon = IconName.settings } ]

module Sidebar =
    let private navButton (item: Navigation.NavItem) (isActive: bool) dispatch =
        let accentBg = Theme.Accent + "18"
        let activeFg = Theme.Accent
        let inactiveFg = Theme.TextSub

        Button.create
            [ Button.background (if isActive then accentBg else "Transparent")

              Button.borderBrush (if isActive then Theme.Accent else "Transparent")
              Button.cornerRadius 0.0
              Button.padding (20.0, 12.0)
              Button.horizontalAlignment HorizontalAlignment.Stretch
              Button.horizontalContentAlignment HorizontalAlignment.Left
              Button.onClick (fun _ -> dispatch (ChangePage item.Page))
              Button.content (
                  StackPanel.create
                      [ StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 14.0
                        StackPanel.verticalAlignment VerticalAlignment.Center
                        StackPanel.children
                            [ Icons.icon item.Icon 18.0 (if isActive then activeFg else inactiveFg)
                              TextBlock.create
                                  [ TextBlock.text item.Label
                                    TextBlock.fontSize 13.0
                                    TextBlock.fontWeight (if isActive then FontWeight.SemiBold else FontWeight.Normal)
                                    TextBlock.foreground (if isActive then activeFg else inactiveFg)
                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
              ) ]

    let sidebar (state: State) dispatch =
        Border.create
            [ Border.width 220.0
              Border.background Theme.BgSidebar
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 1.0, 0.0)
              Border.child (
                  DockPanel.create
                      [ DockPanel.lastChildFill false
                        DockPanel.children
                            [ Border.create
                                  [ DockPanel.dock Dock.Top
                                    Border.padding (20.0, 24.0, 20.0, 20.0)
                                    Border.borderBrush Theme.Border
                                    Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                    Border.child (
                                        StackPanel.create
                                            [ StackPanel.spacing 2.0
                                              StackPanel.children
                                                  [ TextBlock.create
                                                        [ TextBlock.text "FOOTBALL"
                                                          TextBlock.fontSize 16.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMain
                                                          TextBlock.lineSpacing 2.0 ]
                                                    TextBlock.create
                                                        [ TextBlock.text "ENGINE"
                                                          TextBlock.fontSize 16.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.Accent
                                                          TextBlock.lineSpacing 2.0 ] ] ]
                                    ) ]

                              StackPanel.create
                                  [ DockPanel.dock Dock.Top
                                    StackPanel.margin (0.0, 8.0)
                                    StackPanel.children
                                        [ for item in Navigation.menuItems do
                                              navButton item (state.CurrentPage = item.Page) dispatch ] ] ] ]
              ) ]
