namespace FootballEngine.Components

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Domain
open FootballEngine.Icons
open Material.Icons

module Navigation =
    type NavItem =
        { Page: Page
          Label: string
          Icon: MaterialIconKind }

    let menuItems =
        [ { Page = HomePage
            Label = "Home"
            Icon = IconName.home }
          { Page = Inbox
            Label = "Inbox"
            Icon = IconName.inbox }
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
            Icon = ClubIcon.scouting }
          { Page = Transfers
            Label = "Transfers"
            Icon = ClubIcon.transfer }
          { Page = Finances
            Label = "Finances"
            Icon = ClubIcon.finances }
          { Page = Settings
            Label = "Settings"
            Icon = IconName.settings }
          { Page = ModEditor
            Label = "Mod Editor"
            Icon = IconName.fileDocumentEdit } ]

module Sidebar =
    let private navButton (item: Navigation.NavItem) (isActive: bool) (badgeCount: int option) dispatch =
        let accentBg = Theme.Accent + "18"
        let activeFg = Theme.Accent
        let inactiveFg = Theme.TextSub

        let badge =
            match badgeCount with
            | Some count when count > 0 ->
                Border.create
                    [ Border.background Theme.Accent
                      Border.cornerRadius 8.0
                      Border.padding (4.0, 1.0)
                      Border.margin (4.0, 0.0)
                      Border.child (
                          TextBlock.create
                              [ TextBlock.text (string count)
                                TextBlock.fontSize 10.0
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.foreground Theme.BgSidebar ]
                      ) ]
                :> IView
            | _ -> TextBlock.create [ TextBlock.text "" ] :> IView

        Button.create
            [ Button.background (if isActive then accentBg else "Transparent")

              Button.borderBrush (if isActive then Theme.Accent else "Transparent")
              Button.cornerRadius 0.0
              Button.padding (20.0, 12.0)
              Button.horizontalAlignment HorizontalAlignment.Stretch
              Button.horizontalContentAlignment HorizontalAlignment.Left
              Button.onClick (fun _ -> dispatch (ChangePage item.Page))
              Button.content (
                  DockPanel.create
                      [ DockPanel.lastChildFill true
                        DockPanel.children
                            [ badge |> fun b -> Border.create [ Border.child b; Border.dock Dock.Right ]
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 14.0
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children
                                        [ Icons.icon item.Icon 18.0 (if isActive then activeFg else inactiveFg)
                                          TextBlock.create
                                              [ TextBlock.text item.Label
                                                TextBlock.fontSize 13.0
                                                TextBlock.fontWeight (
                                                    if isActive then FontWeight.SemiBold else FontWeight.Normal
                                                )
                                                TextBlock.foreground (if isActive then activeFg else inactiveFg)
                                                TextBlock.verticalAlignment VerticalAlignment.Center ] ] ] ] ]
              ) ]

    let sidebar (state: State) dispatch =
        let unreadCount, visibleItems =
            match state.Mode with
            | InGame(gs, employment) ->
                let unread = GameState.unreadInboxCount gs

                let items =
                    match employment with
                    | Employed _ -> Navigation.menuItems
                    | NotEmployed ->
                        Navigation.menuItems
                        |> List.filter (fun item -> item.Page <> Squad && item.Page <> Tactics && item.Page <> Training)

                unread, items
            | _ -> 0, Navigation.menuItems

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
                                        [ for item in visibleItems do
                                              let badge = if item.Page = Inbox then Some unreadCount else None
                                              navButton item (state.CurrentPage = item.Page) badge dispatch ] ] ] ]
              ) ]
