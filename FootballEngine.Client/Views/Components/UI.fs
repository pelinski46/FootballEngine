namespace FootballEngine.Components

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open FootballEngine
open FootballEngine.Domain


module UI =
    let sidebarButton (isActive: bool) =
        [ Button.background (if isActive then Theme.Accent else "transparent")
          Button.foreground (if isActive then Theme.BgSidebar else Theme.TextMain)
          Button.borderThickness 0.0
          Button.cornerRadius 8.0
          Button.margin (10.0, 4.0)
          Button.padding (15.0, 10.0)
          Button.horizontalAlignment HorizontalAlignment.Stretch ]

    let menuButton title icon description onClick =
        Button.create
            [ Button.onClick onClick
              Button.padding 0.0
              Button.background "transparent"
              Button.content (
                  Border.create
                      [ Border.background Theme.BgCard
                        Border.cornerRadius 12.0
                        Border.padding 30.0
                        Border.borderBrush Theme.Border
                        Border.borderThickness 1.0
                        Border.child (
                            StackPanel.create
                                [ StackPanel.spacing 10.0
                                  StackPanel.children
                                      [ TextBlock.create
                                            [ TextBlock.text icon
                                              TextBlock.fontSize 40.0
                                              TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                        TextBlock.create
                                            [ TextBlock.text title
                                              TextBlock.fontSize 22.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground Theme.Accent
                                              TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                        TextBlock.create
                                            [ TextBlock.text description
                                              TextBlock.fontSize 12.0
                                              TextBlock.foreground Theme.TextMuted
                                              TextBlock.horizontalAlignment HorizontalAlignment.Center ] ] ]
                        ) ]
              ) ]

    let countrySelectionCard name flag isPrimary isSecondary onClickPrimary onClickSecondary =
        Border.create
            [ Border.background Theme.BgCard
              Border.cornerRadius 10.0
              Border.borderThickness 2.0
              Border.borderBrush (
                  if isPrimary then Theme.Accent
                  elif isSecondary then "#3b82f6"
                  else "transparent"
              )
              Border.padding 15.0
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "Auto, *, Auto"
                        Grid.children
                            [ TextBlock.create
                                  [ Grid.column 0
                                    TextBlock.text flag
                                    TextBlock.fontSize 32.0
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              StackPanel.create
                                  [ Grid.column 1
                                    StackPanel.margin (15.0, 0.0)
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text name
                                                TextBlock.fontWeight FontWeight.Bold
                                                TextBlock.fontSize 16.0 ]
                                          TextBlock.create
                                              [ TextBlock.text (
                                                    if isPrimary then "PRIMARY LEAGUE"
                                                    elif isSecondary then "SIMULATED"
                                                    else "NOT ACTIVE"
                                                )
                                                TextBlock.fontSize 10.0
                                                TextBlock.foreground (
                                                    if isPrimary then Theme.Accent
                                                    elif isSecondary then "#3b82f6"
                                                    else Theme.TextMuted
                                                ) ] ] ]
                              StackPanel.create
                                  [ Grid.column 2
                                    StackPanel.spacing 5.0
                                    StackPanel.children
                                        [ Button.create
                                              [ Button.content (if isPrimary then "✓ Main" else "Set Main")
                                                Button.fontSize 10.0
                                                Button.onClick onClickPrimary
                                                Button.background (if isPrimary then Theme.Accent else Theme.BgSidebar) ]
                                          Button.create
                                              [ Button.content (if isSecondary then "✕ Remove" else "+ Add")
                                                Button.fontSize 10.0
                                                Button.onClick onClickSecondary
                                                Button.isEnabled (not isPrimary) ] ] ] ] ]
              ) ]

    let playerTableHeader () =
        Border.create
            [ Border.padding (15.0, 10.0)
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "220, 60, 60, 80, 80, *"
                        Grid.children
                            [ TextBlock.create
                                  [ Grid.column 0
                                    TextBlock.text "PLAYER"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ]
                              TextBlock.create
                                  [ Grid.column 1
                                    TextBlock.text "POS"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ]
                              TextBlock.create
                                  [ Grid.column 2
                                    TextBlock.text "AGE"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ]
                              TextBlock.create
                                  [ Grid.column 3
                                    TextBlock.text "SKILL"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ]
                              TextBlock.create
                                  [ Grid.column 4
                                    TextBlock.text "VALUE"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ]
                              TextBlock.create
                                  [ Grid.column 5
                                    TextBlock.text "STATS"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 11.0 ] ] ]
              ) ]

    let badge text (color: string) =
        Border.create
            [ Border.background color
              Border.cornerRadius 4.0
              Border.padding (6.0, 2.0)
              Border.child (
                  TextBlock.create
                      [ TextBlock.text text
                        TextBlock.fontSize 10.0
                        TextBlock.fontWeight FontWeight.Bold
                        TextBlock.foreground "#ffffff" ]
              ) ]

    let roleBadge (role: string) (duty: string) =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 2.0
              StackPanel.horizontalAlignment HorizontalAlignment.Center
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text role
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.fontSize 10.0
                          TextBlock.foreground "#ffffff" ]
                    TextBlock.create
                        [ TextBlock.text $"- {duty}"
                          TextBlock.fontSize 10.0
                          TextBlock.foreground "#fbbf24" ] ] ]



    let sectionContainer title (content: IView) =
        StackPanel.create
            [ StackPanel.spacing 10.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text title
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground Theme.TextMuted
                          TextBlock.fontSize 12.0 ]
                    Border.create
                        [ Border.background Theme.BgCard
                          Border.borderBrush Theme.Border
                          Border.borderThickness 1.0
                          Border.cornerRadius 8.0
                          Border.clipToBounds true
                          Border.child content ] ] ]

    let statCard title value icon subText =
        Border.create
            [ Border.background Theme.BgCard
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.cornerRadius 8.0
              Border.padding 20.0
              Border.child (
                  StackPanel.create
                      [ StackPanel.children
                            [ DockPanel.create
                                  [ DockPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text icon
                                                TextBlock.fontSize 18.0
                                                DockPanel.dock Dock.Right ]
                                          TextBlock.create
                                              [ TextBlock.text title
                                                TextBlock.foreground Theme.TextMuted
                                                TextBlock.fontSize 10.0
                                                TextBlock.fontWeight FontWeight.Bold ] ] ]
                              TextBlock.create
                                  [ TextBlock.text value
                                    TextBlock.fontSize 24.0
                                    TextBlock.fontWeight FontWeight.Black
                                    TextBlock.foreground Theme.TextMain
                                    TextBlock.margin (0.0, 5.0, 0.0, 0.0) ]
                              if subText <> "" then
                                  TextBlock.create
                                      [ TextBlock.text subText
                                        TextBlock.fontSize 10.0
                                        TextBlock.foreground "#64748b" ] ] ]
              ) ]

    let tabButton (label: string) isActive onClick =
        Button.create
            [ Button.content label
              Button.background (if isActive then Theme.Accent else "transparent")
              Button.foreground (if isActive then Theme.BgSidebar else Theme.TextMuted)
              Button.borderThickness (if isActive then 0.0 else 1.0)
              Button.borderBrush Theme.Border
              Button.padding (15.0, 5.0)
              Button.fontSize 11.0
              Button.fontWeight FontWeight.Bold
              Button.cornerRadius 4.0
              Button.onClick onClick
              Button.margin (0.0, 0.0, 10.0, 0.0) ]



    let playerRow
        (p: Player)
        (currentDate: DateTime)
        (marketValue: decimal)
        (isSelected: bool)
        (isDragged: bool)
        onSelect
        =
        Border.create
            [ Border.background (
                  if isSelected then "#1e293b"
                  elif isDragged then "#334155"
                  else "transparent"
              )
              Border.cornerRadius 8.0
              Border.padding 12.0
              Border.onTapped (fun _ -> onSelect ())
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "220, 60, 60, 80, 80, *"
                        Grid.children
                            [ StackPanel.create
                                  [ Grid.column 0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text p.Name; TextBlock.fontWeight FontWeight.Bold ]
                                          TextBlock.create
                                              [ TextBlock.text $"Morale: %d{p.Morale}%%"
                                                TextBlock.fontSize 10.0
                                                TextBlock.foreground (
                                                    if p.Morale > 70 then "#10b981"
                                                    elif p.Morale > 40 then "#f59e0b"
                                                    else "#ef4444"
                                                ) ] ] ]
                              StackPanel.create
                                  [ Grid.column 1
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children [ badge $"%A{p.Position}" Theme.Border ] ]
                              TextBlock.create
                                  [ Grid.column 2
                                    TextBlock.text (string (Player.age currentDate p))
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              StackPanel.create
                                  [ Grid.column 3
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text (string p.CurrentSkill)
                                                TextBlock.fontWeight FontWeight.Bold
                                                TextBlock.foreground Theme.Accent ]
                                          ProgressBar.create
                                              [ ProgressBar.value (float p.CurrentSkill / 2.0)
                                                ProgressBar.height 3.0
                                                ProgressBar.background Theme.BgSidebar
                                                ProgressBar.foreground Theme.Accent ] ] ]
                              TextBlock.create
                                  [ Grid.column 4
                                    TextBlock.text $"€{int (marketValue / 1_000_000m)}M"
                                    TextBlock.fontSize 11.0
                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
              ) ]

    let playerDetail (player: Player option) (currentDate: DateTime) (marketValue: decimal) =
        let statRow label (value: int) (color: string) =
            StackPanel.create
                [ StackPanel.margin (0.0, 2.0)
                  StackPanel.children
                      [ DockPanel.create
                            [ DockPanel.children
                                  [ TextBlock.create
                                        [ TextBlock.text label
                                          TextBlock.foreground Theme.TextMuted
                                          TextBlock.fontSize 11.0 ]
                                    TextBlock.create
                                        [ TextBlock.text (string value)
                                          TextBlock.fontWeight FontWeight.Bold
                                          TextBlock.fontSize 11.0
                                          DockPanel.dock Dock.Right ] ] ]
                        ProgressBar.create
                            [ ProgressBar.value (float value * 5.0)
                              ProgressBar.height 3.0
                              ProgressBar.background "#1e293b"
                              ProgressBar.foreground color ] ] ]

        let categoryHeader text =
            TextBlock.create
                [ TextBlock.text text
                  TextBlock.fontSize 11.0
                  TextBlock.fontWeight FontWeight.Black
                  TextBlock.foreground Theme.Accent
                  TextBlock.margin (0, 15, 0, 5) ]

        Border.create
            [ Border.background Theme.BgCard
              Border.borderBrush Theme.Border
              Border.borderThickness (1.0, 0.0, 0.0, 0.0)
              Border.child (
                  match player with
                  | None ->
                      TextBlock.create
                          [ TextBlock.text "Select a player"
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.foreground Theme.TextMuted ]
                      :> IView
                  | Some p ->
                      Grid.create
                          [ Grid.rowDefinitions "Auto, Auto"
                            Grid.children
                                [

                                  // --- 1. HEADER FIJO (No se mueve) ---
                                  StackPanel.create
                                      [ Grid.row 0
                                        DockPanel.dock Dock.Top
                                        StackPanel.margin 20.0
                                        StackPanel.spacing 10.0
                                        StackPanel.children
                                            [ TextBlock.create
                                                  [ TextBlock.text p.Name
                                                    TextBlock.fontSize 22.0
                                                    TextBlock.fontWeight FontWeight.Bold
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                              badge $"%A{p.Position}" Theme.Accent
                                              UniformGrid.create
                                                  [ UniformGrid.columns 2
                                                    UniformGrid.children
                                                        [ statCard "AGE" (string (Player.age currentDate p)) "🎂" ""
                                                          statCard "CONDITION" (sprintf "%d%%" p.Condition) "💪" "" ] ]

                                              Border.create
                                                  [ Border.height 1.0
                                                    Border.background Theme.Border
                                                    Border.margin (0, 10, 0, 0) ] ] ]


                                  ScrollViewer.create
                                      [ Grid.row 1
                                        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Visible
                                        ScrollViewer.maxHeight 700
                                        ScrollViewer.padding (20.0, 0.0, 20.0, 20.0)
                                        ScrollViewer.content (
                                            StackPanel.create
                                                [ StackPanel.children
                                                      [

                                                        categoryHeader "PHYSICAL"
                                                        statRow "ACCELERATION" p.Physical.Acceleration Theme.Accent
                                                        statRow "PACE" p.Physical.Pace Theme.Accent
                                                        statRow "AGILITY" p.Physical.Agility Theme.Accent
                                                        statRow "BALANCE" p.Physical.Balance Theme.Accent
                                                        statRow "JUMPING REACH" p.Physical.JumpingReach Theme.Accent
                                                        statRow "STAMINA" p.Physical.Stamina Theme.Accent
                                                        statRow "STRENGTH" p.Physical.Strength Theme.Accent


                                                        categoryHeader "MENTAL"
                                                        statRow "AGGRESSION" p.Mental.Aggression Theme.Accent
                                                        statRow "COMPOSURE" p.Mental.Composure Theme.Accent
                                                        statRow "CONCENTRATION" p.Mental.Concentration Theme.Accent
                                                        statRow "VISION" p.Mental.Vision Theme.Accent
                                                        statRow "POSITIONING" p.Mental.Positioning Theme.Accent
                                                        statRow "BRAVERY" p.Mental.Bravery Theme.Accent
                                                        statRow "WORK RATE" p.Mental.WorkRate Theme.Accent
                                                        statRow "LEADERSHIP" p.Mental.Leadership Theme.Accent

                                                        // --- TECHNICAL ---
                                                        categoryHeader "TECHNICAL"
                                                        statRow "FINISHING" p.Technical.Finishing Theme.Accent
                                                        statRow "LONG SHOTS" p.Technical.LongShots Theme.Accent
                                                        statRow "DRIBBLING" p.Technical.Dribbling Theme.Accent
                                                        statRow "BALL CONTROL" p.Technical.BallControl Theme.Accent
                                                        statRow "PASSING" p.Technical.Passing Theme.Accent
                                                        statRow "CROSSING" p.Technical.Crossing Theme.Accent
                                                        statRow "TACKLING" p.Technical.Tackling Theme.Accent
                                                        statRow "MARKING" p.Technical.Marking Theme.Accent
                                                        statRow "HEADING" p.Technical.Heading Theme.Accent
                                                        statRow "FREE KICK" p.Technical.FreeKick Theme.Accent
                                                        statRow "PENALTY" p.Technical.Penalty Theme.Accent

                                                        // --- GOALKEEPING (Solo si es GK) ---
                                                        if p.Position = GK then
                                                            categoryHeader "GOALKEEPING"
                                                            statRow "REFLEXES" p.Goalkeeping.Reflexes "#3b82f6"
                                                            statRow "HANDLING" p.Goalkeeping.Handling "#3b82f6"
                                                            statRow "KICKING" p.Goalkeeping.Kicking "#3b82f6"
                                                            statRow "ONE ON ONE" p.Goalkeeping.OneOnOne "#3b82f6"
                                                            statRow "AERIAL REACH" p.Goalkeeping.AerialReach "#3b82f6"


                                                        Border.create [ Border.height 50.0 ] ] ]
                                        ) ] ] ]

              ) ]
// 2. DISPLAY: Organismos complejos específicos del dominio (Tablas, Banners)
module Display =

    module Matches =
        let private teamBlock (name: string) initial align =
            StackPanel.create
                [ StackPanel.orientation Orientation.Horizontal
                  StackPanel.spacing 15.0
                  StackPanel.horizontalAlignment align
                  StackPanel.children
                      [ if align = HorizontalAlignment.Right then
                            TextBlock.create
                                [ TextBlock.text (name.ToUpper())
                                  TextBlock.fontSize 24.0
                                  TextBlock.fontWeight FontWeight.Black
                                  TextBlock.foreground "White"
                                  TextBlock.verticalAlignment VerticalAlignment.Center ]

                        Border.create
                            [ Border.width 60.0
                              Border.height 60.0
                              Border.cornerRadius 30.0
                              Border.background (
                                  if align = HorizontalAlignment.Right then
                                      "White"
                                  else
                                      "#334155"
                              )
                              Border.child (
                                  TextBlock.create
                                      [ TextBlock.text initial
                                        TextBlock.fontSize 28.0
                                        TextBlock.fontWeight FontWeight.Bold
                                        TextBlock.foreground (
                                            if align = HorizontalAlignment.Right then
                                                "#7f1d1d"
                                            else
                                                "White"
                                        )
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center ]
                              ) ]

                        if align = HorizontalAlignment.Left then
                            TextBlock.create
                                [ TextBlock.text (name.ToUpper())
                                  TextBlock.fontSize 24.0
                                  TextBlock.fontWeight FontWeight.Black
                                  TextBlock.foreground "#94a3b8"
                                  TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

        let nextMatchBanner (homeName: string) (awayName: string) (date: DateTime) (location: string) =
            Border.create
                [ Border.cornerRadius 12.0
                  Border.margin (0.0, 0.0, 0.0, 30.0)
                  Border.padding 30.0
                  Border.child (
                      Grid.create
                          [ Grid.columnDefinitions "*, Auto, *"
                            Grid.children
                                [ StackPanel.create
                                      [ Grid.column 0
                                        StackPanel.children
                                            [ teamBlock homeName (homeName.Substring(0, 1)) HorizontalAlignment.Right ] ]

                                  StackPanel.create
                                      [ Grid.column 1
                                        StackPanel.margin (40.0, 0.0)
                                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                                        StackPanel.verticalAlignment VerticalAlignment.Center
                                        StackPanel.children
                                            [ TextBlock.create
                                                  [ TextBlock.text $"NEXT MATCH ({location})"
                                                    TextBlock.foreground "#fca5a5"
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                    TextBlock.fontSize 10.0 ]
                                              TextBlock.create
                                                  [ TextBlock.text (date.ToString("HH:mm"))
                                                    TextBlock.fontSize 32.0
                                                    TextBlock.fontWeight FontWeight.Black
                                                    TextBlock.foreground "White"
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                              TextBlock.create
                                                  [ TextBlock.text (date.ToString("dd MMMM yyyy"))
                                                    TextBlock.foreground "#94a3b8"
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                    TextBlock.fontSize 12.0 ] ] ]

                                  // Visitante
                                  StackPanel.create
                                      [ Grid.column 2
                                        StackPanel.children
                                            [ teamBlock awayName (awayName.Substring(0, 1)) HorizontalAlignment.Left ] ] ] ]
                  ) ]

        let emptyBanner () =
            Border.create
                [ Border.cornerRadius 12.0
                  Border.margin (0.0, 0.0, 0.0, 30.0)
                  Border.padding 40.0
                  Border.background "#1e293b"
                  Border.child (
                      TextBlock.create
                          [ TextBlock.text "No upcoming matches"
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.foreground "#94a3b8" ]
                  ) ]

    module Tables =

        type RowViewModel =
            { Pos: int
              TeamName: string
              Points: int
              Stats: string // "W-D-L"
              IsUser: bool
              PosColor: string }

        let leagueTable
            (leagues: (int * string) list)
            (selectedId: int)
            (onSelect: int -> unit)
            (rows: RowViewModel list)
            =
            StackPanel.create
                [ StackPanel.children
                      [

                        Border.create
                            [ Border.padding 15.0
                              Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                              Border.child (
                                  DockPanel.create
                                      [ DockPanel.children
                                            [

                                              StackPanel.create
                                                  [ StackPanel.dock Dock.Right
                                                    StackPanel.orientation Orientation.Horizontal
                                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                                    StackPanel.children
                                                        [ TextBlock.create
                                                              [ TextBlock.text "W-D-L"
                                                                TextBlock.width 60.0
                                                                TextBlock.textAlignment TextAlignment.Right
                                                                TextBlock.fontSize 10.0
                                                                TextBlock.foreground Theme.TextMuted ]
                                                          TextBlock.create
                                                              [ TextBlock.text "PTS"
                                                                TextBlock.width 40.0
                                                                TextBlock.textAlignment TextAlignment.Right
                                                                TextBlock.fontSize 10.0
                                                                TextBlock.foreground Theme.TextMuted
                                                                TextBlock.fontWeight FontWeight.Bold ] ] ]

                                              StackPanel.create
                                                  [ StackPanel.orientation Orientation.Horizontal
                                                    StackPanel.children
                                                        [ for id, name in leagues do
                                                              UI.tabButton name (id = selectedId) (fun _ -> onSelect id) ] ] ] ]
                              ) ]


                        ScrollViewer.create
                            [ ScrollViewer.maxHeight 450.0
                              ScrollViewer.content (
                                  StackPanel.create
                                      [ StackPanel.children
                                            [ for row in rows do
                                                  Border.create
                                                      [ Border.background (
                                                            if row.IsUser then "#1e3a8a" else "transparent"
                                                        )
                                                        Border.padding (15.0, 8.0)
                                                        Border.borderBrush (
                                                            if row.IsUser then "transparent" else "#1e293b"
                                                        )
                                                        Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                                        Border.child (
                                                            DockPanel.create
                                                                [ DockPanel.children
                                                                      [

                                                                        TextBlock.create
                                                                            [ TextBlock.dock Dock.Right
                                                                              TextBlock.width 40.0
                                                                              TextBlock.textAlignment
                                                                                  TextAlignment.Right
                                                                              TextBlock.text (string row.Points)
                                                                              TextBlock.fontWeight FontWeight.Bold
                                                                              TextBlock.foreground (
                                                                                  if row.IsUser then
                                                                                      "White"
                                                                                  else
                                                                                      Theme.TextMain
                                                                              ) ]

                                                                        TextBlock.create
                                                                            [ TextBlock.dock Dock.Right
                                                                              TextBlock.width 60.0
                                                                              TextBlock.textAlignment
                                                                                  TextAlignment.Right
                                                                              TextBlock.text row.Stats
                                                                              TextBlock.fontSize 11.0
                                                                              TextBlock.foreground (
                                                                                  if row.IsUser then
                                                                                      "#cbd5e1"
                                                                                  else
                                                                                      Theme.TextMuted
                                                                              ) ]


                                                                        TextBlock.create
                                                                            [ TextBlock.text (string row.Pos)
                                                                              TextBlock.width 30.0
                                                                              TextBlock.foreground row.PosColor
                                                                              TextBlock.fontWeight FontWeight.Bold ]

                                                                        TextBlock.create
                                                                            [ TextBlock.text row.TeamName
                                                                              TextBlock.fontWeight (
                                                                                  if row.IsUser then
                                                                                      FontWeight.Bold
                                                                                  else
                                                                                      FontWeight.Normal
                                                                              )
                                                                              TextBlock.foreground (
                                                                                  if row.IsUser then
                                                                                      "White"
                                                                                  else
                                                                                      Theme.TextMain
                                                                              ) ] ] ]
                                                        ) ] ] ]
                              ) ] ] ]
