namespace FootballEngine.Components

open Avalonia
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Domain
open FootballEngine.Icons
open FootballEngine.MatchState


module MatchViewer =

    [<Literal>]
    let private PitchW = 820.0

    [<Literal>]
    let private PitchH = 540.0

    let private positionLabel (p: Position) = $"%A{p}"
    let private toCanvas (x: float, y: float) = x * PitchW / 100.0, y * PitchH / 100.0

    let private playerOpacity c =
        0.60 + (float (max 0 (min 100 c)) / 250.0)

    let private conditionColor c =
        if c >= 75 then Theme.Success
        elif c >= 50 then Theme.Warning
        else Theme.Danger

    let private drawPlayer (x: float) (y: float) (label: string) (teamColor: string) (condition: int) : IView =
        let opacity = playerOpacity condition
        let r = 15.0
        let d = r * 2.0

        Canvas.create
            [ Canvas.left (x - r - 2.0)
              Canvas.top (y - r - 2.0)
              Canvas.children
                  [ Ellipse.create
                        [ Ellipse.width (d + 6.0)
                          Ellipse.height (d + 6.0)
                          Ellipse.fill teamColor
                          Ellipse.opacity (opacity * 0.20) ]
                    Ellipse.create
                        [ Canvas.left 1.0
                          Canvas.top 1.0
                          Ellipse.width (d + 2.0)
                          Ellipse.height (d + 2.0)
                          Ellipse.fill "transparent"
                          Ellipse.stroke (conditionColor condition)
                          Ellipse.strokeThickness 2.0
                          Ellipse.opacity (opacity * 0.9) ]
                    Ellipse.create
                        [ Canvas.left 3.0
                          Canvas.top 3.0
                          Ellipse.width d
                          Ellipse.height d
                          Ellipse.fill teamColor
                          Ellipse.opacity opacity ]
                    TextBlock.create
                        [ Canvas.left 3.0
                          Canvas.top 3.0
                          TextBlock.text label
                          TextBlock.foreground Theme.TextMain
                          TextBlock.fontSize 7.5
                          TextBlock.width d
                          TextBlock.height d
                          TextBlock.textAlignment TextAlignment.Center
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.fontWeight FontWeight.Bold ] ] ]

    let private drawTeam
        (players: Player[])
        (conditions: int[])
        (positions: Map<PlayerId, float * float>)
        (sidelined: Map<PlayerId, PlayerOut>)
        (color: string)
        : IView list =
        players
        |> Array.mapi (fun i p ->
            if Map.containsKey p.Id sidelined then
                None
            else
                let cx, cy = toCanvas (positionOf positions p)
                let cond = conditions |> Array.tryItem i |> Option.defaultValue 100
                Some(drawPlayer cx cy (positionLabel p.Position) color cond))
        |> Array.choose id
        |> Array.toList

    let private drawBall (bx: float) (by: float) : IView list =
        [ Ellipse.create
              [ Canvas.left (bx - 9.0)
                Canvas.top (by + 3.0)
                Ellipse.width 18.0
                Ellipse.height 6.0
                Ellipse.fill "#00000060" ]
          Ellipse.create
              [ Canvas.left (bx - 7.0)
                Canvas.top (by - 7.0)
                Ellipse.width 14.0
                Ellipse.height 14.0
                Ellipse.fill Theme.TextMain
                Ellipse.stroke "#AAAAAA"
                Ellipse.strokeThickness 1.0 ]
          Ellipse.create
              [ Canvas.left (bx - 4.0)
                Canvas.top (by - 5.5)
                Ellipse.width 5.0
                Ellipse.height 4.0
                Ellipse.fill "#FFFFFF99" ] ]

    let private pitchMarkings () : IView list =
        let w, h = PitchW, PitchH
        let lc = "#FFFFFF55"
        let lw = 1.5
        let paW, paH = w / 5.0, 220.0
        let gkW, gkH = w / 12.0, 110.0

        let rect left top width height =
            Rectangle.create
                [ Canvas.left left
                  Canvas.top top
                  Rectangle.width width
                  Rectangle.height height
                  Rectangle.stroke lc
                  Rectangle.strokeThickness lw
                  Rectangle.fill "transparent" ]

        let dot cx cy =
            Ellipse.create
                [ Canvas.left (cx - 3.5)
                  Canvas.top (cy - 3.5)
                  Ellipse.width 7.0
                  Ellipse.height 7.0
                  Ellipse.fill lc ]

        [ Rectangle.create
              [ Rectangle.width w
                Rectangle.height h
                Rectangle.stroke "#FFFFFF88"
                Rectangle.strokeThickness 2.0
                Rectangle.fill "transparent" ]
          Line.create
              [ Line.startPoint (w / 2.0, 0.0)
                Line.endPoint (w / 2.0, h)
                Line.stroke lc
                Line.strokeThickness lw ]
          Ellipse.create
              [ Canvas.left (w / 2.0 - 65.0)
                Canvas.top (h / 2.0 - 65.0)
                Ellipse.width 130.0
                Ellipse.height 130.0
                Ellipse.stroke lc
                Ellipse.strokeThickness lw
                Ellipse.fill "transparent" ]
          dot (w / 2.0) (h / 2.0)
          rect 0.0 (h / 2.0 - paH / 2.0) paW paH
          rect (w - paW) (h / 2.0 - paH / 2.0) paW paH
          rect 0.0 (h / 2.0 - gkH / 2.0) gkW gkH
          rect (w - gkW) (h / 2.0 - gkH / 2.0) gkW gkH
          dot (paW * 0.75) (h / 2.0)
          dot (w - paW * 0.75) (h / 2.0) ]

    let private hudPill (left: float) (top: float) (child: IView) : IView =
        Border.create
            [ Canvas.left left
              Canvas.top top
              Border.background (Theme.BgMain + "DD")
              Border.borderBrush Theme.Border
              Border.borderThickness (Thickness 1.0)
              Border.cornerRadius 10.0
              Border.padding (Thickness(12.0, 6.0))
              Border.child child ]

    let private possessionOverlay (s: MatchState) : IView =
        let name, color =
            match s.Possession with
            | Home -> s.Home.Name, Theme.AccentAlt
            | Away -> s.Away.Name, Theme.Danger

        hudPill 10.0 10.0
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 7.0
              StackPanel.children
                  [ Icons.iconSm UI.simulate color
                    Ellipse.create
                        [ Ellipse.width 9.0
                          Ellipse.height 9.0
                          Ellipse.fill color
                          Ellipse.verticalAlignment VerticalAlignment.Center ]
                    TextBlock.create
                        [ TextBlock.text name
                          TextBlock.foreground color
                          TextBlock.fontSize 12.0
                          TextBlock.fontWeight FontWeight.SemiBold
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private momentumBar (s: MatchState) : IView =
        let barW, barH = 200.0, 6.0
        let homeW = ((s.Momentum + 10.0) / 20.0) * barW
        let awayW = barW - homeW

        let teamLabel name align =
            TextBlock.create
                [ TextBlock.text name
                  TextBlock.foreground Theme.TextMuted
                  TextBlock.fontSize 9.0
                  TextBlock.width (barW / 2.0)
                  TextBlock.textAlignment align ]

        let bar (color: string) left width =
            Rectangle.create
                [ Canvas.left left
                  Canvas.top 0.0
                  Rectangle.width (max 3.0 width)
                  Rectangle.height barH
                  Rectangle.fill color
                  Rectangle.radiusX 3.0
                  Rectangle.radiusY 3.0 ]

        hudPill (PitchW / 2.0 - 122.0) (PitchH - 44.0)
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.spacing 4.0
              StackPanel.children
                  [ StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.children
                              [ teamLabel s.Home.Name TextAlignment.Left
                                teamLabel s.Away.Name TextAlignment.Right ] ]
                    Border.create
                        [ Border.width barW
                          Border.height barH
                          Border.cornerRadius 3.0
                          Border.background Theme.BgHover
                          Border.child (
                              Canvas.create
                                  [ Canvas.width barW
                                    Canvas.height barH
                                    Canvas.children
                                        [ bar Theme.AccentAlt 0.0 homeW
                                          bar Theme.Danger (barW - (max 3.0 awayW)) awayW ] ]
                          ) ] ] ]

    let view (s: MatchState) : IView =
        let bcx, bcy = toCanvas s.BallPosition

        Viewbox.create
            [ Viewbox.stretch Stretch.Uniform
              Viewbox.horizontalAlignment HorizontalAlignment.Stretch
              Viewbox.verticalAlignment VerticalAlignment.Stretch
              Viewbox.child (
                  Canvas.create
                      [ Canvas.width PitchW
                        Canvas.height PitchH
                        Canvas.background "#2d5a1b"
                        Canvas.children
                            [ yield! pitchMarkings ()
                              yield!
                                  drawTeam
                                      s.HomePlayers
                                      s.HomeConditions
                                      s.HomePositions
                                      s.HomeSidelined
                                      Theme.AccentAlt
                              yield!
                                  drawTeam s.AwayPlayers s.AwayConditions s.AwayPositions s.AwaySidelined Theme.Danger
                              yield! drawBall bcx bcy
                              possessionOverlay s
                              momentumBar s ] ]
              ) ]


module MatchLabView =

    let private clubPicker
        (label: string)
        (selected: ClubId option)
        (clubs: Club list)
        (msg: ClubId -> Msg)
        (dispatch: Msg -> unit)
        =
        StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.spacing 4.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text label
                          TextBlock.foreground Theme.TextMuted
                          TextBlock.fontSize 10.0
                          TextBlock.fontWeight FontWeight.SemiBold ]
                    ComboBox.create
                        [ ComboBox.width 200.0
                          ComboBox.dataItems clubs
                          ComboBox.itemTemplate (
                              DataTemplateView<Club>.create (fun c ->
                                  TextBlock.create [ TextBlock.text c.Name; TextBlock.foreground Theme.TextMain ])
                          )
                          ComboBox.selectedItem (
                              selected
                              |> Option.bind (fun id -> clubs |> List.tryFind (fun c -> c.Id = id))
                              |> Option.toObj
                          )
                          ComboBox.onSelectionChanged (fun e ->
                              if e.AddedItems.Count > 0 then
                                  match e.AddedItems[0] with
                                  | :? Club as c -> dispatch (msg c.Id)
                                  | _ -> ()) ] ] ]

    let private eventMeta =
        function
        | Goal -> Icons.iconMd MatchEvent.goal Theme.Accent, Theme.Accent, "Goal"
        | OwnGoal -> Icons.iconMd MatchEvent.ownGoal Theme.Danger, Theme.Danger, "Own Goal"
        | Assist -> Icons.iconMd MatchEvent.assist Theme.Accent, Theme.Accent, "Assist"
        | YellowCard -> Icons.iconMd MatchEvent.yellowCard Theme.Warning, Theme.Warning, "Yellow"
        | RedCard -> Icons.iconMd MatchEvent.redCard Theme.Danger, Theme.Danger, "Red Card"
        | Injury _ -> Icons.iconMd MatchEvent.injury Theme.Danger, Theme.Danger, "Injury"
        | SubstitutionIn -> Icons.iconMd MatchEvent.subOn Theme.TextSub, Theme.TextSub, "Sub On"
        | SubstitutionOut -> Icons.iconMd MatchEvent.subOff Theme.TextSub, Theme.TextSub, "Sub Off"

    let private eventRow (clubs: Map<ClubId, Club>) (players: Map<PlayerId, Player>) (ev: MatchEvent) : IView =
        let icon, color, label = eventMeta ev.Type

        let clubName =
            clubs |> Map.tryFind ev.ClubId |> Option.map _.Name |> Option.defaultValue "?"

        let playerName =
            players
            |> Map.tryFind ev.PlayerId
            |> Option.map _.Name
            |> Option.defaultValue ""

        Border.create
            [ Border.padding (Thickness(8.0, 6.0))
              Border.cornerRadius 6.0
              Border.margin (Thickness(0.0, 1.5))
              Border.background Theme.BgCard
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "Auto,Auto,*"
                        Grid.rowDefinitions "Auto,Auto"
                        Grid.children
                            [ Border.create
                                  [ Grid.column 0
                                    Grid.rowSpan 2
                                    Border.background Theme.BgHover
                                    Border.cornerRadius 4.0
                                    Border.padding (Thickness(6.0, 2.0))
                                    Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text $"{ev.Second / 60}'"
                                              TextBlock.foreground Theme.TextMuted
                                              TextBlock.fontSize 10.0
                                              TextBlock.fontWeight FontWeight.SemiBold ]
                                    ) ]
                              Grid.create
                                  [ Grid.column 1
                                    Grid.row 0
                                    Grid.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                                    Grid.children [ icon ] ]
                              StackPanel.create
                                  [ Grid.column 2
                                    Grid.row 0
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 5.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text label
                                                TextBlock.foreground color
                                                TextBlock.fontSize 11.0
                                                TextBlock.fontWeight FontWeight.SemiBold ]
                                          TextBlock.create
                                              [ TextBlock.text $"· {clubName}"
                                                TextBlock.foreground Theme.TextMuted
                                                TextBlock.fontSize 11.0 ] ] ]
                              TextBlock.create
                                  [ Grid.column 1
                                    Grid.columnSpan 2
                                    Grid.row 1
                                    TextBlock.text playerName
                                    TextBlock.foreground Theme.TextSub
                                    TextBlock.fontSize 10.0 ] ] ]
              ) ]

    let private eventLog (replay: MatchReplay) (clubs: Map<ClubId, Club>) (players: Map<PlayerId, Player>) : IView =
        let relevant =
            replay.Final.EventsRev
            |> List.rev
            |> List.filter (fun ev ->
                match ev.Type with
                | Goal
                | OwnGoal
                | Assist
                | YellowCard
                | RedCard
                | Injury _
                | SubstitutionIn
                | SubstitutionOut -> true)

        DockPanel.create
            [ DockPanel.width 260.0
              DockPanel.lastChildFill true
              DockPanel.background Theme.BgSidebar
              DockPanel.children
                  [ Border.create
                        [ DockPanel.dock Dock.Top
                          Border.padding (Thickness(12.0, 12.0, 12.0, 8.0))
                          Border.borderBrush Theme.Border
                          Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 6.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text "Events"
                                                TextBlock.foreground Theme.TextSub
                                                TextBlock.fontSize 11.0
                                                TextBlock.fontWeight FontWeight.SemiBold ]
                                          Border.create
                                              [ Border.background Theme.AccentLight
                                                Border.cornerRadius 8.0
                                                Border.padding (Thickness(6.0, 1.0))
                                                Border.child (
                                                    TextBlock.create
                                                        [ TextBlock.text $"{relevant.Length}"
                                                          TextBlock.foreground Theme.Accent
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.Bold ]
                                                ) ] ] ]
                          ) ]
                    ScrollViewer.create
                        [ ScrollViewer.padding (Thickness 10.0)
                          ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                          ScrollViewer.content (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Vertical
                                    StackPanel.children (
                                        if relevant.IsEmpty then
                                            [ TextBlock.create
                                                  [ TextBlock.text "No events yet"
                                                    TextBlock.foreground Theme.TextMuted
                                                    TextBlock.fontSize 11.0
                                                    TextBlock.margin (Thickness(0.0, 8.0)) ]
                                              :> IView ]
                                        else
                                            [ yield! relevant |> List.map (eventRow clubs players)
                                              Border.create [ Border.height 30.0 ] ]
                                    ) ]
                          ) ] ] ]

    let private vSep () : IView =
        Border.create
            [ Border.width 1.0
              Border.height 28.0
              Border.background Theme.Border
              Border.verticalAlignment VerticalAlignment.Center ]

    let private navBtn (icon: IView) (enabled: bool) (onClick: unit -> unit) : IView =
        Button.create
            [ Button.content icon
              Button.width 32.0
              Button.height 30.0
              Button.padding (Thickness 0.0)
              Button.background Theme.BgHover
              Button.foreground (if enabled then Theme.TextMain else Theme.TextMuted)
              Button.cornerRadius 5.0
              Button.isEnabled enabled
              Button.horizontalContentAlignment HorizontalAlignment.Center
              Button.onClick (fun _ -> onClick ()) ]

    let private go (delta: int) (dispatch: Msg -> unit) = dispatch (MatchLabMsg(Step delta))

    let private toolbarNav (current: MatchState) (idx: int) (total: int) (replayKey: string) (dispatch: Msg -> unit) =
        let navButtons =
            [ Nav.skipFirst, -total, idx > 0
              Nav.rewind, -10, idx > 0
              Nav.prev, -1, idx > 0
              Nav.next, +1, idx < total
              Nav.fastForward, +10, idx < total
              Nav.skipLast, +total, idx < total ]

        Component.create (
            $"toolbar-nav-{replayKey}-{idx}",
            fun _ ->
                StackPanel.create
                    [ StackPanel.orientation Orientation.Horizontal
                      StackPanel.spacing 6.0
                      StackPanel.verticalAlignment VerticalAlignment.Center
                      StackPanel.children
                          [ vSep ()
                            Icons.iconMd UI.calendar Theme.Warning
                            TextBlock.create
                                [ TextBlock.text $"{current.Second / 60}:{current.Second % 60:D2}"
                                  TextBlock.foreground Theme.Warning
                                  TextBlock.fontSize 12.0
                                  TextBlock.fontWeight FontWeight.Bold
                                  TextBlock.verticalAlignment VerticalAlignment.Center ]
                            TextBlock.create
                                [ TextBlock.text current.Home.Name
                                  TextBlock.foreground Theme.AccentAlt
                                  TextBlock.fontSize 13.0
                                  TextBlock.fontWeight FontWeight.SemiBold
                                  TextBlock.verticalAlignment VerticalAlignment.Center ]
                            TextBlock.create
                                [ TextBlock.text $"{current.HomeScore} – {current.AwayScore}"
                                  TextBlock.foreground Theme.TextMain
                                  TextBlock.fontSize 16.0
                                  TextBlock.fontWeight FontWeight.Black
                                  TextBlock.verticalAlignment VerticalAlignment.Center ]
                            TextBlock.create
                                [ TextBlock.text current.Away.Name
                                  TextBlock.foreground Theme.Danger
                                  TextBlock.fontSize 13.0
                                  TextBlock.fontWeight FontWeight.SemiBold
                                  TextBlock.verticalAlignment VerticalAlignment.Center ]
                            vSep ()
                            // Nav buttons generados desde la lista
                            yield!
                                navButtons
                                |> List.mapi (fun i (iconKind, delta, enabled) ->
                                    // Insertar counter entre prev y next
                                    let btn =
                                        navBtn (Icons.iconMd iconKind Theme.TextMain) enabled (fun () ->
                                            go delta dispatch)

                                    if i = 2 then
                                        [ btn
                                          Border.create
                                              [ Border.background Theme.BgCard
                                                Border.cornerRadius 4.0
                                                Border.padding (Thickness(8.0, 4.0))
                                                Border.verticalAlignment VerticalAlignment.Center
                                                Border.child (
                                                    TextBlock.create
                                                        [ TextBlock.text $"{idx} / {total}"
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.SemiBold ]
                                                ) ]
                                          :> IView ]
                                    else
                                        [ btn ])
                                |> List.concat ] ]
        )

    let private replayControls (state: State) (replay: MatchReplay) (dispatch: Msg -> unit) : IView =
        let idx = state.MatchLab.Snapshot

        let current =
            replay.Snapshots |> Array.tryItem idx |> Option.defaultValue replay.Final

        DockPanel.create
            [ DockPanel.lastChildFill true
              DockPanel.children
                  [ Border.create
                        [ DockPanel.dock Dock.Right
                          Border.borderBrush Theme.Border
                          Border.borderThickness (Thickness(1.0, 0.0, 0.0, 0.0))
                          Border.child (eventLog replay state.GameState.Clubs state.GameState.Players) ]
                    MatchViewer.view current ] ]
        :> IView

    let view (state: State) (dispatch: Msg -> unit) =
        let clubs =
            state.GameState.Clubs |> Map.toList |> List.map snd |> List.sortBy _.Name

        DockPanel.create
            [ DockPanel.background Theme.BgMain
              DockPanel.children
                  [ Border.create
                        [ DockPanel.dock Dock.Top
                          Border.background Theme.BgCard
                          Border.borderBrush Theme.Border
                          Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
                          Border.padding (Thickness(16.0, 10.0))
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 16.0
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children
                                        [ clubPicker
                                              "Home"
                                              state.MatchLab.HomeClubId
                                              clubs
                                              (fun id -> MatchLabMsg(SelectHome id))
                                              dispatch
                                          clubPicker
                                              "Away"
                                              state.MatchLab.AwayClubId
                                              clubs
                                              (fun id -> MatchLabMsg(SelectAway id))
                                              dispatch
                                          Button.create
                                              [ Button.content (
                                                    StackPanel.create
                                                        [ StackPanel.orientation Orientation.Horizontal
                                                          StackPanel.spacing 6.0
                                                          StackPanel.children
                                                              [ Icons.iconMd UI.simulate Theme.BgMain
                                                                TextBlock.create
                                                                    [ TextBlock.text "Simulate"
                                                                      TextBlock.foreground Theme.BgMain
                                                                      TextBlock.fontWeight FontWeight.Bold
                                                                      TextBlock.verticalAlignment
                                                                          VerticalAlignment.Center ] ] ]
                                                )
                                                Button.verticalAlignment VerticalAlignment.Bottom
                                                Button.background Theme.Accent
                                                Button.foreground Theme.BgMain
                                                Button.padding (Thickness(18.0, 8.0))
                                                Button.cornerRadius 7.0
                                                Button.fontWeight FontWeight.Bold
                                                Button.onClick (fun _ -> dispatch (MatchLabMsg Run)) ]
                                          match state.MatchLab.Result with
                                          | Some replay ->
                                              let idx = state.MatchLab.Snapshot
                                              let total = max 1 (replay.Snapshots.Length - 1)

                                              let cur =
                                                  replay.Snapshots
                                                  |> Array.tryItem idx
                                                  |> Option.defaultValue replay.Final

                                              let replayKey =
                                                  $"{replay.Snapshots.Length}-{replay.Final.HomeScore}-{replay.Final.AwayScore}"

                                              toolbarNav cur idx total replayKey dispatch
                                          | None -> () ] ]
                          ) ]

                    match state.MatchLab.Result with
                    | None ->
                        StackPanel.create
                            [ StackPanel.orientation Orientation.Vertical
                              StackPanel.spacing 12.0
                              StackPanel.horizontalAlignment HorizontalAlignment.Center
                              StackPanel.verticalAlignment VerticalAlignment.Center
                              StackPanel.children
                                  [ Icons.iconXl MatchEvent.goal Theme.TextMuted
                                    TextBlock.create
                                        [ TextBlock.text "Select two clubs and simulate a match"
                                          TextBlock.foreground Theme.TextMuted
                                          TextBlock.fontSize 16.0
                                          TextBlock.horizontalAlignment HorizontalAlignment.Center ] ] ]
                        :> IView
                    | Some replay -> replayControls state replay dispatch ] ]
