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

module MatchViewer =

    [<Literal>]
    let private PitchW = 820.0

    [<Literal>]
    let private PitchH = 540.0

    let private positionLabel (p: Position) = $"%A{p}"
    let private toCanvas (x: float, y: float) = x * PitchW / 100.0, y * PitchH / 100.0

    let private lerpFloat (t: float) (a: float) (b: float) =
        let t' = t * t * (3.0 - 2.0 * t)
        a + (b - a) * t'

    let private lerpPos (t: float) (ax, ay) (bx, by) = lerpFloat t ax bx, lerpFloat t ay by

    let interpolateState (t: float) (curr: MatchState) (next: MatchState) =
        let lerpArr (a: (float * float)[]) (b: (float * float)[]) =
            let len = min a.Length b.Length
            Array.init len (fun i -> lerpPos t a[i] b[i])

        {| HomePosLerped = lerpArr curr.HomeSide.Positions next.HomeSide.Positions
           AwayPosLerped = lerpArr curr.AwaySide.Positions next.AwaySide.Positions
           BallLerped = lerpPos t curr.BallPosition next.BallPosition |}

    let private conditionColor c =
        if c >= 75 then Theme.Success
        elif c >= 50 then Theme.Warning
        else Theme.Danger

    let private shortName (p: Player) =
        let parts = p.Name.Split(' ')

        if parts.Length >= 2 then
            $"{parts[0][0]}. {parts[parts.Length - 1]}"
        else
            p.Name

    let private skillColor skill =
        if skill >= 75 then Theme.Accent
        elif skill >= 55 then Theme.Warning
        else Theme.Danger

    let private drawPlayer (x: float) (y: float) (p: Player) (teamColor: string) (condition: int) : IView =
        let r = 18.0
        let d = r * 2.0
        let condColor = conditionColor condition
        let skColor = skillColor p.CurrentSkill
        let abbrev = positionLabel p.Position
        let name = shortName p
        let skill = string p.CurrentSkill

        Canvas.create
            [ Canvas.left (x - r - 4.0)
              Canvas.top (y - r - 14.0)
              Canvas.children
                  [ Ellipse.create
                        [ Canvas.left 2.0
                          Canvas.top 2.0
                          Ellipse.width (d + 6.0)
                          Ellipse.height (d + 6.0)
                          Ellipse.fill (teamColor + "DD") ]
                    Ellipse.create
                        [ Canvas.left 2.0
                          Canvas.top 2.0
                          Ellipse.width (d + 6.0)
                          Ellipse.height (d + 6.0)
                          Ellipse.fill "transparent"
                          Ellipse.stroke condColor
                          Ellipse.strokeThickness 2.0 ]
                    TextBlock.create
                        [ Canvas.left 5.0
                          Canvas.top 5.0
                          TextBlock.text abbrev
                          TextBlock.foreground "#FFFFFF"
                          TextBlock.fontSize 6.5
                          TextBlock.width d
                          TextBlock.height (d * 0.5)
                          TextBlock.textAlignment TextAlignment.Center
                          TextBlock.verticalAlignment VerticalAlignment.Bottom
                          TextBlock.fontWeight FontWeight.Black ]
                    TextBlock.create
                        [ Canvas.left 5.0
                          Canvas.top (5.0 + d * 0.5)
                          TextBlock.text skill
                          TextBlock.foreground skColor
                          TextBlock.fontSize 8.0
                          TextBlock.width d
                          TextBlock.height (d * 0.5)
                          TextBlock.textAlignment TextAlignment.Center
                          TextBlock.verticalAlignment VerticalAlignment.Top
                          TextBlock.fontWeight FontWeight.Black ]
                    Border.create
                        [ Canvas.left -4.0
                          Canvas.top (d + 10.0)
                          Border.background (Theme.BgMain + "CC")
                          Border.cornerRadius 3.0
                          Border.padding (Thickness(3.0, 1.0))
                          Border.child (
                              TextBlock.create
                                  [ TextBlock.text name
                                    TextBlock.foreground "#FFFFFFCC"
                                    TextBlock.fontSize 6.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.textAlignment TextAlignment.Center
                                    TextBlock.width (d + 8.0) ]
                          ) ] ] ]

    let private drawTeam
        (players: Player[])
        (conditions: int[])
        (positions: (float * float)[])
        (sidelined: Map<PlayerId, PlayerOut>)
        (color: string)
        : IView list =
        players
        |> Array.mapi (fun i p ->
            if Map.containsKey p.Id sidelined then
                None
            else
                let cx, cy = toCanvas positions[i]
                let cond = conditions |> Array.tryItem i |> Option.defaultValue 100
                Some(drawPlayer cx cy p color cond))
        |> Array.choose id
        |> Array.toList

    let private drawBall (bx: float) (by: float) : IView list =
        [ Ellipse.create
              [ Canvas.left (bx - 10.0)
                Canvas.top (by + 5.0)
                Ellipse.width 20.0
                Ellipse.height 7.0
                Ellipse.fill "#00000055" ]
          Ellipse.create
              [ Canvas.left (bx - 8.0)
                Canvas.top (by - 8.0)
                Ellipse.width 16.0
                Ellipse.height 16.0
                Ellipse.fill "#F8F8F8"
                Ellipse.stroke "#CCCCCC"
                Ellipse.strokeThickness 1.0 ]
          Ellipse.create
              [ Canvas.left (bx - 4.0)
                Canvas.top (by - 6.0)
                Ellipse.width 5.0
                Ellipse.height 4.0
                Ellipse.fill "#FFFFFFBB" ] ]

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
              Border.background (Theme.BgMain + "E0")
              Border.borderBrush (Theme.Border + "88")
              Border.borderThickness (Thickness 1.0)
              Border.cornerRadius 8.0
              Border.padding (Thickness(10.0, 5.0))
              Border.child child ]

    let private possessionOverlay (s: MatchState) : IView =
        let name, color =
            match s.Possession with
            | Home -> s.Home.Name, Theme.AccentAlt
            | Away -> s.Away.Name, Theme.Danger

        hudPill 10.0 10.0
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 6.0
              StackPanel.children
                  [ Ellipse.create
                        [ Ellipse.width 8.0
                          Ellipse.height 8.0
                          Ellipse.fill color
                          Ellipse.verticalAlignment VerticalAlignment.Center ]
                    TextBlock.create
                        [ TextBlock.text (name.ToUpper())
                          TextBlock.foreground color
                          TextBlock.fontSize 10.0
                          TextBlock.fontWeight FontWeight.Black
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                    TextBlock.create
                        [ TextBlock.text "IN POSSESSION"
                          TextBlock.foreground Theme.TextMuted
                          TextBlock.fontSize 9.0
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private scoreOverlay (s: MatchState) : IView =
        hudPill (PitchW / 2.0 - 75.0) 10.0
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 8.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text s.Home.Name
                          TextBlock.foreground Theme.AccentAlt
                          TextBlock.fontSize 11.0
                          TextBlock.fontWeight FontWeight.SemiBold
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                    TextBlock.create
                        [ TextBlock.text $"{s.HomeScore} – {s.AwayScore}"
                          TextBlock.foreground Theme.TextMain
                          TextBlock.fontSize 15.0
                          TextBlock.fontWeight FontWeight.Black
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                    TextBlock.create
                        [ TextBlock.text s.Away.Name
                          TextBlock.foreground Theme.Danger
                          TextBlock.fontSize 11.0
                          TextBlock.fontWeight FontWeight.SemiBold
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private minuteOverlay (s: MatchState) : IView =
        hudPill (PitchW - 72.0) 10.0
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 4.0
              StackPanel.children
                  [ Icons.iconSm IconName.calendar Theme.Warning
                    TextBlock.create
                        [ TextBlock.text $"{s.Second / 60}'"
                          TextBlock.foreground Theme.Warning
                          TextBlock.fontSize 11.0
                          TextBlock.fontWeight FontWeight.Black
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private momentumBar (s: MatchState) : IView =
        let barW = 240.0
        let homeW = ((s.Momentum + 10.0) / 20.0) * barW |> max 4.0 |> min (barW - 4.0)
        let awayW = barW - homeW

        hudPill (PitchW / 2.0 - 135.0) (PitchH - 42.0)
        <| StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.spacing 3.0
              StackPanel.children
                  [ StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.children
                              [ TextBlock.create
                                    [ TextBlock.text (s.Home.Name.ToUpper())
                                      TextBlock.foreground Theme.AccentAlt
                                      TextBlock.fontSize 8.0
                                      TextBlock.fontWeight FontWeight.Black
                                      TextBlock.width (barW / 2.0)
                                      TextBlock.textAlignment TextAlignment.Left ]
                                TextBlock.create
                                    [ TextBlock.text "MOMENTUM"
                                      TextBlock.foreground Theme.TextMuted
                                      TextBlock.fontSize 8.0
                                      TextBlock.fontWeight FontWeight.Bold
                                      TextBlock.width (barW / 2.0)
                                      TextBlock.textAlignment TextAlignment.Center ]
                                TextBlock.create
                                    [ TextBlock.text (s.Away.Name.ToUpper())
                                      TextBlock.foreground Theme.Danger
                                      TextBlock.fontSize 8.0
                                      TextBlock.fontWeight FontWeight.Black
                                      TextBlock.width (barW / 2.0)
                                      TextBlock.textAlignment TextAlignment.Right ] ] ]
                    Border.create
                        [ Border.width barW
                          Border.height 5.0
                          Border.cornerRadius 3.0
                          Border.background Theme.BgHover
                          Border.child (
                              Canvas.create
                                  [ Canvas.width barW
                                    Canvas.height 5.0
                                    Canvas.children
                                        [ Rectangle.create
                                              [ Canvas.left 0.0
                                                Canvas.top 0.0
                                                Rectangle.width homeW
                                                Rectangle.height 5.0
                                                Rectangle.fill Theme.AccentAlt
                                                Rectangle.radiusX 3.0
                                                Rectangle.radiusY 3.0 ]
                                          Rectangle.create
                                              [ Canvas.left (barW - awayW)
                                                Canvas.top 0.0
                                                Rectangle.width awayW
                                                Rectangle.height 5.0
                                                Rectangle.fill Theme.Danger
                                                Rectangle.radiusX 3.0
                                                Rectangle.radiusY 3.0 ] ] ]
                          ) ] ] ]

    let view
        (s: MatchState)
        (homePositions: (float * float)[])
        (awayPositions: (float * float)[])
        (ballPos: float * float)
        : IView =
        let bcx, bcy = toCanvas ballPos

        Viewbox.create
            [ Viewbox.stretch Stretch.Uniform
              Viewbox.horizontalAlignment HorizontalAlignment.Stretch
              Viewbox.verticalAlignment VerticalAlignment.Stretch
              Viewbox.margin (8.0, 12.0, 8.0, 12.0)
              Viewbox.child (
                  Canvas.create
                      [ Canvas.width PitchW
                        Canvas.height PitchH
                        Canvas.background "#2d5a1b"
                        Canvas.children
                            [ yield! pitchMarkings ()
                              yield!
                                  drawTeam
                                      s.HomeSide.Players
                                      s.HomeSide.Conditions
                                      homePositions
                                      s.HomeSide.Sidelined
                                      Theme.AccentAlt
                              yield!
                                  drawTeam
                                      s.AwaySide.Players
                                      s.AwaySide.Conditions
                                      awayPositions
                                      s.AwaySide.Sidelined
                                      Theme.Danger
                              yield! drawBall bcx bcy
                              possessionOverlay s
                              scoreOverlay s
                              minuteOverlay s
                              momentumBar s ] ]
              ) ]

open MatchViewer

module MatchDayView =

    let private vSep () : IView =
        Border.create
            [ Border.width 1.0
              Border.height 22.0
              Border.background Theme.Border
              Border.verticalAlignment VerticalAlignment.Center ]

    let private navBtn (iconKind: Material.Icons.MaterialIconKind) (enabled: bool) (onClick: unit -> unit) : IView =
        Button.create
            [ Button.width 28.0
              Button.height 28.0
              Button.padding (Thickness 0.0)
              Button.background (if enabled then Theme.BgHover else "transparent")
              Button.cornerRadius 5.0
              Button.isEnabled enabled
              Button.horizontalContentAlignment HorizontalAlignment.Center
              Button.content (Icons.iconMd iconKind (if enabled then Theme.TextMain else Theme.TextMuted))
              Button.onClick (fun _ -> onClick ()) ]

    let private finalScorePill (replay: MatchReplay) : IView =
        Border.create
            [ Border.background Theme.BgCard
              Border.cornerRadius 8.0
              Border.borderBrush Theme.Border
              Border.borderThickness (Thickness 1.0)
              Border.padding (Thickness(14.0, 6.0))
              Border.verticalAlignment VerticalAlignment.Center
              Border.child (
                  StackPanel.create
                      [ StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 8.0
                        StackPanel.children
                            [ TextBlock.create
                                  [ TextBlock.text "FT"
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.fontSize 9.0
                                    TextBlock.fontWeight FontWeight.Black
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              TextBlock.create
                                  [ TextBlock.text replay.Final.Home.Name
                                    TextBlock.foreground Theme.AccentAlt
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              TextBlock.create
                                  [ TextBlock.text $"{replay.Final.HomeScore} – {replay.Final.AwayScore}"
                                    TextBlock.foreground Theme.TextMain
                                    TextBlock.fontSize 16.0
                                    TextBlock.fontWeight FontWeight.Black
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              TextBlock.create
                                  [ TextBlock.text replay.Final.Away.Name
                                    TextBlock.foreground Theme.Danger
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
              ) ]

    let private eventTypeMeta =
        function
        | Goal -> MatchEvent.goal, Theme.Success, "GOAL"
        | OwnGoal -> MatchEvent.ownGoal, Theme.Danger, "OWN GOAL"
        | Assist -> MatchEvent.assist, Theme.AccentAlt, "ASSIST"
        | YellowCard -> MatchEvent.yellowCard, Theme.Warning, "YELLOW"
        | RedCard -> MatchEvent.redCard, Theme.Danger, "RED CARD"
        | Injury _ -> MatchEvent.injury, Theme.Danger, "INJURY"
        | SubstitutionIn -> MatchEvent.subOn, Theme.Success, "SUB ON"
        | SubstitutionOut -> MatchEvent.subOff, Theme.Danger, "SUB OFF"
        | PenaltyAwarded true -> MatchEvent.goal, Theme.Success, "PENALTY GOAL"
        | PenaltyAwarded false -> MatchEvent.penaltyMiss, Theme.Danger, "PENALTY MISS"
        | FreeKick true -> MatchEvent.goal, Theme.Success, "FREE KICK GOAL"
        | FreeKick false -> MatchEvent.freeKickMiss, Theme.Danger, "FREE KICK MISS"
        | Corner -> MatchEvent.corner, Theme.TextMuted, "CORNER"
        | PassCompleted (_, _) -> MatchEvent.pass, Theme.AccentAlt, "PASS"
        | PassIncomplete _ -> MatchEvent.passIncomplete, Theme.TextMuted, "PASS INCOMPLETE"
        | DribbleSuccess -> MatchEvent.dribble, Theme.AccentAlt, "DRIBBLE"
        | DribbleFail -> MatchEvent.dribbleFail, Theme.TextMuted, "DRIBBLE FAIL"
        | TackleSuccess -> MatchEvent.tackle, Theme.AccentAlt, "TACKLE"
        | TackleFail -> MatchEvent.tackleFail, Theme.TextMuted, "TACKLE FAIL"
        | CrossAttempt true -> MatchEvent.cross, Theme.AccentAlt, "CROSS"
        | CrossAttempt false -> MatchEvent.cross, Theme.TextMuted, "CROSS INCOMPLETE"
        | LongBall true -> MatchEvent.longBall, Theme.AccentAlt, "LONG BALL"
        | LongBall false -> MatchEvent.longBall, Theme.TextMuted, "LONG BALL INCOMPLETE"
        | ShotBlocked -> MatchEvent.shotBlocked, Theme.TextMuted, "SHOT BLOCKED"
        | ShotOffTarget -> MatchEvent.shotOffTarget, Theme.TextMuted, "SHOT OFF TARGET"
        | Save -> MatchEvent.save, Theme.AccentAlt, "SAVE"
        | FoulCommitted -> MatchEvent.foul, Theme.Warning, "FOUL"

    let private eventRow (clubs: Map<ClubId, Club>) (players: Map<PlayerId, Player>) (ev: MatchEvent) : IView =
        let iconKind, color, label = eventTypeMeta ev.Type

        let clubName =
            clubs |> Map.tryFind ev.ClubId |> Option.map _.Name |> Option.defaultValue "?"

        let playerName =
            players
            |> Map.tryFind ev.PlayerId
            |> Option.map _.Name
            |> Option.defaultValue ""

        let minute = ev.Second / 60

        Border.create
            [ Border.padding (Thickness(10.0, 7.0))
              Border.cornerRadius 6.0
              Border.margin (Thickness(0.0, 1.0))
              Border.background Theme.BgCard
              Border.borderBrush (color + "33")
              Border.borderThickness (Thickness(2.0, 0.0, 0.0, 0.0))
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "38, *, Auto"
                        Grid.children
                            [ Border.create
                                  [ Grid.column 0
                                    Border.background (color + "20")
                                    Border.cornerRadius 4.0
                                    Border.width 32.0
                                    Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text $"{minute}'"
                                              TextBlock.foreground color
                                              TextBlock.fontSize 10.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.textAlignment TextAlignment.Center
                                              TextBlock.padding (Thickness(0.0, 3.0)) ]
                                    ) ]
                              StackPanel.create
                                  [ Grid.column 1
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.spacing 1.0
                                    StackPanel.children
                                        [ StackPanel.create
                                              [ StackPanel.orientation Orientation.Horizontal
                                                StackPanel.spacing 5.0
                                                StackPanel.children
                                                    [ Icons.iconSm iconKind color
                                                      TextBlock.create
                                                          [ TextBlock.text label
                                                            TextBlock.foreground color
                                                            TextBlock.fontSize 10.0
                                                            TextBlock.fontWeight FontWeight.Black ] ] ]
                                          TextBlock.create
                                              [ TextBlock.text playerName
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.fontSize 11.0
                                                TextBlock.fontWeight FontWeight.SemiBold ] ] ]
                              Border.create
                                  [ Grid.column 2
                                    Border.background Theme.BgHover
                                    Border.cornerRadius 4.0
                                    Border.padding (Thickness(5.0, 2.0))
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text clubName
                                              TextBlock.foreground Theme.TextMuted
                                              TextBlock.fontSize 9.0 ]
                                    ) ] ] ]
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
                | SubstitutionOut
                | PenaltyAwarded _
                | FreeKick _
                | Corner
                | PassCompleted _
                | PassIncomplete _
                | DribbleSuccess
                | DribbleFail
                | TackleSuccess
                | TackleFail
                | CrossAttempt _
                | LongBall _
                | ShotBlocked
                | ShotOffTarget
                | Save
                | FoulCommitted -> true)

        DockPanel.create
            [ DockPanel.width 270.0
              DockPanel.lastChildFill true
              DockPanel.background Theme.BgSidebar
              DockPanel.children
                  [ Border.create
                        [ DockPanel.dock Dock.Top
                          Border.padding (Thickness(12.0, 10.0))
                          Border.borderBrush Theme.Border
                          Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 8.0
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text "MATCH EVENTS"
                                                TextBlock.foreground Theme.TextMuted
                                                TextBlock.fontSize 9.0
                                                TextBlock.fontWeight FontWeight.Black
                                                TextBlock.verticalAlignment VerticalAlignment.Center ]
                                          Border.create
                                              [ Border.background Theme.AccentLight
                                                Border.cornerRadius 10.0
                                                Border.padding (Thickness(7.0, 2.0))
                                                Border.child (
                                                    TextBlock.create
                                                        [ TextBlock.text $"{relevant.Length}"
                                                          TextBlock.foreground Theme.Accent
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.Black ]
                                                ) ] ] ]
                          ) ]
                    ScrollViewer.create
                        [ ScrollViewer.padding (Thickness(10.0, 8.0))
                          ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                          ScrollViewer.content (
                              StackPanel.create
                                  [ StackPanel.orientation Orientation.Vertical
                                    StackPanel.children (
                                        if relevant.IsEmpty then
                                            [ TextBlock.create
                                                  [ TextBlock.text "No events recorded"
                                                    TextBlock.foreground Theme.TextMuted
                                                    TextBlock.fontSize 11.0
                                                    TextBlock.margin (Thickness(0.0, 12.0)) ]
                                              :> IView ]
                                        else
                                            [ yield! relevant |> List.map (eventRow clubs players)
                                              Border.create [ Border.height 24.0 ] ]
                                    ) ]
                          ) ] ] ]

    let private toolbar
        (idx: int)
        (total: int)
        (replayKey: string)
        (replay: MatchReplay)
        (step: int -> unit)
        (onClose: unit -> unit)
        (isPlaying: bool)
        (playbackSpeed: int)
        (onTogglePlayback: unit -> unit)
        (onSetSpeed: int -> unit)
        =
        let navButtons =
            [ Nav.skipFirst, -total, idx > 0
              Nav.rewind, -10, idx > 0
              Nav.prev, -1, idx > 0
              Nav.next, +1, idx < total
              Nav.fastForward, +10, idx < total
              Nav.skipLast, +total, idx < total ]

        let speedButtons = [ 400, "1×"; 200, "2×"; 100, "4×" ]

        Component.create (
            $"matchday-toolbar-{replayKey}",
            fun _ ->
                StackPanel.create
                    [ StackPanel.orientation Orientation.Horizontal
                      StackPanel.spacing 4.0
                      StackPanel.verticalAlignment VerticalAlignment.Center
                      StackPanel.children
                          [ vSep ()
                            finalScorePill replay
                            vSep ()
                            yield!
                                navButtons
                                |> List.mapi (fun i (iconKind, delta, enabled) ->
                                    let btn = navBtn iconKind enabled (fun () -> step delta)

                                    if i = 2 then
                                        [ btn
                                          Border.create
                                              [ Border.background Theme.BgCard
                                                Border.cornerRadius 4.0
                                                Border.padding (Thickness(8.0, 3.0))
                                                Border.verticalAlignment VerticalAlignment.Center
                                                Border.child (
                                                    TextBlock.create
                                                        [ TextBlock.text $"{idx} / {total}"
                                                          TextBlock.foreground Theme.TextSub
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.SemiBold ]
                                                ) ]
                                          :> IView ]
                                    else
                                        [ btn ])
                                |> List.concat
                            vSep ()
                            Button.create
                                [ Button.background (if isPlaying then Theme.Accent else Theme.BgCard)
                                  Button.foreground (if isPlaying then Theme.BgMain else Theme.TextSub)
                                  Button.padding (Thickness(10.0, 6.0))
                                  Button.cornerRadius 7.0
                                  Button.fontWeight FontWeight.Bold
                                  Button.onClick (fun _ -> onTogglePlayback ())
                                  Button.content (
                                      StackPanel.create
                                          [ StackPanel.orientation Orientation.Horizontal
                                            StackPanel.spacing 4.0
                                            StackPanel.children
                                                [ Icons.iconSm
                                                      (if isPlaying then Nav.pause else Nav.play)
                                                      (if isPlaying then Theme.BgMain else Theme.TextSub)
                                                  TextBlock.create
                                                      [ TextBlock.text (if isPlaying then "PAUSE" else "PLAY")
                                                        TextBlock.foreground (
                                                            if isPlaying then Theme.BgMain else Theme.TextSub
                                                        )
                                                        TextBlock.fontSize 11.0
                                                        TextBlock.fontWeight FontWeight.Bold
                                                        TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                  ) ]
                            vSep ()
                            yield!
                                speedButtons
                                |> List.map (fun (speed, label) ->
                                    let isActive = playbackSpeed = speed

                                    Button.create
                                        [ Button.background (if isActive then Theme.Accent else Theme.BgCard)
                                          Button.foreground (if isActive then Theme.BgMain else Theme.TextSub)
                                          Button.padding (Thickness(8.0, 4.0))
                                          Button.cornerRadius 5.0
                                          Button.fontWeight (if isActive then FontWeight.Bold else FontWeight.Normal)
                                          Button.onClick (fun _ -> onSetSpeed speed)
                                          Button.content (
                                              TextBlock.create
                                                  [ TextBlock.text label
                                                    TextBlock.foreground (
                                                        if isActive then Theme.BgMain else Theme.TextSub
                                                    )
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold ]
                                          ) ]
                                    :> IView)
                            vSep ()
                            Button.create
                                [ Button.background Theme.Accent
                                  Button.foreground Theme.BgMain
                                  Button.padding (Thickness(16.0, 6.0))
                                  Button.cornerRadius 7.0
                                  Button.fontWeight FontWeight.Bold
                                  Button.onClick (fun _ -> onClose ())
                                  Button.content (
                                      StackPanel.create
                                          [ StackPanel.orientation Orientation.Horizontal
                                            StackPanel.spacing 6.0
                                            StackPanel.children
                                                [ Icons.iconSm Nav.next Theme.BgMain
                                                  TextBlock.create
                                                      [ TextBlock.text "BACK TO GAME"
                                                        TextBlock.foreground Theme.BgMain
                                                        TextBlock.fontSize 11.0
                                                        TextBlock.fontWeight FontWeight.Bold
                                                        TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                  ) ] ] ]
        )

    let view (state: State) (dispatch: Msg -> unit) : IView =
        match state.ActiveMatchReplay with
        | None ->
            StackPanel.create
                [ StackPanel.horizontalAlignment HorizontalAlignment.Center
                  StackPanel.verticalAlignment VerticalAlignment.Center
                  StackPanel.children
                      [ TextBlock.create
                            [ TextBlock.text "No match to display"
                              TextBlock.foreground Theme.TextMuted
                              TextBlock.fontSize 15.0 ] ] ]
            :> IView
        | Some replay ->
            let idx = state.ActiveMatchSnapshot
            let total = max 1 (replay.Snapshots.Length - 1)

            let current =
                replay.Snapshots |> Array.tryItem idx |> Option.defaultValue replay.Final

            let nextSnap =
                replay.Snapshots |> Array.tryItem (idx + 1) |> Option.defaultValue current

            let interp = interpolateState state.InterpolationT current nextSnap

            let replayKey =
                $"{replay.Snapshots.Length}-{replay.Final.HomeScore}-{replay.Final.AwayScore}"

            DockPanel.create
                [ DockPanel.background Theme.BgMain
                  DockPanel.children
                      [ Border.create
                            [ DockPanel.dock Dock.Top
                              Border.background Theme.BgSidebar
                              Border.borderBrush Theme.Border
                              Border.borderThickness (Thickness(0.0, 0.0, 0.0, 1.0))
                              Border.padding (Thickness(16.0, 10.0))
                              Border.child (
                                  StackPanel.create
                                      [ StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 12.0
                                        StackPanel.verticalAlignment VerticalAlignment.Center
                                        StackPanel.children
                                            [ Icons.iconSm IconName.simulate Theme.TextMuted
                                              TextBlock.create
                                                  [ TextBlock.text "MATCH DAY"
                                                    TextBlock.foreground Theme.TextMuted
                                                    TextBlock.fontSize 9.0
                                                    TextBlock.fontWeight FontWeight.Black
                                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                                              toolbar
                                                  idx
                                                  total
                                                  replayKey
                                                  replay
                                                  (fun delta -> dispatch (StepActiveMatch delta))
                                                  (fun () -> dispatch CloseActiveMatch)
                                                  state.IsPlaying
                                                  state.PlaybackSpeed
                                                  (fun () -> dispatch TogglePlayback)
                                                  (fun speed -> dispatch (SetPlaybackSpeed speed)) ] ]
                              ) ]
                        DockPanel.create
                            [ DockPanel.lastChildFill true
                              DockPanel.children
                                  [ Border.create
                                        [ DockPanel.dock Dock.Right
                                          Border.borderBrush Theme.Border
                                          Border.borderThickness (Thickness(1.0, 0.0, 0.0, 0.0))
                                          Border.child (
                                              match state.Mode with
                                              | InGame(gs, _) -> eventLog replay gs.Clubs gs.Players
                                              | _ -> eventLog replay Map.empty Map.empty
                                          ) ]
                                    view current interp.HomePosLerped interp.AwayPosLerped interp.BallLerped ] ] ] ]
            :> IView
