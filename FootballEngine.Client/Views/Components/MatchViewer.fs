namespace FootballEngine.Components

open Avalonia.Controls.Shapes
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine.AppState
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FootballEngine.MatchContext

module MatchViewer =

    let private positionLabel (p: Position) = sprintf "%A" p

    let private toCanvas pitchW pitchH (engineX: float, engineY: float) =
        engineX * pitchW / 100.0, engineY * pitchH / 100.0

    let private playerOpacity condition = 0.5 + (float condition / 200.0)

    let private drawPlayer x y label (color: string) opacity : IView =
        Canvas.create
            [ Canvas.left (x - 18.0)
              Canvas.top (y - 18.0)
              Canvas.children
                  [ Ellipse.create
                        [ Ellipse.width 30.0
                          Ellipse.height 30.0
                          Ellipse.fill color
                          Ellipse.stroke "white"
                          Ellipse.strokeThickness 1.5
                          Ellipse.opacity opacity ]
                    TextBlock.create
                        [ TextBlock.text label
                          TextBlock.foreground "white"
                          TextBlock.fontSize 9.0
                          TextBlock.width 30.0
                          TextBlock.height 30.0
                          TextBlock.textAlignment TextAlignment.Center
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.fontWeight FontWeight.Bold ] ] ]

    let private drawTeam
        (players: Player[])
        (conditions: int[])
        (positions: Map<PlayerId, float * float>)
        (sidelined: Map<PlayerId, PlayerOut>)
        (color: string)
        (pitchW: float)
        (pitchH: float)
        : IView list =

        players
        |> Array.mapi (fun i p ->
            if Map.containsKey p.Id sidelined then
                None
            else
                let enginePos = positionOf positions p
                let cx, cy = toCanvas pitchW pitchH enginePos

                let opacity =
                    conditions |> Array.tryItem i |> Option.defaultValue 100 |> playerOpacity

                Some(drawPlayer cx cy (positionLabel p.Position) color opacity))
        |> Array.choose id
        |> Array.toList

    let private drawBall (bx: float) (by: float) : IView list =
        [ Ellipse.create
              [ Canvas.left (bx - 7.0)
                Canvas.top (by - 3.0)
                Ellipse.width 14.0
                Ellipse.height 8.0
                Ellipse.fill "#40000000" ]
          Ellipse.create
              [ Canvas.left (bx - 6.0)
                Canvas.top (by - 6.0)
                Ellipse.width 12.0
                Ellipse.height 12.0
                Ellipse.fill "white"
                Ellipse.stroke "#888888"
                Ellipse.strokeThickness 1.0 ] ]

    let private pitchMarkings (w: float) (h: float) : IView list =
        [ Rectangle.create
              [ Rectangle.width w
                Rectangle.height h
                Rectangle.stroke "white"
                Rectangle.strokeThickness 2.0
                Rectangle.fill "transparent" ]
          Line.create
              [ Line.startPoint (w / 2.0, 0.0)
                Line.endPoint (w / 2.0, h)
                Line.stroke "white"
                Line.strokeThickness 1.5 ]
          Ellipse.create
              [ Canvas.left (w / 2.0 - 60.0)
                Canvas.top (h / 2.0 - 60.0)
                Ellipse.width 120.0
                Ellipse.height 120.0
                Ellipse.stroke "white"
                Ellipse.strokeThickness 1.5
                Ellipse.fill "transparent" ]
          Rectangle.create
              [ Canvas.left 0.0
                Canvas.top (h / 2.0 - 110.0)
                Rectangle.width (w / 5.0)
                Rectangle.height 220.0
                Rectangle.stroke "white"
                Rectangle.strokeThickness 1.5
                Rectangle.fill "transparent" ]
          Rectangle.create
              [ Canvas.left (w - w / 5.0)
                Canvas.top (h / 2.0 - 110.0)
                Rectangle.width (w / 5.0)
                Rectangle.height 220.0
                Rectangle.stroke "white"
                Rectangle.strokeThickness 1.5
                Rectangle.fill "transparent" ]
          Rectangle.create
              [ Canvas.left 0.0
                Canvas.top (h / 2.0 - 55.0)
                Rectangle.width (w / 12.0)
                Rectangle.height 110.0
                Rectangle.stroke "white"
                Rectangle.strokeThickness 1.5
                Rectangle.fill "transparent" ]
          Rectangle.create
              [ Canvas.left (w - w / 12.0)
                Canvas.top (h / 2.0 - 55.0)
                Rectangle.width (w / 12.0)
                Rectangle.height 110.0
                Rectangle.stroke "white"
                Rectangle.strokeThickness 1.5
                Rectangle.fill "transparent" ] ]

    let private overlayBox left top (content: IView) =
        Border.create
            [ Canvas.left left
              Canvas.top top
              Border.background "#000000CC"
              Border.cornerRadius 8.0
              Border.padding (12.0, 8.0)
              Border.child content ]

    let private scoreOverlay (s: MatchState) pitchW =
        overlayBox (pitchW / 2.0 - 160.0) 8.0
        <| TextBlock.create
            [ TextBlock.text $"{s.Home.Name}  {s.HomeScore} - {s.AwayScore}  {s.Away.Name}"
              TextBlock.foreground "white"
              TextBlock.fontSize 18.0
              TextBlock.fontWeight FontWeight.Bold ]

    let private minuteOverlay (second: int) pitchW =
        overlayBox (pitchW - 70.0) 8.0
        <| TextBlock.create
            [ TextBlock.text $"{second / 60}'"
              TextBlock.foreground "white"
              TextBlock.fontSize 16.0
              TextBlock.fontWeight FontWeight.Bold ]

    let private possessionOverlay (s: MatchState) =
        let name, color =
            match s.Possession with
            | Home -> s.Home.Name, "#60A5FA"
            | Away -> s.Away.Name, "#F87171"

        overlayBox 8.0 8.0
        <| TextBlock.create
            [ TextBlock.text $"● {name}"
              TextBlock.foreground color
              TextBlock.fontSize 13.0
              TextBlock.fontWeight FontWeight.Bold ]

    let view (s: MatchState) =
        let pitchW, pitchH = 800.0, 520.0
        let project = toCanvas pitchW pitchH

        let homePlayers =
            drawTeam s.HomePlayers s.HomeConditions s.HomePositions s.HomeSidelined "#1E40AF" pitchW pitchH

        let awayPlayers =
            drawTeam s.AwayPlayers s.AwayConditions s.AwayPositions s.AwaySidelined "#DC2626" pitchW pitchH

        let bcx, bcy = project s.BallPosition

        Canvas.create
            [ Canvas.width pitchW
              Canvas.height pitchH
              Canvas.background "#2d5016"
              Canvas.children
                  [ yield! pitchMarkings pitchW pitchH
                    yield! homePlayers
                    yield! awayPlayers
                    yield! drawBall bcx bcy
                    scoreOverlay s pitchW
                    minuteOverlay s.Second pitchW
                    possessionOverlay s ] ]

module MatchLabView =

    let view (state: State) (dispatch: Msg -> unit) =
        let clubs =
            state.GameState.Clubs |> Map.toList |> List.map snd |> List.sortBy _.Name

        let clubPicker label selected msg =
            StackPanel.create
                [ StackPanel.orientation Orientation.Vertical
                  StackPanel.spacing 4.0
                  StackPanel.children
                      [ TextBlock.create
                            [ TextBlock.text label
                              TextBlock.foreground "white"
                              TextBlock.fontWeight FontWeight.Bold ]
                        ComboBox.create
                            [ ComboBox.width 200.0
                              ComboBox.dataItems clubs
                              ComboBox.itemTemplate (
                                  DataTemplateView<Club>.create (fun c -> TextBlock.create [ TextBlock.text c.Name ])
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

        let replayControls (replay: MatchReplay) =
            let idx = state.MatchLabSnapshot

            let current =
                replay.Snapshots |> Array.tryItem idx |> Option.defaultValue replay.Final

            StackPanel.create
                [ StackPanel.orientation Orientation.Vertical
                  StackPanel.children
                      [ StackPanel.create
                            [ StackPanel.orientation Orientation.Horizontal
                              StackPanel.spacing 12.0
                              StackPanel.margin (12.0, 6.0)
                              StackPanel.children
                                  [ TextBlock.create
                                        [ TextBlock.text $"{current.Second / 60}'"
                                          TextBlock.foreground "white"
                                          TextBlock.width 35.0
                                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                                    Slider.create
                                        [ Slider.minimum 0.0
                                          Slider.maximum (float (max 0 (replay.Snapshots.Length - 1)))
                                          Slider.value (float idx)
                                          Slider.width 650.0
                                          Slider.onValueChanged (fun v -> dispatch (SetMatchLabSnapshot(int v))) ]
                                    TextBlock.create
                                        [ TextBlock.text $"{current.HomeScore} - {current.AwayScore}"
                                          TextBlock.foreground "white"
                                          TextBlock.width 50.0
                                          TextBlock.verticalAlignment VerticalAlignment.Center
                                          TextBlock.fontWeight FontWeight.Bold ] ] ]
                        ScrollViewer.create [ ScrollViewer.content (MatchViewer.view current) ] ] ]
            :> IView

        DockPanel.create
            [ DockPanel.children
                  [ StackPanel.create
                        [ DockPanel.dock Dock.Top
                          StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 20.0
                          StackPanel.margin 16.0
                          StackPanel.children
                              [ clubPicker "Home" state.MatchLabHome SelectMatchLabHome
                                clubPicker "Away" state.MatchLabAway SelectMatchLabAway
                                Button.create
                                    [ Button.content "▶ Simulate"
                                      Button.verticalAlignment VerticalAlignment.Bottom
                                      Button.onClick (fun _ -> dispatch RunMatchLab) ]
                                match state.MatchLabResult with
                                | Some replay ->
                                    let f = replay.Final

                                    TextBlock.create
                                        [ TextBlock.text
                                              $"Final: {f.Home.Name} {f.HomeScore} - {f.AwayScore} {f.Away.Name}"
                                          TextBlock.foreground "white"
                                          TextBlock.fontSize 16.0
                                          TextBlock.verticalAlignment VerticalAlignment.Bottom ]
                                | None -> () ] ]

                    match state.MatchLabResult with
                    | None ->
                        TextBlock.create
                            [ TextBlock.text "Select two clubs and click Simulate"
                              TextBlock.foreground "#888888"
                              TextBlock.horizontalAlignment HorizontalAlignment.Center
                              TextBlock.verticalAlignment VerticalAlignment.Center
                              TextBlock.fontSize 18.0 ]
                        :> IView
                    | Some replay -> replayControls replay ] ]
