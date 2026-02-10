namespace FootballEngine.Components

open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine.Domain

module MatchViewer =

    type MatchState =
        { Home: Club
          Away: Club
          HomeLineup: Player array
          AwayLineup: Player array
          BallPosition: float * float
          Minute: int
          HomeScore: int
          AwayScore: int
          Events: MatchEvent list }

    let private getPlayerPositions (formation: string) =
        match formation with
        | "4-3-3" ->
            [| (10.0, 50.0)
               (20.0, 20.0)
               (20.0, 40.0)
               (20.0, 60.0)
               (20.0, 80.0)
               (45.0, 30.0)
               (45.0, 50.0)
               (45.0, 70.0)
               (75.0, 25.0)
               (75.0, 50.0)
               (75.0, 75.0) |]
        | "4-4-2" ->
            [| (10.0, 50.0)
               (20.0, 20.0)
               (20.0, 40.0)
               (20.0, 60.0)
               (20.0, 80.0)
               (45.0, 20.0)
               (45.0, 40.0)
               (45.0, 60.0)
               (45.0, 80.0)
               (75.0, 35.0)
               (75.0, 65.0) |]
        | _ ->
            [| (10.0, 50.0)
               (25.0, 25.0)
               (25.0, 50.0)
               (25.0, 75.0)
               (50.0, 25.0)
               (50.0, 50.0)
               (50.0, 75.0)
               (75.0, 25.0)
               (75.0, 50.0)
               (75.0, 75.0)
               (75.0, 50.0) |]

    let private drawPlayer x y number (color: string) : IView =
        Canvas.create
            [ Canvas.left x
              Canvas.top y
              Canvas.children
                  [ Ellipse.create
                        [ Ellipse.width 24.0
                          Ellipse.height 24.0
                          Ellipse.fill color
                          Ellipse.stroke "white"
                          Ellipse.strokeThickness 2.0 ]
                    TextBlock.create
                        [ TextBlock.text (string number)
                          TextBlock.foreground "white"
                          TextBlock.fontSize 12.0
                          TextBlock.width 24.0
                          TextBlock.height 24.0
                          TextBlock.textAlignment TextAlignment.Center
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.fontWeight FontWeight.Bold ] ] ]

    let view (matchState: MatchState) =
        let pitchWidth = 800.0
        let pitchHeight = 600.0

        Canvas.create
            [ Canvas.width pitchWidth
              Canvas.height pitchHeight
              Canvas.background "#2d5016"
              Canvas.children
                  [ Rectangle.create
                        [ Rectangle.width pitchWidth
                          Rectangle.height pitchHeight
                          Rectangle.stroke "white"
                          Rectangle.strokeThickness 2.0
                          Rectangle.fill "transparent" ]

                    Line.create
                        [ Line.startPoint (pitchWidth / 2.0, 0.0)
                          Line.endPoint (pitchWidth / 2.0, pitchHeight)
                          Line.stroke "white"
                          Line.strokeThickness 2.0 ]

                    Ellipse.create
                        [ Canvas.left (pitchWidth / 2.0 - 60.0)
                          Canvas.top (pitchHeight / 2.0 - 60.0)
                          Ellipse.width 120.0
                          Ellipse.height 120.0
                          Ellipse.stroke "white"
                          Ellipse.strokeThickness 2.0
                          Ellipse.fill "transparent" ]

                    Rectangle.create
                        [ Canvas.left 0.0
                          Canvas.top (pitchHeight / 2.0 - 120.0)
                          Rectangle.width (pitchWidth / 5.0)
                          Rectangle.height 240.0
                          Rectangle.stroke "white"
                          Rectangle.strokeThickness 2.0
                          Rectangle.fill "transparent" ]

                    Rectangle.create
                        [ Canvas.left (pitchWidth - pitchWidth / 5.0)
                          Canvas.top (pitchHeight / 2.0 - 120.0)
                          Rectangle.width (pitchWidth / 5.0)
                          Rectangle.height 240.0
                          Rectangle.stroke "white"
                          Rectangle.strokeThickness 2.0
                          Rectangle.fill "transparent" ]

                    // Arco local
                    Rectangle.create
                        [ Canvas.left 0.0
                          Canvas.top (pitchHeight / 2.0 - 85.0)
                          Rectangle.width (pitchWidth / 10.0)
                          Rectangle.height 170.0
                          Rectangle.stroke "white"
                          Rectangle.strokeThickness 2.0
                          Rectangle.fill "transparent" ]

                    Rectangle.create
                        [ Canvas.left (pitchWidth - pitchWidth / 10.0)
                          Canvas.top (pitchHeight / 2.0 - 85.0)
                          Rectangle.width (pitchWidth / 10.0)
                          Rectangle.height 170.0
                          Rectangle.stroke "white"
                          Rectangle.strokeThickness 2.0
                          Rectangle.fill "transparent" ]

                    yield!
                        getPlayerPositions "4-3-3"
                        |> Array.mapi (fun i (x, y) ->
                            drawPlayer
                                (x * pitchWidth / 100.0 - 12.0)
                                (y * pitchHeight / 100.0 - 12.0)
                                (i + 1)
                                "#1E40AF")


                    yield!
                        getPlayerPositions "4-3-3"
                        |> Array.mapi (fun i (x, y) ->
                            let mirrorX = 100.0 - x
                            let mirrorY = 100.0 - y

                            drawPlayer
                                (mirrorX * pitchWidth / 100.0 - 12.0)
                                (mirrorY * pitchHeight / 100.0 - 12.0)
                                (i + 1)
                                "#DC2626")


                    let ballX, ballY = matchState.BallPosition

                    Ellipse.create
                        [ Canvas.left (ballX * pitchWidth / 100.0 - 6.0)
                          Canvas.top (ballY * pitchHeight / 100.0 - 4.0)
                          Ellipse.width 12.0
                          Ellipse.height 12.0
                          Ellipse.fill "#40000000" ]

                    Ellipse.create
                        [ Canvas.left (ballX * pitchWidth / 100.0 - 5.0)
                          Canvas.top (ballY * pitchHeight / 100.0 - 5.0)
                          Ellipse.width 10.0
                          Ellipse.height 10.0
                          Ellipse.fill "white" ]

                    Border.create
                        [ Canvas.left (pitchWidth / 2.0 - 150.0)
                          Canvas.top 10.0
                          Border.background "#000000CC"
                          Border.cornerRadius 8.0
                          Border.padding (20.0, 10.0)
                          Border.child (
                              TextBlock.create
                                  [ TextBlock.text
                                        $"{matchState.Home.Name} {matchState.HomeScore} - {matchState.AwayScore} {matchState.Away.Name}"
                                    TextBlock.foreground "white"
                                    TextBlock.fontSize 20.0
                                    TextBlock.fontWeight FontWeight.Bold ]
                          ) ]

                    Border.create
                        [ Canvas.left (pitchWidth - 80.0)
                          Canvas.top 10.0
                          Border.background "#000000CC"
                          Border.cornerRadius 8.0
                          Border.padding (15.0, 8.0)
                          Border.child (
                              TextBlock.create
                                  [ TextBlock.text $"{matchState.Minute}'"
                                    TextBlock.foreground "white"
                                    TextBlock.fontSize 18.0
                                    TextBlock.fontWeight FontWeight.Bold ]
                          ) ] ] ]
