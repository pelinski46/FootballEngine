namespace FootballEngine.Components

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Media
open FootballEngine.Domain
open FootballEngine.FormationData

module FootballPitch =


    type PitchConfig =
        { Width: float
          Height: float
          Margin: float
          LineThickness: float
          PlayerNodeSize: float }

    let defaultCfg =
        { Width = 800.0
          Height = 800.0
          Margin = 30.0
          LineThickness = 2.5
          PlayerNodeSize = 85.0 }


    module Colors =
        let grassDark = SolidColorBrush.Parse "#1a5f35"
        let grassLight = SolidColorBrush.Parse "#22753f"
        let lineWhite = SolidColorBrush.Parse "#ffffffdd"
        let lineGlow = SolidColorBrush.Parse "#ffffff33"


    let private stripedGrass (cfg: PitchConfig) =
        Grid.create
            [ Grid.rowDefinitions "*,*,*,*,*,*,*,*,*,*,*,*"
              Grid.children
                  [ for i in 0..11 do
                        Border.create
                            [ Grid.row i
                              Border.background (if i % 2 = 0 then Colors.grassDark else Colors.grassLight) ] ] ]


    let private penaltyArea (cfg: PitchConfig) (isTop: bool) =
        let centerX = cfg.Width / 2.0

        let topPos =
            if isTop then
                cfg.Margin
            else
                cfg.Height - cfg.Margin - 120.0

        let smallAreaH = 50.0
        let largeAreaH = 120.0

        [ Rectangle.create
              [ Canvas.left (centerX - 160.0)
                Canvas.top (
                    if isTop then
                        cfg.Margin
                    else
                        cfg.Height - cfg.Margin - largeAreaH
                )
                Shape.width 320.0
                Shape.height largeAreaH
                Shape.stroke Colors.lineWhite
                Shape.strokeThickness cfg.LineThickness ]
          :> IView

          Rectangle.create
              [ Canvas.left (centerX - 80.0)
                Canvas.top (
                    if isTop then
                        cfg.Margin
                    else
                        cfg.Height - cfg.Margin - smallAreaH
                )
                Shape.width 160.0
                Shape.height smallAreaH
                Shape.stroke Colors.lineWhite
                Shape.strokeThickness cfg.LineThickness ]
          :> IView

          Ellipse.create
              [ let pDotY =
                    if isTop then
                        cfg.Margin + 90.0
                    else
                        cfg.Height - cfg.Margin - 90.0

                Canvas.left (centerX - 4.0)
                Canvas.top (pDotY - 4.0)
                Shape.width 8.0
                Shape.height 8.0
                Shape.fill Colors.lineWhite ]
          :> IView ]

    let private markings (cfg: PitchConfig) =
        let w, h, m = cfg.Width, cfg.Height, cfg.Margin
        let centerX, centerY = w / 2.0, h / 2.0

        Canvas.create
            [ Canvas.width w
              Canvas.height h
              Canvas.isHitTestVisible false
              Canvas.children
                  [

                    Rectangle.create
                        [ Canvas.left m
                          Canvas.top m
                          Shape.width (w - m * 2.0)
                          Shape.height (h - m * 2.0)
                          Shape.stroke Colors.lineWhite
                          Shape.strokeThickness cfg.LineThickness ]
                    Rectangle.create
                        [ Canvas.left m
                          Canvas.top centerY
                          Shape.width (w - m * 2.0)
                          Shape.height cfg.LineThickness
                          Shape.fill Colors.lineWhite ]

                    Ellipse.create
                        [ Canvas.left (centerX - 75.0)
                          Canvas.top (centerY - 75.0)
                          Shape.width 150.0
                          Shape.height 150.0
                          Shape.stroke Colors.lineWhite
                          Shape.strokeThickness cfg.LineThickness ]

                    yield! penaltyArea cfg true
                    yield! penaltyArea cfg false ] ]


    let render (formationName: string) (renderPlayer: FormationSlot -> IView) =
        let cfg = defaultCfg
        let slots = getFormation formationName

        Viewbox.create
            [ Viewbox.stretch Stretch.Uniform
              Viewbox.width cfg.Width
              Viewbox.height cfg.Height
              Viewbox.child (
                  Grid.create
                      [ Grid.children
                            [ stripedGrass cfg
                              markings cfg

                              Canvas.create
                                  [ Canvas.width cfg.Width
                                    Canvas.height cfg.Height
                                    Canvas.children
                                        [ for slot in slots do
                                              let xPos = (slot.X * cfg.Width) - (cfg.PlayerNodeSize / 2.0)
                                              let yPos = (slot.Y * cfg.Height) - (cfg.PlayerNodeSize / 2.0)

                                              ContentControl.create
                                                  [ Canvas.left xPos
                                                    Canvas.top yPos
                                                    ContentControl.content (renderPlayer slot) ] ] ] ] ]
              ) ]
