namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppTypes
open FootballEngine.AppMsgs
open FootballEngine.Domain
open FootballEngine.Components
open FootballEngine.Icons

module Tactics =

    let private GkColor = Theme.Warning
    let private PlayerColor = Theme.AccentAlt

    let private draggedId = ref<PlayerId option> None

    let private startDrag (playerId: int) (_dispatch: Msg -> unit) (args: PointerPressedEventArgs) =
        draggedId.Value <- Some playerId

        task {
            let! _ = DragDrop.DoDragDropAsync(args, new DataTransfer(), DragDropEffects.Move)
            draggedId.Value <- None
        }
        |> ignore

    let private fitnessColor v =
        if v >= 80 then Theme.Success
        elif v >= 55 then Theme.Warning
        else Theme.Danger

    let private positionSortKey (pos: Position) =
        match pos with
        | GK -> 0
        | DR
        | DC
        | DL
        | WBR
        | WBL -> 1
        | DM
        | MC
        | MR
        | ML -> 2
        | AMR
        | AMC
        | AML -> 3
        | ST -> 4

    let private getCurrentLineup (gs: GameState) : Lineup option =
        gs
        |> GameState.headCoach gs.UserClubId
        |> Option.bind (fun coach -> coach.Attributes.Coaching.Lineup)

    let benchRow (p: Player) dispatch : IView =
        let col = Theme.positionColor p.Position
        let fitCol = fitnessColor p.MatchFitness

        Border.create
            [ Border.background "Transparent"
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.padding (14.0, 10.0)
              Border.onPointerPressed (fun e ->
                  let pt = e.GetCurrentPoint(e.Source :?> Control)

                  if pt.Properties.IsLeftButtonPressed then
                      e.Handled <- true
                      startDrag p.Id dispatch e)
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "3, Auto, *, Auto"
                        Grid.children
                            [ Rectangle.create
                                  [ Grid.column 0
                                    Rectangle.width 3.0
                                    Rectangle.fill col
                                    Rectangle.radiusX 2.0
                                    Rectangle.radiusY 2.0
                                    Rectangle.margin (0.0, 0.0, 12.0, 0.0) ]
                              UI.positionBadge p.Position
                              |> fun b ->
                                  Border.create
                                      [ Grid.column 1
                                        Border.margin (0.0, 0.0, 12.0, 0.0)
                                        Border.verticalAlignment VerticalAlignment.Center
                                        Border.child b ]
                              StackPanel.create
                                  [ Grid.column 2
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.spacing 2.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text p.Name
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.fontSize 12.0
                                                TextBlock.fontWeight FontWeight.SemiBold ]
                                          TextBlock.create
                                              [ TextBlock.text $"CA {p.CurrentSkill}"
                                                TextBlock.foreground Theme.TextMuted
                                                TextBlock.fontSize 10.0 ] ] ]
                              StackPanel.create
                                  [ Grid.column 3
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.horizontalAlignment HorizontalAlignment.Right
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 4.0
                                    StackPanel.children
                                        [ Icons.iconSm PlayerIcon.condition fitCol
                                          TextBlock.create
                                              [ TextBlock.text $"{p.MatchFitness}%%"
                                                TextBlock.foreground fitCol
                                                TextBlock.fontSize 11.0
                                                TextBlock.fontWeight FontWeight.Bold
                                                TextBlock.verticalAlignment VerticalAlignment.Center ] ] ] ] ]
              ) ]
        |> View.withKey $"bench-{p.Id}-{p.MatchFitness}"
        :> IView

    let playerNode (player: Player option) (slot: LineupSlot) (_state: State) (dispatch: Msg -> unit) =
        let pIdFijo = player |> Option.map _.Id |> Option.defaultValue -1
        let isEmpty = player.IsNone

        let nodeCol =
            match player with
            | Some p -> Theme.positionColor p.Position
            | None -> "#ffffff20"

        let shortName =
            match player with
            | None -> ""
            | Some p ->
                let parts = p.Name.Split(' ')

                if parts.Length >= 2 then
                    let last = parts |> Array.last
                    last.Substring(0, min 7 last.Length).ToUpper()
                else
                    p.Name.Substring(0, min 7 p.Name.Length).ToUpper()

        let posLabel =
            match player with
            | Some p -> $"%A{p.Position}"
            | None -> $"{slot.Role}"

        Border.create
            [ Border.width 88.0
              Border.background "Transparent"
              Control.allowDrop true
              Control.onDragOver (fun e -> e.DragEffects <- DragDropEffects.Move)
              Control.onDrop (fun _ ->
                  match draggedId.Value with
                  | Some pId ->
                      draggedId.Value <- None
                      dispatch (DropPlayerInSlot(slot.Index, pId))
                  | None -> ())
              if player.IsSome then
                  Border.onPointerPressed (fun e ->
                      let pt = e.GetCurrentPoint(e.Source :?> Control)

                      if pt.Properties.IsLeftButtonPressed then
                          e.Handled <- true
                          startDrag player.Value.Id dispatch e)
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 5.0
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children
                            [ Grid.create
                                  [ Grid.width 56.0
                                    Grid.height 56.0
                                    Grid.horizontalAlignment HorizontalAlignment.Center
                                    Grid.children
                                        [ Ellipse.create
                                              [ Shape.width 56.0
                                                Shape.height 56.0
                                                Shape.fill (if isEmpty then "#ffffff06" else nodeCol + "22")
                                                Shape.stroke (if isEmpty then "#ffffff25" else nodeCol)
                                                Shape.strokeThickness (if isEmpty then 1.0 else 2.5) ]
                                          if isEmpty then
                                              TextBlock.create
                                                  [ TextBlock.text "+"
                                                    TextBlock.fontSize 20.0
                                                    TextBlock.fontWeight FontWeight.Thin
                                                    TextBlock.foreground "#ffffff30"
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                                          else
                                              TextBlock.create
                                                  [ TextBlock.text shortName
                                                    TextBlock.fontSize (if shortName.Length > 5 then 9.0 else 11.0)
                                                    TextBlock.fontWeight FontWeight.Black
                                                    TextBlock.foreground "#ffffff"
                                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                              Border.create
                                  [ Border.background (if isEmpty then "#ffffff08" else nodeCol)
                                    Border.borderBrush (if isEmpty then "#ffffff18" else nodeCol)
                                    Border.borderThickness 1.0
                                    Border.cornerRadius 5.0
                                    Border.padding (8.0, 3.0)
                                    Border.horizontalAlignment HorizontalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text posLabel
                                              TextBlock.fontSize 10.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground (if isEmpty then "#ffffff50" else "#ffffff")
                                              TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                    ) ] ] ]
              ) ]
        |> View.withKey (string pIdFijo)

    let pitchContainer (state: State) dispatch =
        let lineup = getCurrentLineup state.GameState

        let formation =
            lineup |> Option.map _.Formation |> Option.defaultValue state.SelectedTactics

        let slots = lineup |> Option.map _.Slots |> Option.defaultValue []

        FootballPitch.render formation (fun slot ->
            let assignedPlayer =
                slots
                |> List.tryFind (fun s -> s.Index = slot.Index)
                |> Option.bind _.PlayerId
                |> Option.bind state.GameState.Players.TryFind

            playerNode assignedPlayer slot state dispatch)

    let private formationPicker (currentFormation: Formation) dispatch =
        StackPanel.create
            [ StackPanel.spacing 0.0
              StackPanel.children
                  [ UI.sectionHeader IconName.tactics "FORMATION"
                    Border.create
                        [ Border.padding (16.0, 12.0)
                          Border.child (
                              ComboBox.create
                                  [ ComboBox.dataItems FormationLineups.all
                                    ComboBox.selectedItem currentFormation
                                    ComboBox.onSelectedItemChanged (fun obj ->
                                        if obj <> null then
                                            dispatch (SetTactics(obj :?> Formation)))
                                    ComboBox.horizontalAlignment HorizontalAlignment.Stretch
                                    ComboBox.fontSize 13.0
                                    ComboBox.fontWeight FontWeight.Bold ]
                          ) ] ] ]

    let private teamStats (starterIds: Set<PlayerId>) (gs: GameState) =
        let starters = starterIds |> Set.toList |> List.choose gs.Players.TryFind

        let avg f =
            if starters.IsEmpty then
                0
            else
                starters |> List.averageBy (fun p -> float (f p)) |> int

        let avgFitness = avg (fun p -> p.MatchFitness)
        let avgSkill = avg (fun p -> p.CurrentSkill)
        let avgMorale = avg (fun p -> p.Morale)

        StackPanel.create
            [ StackPanel.spacing 0.0
              StackPanel.children
                  [ UI.sectionHeader IconName.info "SQUAD OVERVIEW"
                    Border.create
                        [ Border.padding (16.0, 14.0)
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.spacing 8.0
                                    StackPanel.children
                                        [ Grid.create
                                              [ Grid.columnDefinitions "*, 6, *"
                                                Grid.children
                                                    [ UI.statMiniCard
                                                          PlayerIcon.condition
                                                          "FITNESS"
                                                          $"{avgFitness}%%"
                                                          (fitnessColor avgFitness)
                                                      Grid.create
                                                          [ Grid.column 2
                                                            Grid.children
                                                                [ UI.statMiniCard
                                                                      PlayerIcon.skill
                                                                      "AVG SKILL"
                                                                      (string avgSkill)
                                                                      Theme.Accent ] ] ] ]
                                          UI.statMiniCard
                                              PlayerIcon.morale
                                              "MORALE"
                                              $"{avgMorale}%%"
                                              (Theme.moraleColor avgMorale) ] ]
                          ) ] ] ]

    let tacticView (state: State) dispatch : IView =
        let lineup = getCurrentLineup state.GameState

        let currentFormation =
            lineup |> Option.map _.Formation |> Option.defaultValue state.SelectedTactics

        let starterIds =
            lineup
            |> Option.map (fun l -> l.Slots |> List.choose _.PlayerId)
            |> Option.defaultValue []
            |> Set.ofList

        let benchPlayers =
            GameState.getSquad state.GameState.UserClubId state.GameState
            |> List.filter (fun p -> not (starterIds.Contains p.Id))
            |> List.sortBy (fun p -> positionSortKey p.Position, p.CurrentSkill * -1)

        let benchItems: IView list =
            [ yield! benchPlayers |> Seq.map (fun p -> benchRow p dispatch)
              yield Border.create [ Border.height 20.0 ] ]

        Grid.create
            [ Grid.columnDefinitions "272, *, 296"
              Grid.children
                  [ Border.create
                        [ Grid.column 0
                          Border.background Theme.BgSidebar
                          Border.borderBrush Theme.Border
                          Border.borderThickness (0.0, 0.0, 1.0, 0.0)
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.spacing 0.0
                                    StackPanel.children
                                        [ formationPicker currentFormation dispatch
                                          Border.create
                                              [ Border.height 1.0
                                                Border.background Theme.Border
                                                Border.margin (0.0, 4.0) ]
                                          teamStats starterIds state.GameState ] ]
                          ) ]
                    Border.create
                        [ Grid.column 1
                          Border.padding (20.0, 16.0)
                          Border.child (pitchContainer state dispatch) ]
                    Border.create
                        [ Grid.column 2
                          Border.background Theme.BgSidebar
                          Border.borderBrush Theme.Border
                          Border.borderThickness (1.0, 0.0, 0.0, 0.0)
                          Border.child (
                              DockPanel.create
                                  [ DockPanel.lastChildFill true
                                    DockPanel.children
                                        [ UI.sectionHeaderWithBadge IconName.squad "BENCH" benchPlayers.Length
                                          |> fun h -> Border.create [ DockPanel.dock Dock.Top; Border.child h ]
                                          ScrollViewer.create
                                              [ ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                                ScrollViewer.verticalAlignment VerticalAlignment.Stretch
                                                ScrollViewer.content (
                                                    StackPanel.create [ StackPanel.children benchItems ]
                                                ) ] ] ]
                          ) ] ] ]
        |> View.withKey $"tactics-{currentFormation}-{starterIds.Count}"
        :> IView
