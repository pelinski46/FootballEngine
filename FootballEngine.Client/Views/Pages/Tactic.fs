namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppState
open FootballEngine.Domain
open FootballEngine.Components






module Tactics =
    let private startDrag (playerId: int) (args: PointerPressedEventArgs) =
        let data = DataObject()
        data.Set("PLAYER_ID", string playerId)

        async {
            let! _ = DragDrop.DoDragDrop(args, data, DragDropEffects.Move) |> Async.AwaitTask
            ()
        }
        |> Async.StartImmediate

    let benchRow (p: Player) dispatch =
        let pId = p.Id
        let pName = p.Name

        Border.create
            [

              Border.background "#1e293b"
              Border.margin (0.0, 1.0)
              Border.padding (10.0, 8.0)


              Border.onPointerPressed (fun e ->
                  let point = e.GetCurrentPoint(e.Source :?> Control)

                  if point.Properties.IsLeftButtonPressed then
                      e.Handled <- true
                      startDrag pId e)

              Border.child (
                  DockPanel.create
                      [ DockPanel.children
                            [ TextBlock.create
                                  [ TextBlock.text $"[{pId}]"
                                    TextBlock.width 40.0
                                    TextBlock.foreground "#fbbf24"
                                    TextBlock.fontWeight FontWeight.Bold ]
                              TextBlock.create
                                  [ TextBlock.text $"%A{p.Position}"
                                    TextBlock.width 35.0
                                    TextBlock.foreground Theme.Accent
                                    TextBlock.fontWeight FontWeight.Bold ]
                              TextBlock.create [ TextBlock.text pName; TextBlock.foreground "White" ] ] ]
              ) ]
        |> View.withKey (string pId)



    // Nodo visual de un jugador en la cancha
    let playerNode (player: Player option) (slot: FormationSlot) (dispatch: Msg -> unit) =
        let pIdFijo = player |> Option.map (fun p -> p.Id) |> Option.defaultValue -1

        let name, color, displayId =
            match player with
            | Some p -> p.Name.ToUpper(), (if p.MatchFitness > 80 then "#22c55e" else "#ef4444"), $"ID: {p.Id}"
            | None -> "VACÍO", "#334155", "-"

        Border.create
            [ Border.width 150
              Border.background "Transparent"
              Control.allowDrop true
              Control.onDragOver (fun e -> e.DragEffects <- DragDropEffects.Move)
              Control.onDrop (fun e ->
                  if e.Data.Contains("PLAYER_ID") then
                      let dataRaw = e.Data.Get("PLAYER_ID") :?> string
                      let pId = System.Int32.Parse(dataRaw)
                      dispatch (DropPlayerInSlot(slot.Index, pId)))
              if player.IsSome then
                  Border.onPointerPressed (fun e ->
                      let point = e.GetCurrentPoint(e.Source :?> Control)

                      if point.Properties.IsLeftButtonPressed then
                          e.Handled <- true
                          startDrag player.Value.Id e)
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 4.0
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children
                            [ Grid.create
                                  [ Grid.children
                                        [ Ellipse.create
                                              [ Shape.width 46.0
                                                Shape.height 46.0
                                                Shape.fill (SolidColorBrush.Parse "#00000033") ]
                                          Ellipse.create
                                              [ Shape.width 44.0
                                                Shape.height 44.0
                                                Shape.fill (if player.IsSome then "#f8fafc" else "#1e293b")
                                                Shape.stroke "#0f172a"
                                                Shape.strokeThickness 2.0 ]
                                          if player.IsSome then
                                              Border.create
                                                  [ Border.width 12.0
                                                    Border.height 12.0
                                                    Border.cornerRadius 6.0
                                                    Border.background color
                                                    Border.horizontalAlignment HorizontalAlignment.Right
                                                    Border.verticalAlignment VerticalAlignment.Top ] ] ]
                              UI.roleBadge (string slot.Role) (if player.IsSome then "OK" else "-")
                              Border.create
                                  [ Border.background "#0f172aee"
                                    Border.cornerRadius 4.0
                                    Border.padding (8.0, 2.0)
                                    Border.minWidth 80.0
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text name
                                              TextBlock.fontSize 10.0
                                              TextBlock.fontWeight FontWeight.Bold
                                              TextBlock.textAlignment TextAlignment.Center
                                              TextBlock.foreground (if player.IsSome then "White" else "#64748b") ]
                                    ) ] ] ]
              ) ]
        |> View.withKey (string pIdFijo)

    let pitchContainer (state: AppState.State) dispatch =
        let myTeam = state.GameState.Clubs[state.GameState.UserClubId]

        let lineup =
            myTeam.CurrentLineup |> Option.map (fun l -> l.Slots) |> Option.defaultValue []

        let formationName =
            if state.SelectedTactics <> "" then
                state.SelectedTactics
            else
                myTeam.CurrentLineup
                |> Option.map (fun l -> l.FormationName)
                |> Option.defaultValue "4-4-2"


        FootballPitch.render formationName (fun slot ->
            let assignedPlayer =
                lineup
                |> List.tryFind (fun s -> s.Index = slot.Index)
                |> Option.bind _.PlayerId
                |> Option.bind state.GameState.Players.TryFind

            playerNode assignedPlayer slot dispatch)


    let tacticView (state: AppState.State) dispatch =
        let myTeam = state.GameState.Clubs[state.GameState.UserClubId]

        let currentFormation =
            if state.SelectedTactics <> "" then
                state.SelectedTactics
            else
                myTeam.CurrentLineup
                |> Option.map (fun l -> l.FormationName)
                |> Option.defaultValue "4-4-2"

        let starterIds =
            myTeam.CurrentLineup
            |> Option.map (fun l -> l.Slots |> List.choose (fun s -> s.PlayerId))
            |> Option.defaultValue []
            |> Set.ofList

        let benchPlayers =
            myTeam.Players
            |> List.filter (fun p -> not (starterIds.Contains p.Id))
            |> List.sortBy (fun p -> p.Id)

        Grid.create
            [ Grid.columnDefinitions "260, *, 320"
              Grid.children
                  [ Border.create
                        [ Grid.column 0
                          Border.background "#0f172a"
                          Border.padding 15.0
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.spacing 20.0
                                    StackPanel.children
                                        [ StackPanel.create
                                              [ StackPanel.spacing 5.0
                                                StackPanel.children
                                                    [ TextBlock.create
                                                          [ TextBlock.text "FORMATION"
                                                            TextBlock.foreground Theme.TextMuted
                                                            TextBlock.fontSize 11.0 ]
                                                      ComboBox.create
                                                          [ ComboBox.dataItems FormationData.availableFormations
                                                            ComboBox.selectedItem currentFormation
                                                            ComboBox.onSelectedItemChanged (fun obj ->
                                                                if obj <> null then
                                                                    dispatch (SetTactics(obj.ToString())))
                                                            ComboBox.horizontalAlignment HorizontalAlignment.Stretch ] ] ] ] ]
                          ) ]
                    Border.create
                        [ Grid.column 1
                          Border.padding 20.0
                          Border.child (pitchContainer state dispatch) ]
                    Border.create
                        [ Grid.column 2
                          Border.background Theme.BgMain
                          Border.child (
                              ScrollViewer.create
                                  [ ScrollViewer.maxHeight 800
                                    ScrollViewer.verticalAlignment VerticalAlignment.Top

                                    ScrollViewer.content (
                                        StackPanel.create
                                            [ StackPanel.margin (0.0, 0.0, 0.0, 25)
                                              StackPanel.children
                                                  [ Border.create
                                                        [ Border.background "#1e293b"
                                                          Border.padding (10.0, 8.0)
                                                          Border.child (
                                                              TextBlock.create
                                                                  [ TextBlock.text "REPLACEMENTS"
                                                                    TextBlock.fontWeight FontWeight.Bold ]
                                                          ) ]
                                                    for p in benchPlayers do
                                                        benchRow p dispatch ] ]
                                    ) ]
                          ) ] ] ]
