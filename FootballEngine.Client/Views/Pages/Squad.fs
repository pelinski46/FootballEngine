namespace FootballEngine.Pages

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Domain
open FootballEngine.Components

open FootballEngine.Icons


module SquadPresenter =

    type TeamStatsVM =
        { TotalValue: string
          AvgSkill: string
          TotalWages: string
          SquadSize: string }

    let getTeamStats (players: Player list) =
        let totalValue = players |> List.sumBy (fun p -> Player.playerValue p.CurrentSkill)

        let totalWages =
            players
            |> List.sumBy (fun p ->
                Player.contractOf p |> Option.map _.Salary |> Option.defaultValue 0m)

        let avgSkill =
            if players.IsEmpty then
                0.0
            else
                players |> List.averageBy (fun p -> float p.CurrentSkill)

        { TotalValue = $"€{int (totalValue / 1_000_000m)}M"
          AvgSkill = $"%.0f{avgSkill}"
          TotalWages = $"€{int (totalWages / 1_000m)}K/pw"
          SquadSize = string players.Length }

    let private positionLine (pos: Position) =
        match pos with
        | GK -> 0, "GOALKEEPERS"
        | DR
        | DC
        | DL
        | WBR
        | WBL -> 1, "DEFENDERS"
        | DM
        | MC
        | MR
        | ML
        | AMR
        | AMC
        | AML -> 2, "MIDFIELDERS"
        | ST -> 3, "ATTACKERS"

    let getSortedPlayers (players: Player list) (currentDate: DateTime) (sortKey: string) =
        match sortKey with
        | "name" -> players |> List.sortBy _.Name
        | "skill" -> players |> List.sortByDescending _.CurrentSkill
        | "age" -> players |> List.sortBy (fun p -> Player.age currentDate p)
        | "value" -> players |> List.sortByDescending (fun p -> Player.playerValue p.CurrentSkill)
        | _ -> players |> List.sortBy (fun p -> fst (positionLine p.Position), -p.CurrentSkill)

    let getGroupedPlayers (players: Player list) (currentDate: DateTime) (sortKey: string) =
        match sortKey with
        | "name"
        | "skill"
        | "age"
        | "value" -> [ None, getSortedPlayers players currentDate sortKey ]
        | _ ->
            players
            |> List.groupBy (fun p -> positionLine p.Position)
            |> List.sortBy fst
            |> List.map (fun ((_, label), ps) -> Some label, ps |> List.sortByDescending _.CurrentSkill)


module Squad =

    let private positionGroupHeader (label: string) =
        Grid.create
            [ Grid.columnDefinitions "auto, *"
              Grid.margin (0.0, 8.0, 0.0, 4.0)
              Grid.children
                  [ Border.create
                        [ Grid.column 0
                          Border.background Theme.Accent
                          Border.cornerRadius 3.0
                          Border.width 3.0
                          Border.margin (0.0, 0.0, 8.0, 0.0) ]
                    TextBlock.create
                        [ Grid.column 1
                          TextBlock.text label
                          TextBlock.fontSize 10.0
                          TextBlock.fontWeight FontWeight.Black
                          TextBlock.foreground Theme.Accent
                          TextBlock.lineSpacing 1.5
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private sectionHeader (squadCount: int) (dispatch: Msg -> unit) (state: State) =
        DockPanel.create
            [ DockPanel.margin (0.0, 0.0, 0.0, 10.0)
              DockPanel.children
                  [ StackPanel.create
                        [ StackPanel.dock Dock.Right
                          StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 4.0
                          StackPanel.children
                              [ let a = state.PlayerSortBy

                                UI.iconToggleButton "Line" PlayerIcon.position (a = "position") (fun _ ->
                                    dispatch (SortPlayersBy "position"))

                                UI.iconToggleButton "Skill" PlayerIcon.skill (a = "skill") (fun _ ->
                                    dispatch (SortPlayersBy "skill"))

                                UI.iconToggleButton "Age" PlayerIcon.age (a = "age") (fun _ ->
                                    dispatch (SortPlayersBy "age"))

                                UI.iconToggleButton "Value" PlayerIcon.value (a = "value") (fun _ ->
                                    dispatch (SortPlayersBy "value"))

                                UI.iconToggleButton "Name" IconName.sort (a = "name") (fun _ ->
                                    dispatch (SortPlayersBy "name")) ] ]

                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 10.0
                          StackPanel.verticalAlignment VerticalAlignment.Center
                          StackPanel.margin (8.0, 8.0, 8.0, 8.0)
                          StackPanel.children
                              [ Icons.iconMd IconName.squad Theme.Accent
                                TextBlock.create
                                    [ TextBlock.text "First Team"
                                      TextBlock.fontSize 18.0
                                      TextBlock.fontWeight FontWeight.Black
                                      TextBlock.foreground Theme.TextMain
                                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                                UI.countBadge squadCount ] ] ] ]

    let squadView (state: State) dispatch =
        match state.Mode with
        | InGame (gs, _) ->
            let squad = GameState.getSquad gs.UserClubId gs
            let stats = SquadPresenter.getTeamStats squad

            let groups =
                SquadPresenter.getGroupedPlayers squad gs.CurrentDate state.PlayerSortBy

            Grid.create
                [ Grid.columnDefinitions "*, 420"
                  Grid.verticalAlignment VerticalAlignment.Stretch
                  Grid.children
                      [ Grid.create
                            [ Grid.column 0
                              Grid.rowDefinitions "auto, auto, auto, *"
                              Grid.margin (0.0, 0.0, 20.0, 0.0)
                              Grid.children
                                  [ UniformGrid.create
                                        [ Grid.row 0
                                          UniformGrid.columns 4
                                          UniformGrid.margin (0.0, 0.0, 0.0, 16.0)
                                          UniformGrid.horizontalAlignment HorizontalAlignment.Stretch
                                          UniformGrid.children
                                              [ UI.iconStatCard "SQUAD SIZE" stats.SquadSize IconName.squad ""
                                                UI.iconStatCard "TEAM VALUE" stats.TotalValue PlayerIcon.value "Market"
                                                UI.iconStatCard "AVG SKILL" stats.AvgSkill PlayerIcon.skill "CA"
                                                UI.iconStatCard "WAGE BILL" stats.TotalWages Club.finances "Weekly" ] ]

                                    (sectionHeader squad.Length dispatch state)
                                    |> fun h -> Border.create [ Grid.row 1; Border.child h ]

                                    PlayerView.tableHeader ()
                                    |> fun h -> Border.create [ Grid.row 2; Border.child h ]

                                    ScrollViewer.create
                                        [ Grid.row 3
                                          ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                          ScrollViewer.content (
                                              StackPanel.create
                                                  [ StackPanel.spacing 1.0
                                                    StackPanel.children
                                                        [ for label, players in groups do
                                                              match label with
                                                              | Some l -> positionGroupHeader l
                                                              | None -> ()

                                                              for player in players do
                                                                  let isSelected =
                                                                      state.SelectedPlayer
                                                                      |> Option.exists (fun id -> id = player.Id)

                                                                  PlayerView.row
                                                                      player
                                                                      gs.CurrentDate
                                                                      isSelected
                                                                      (state.DraggedPlayer = Some player.Id)
                                                                      (fun () -> dispatch (SelectPlayer player.Id))
                                                                  |> View.withKey (string player.Id) ] ]
                                          ) ] ] ]

                        Grid.create
                            [ Grid.column 1
                              Grid.children
                                  [ match state.SelectedPlayer |> Option.bind (fun id -> gs.Players |> Map.tryFind id) with
                                    | Some player -> PlayerView.detail (Some player) gs.CurrentDate
                                    | None -> PlayerView.detail None gs.CurrentDate ] ] ] ]
            :> IView
        | _ -> Border.create [] :> IView
