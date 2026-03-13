namespace FootballEngine.Pages

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.Domain
open FootballEngine.AppState
open FootballEngine.Components
open FootballEngine.DomainTypes


module SquadPresenter =

    type TeamStatsVM =
        { TotalValue: string
          AvgSkill: string
          TotalWages: string
          SquadSize: string }

    let getTeamStats (team: Club) =
        let totalValue = team.Players |> List.sumBy _.Value
        let totalWages = team.Players |> List.sumBy _.Salary

        let avgSkill =
            if team.Players.IsEmpty then
                0.0
            else
                team.Players |> List.averageBy (fun p -> float p.CurrentSkill)

        { TotalValue = $"€{int (totalValue / 1_000_000m)}M"
          AvgSkill = $"%.0f{avgSkill}"
          TotalWages = $"€{int (totalWages / 1_000m)}K/pw"
          SquadSize = string team.Players.Length }

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

    let getSortedPlayers (team: Club) (currentDate: DateTime) (sortKey: string) =
        match sortKey with
        | "name" -> team.Players |> List.sortBy _.Name
        | "skill" -> team.Players |> List.sortByDescending _.CurrentSkill
        | "age" -> team.Players |> List.sortBy (Player.age currentDate)
        | "value" -> team.Players |> List.sortByDescending _.Value
        | _ ->
            team.Players
            |> List.sortBy (fun p -> fst (positionLine p.Position), -p.CurrentSkill)

    let getGroupedPlayers (team: Club) (currentDate: DateTime) (sortKey: string) =
        match sortKey with
        | "name"
        | "skill"
        | "age"
        | "value" -> [ None, getSortedPlayers team currentDate sortKey ]
        | _ ->
            team.Players
            |> List.groupBy (fun p -> positionLine p.Position)
            |> List.sortBy fst
            |> List.map (fun ((_, label), players) -> Some label, players |> List.sortByDescending _.CurrentSkill)


module Squad =

    let private positionGroupHeader (label: string) =
        Border.create
            [ Border.padding (12.0, 6.0)
              Border.background Theme.BgSidebar
              Border.child (
                  TextBlock.create
                      [ TextBlock.text label
                        TextBlock.fontSize 10.0
                        TextBlock.fontWeight FontWeight.Black
                        TextBlock.foreground Theme.Accent
                        TextBlock.lineSpacing 1.5 ]
              ) ]

    let squadView (state: State) dispatch =
        let userTeam = state.GameState.Clubs[state.GameState.UserClubId]
        let stats = SquadPresenter.getTeamStats userTeam

        let groups =
            SquadPresenter.getGroupedPlayers userTeam state.GameState.CurrentDate state.PlayerSortBy

        let sortButton label key =
            UI.tabButton label (state.PlayerSortBy = key) (fun _ -> dispatch (SortPlayersBy key))

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
                                      UniformGrid.margin (0.0, 0.0, 0.0, 20.0)
                                      UniformGrid.children
                                          [ UI.statCard "SQUAD SIZE" stats.SquadSize "👤" ""
                                            UI.statCard "TEAM VALUE" stats.TotalValue "💰" "Market Value"
                                            UI.statCard "AVG SKILL" stats.AvgSkill "⭐" "CA (1-200)"
                                            UI.statCard "WAGE BILL" stats.TotalWages "💵" "Weekly" ] ]

                                DockPanel.create
                                    [ Grid.row 1
                                      DockPanel.margin (0.0, 0.0, 0.0, 10.0)
                                      DockPanel.children
                                          [ TextBlock.create
                                                [ TextBlock.text "First Team Squad"
                                                  TextBlock.fontSize 20.0
                                                  TextBlock.fontWeight FontWeight.Black
                                                  TextBlock.verticalAlignment VerticalAlignment.Center ]
                                            StackPanel.create
                                                [ StackPanel.dock Dock.Right
                                                  StackPanel.orientation Orientation.Horizontal
                                                  StackPanel.spacing 4.0
                                                  StackPanel.children
                                                      [ sortButton "Line" "position"
                                                        sortButton "Skill" "skill"
                                                        sortButton "Age" "age"
                                                        sortButton "Value" "value"
                                                        sortButton "Name" "name" ] ] ] ]

                                PlayerView.tableHeader ()
                                |> fun h -> Border.create [ Grid.row 2; Border.child h ]

                                ScrollViewer.create
                                    [ Grid.row 3
                                      ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                      ScrollViewer.content (
                                          StackPanel.create
                                              [ StackPanel.spacing 2.0
                                                StackPanel.children
                                                    [ for label, players in groups do
                                                          match label with
                                                          | Some l -> positionGroupHeader l
                                                          | None -> ()

                                                          for player in players do
                                                              let isSelected =
                                                                  state.SelectedPlayer
                                                                  |> Option.map (fun p -> p.Id = player.Id)
                                                                  |> Option.defaultValue false

                                                              PlayerView.row
                                                                  player
                                                                  state.GameState.CurrentDate
                                                                  isSelected
                                                                  (state.DraggedPlayer = Some player.Id)
                                                                  (fun () -> dispatch (SelectPlayer player.Id))
                                                              |> View.withKey (string player.Id) ] ]
                                      ) ] ] ]

                    Grid.create
                        [ Grid.column 1
                          Grid.children [ PlayerView.detail state.SelectedPlayer state.GameState.CurrentDate ] ] ] ]
