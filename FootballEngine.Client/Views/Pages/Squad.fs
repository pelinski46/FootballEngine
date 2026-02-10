namespace FootballEngine.Pages

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open FootballEngine.Domain
open FootballEngine.AppState
open FootballEngine.Components


module SquadPresenter =


    type TeamStatsVM =
        { TotalValue: string
          AvgSkill: string
          TotalWages: string }

    let getTeamStats (team: Club) =

        let totalValue = team.Players |> List.sumBy (fun p -> p.Value)
        let totalWages = team.Players |> List.sumBy (fun p -> p.Salary)

        let avgSkill =
            if team.Players.IsEmpty then
                0.0
            else
                team.Players |> List.averageBy (fun p -> float p.CurrentSkill)

        { TotalValue = $"€{int (totalValue / 1_000_000m)}M"
          AvgSkill = $"%.0f{avgSkill}"
          TotalWages = $"€{int (totalWages / 1_000m)}K/pw" }

    let getSortedPlayers (team: Club) (currentDate: DateTime) (sortKey: string) =
        match sortKey with
        | "name" -> team.Players |> List.sortBy (fun p -> p.Name)
        | "skill" -> team.Players |> List.sortByDescending (fun p -> p.CurrentSkill)
        | "age" -> team.Players |> List.sortBy (fun p -> Player.age currentDate p)
        | "value" -> team.Players |> List.sortByDescending (fun p -> p.Value)
        | _ -> team.Players |> List.sortBy (fun p -> p.Position, -p.CurrentSkill)

// --- PAGE VIEW ---
module Squad =

    let squadView (state: State) dispatch =

        let userTeam = state.GameState.Clubs[state.GameState.UserClubId]


        let stats = SquadPresenter.getTeamStats userTeam

        let players =
            SquadPresenter.getSortedPlayers userTeam state.GameState.CurrentDate state.PlayerSortBy

        Grid.create
            [ Grid.columnDefinitions "*, 400"
              Grid.verticalAlignment VerticalAlignment.Stretch
              Grid.children
                  [


                    ScrollViewer.create
                        [ Grid.column 0
                          ScrollViewer.content (
                              StackPanel.create
                                  [ StackPanel.spacing 20.0
                                    StackPanel.margin (0.0, 0.0, 20.0, 0.0)
                                    StackPanel.children
                                        [


                                          UniformGrid.create
                                              [ UniformGrid.columns 3
                                                UniformGrid.children
                                                    [ UI.statCard "TEAM VALUE" stats.TotalValue "💰" "Market Value"
                                                      UI.statCard "AVG SKILL" stats.AvgSkill "⭐" "Squad Rating (1-200)"
                                                      UI.statCard "WAGE BILL" stats.TotalWages "💵" "Weekly" ] ]


                                          DockPanel.create
                                              [ DockPanel.children
                                                    [ TextBlock.create
                                                          [ TextBlock.text "First Team Squad"
                                                            TextBlock.fontSize 24.0
                                                            TextBlock.fontWeight FontWeight.Black ]
                                                      StackPanel.create
                                                          [ StackPanel.dock Dock.Right
                                                            StackPanel.orientation Orientation.Horizontal
                                                            StackPanel.children
                                                                [ UI.tabButton
                                                                      "Pos"
                                                                      (state.PlayerSortBy = "position")
                                                                      (fun _ -> dispatch (SortPlayersBy "position"))
                                                                  UI.tabButton
                                                                      "Skill"
                                                                      (state.PlayerSortBy = "skill")
                                                                      (fun _ -> dispatch (SortPlayersBy "skill"))
                                                                  UI.tabButton
                                                                      "Age"
                                                                      (state.PlayerSortBy = "age")
                                                                      (fun _ -> dispatch (SortPlayersBy "age")) ] ] ] ]


                                          StackPanel.create
                                              [ StackPanel.children
                                                    [ UI.playerTableHeader ()

                                                      StackPanel.create
                                                          [ StackPanel.spacing 5.0
                                                            StackPanel.children
                                                                [ for player in players do
                                                                      let isSelected =
                                                                          state.SelectedPlayer
                                                                          |> Option.map (fun p -> p.Id = player.Id)
                                                                          |> Option.defaultValue false


                                                                      UI.playerRow
                                                                          player
                                                                          state.GameState.CurrentDate
                                                                          player.Value
                                                                          isSelected
                                                                          (state.DraggedPlayer = Some player.Id)
                                                                          (fun () -> dispatch (SelectPlayer player.Id))
                                                                          (fun () ->
                                                                              dispatch (DragStartPlayer player.Id))
                                                                      |> View.withKey (string player.Id) ] ] ] ] ] ]
                          ) ]


                    Grid.create
                        [ Grid.column 1

                          Grid.children
                              [ UI.playerDetail
                                    state.SelectedPlayer
                                    state.GameState.CurrentDate
                                    (match state.SelectedPlayer with
                                     | Some p -> p.Value
                                     | None -> 0m) ] ] ] ]
