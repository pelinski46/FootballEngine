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
open FootballEngine.Icons


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
        | "age" -> team.Players |> List.sortBy (fun p -> Player.age currentDate p)
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

                                UI.iconToggleButton "Name" UI.sort (a = "name") (fun _ ->
                                    dispatch (SortPlayersBy "name")) ] ]

                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 10.0
                          StackPanel.verticalAlignment VerticalAlignment.Center
                          StackPanel.children
                              [ Icons.iconMd UI.squad Theme.Accent
                                TextBlock.create
                                    [ TextBlock.text "First Team"
                                      TextBlock.fontSize 18.0
                                      TextBlock.fontWeight FontWeight.Black
                                      TextBlock.foreground Theme.TextMain
                                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                                UI.countBadge squadCount ] ] ] ]

    let squadView (state: State) dispatch =
        let userTeam = state.GameState.Clubs[state.GameState.UserClubId]
        let stats = SquadPresenter.getTeamStats userTeam

        let groups =
            SquadPresenter.getGroupedPlayers userTeam state.GameState.CurrentDate state.PlayerSortBy

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
                                          [ UI.iconStatCard "SQUAD SIZE" stats.SquadSize UI.squad ""
                                            UI.iconStatCard "TEAM VALUE" stats.TotalValue PlayerIcon.value "Market"
                                            UI.iconStatCard "AVG SKILL" stats.AvgSkill PlayerIcon.skill "CA"
                                            UI.iconStatCard "WAGE BILL" stats.TotalWages Club.finances "Weekly" ] ]

                                (sectionHeader userTeam.Players.Length dispatch state)
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
