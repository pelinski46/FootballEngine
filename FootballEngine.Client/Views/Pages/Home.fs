namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Media
open FootballEngine
open FootballEngine.Domain
open FootballEngine.AppState
open FootballEngine.Components


module HomePresenter =
    type NextMatchVM =
        { LocalName: string
          VisitName: string
          Date: System.DateTime
          LocationTag: string }

    let getNextMatch (state: GameState) =
        match getUserNextFixture state with
        | None -> None
        | Some(_, fixture) ->
            let userTeam = state.Clubs[state.UserClubId]
            let isHome = fixture.HomeClubId = state.UserClubId
            let rivalId = if isHome then fixture.AwayClubId else fixture.HomeClubId
            let rivalTeam = state.Clubs[rivalId]


            let homeName = if isHome then userTeam.Name else rivalTeam.Name
            let awayName = if isHome then rivalTeam.Name else userTeam.Name

            Some
                { LocalName = homeName
                  VisitName = awayName
                  Date = fixture.ScheduledDate
                  LocationTag = if isHome then "HOME" else "AWAY" }

    let private getPosColor (leagueId: int) (pos: int) =
        match leagueId, pos with
        | 1, p when p <= 4 -> "#10b981"
        | 1, p when p >= 18 -> "#ef4444"
        | 2, p when p <= 2 -> "#10b981"
        | 2, p when p <= 6 -> "#f59e0b"
        | _ -> Theme.TextMuted

    let getTableData (state: GameState) (leagueId: int) =
        match state.Leagues.TryFind leagueId with
        | None -> []
        | Some league ->
            league.ClubIds
            |> List.choose (fun tid -> state.Clubs.TryFind tid)
            |> List.sortByDescending (fun t -> ((t.Wins * 3) + t.Draws, t.Wins, -t.Losses))
            |> List.indexed
            |> List.map (fun (i, t) ->
                { Display.Tables.Pos = i + 1
                  Display.Tables.TeamName = t.Name
                  Display.Tables.Points = (t.Wins * 3) + t.Draws
                  Display.Tables.Stats = $"{t.Wins}-{t.Draws}-{t.Losses}"
                  Display.Tables.IsUser = (t.Id = state.UserClubId)
                  Display.Tables.PosColor = getPosColor leagueId (i + 1) })

    let getAvailableLeagues (state: GameState) =
        state.Leagues
        |> Map.toList
        |> List.map (fun (k, v) -> k, v.Name)
        |> List.sortBy fst

module Home =

    let homeView (state: State) (selectedLeagueId: LeagueId) (onLeagueChange: LeagueId -> unit) =
        let userTeam = state.GameState.Clubs[state.GameState.UserClubId]

        ScrollViewer.create
            [ ScrollViewer.content (
                  StackPanel.create
                      [ StackPanel.margin 30.0
                        StackPanel.children
                            [

                              match HomePresenter.getNextMatch state.GameState with
                              | None -> Display.Matches.emptyBanner ()
                              | Some m -> Display.Matches.nextMatchBanner m.LocalName m.VisitName m.Date m.LocationTag

                              // 2. MAIN GRID
                              Grid.create
                                  [ Grid.columnDefinitions "1.6*, 30, 1*"
                                    Grid.children
                                        [

                                          UI.sectionContainer
                                              "LEAGUE STANDINGS"
                                              (Display.Tables.leagueTable
                                                  (HomePresenter.getAvailableLeagues state.GameState)
                                                  selectedLeagueId
                                                  onLeagueChange
                                                  (HomePresenter.getTableData state.GameState selectedLeagueId))

                                          // RIGHT COL: WIDGETS
                                          StackPanel.create
                                              [ Grid.column 2
                                                StackPanel.spacing 20.0
                                                StackPanel.children
                                                    [

                                                      // Quick Stats
                                                      Grid.create
                                                          [ Grid.columnDefinitions "*, 15, *"
                                                            Grid.children
                                                                [ StackPanel.create
                                                                      [ Grid.column 0
                                                                        StackPanel.children
                                                                            [ UI.statCard
                                                                                  "BUDGET"
                                                                                  $"€{int (userTeam.Budget / 1_000_000m)}M"
                                                                                  "💰"
                                                                                  "Transfer Available" ] ]
                                                                  StackPanel.create
                                                                      [ Grid.column 2
                                                                        StackPanel.children
                                                                            [ UI.statCard
                                                                                  "SQUAD"
                                                                                  (string userTeam.Players.Length)
                                                                                  "👥"
                                                                                  "Registered Players" ] ] ] ]

                                                      UI.sectionContainer
                                                          "INBOX & NEWS"
                                                          (ScrollViewer.create
                                                              [ ScrollViewer.maxHeight 300.0
                                                                ScrollViewer.padding 15.0
                                                                ScrollViewer.content (
                                                                    StackPanel.create
                                                                        [ StackPanel.spacing 8.0
                                                                          StackPanel.children
                                                                              [ for msg in state.LogMessages do
                                                                                    TextBlock.create
                                                                                        [ TextBlock.text $"• {msg}"
                                                                                          TextBlock.fontSize 11.0
                                                                                          TextBlock.foreground
                                                                                              Theme.TextMain
                                                                                          TextBlock.textWrapping
                                                                                              TextWrapping.Wrap ] ] ]
                                                                ) ]) ] ] ] ] ] ]
              ) ]
