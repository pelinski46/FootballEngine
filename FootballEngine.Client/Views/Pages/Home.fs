namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.FuncUI.DSL
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
            let rival = state.Clubs[rivalId]

            Some
                { LocalName = if isHome then userTeam.Name else rival.Name
                  VisitName = if isHome then rival.Name else userTeam.Name
                  Date = fixture.ScheduledDate
                  LocationTag = if isHome then "HOME" else "AWAY" }

    let private getPosColor (leagueId: int) (pos: int) =
        match leagueId, pos with
        | 1, p when p <= 4 -> Theme.Success
        | 1, p when p >= 18 -> Theme.Danger
        | 2, p when p <= 2 -> Theme.Success
        | 2, p when p <= 6 -> Theme.Warning
        | _ -> Theme.TextMuted

    let getTableData (state: GameState) (leagueId: int) =
        match state.Competitions.TryFind leagueId with
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
        state.Competitions
        |> Map.toList
        |> List.map (fun (k, v) -> k, v.Name)
        |> List.sortBy fst


module Home =

    let private quickStatsRow (userTeam: Club) =
        let budgetCard =
            UI.iconStatCard "BUDGET" $"€{int (userTeam.Budget / 1_000_000m)}M" Icons.Club.finances "Available"

        let squadCard =
            UI.iconStatCard "SQUAD" (string userTeam.Players.Length) Icons.UI.squad "Players"

        Grid.create
            [ Grid.columnDefinitions "*, 8, *"
              Grid.children
                  [ Border.create [ Grid.column 0; Border.child budgetCard ]
                    Border.create [ Grid.column 2; Border.child squadCard ] ] ]

    let private inboxPanel (messages: string list) =
        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.padding (0.0, 4.0)
              Border.child (
                  ScrollViewer.create
                      [ ScrollViewer.maxHeight 280.0
                        ScrollViewer.content (
                            StackPanel.create
                                [ StackPanel.spacing 4.0
                                  StackPanel.margin (8.0, 4.0)
                                  StackPanel.children
                                      [ for msg in messages do
                                            UI.iconRow Icons.UI.info msg ] ]
                        ) ]
              ) ]

    let private rightColumn (userTeam: Club) (messages: string list) =
        StackPanel.create
            [ Grid.column 2
              StackPanel.spacing 20.0
              StackPanel.children
                  [ quickStatsRow userTeam
                    StackPanel.create
                        [ StackPanel.spacing 0.0
                          StackPanel.children [ UI.sectionTitle "INBOX & NEWS" Icons.UI.info; inboxPanel messages ] ] ] ]

    let private leftColumn (gs: GameState) (selectedLeagueId: CompetitionId) (onLeagueChange: CompetitionId -> unit) =
        StackPanel.create
            [ Grid.column 0
              StackPanel.spacing 0.0
              StackPanel.children
                  [ UI.sectionTitle "LEAGUE STANDINGS" Icons.UI.league
                    Display.Tables.leagueTable
                        (HomePresenter.getAvailableLeagues gs)
                        selectedLeagueId
                        onLeagueChange
                        (HomePresenter.getTableData gs selectedLeagueId) ] ]

    let homeView (state: State) (selectedLeagueId: CompetitionId) (onLeagueChange: CompetitionId -> unit) =
        let userTeam = state.GameState.Clubs[state.GameState.UserClubId]

        let banner =
            match HomePresenter.getNextMatch state.GameState with
            | None -> Display.Matches.emptyBanner ()
            | Some m -> Display.Matches.nextMatchBanner m.LocalName m.VisitName m.Date m.LocationTag

        ScrollViewer.create
            [ ScrollViewer.content (
                  StackPanel.create
                      [ StackPanel.margin 24.0
                        StackPanel.spacing 20.0
                        StackPanel.children
                            [ banner

                              Grid.create
                                  [ Grid.columnDefinitions "1.6*, 24, 1*"
                                    Grid.children
                                        [ leftColumn state.GameState selectedLeagueId onLeagueChange
                                          rightColumn userTeam state.LogMessages ] ] ] ]
              ) ]
