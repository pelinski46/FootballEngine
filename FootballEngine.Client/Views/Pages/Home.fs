namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppTypes
open FootballEngine.AppMsgs
open FootballEngine.Domain
open FootballEngine.Components
open FootballEngine.UpdateSim
open FootballEngine.Icons

module HomePresenter =

    type NextMatchVM =
        { LocalName: string
          VisitName: string
          Date: System.DateTime
          LocationTag: string }

    type RecentResultVM =
        { HomeName: string
          AwayName: string
          HomeScore: int
          AwayScore: int
          UserWon: bool option }

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

    let getUpcomingFixtures (state: GameState) (count: int) =
        state.Competitions
        |> Map.toSeq
        |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
        |> Seq.filter (fun (_, f) ->
            not f.Played
            && (f.HomeClubId = state.UserClubId || f.AwayClubId = state.UserClubId))
        |> Seq.sortBy (fun (_, f) -> f.ScheduledDate)
        |> Seq.truncate count
        |> List.ofSeq

    let getRecentResults (state: GameState) (count: int) =
        state.Competitions
        |> Map.toSeq
        |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
        |> Seq.filter (fun (_, f) -> f.Played && (f.HomeClubId = state.UserClubId || f.AwayClubId = state.UserClubId))
        |> Seq.sortByDescending (fun (_, f) -> f.ScheduledDate)
        |> Seq.truncate count
        |> Seq.choose (fun (_, f) ->
            match f.HomeScore, f.AwayScore with
            | Some hScore, Some aScore ->
                let isHome = f.HomeClubId = state.UserClubId

                let homeName =
                    state.Clubs
                    |> Map.tryFind f.HomeClubId
                    |> Option.map _.Name
                    |> Option.defaultValue "?"

                let awayName =
                    state.Clubs
                    |> Map.tryFind f.AwayClubId
                    |> Option.map _.Name
                    |> Option.defaultValue "?"

                let userWon =
                    if isHome then
                        if hScore > aScore then Some true
                        elif hScore < aScore then Some false
                        else None
                    else if aScore > hScore then
                        Some true
                    elif aScore < hScore then
                        Some false
                    else
                        None

                Some
                    { HomeScore = hScore
                      AwayScore = aScore
                      HomeName = homeName
                      AwayName = awayName
                      UserWon = userWon }
            | _ -> None)
        |> List.ofSeq

    let private getPosColor (leagueId: int) (pos: int) =
        match leagueId, pos with
        | 1, p when p <= 4 -> Theme.Success
        | 1, p when p >= 18 -> Theme.Danger
        | 2, p when p <= 2 -> Theme.Success
        | 2, p when p <= 6 -> Theme.Warning
        | _ -> Theme.TextMuted

    let private emptyStanding clubId =
        { ClubId = clubId
          Played = 0
          Won = 0
          Drawn = 0
          Lost = 0
          GoalsFor = 0
          GoalsAgainst = 0
          Points = 0 }

    let getUserStanding (state: GameState) (leagueId: CompetitionId) =
        state.Competitions.TryFind leagueId
        |> Option.bind (fun comp ->
            let sorted =
                comp.ClubIds
                |> List.choose (fun id ->
                    state.Clubs.TryFind id
                    |> Option.map (fun club ->
                        let s = comp.Standings.TryFind id |> Option.defaultWith (fun () -> emptyStanding id)
                        club, s))
                |> List.sortByDescending (fun (_, s) -> s.Points, s.Won, -s.Lost)
                |> List.indexed

            sorted
            |> List.tryFind (fun (_, (club, _)) -> club.Id = state.UserClubId)
            |> Option.map (fun (i, (_, s)) -> i + 1, sorted.Length, s))

    let getTableData (state: GameState) (leagueId: CompetitionId) =
        match state.Competitions.TryFind leagueId with
        | None -> []
        | Some league ->
            league.ClubIds
            |> List.choose (fun tid -> state.Clubs.TryFind tid |> Option.map (fun club -> club, tid))
            |> List.map (fun (club, tid) ->
                let s =
                    league.Standings.TryFind tid |> Option.defaultWith (fun () -> emptyStanding tid)

                club, s)
            |> List.sortByDescending (fun (_, s) -> s.Points, s.Won, -s.Lost)
            |> List.indexed
            |> List.map (fun (i, (club, s)) ->
                { Display.Tables.Pos = i + 1
                  Display.Tables.TeamName = club.Name
                  Display.Tables.Points = s.Points
                  Display.Tables.Stats = $"{s.Won}-{s.Drawn}-{s.Lost}"
                  Display.Tables.IsUser = (club.Id = state.UserClubId)
                  Display.Tables.PosColor = getPosColor leagueId (i + 1) })

    let getGroupedCompetitions (state: GameState) =
        let all =
            state.Competitions
            |> Map.toList
            |> List.map (fun (id, comp) -> id, comp.Name, comp.Type)
            |> List.sortBy (fun (_, name, _) -> name)

        let leagues =
            all
            |> List.choose (fun (id, name, t) ->
                match t with
                | NationalLeague _ -> Some(id, name)
                | _ -> None)

        let cups =
            all
            |> List.choose (fun (id, name, t) ->
                match t with
                | NationalCup _ -> Some(id, name)
                | _ -> None)

        let international =
            all
            |> List.choose (fun (id, name, t) ->
                match t with
                | InternationalCup _ -> Some(id, name)
                | _ -> None)

        [ if not leagues.IsEmpty then
              "LEAGUES", UI.league, leagues
          if not cups.IsEmpty then
              "CUPS", Club.stadium, cups
          if not international.IsEmpty then
              "INTERNATIONAL", UI.league, international ]


module Home =

    let private resultDot (userWon: bool option) =
        let color =
            match userWon with
            | Some true -> Theme.Success
            | Some false -> Theme.Danger
            | None -> Theme.Warning

        let letter =
            match userWon with
            | Some true -> "W"
            | Some false -> "L"
            | None -> "D"

        Border.create
            [ Border.width 22.0
              Border.height 22.0
              Border.cornerRadius 11.0
              Border.background (color + "33")
              Border.borderBrush (color + "88")
              Border.borderThickness 1.0
              Border.child (
                  TextBlock.create
                      [ TextBlock.text letter
                        TextBlock.fontSize 9.0
                        TextBlock.fontWeight FontWeight.Black
                        TextBlock.foreground color
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.verticalAlignment VerticalAlignment.Center ]
              ) ]

    let private summaryBar (userTeam: Club) (pos: int option) (total: int option) (standing: LeagueStanding option) =
        Grid.create
            [ Grid.columnDefinitions "*, 8, *, 8, *, 8, *"
              Grid.children
                  [ UI.iconStatCard "BUDGET" $"€{int (userTeam.Budget / 1_000_000m)}M" Club.finances "Available"
                    |> fun c -> Border.create [ Grid.column 0; Border.child c ]

                    Border.create
                        [ Grid.column 2
                          Border.child (
                              UI.iconStatCard
                                  "POSITION"
                                  (match pos, total with
                                   | Some p, Some t -> $"{p} / {t}"
                                   | _ -> "—")
                                  UI.league
                                  (match standing with
                                   | Some s -> $"{s.Points} pts"
                                   | _ -> "")
                          ) ]

                    Border.create
                        [ Grid.column 4
                          Border.child (
                              UI.iconStatCard
                                  "FORM"
                                  (match standing with
                                   | Some s -> $"{s.Won}W {s.Drawn}D {s.Lost}L"
                                   | None -> "—")
                                  MatchEvent.goal
                                  (match standing with
                                   | Some s -> $"{s.Played} played"
                                   | None -> "")
                          ) ]

                    UI.iconStatCard "MORALE" $"{userTeam.Morale}%%" PlayerIcon.morale ""
                    |> fun c -> Border.create [ Grid.column 6; Border.child c ] ] ]

    let private notificationKindColor =
        function
        | MatchResult -> Theme.Accent
        | SeasonEnd -> Theme.Warning
        | Transfer -> Theme.AccentAlt
        | Info -> Theme.TextMuted

    let private notificationRow (note: Notification) dispatch =
        let color = notificationKindColor note.Kind

        Border.create
            [ Border.padding (12.0, 10.0)
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.background (color + "0A")
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "4, *, Auto"
                        Grid.children
                            [ Border.create
                                  [ Grid.column 0
                                    Border.width 3.0
                                    Border.cornerRadius 2.0
                                    Border.background color
                                    Border.margin (0.0, 0.0, 10.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Stretch ]

                              StackPanel.create
                                  [ Grid.column 1
                                    StackPanel.spacing 2.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text note.Title
                                                TextBlock.fontSize 12.0
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.textWrapping TextWrapping.Wrap ]
                                          if note.Body <> "" then
                                              TextBlock.create
                                                  [ TextBlock.text note.Body
                                                    TextBlock.fontSize 11.0
                                                    TextBlock.foreground Theme.TextMuted
                                                    TextBlock.textWrapping TextWrapping.Wrap ] ] ]

                              Button.create
                                  [ Grid.column 2
                                    Button.background "Transparent"
                                    Button.borderThickness 0.0
                                    Button.padding (4.0, 0.0)
                                    Button.verticalAlignment VerticalAlignment.Top
                                    Button.cursor Avalonia.Input.Cursor.Default
                                    Button.onClick (fun _ -> dispatch (NotificationMsg(DismissNotification note.Id)))
                                    Button.content (
                                        TextBlock.create
                                            [ TextBlock.text "✕"
                                              TextBlock.fontSize 10.0
                                              TextBlock.foreground Theme.TextMuted ]
                                    ) ] ] ]
              ) ]

    let private notificationsPanel (notes: Notification list) dispatch =
        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 0.0
                        StackPanel.children
                            [ Border.create
                                  [ Border.padding (14.0, 10.0)
                                    Border.borderBrush Theme.Border
                                    Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                    Border.child (
                                        DockPanel.create
                                            [ DockPanel.lastChildFill false
                                              DockPanel.children
                                                  [ StackPanel.create
                                                        [ DockPanel.dock Dock.Left
                                                          StackPanel.orientation Orientation.Horizontal
                                                          StackPanel.spacing 6.0
                                                          StackPanel.children
                                                              [ Icons.iconSm UI.info Theme.Accent
                                                                TextBlock.create
                                                                    [ TextBlock.text "NOTIFICATIONS"
                                                                      TextBlock.fontSize 10.0
                                                                      TextBlock.fontWeight FontWeight.Black
                                                                      TextBlock.foreground Theme.TextMuted
                                                                      TextBlock.verticalAlignment
                                                                          VerticalAlignment.Center ]
                                                                if not notes.IsEmpty then
                                                                    UI.countBadge notes.Length ] ]

                                                    if not notes.IsEmpty then
                                                        Button.create
                                                            [ DockPanel.dock Dock.Right
                                                              Button.background "Transparent"
                                                              Button.borderThickness 0.0
                                                              Button.padding (4.0, 2.0)
                                                              Button.cursor Avalonia.Input.Cursor.Default
                                                              Button.onClick (fun _ ->
                                                                  dispatch (NotificationMsg DismissAll))
                                                              Button.content (
                                                                  TextBlock.create
                                                                      [ TextBlock.text "Clear all"
                                                                        TextBlock.fontSize 10.0
                                                                        TextBlock.foreground Theme.TextMuted ]
                                                              ) ] ] ]
                                    ) ]

                              if notes.IsEmpty then
                                  Border.create
                                      [ Border.padding (14.0, 16.0)
                                        Border.child (
                                            TextBlock.create
                                                [ TextBlock.text "No notifications"
                                                  TextBlock.fontSize 12.0
                                                  TextBlock.foreground Theme.TextMuted ]
                                        ) ]
                              else
                                  ScrollViewer.create
                                      [ ScrollViewer.maxHeight 260.0
                                        ScrollViewer.content (
                                            StackPanel.create
                                                [ StackPanel.spacing 0.0
                                                  StackPanel.children
                                                      [ for note in notes do
                                                            notificationRow note dispatch ] ]
                                        ) ] ] ]
              ) ]

    let private upcomingFixtureRow (fixture: MatchId * MatchFixture) (gs: GameState) =
        let _, f = fixture
        let isHome = f.HomeClubId = gs.UserClubId
        let rivalId = if isHome then f.AwayClubId else f.HomeClubId

        let rival =
            gs.Clubs |> Map.tryFind rivalId |> Option.map _.Name |> Option.defaultValue "?"

        let locationColor = if isHome then Theme.AccentAlt else Theme.Warning

        Border.create
            [ Border.padding (14.0, 10.0)
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "Auto, *, Auto"
                        Grid.children
                            [ Border.create
                                  [ Grid.column 0
                                    Border.background (locationColor + "22")
                                    Border.borderBrush (locationColor + "55")
                                    Border.borderThickness 1.0
                                    Border.cornerRadius 4.0
                                    Border.padding (5.0, 2.0)
                                    Border.margin (0.0, 0.0, 10.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text (if isHome then "H" else "A")
                                              TextBlock.fontSize 9.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground locationColor ]
                                    ) ]
                              TextBlock.create
                                  [ Grid.column 1
                                    TextBlock.text rival
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.foreground Theme.TextMain
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              TextBlock.create
                                  [ Grid.column 2
                                    TextBlock.text (f.ScheduledDate.ToString("dd MMM"))
                                    TextBlock.fontSize 11.0
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
              ) ]

    let private upcomingPanel (fixtures: (MatchId * MatchFixture) list) (gs: GameState) =
        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 0.0
                        StackPanel.children
                            [ Border.create
                                  [ Border.padding (14.0, 10.0)
                                    Border.borderBrush Theme.Border
                                    Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                    Border.child (
                                        StackPanel.create
                                            [ StackPanel.orientation Orientation.Horizontal
                                              StackPanel.spacing 6.0
                                              StackPanel.children
                                                  [ Icons.iconSm UI.calendar Theme.Accent
                                                    TextBlock.create
                                                        [ TextBlock.text "UPCOMING FIXTURES"
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                    ) ]
                              if fixtures.IsEmpty then
                                  Border.create
                                      [ Border.padding (14.0, 16.0)
                                        Border.child (
                                            TextBlock.create
                                                [ TextBlock.text "No upcoming fixtures"
                                                  TextBlock.fontSize 12.0
                                                  TextBlock.foreground Theme.TextMuted ]
                                        ) ]
                              else
                                  for fx in fixtures do
                                      upcomingFixtureRow fx gs ] ]
              ) ]

    let private recentResultRow (r: HomePresenter.RecentResultVM) =
        let resultColor =
            match r.UserWon with
            | Some true -> Theme.Success
            | Some false -> Theme.Danger
            | None -> Theme.Warning

        Border.create
            [ Border.padding (14.0, 9.0)
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "*, Auto, *"
                        Grid.children
                            [ TextBlock.create
                                  [ Grid.column 0
                                    TextBlock.text r.HomeName
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.foreground Theme.TextMain
                                    TextBlock.textAlignment TextAlignment.Right
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                              Border.create
                                  [ Grid.column 1
                                    Border.background (resultColor + "22")
                                    Border.borderBrush (resultColor + "55")
                                    Border.borderThickness 1.0
                                    Border.cornerRadius 6.0
                                    Border.padding (10.0, 4.0)
                                    Border.margin (10.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text $"{r.HomeScore} - {r.AwayScore}"
                                              TextBlock.fontSize 12.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground resultColor ]
                                    ) ]
                              TextBlock.create
                                  [ Grid.column 2
                                    TextBlock.text r.AwayName
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.foreground Theme.TextMain
                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
              ) ]

    let private recentResultsPanel (results: HomePresenter.RecentResultVM list) =
        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 0.0
                        StackPanel.children
                            [ Border.create
                                  [ Border.padding (14.0, 10.0)
                                    Border.borderBrush Theme.Border
                                    Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                    Border.child (
                                        StackPanel.create
                                            [ StackPanel.orientation Orientation.Horizontal
                                              StackPanel.spacing 6.0
                                              StackPanel.children
                                                  [ Icons.iconSm MatchEvent.goal Theme.Accent
                                                    TextBlock.create
                                                        [ TextBlock.text "RECENT RESULTS"
                                                          TextBlock.fontSize 10.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                    ) ]
                              if results.IsEmpty then
                                  Border.create
                                      [ Border.padding (14.0, 16.0)
                                        Border.child (
                                            TextBlock.create
                                                [ TextBlock.text "No results yet"
                                                  TextBlock.fontSize 12.0
                                                  TextBlock.foreground Theme.TextMuted ]
                                        ) ]
                              else
                                  for r in results do
                                      recentResultRow r ] ]
              ) ]

    let private competitionPicker
        (groups: (string * Material.Icons.MaterialIconKind * (CompetitionId * string) list) list)
        (selectedId: CompetitionId)
        (onSelect: CompetitionId -> unit)
        =
        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.clipToBounds true
              Border.child (
                  ScrollViewer.create
                      [ ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                        ScrollViewer.content (
                            StackPanel.create
                                [ StackPanel.spacing 0.0
                                  StackPanel.children
                                      [ for label, icon, items in groups do
                                            Border.create
                                                [ Border.padding (12.0, 7.0)
                                                  Border.background Theme.BgCard
                                                  Border.borderBrush Theme.Border
                                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                                  Border.child (
                                                      StackPanel.create
                                                          [ StackPanel.orientation Orientation.Horizontal
                                                            StackPanel.spacing 5.0
                                                            StackPanel.children
                                                                [ Icons.iconSm icon Theme.TextMuted
                                                                  TextBlock.create
                                                                      [ TextBlock.text label
                                                                        TextBlock.fontSize 9.0
                                                                        TextBlock.fontWeight FontWeight.Black
                                                                        TextBlock.foreground Theme.TextMuted
                                                                        TextBlock.verticalAlignment
                                                                            VerticalAlignment.Center ] ] ]
                                                  ) ]

                                            for id, name in items do
                                                let isActive = selectedId = id

                                                Button.create
                                                    [ Button.horizontalAlignment HorizontalAlignment.Stretch
                                                      Button.padding (16.0, 9.0)
                                                      Button.background (
                                                          if isActive then Theme.AccentLight else "Transparent"
                                                      )
                                                      Button.borderThickness (Avalonia.Thickness(3.0, 0.0, 0.0, 0.0))
                                                      Button.borderBrush (
                                                          if isActive then Theme.Accent else "Transparent"
                                                      )
                                                      Button.cornerRadius 0.0
                                                      Button.onClick (fun _ -> onSelect id)
                                                      Button.content (
                                                          TextBlock.create
                                                              [ TextBlock.text name
                                                                TextBlock.fontSize 12.0
                                                                TextBlock.fontWeight (
                                                                    if isActive then
                                                                        FontWeight.SemiBold
                                                                    else
                                                                        FontWeight.Normal
                                                                )
                                                                TextBlock.foreground (
                                                                    if isActive then Theme.Accent else Theme.TextMain
                                                                ) ]
                                                      ) ] ] ]
                        ) ]
              ) ]

    let private standingsPanel (gs: GameState) (selectedId: CompetitionId) =
        let rows = HomePresenter.getTableData gs selectedId

        Border.create
            [ Border.background Theme.BgSidebar
              Border.cornerRadius 10.0
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 0.0
                        StackPanel.children
                            [ Border.create
                                  [ Border.padding (14.0, 10.0)
                                    Border.background Theme.BgCard
                                    Border.borderBrush Theme.Border
                                    Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                    Border.child (
                                        Grid.create
                                            [ Grid.columnDefinitions "*, Auto, Auto, Auto"
                                              Grid.children
                                                  [ TextBlock.create
                                                        [ Grid.column 0
                                                          TextBlock.text "CLUB"
                                                          TextBlock.fontSize 9.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted ]
                                                    TextBlock.create
                                                        [ Grid.column 1
                                                          TextBlock.text "W-D-L"
                                                          TextBlock.fontSize 9.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.width 60.0
                                                          TextBlock.textAlignment TextAlignment.Right ]
                                                    TextBlock.create
                                                        [ Grid.column 2
                                                          TextBlock.text "GD"
                                                          TextBlock.fontSize 9.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.width 36.0
                                                          TextBlock.textAlignment TextAlignment.Right ]
                                                    TextBlock.create
                                                        [ Grid.column 3
                                                          TextBlock.text "PTS"
                                                          TextBlock.fontSize 9.0
                                                          TextBlock.fontWeight FontWeight.Black
                                                          TextBlock.foreground Theme.TextMuted
                                                          TextBlock.width 36.0
                                                          TextBlock.textAlignment TextAlignment.Right ] ] ]
                                    ) ]
                              if rows.IsEmpty then
                                  Border.create
                                      [ Border.padding (14.0, 16.0)
                                        Border.child (
                                            TextBlock.create
                                                [ TextBlock.text "No standings available"
                                                  TextBlock.fontSize 12.0
                                                  TextBlock.foreground Theme.TextMuted ]
                                        ) ]
                              else
                                  ScrollViewer.create
                                      [ ScrollViewer.maxHeight 440.0
                                        ScrollViewer.content (
                                            StackPanel.create
                                                [ StackPanel.children
                                                      [ for row in rows do
                                                            Border.create
                                                                [ Border.background (
                                                                      if row.IsUser then
                                                                          Theme.AccentAlt + "22"
                                                                      else
                                                                          "Transparent"
                                                                  )
                                                                  Border.borderBrush Theme.Border
                                                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                                                  Border.padding (14.0, 9.0)
                                                                  Border.child (
                                                                      Grid.create
                                                                          [ Grid.columnDefinitions
                                                                                "24, *, Auto, Auto, Auto"
                                                                            Grid.children
                                                                                [ TextBlock.create
                                                                                      [ Grid.column 0
                                                                                        TextBlock.text (string row.Pos)
                                                                                        TextBlock.fontSize 11.0
                                                                                        TextBlock.fontWeight
                                                                                            FontWeight.Bold
                                                                                        TextBlock.foreground
                                                                                            row.PosColor
                                                                                        TextBlock.verticalAlignment
                                                                                            VerticalAlignment.Center ]
                                                                                  TextBlock.create
                                                                                      [ Grid.column 1
                                                                                        TextBlock.text row.TeamName
                                                                                        TextBlock.fontSize 12.0
                                                                                        TextBlock.fontWeight (
                                                                                            if row.IsUser then
                                                                                                FontWeight.Bold
                                                                                            else
                                                                                                FontWeight.Normal
                                                                                        )
                                                                                        TextBlock.foreground (
                                                                                            if row.IsUser then
                                                                                                Theme.TextMain
                                                                                            else
                                                                                                Theme.TextSub
                                                                                        )
                                                                                        TextBlock.verticalAlignment
                                                                                            VerticalAlignment.Center ]
                                                                                  TextBlock.create
                                                                                      [ Grid.column 2
                                                                                        TextBlock.text row.Stats
                                                                                        TextBlock.fontSize 11.0
                                                                                        TextBlock.foreground
                                                                                            Theme.TextMuted
                                                                                        TextBlock.width 60.0
                                                                                        TextBlock.textAlignment
                                                                                            TextAlignment.Right
                                                                                        TextBlock.verticalAlignment
                                                                                            VerticalAlignment.Center ]
                                                                                  TextBlock.create
                                                                                      [ Grid.column 3
                                                                                        TextBlock.text ""
                                                                                        TextBlock.width 36.0
                                                                                        TextBlock.textAlignment
                                                                                            TextAlignment.Right ]
                                                                                  TextBlock.create
                                                                                      [ Grid.column 4
                                                                                        TextBlock.text (
                                                                                            string row.Points
                                                                                        )
                                                                                        TextBlock.fontSize 12.0
                                                                                        TextBlock.fontWeight
                                                                                            FontWeight.Bold
                                                                                        TextBlock.foreground
                                                                                            Theme.TextMain
                                                                                        TextBlock.width 36.0
                                                                                        TextBlock.textAlignment
                                                                                            TextAlignment.Right
                                                                                        TextBlock.verticalAlignment
                                                                                            VerticalAlignment.Center ] ] ]
                                                                  ) ] ] ]
                                        ) ] ] ]
              ) ]

    let homeView
        (state: State)
        (selectedLeagueId: CompetitionId)
        (onLeagueChange: CompetitionId -> unit)
        dispatch
        : IView =
        let gs = state.GameState
        let userTeam = gs.Clubs[gs.UserClubId]

        let banner =
            match HomePresenter.getNextMatch gs with
            | None -> Display.Matches.emptyBanner ()
            | Some m -> Display.Matches.nextMatchBanner m.LocalName m.VisitName m.Date m.LocationTag

        let upcomingFixtures = HomePresenter.getUpcomingFixtures gs 5
        let recentResults = HomePresenter.getRecentResults gs 5
        let userStanding = HomePresenter.getUserStanding gs selectedLeagueId
        let groups = HomePresenter.getGroupedCompetitions gs

        let pos, total, standing =
            match userStanding with
            | Some(p, t, s) -> Some p, Some t, Some s
            | None -> None, None, None

        ScrollViewer.create
            [ ScrollViewer.content (
                  StackPanel.create
                      [ StackPanel.margin (24.0, 24.0)
                        StackPanel.spacing 16.0
                        StackPanel.children
                            [ banner
                              summaryBar userTeam pos total standing
                              Grid.create
                                  [ Grid.columnDefinitions "1.6*, 20, 1*"
                                    Grid.children
                                        [ Grid.create
                                              [ Grid.column 0
                                                Grid.columnDefinitions "190, 12, *"
                                                Grid.children
                                                    [ Border.create
                                                          [ Grid.column 0
                                                            Border.child (
                                                                competitionPicker groups selectedLeagueId onLeagueChange
                                                            ) ]
                                                      StackPanel.create
                                                          [ Grid.column 2
                                                            StackPanel.spacing 16.0
                                                            StackPanel.children
                                                                [ standingsPanel gs selectedLeagueId
                                                                  recentResultsPanel recentResults ] ] ] ]
                                          StackPanel.create
                                              [ Grid.column 2
                                                StackPanel.spacing 16.0
                                                StackPanel.children
                                                    [ upcomingPanel upcomingFixtures gs
                                                      notificationsPanel state.Notifications dispatch ] ] ] ] ] ]
              ) ]
