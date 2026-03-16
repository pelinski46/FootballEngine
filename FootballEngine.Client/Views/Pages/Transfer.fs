namespace FootballEngine.Pages

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Components
open FootballEngine.Domain
open FootballEngine.Icons

module Transfers =

    let private pageSize = 100

    let private positionColor (pos: Position) =
        match pos with
        | GK -> "#f59e0b"
        | DC
        | DL
        | DR
        | WBL
        | WBR -> "#3b82f6"
        | DM
        | MC
        | ML
        | MR -> "#10b981"
        | AMC
        | AML
        | AMR
        | ST -> "#ef4444"

    let private formatValue (v: decimal) =
        if v >= 1_000_000m then $"£{v / 1_000_000m:F1}M"
        elif v >= 1_000m then $"£{v / 1_000m:F0}K"
        else $"£{v:F0}"

    let private positionLabel (f: TransferFilter) =
        match f with
        | AllPositions -> "All"
        | Goalkeepers -> "GK"
        | Defenders -> "DEF"
        | Midfielders -> "MID"
        | Attackers -> "ATT"

    let private tabLabel (t: TransferTab) =
        match t with
        | MarketSearch -> "Market"
        | MyWatchlist -> "Watchlist"
        | IncomingOffers -> "Incoming"
        | OutgoingOffers -> "Outgoing"
        | TransferHistory -> "History"

    let private tabIcon (t: TransferTab) =
        match t with
        | MarketSearch -> UI.search
        | MyWatchlist -> PlayerIcon.skill
        | IncomingOffers -> UI.add
        | OutgoingOffers -> Club.transfer
        | TransferHistory -> PlayerIcon.contract

    let private playerAvatar (name: string) (pos: Position) (size: float) =
        let color = positionColor pos

        Border.create
            [ Border.width size
              Border.height size
              Border.cornerRadius (size / 2.0)
              Border.background (color + "22")
              Border.borderBrush (color + "66")
              Border.borderThickness 1.5
              Border.child (
                  TextBlock.create
                      [ TextBlock.text (string name[0])
                        TextBlock.fontSize (size * 0.4)
                        TextBlock.fontWeight FontWeight.Black
                        TextBlock.foreground color
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.verticalAlignment VerticalAlignment.Center ]
              ) ]

    let private caBar (ca: int) =
        let pct = float ca / 200.0

        let color =
            if ca >= 140 then Theme.Accent
            elif ca >= 110 then Theme.AccentAlt
            elif ca >= 85 then Theme.Warning
            else Theme.TextMuted

        Border.create
            [ Border.cornerRadius 3.0
              Border.background Theme.BgSidebar
              Border.height 4.0
              Border.child (
                  Border.create
                      [ Border.cornerRadius 3.0
                        Border.background color
                        Border.width (pct * 64.0)
                        Border.height 4.0
                        Border.horizontalAlignment HorizontalAlignment.Left ]
              ) ]

    let private playerRow
        (p: Player)
        (clubName: string)
        (isSelected: bool)
        (isWatched: bool)
        (onSelect: unit -> unit)
        (onWatch: unit -> unit)
        =
        let posColor = positionColor p.Position

        Button.create
            [ Button.horizontalAlignment HorizontalAlignment.Stretch
              Button.padding (16.0, 10.0)
              Button.background (if isSelected then Theme.AccentLight else "Transparent")
              Button.borderThickness (Avalonia.Thickness(0.0, 0.0, 0.0, 1.0))
              Button.borderBrush Theme.Border
              Button.onClick (fun _ -> onSelect ())
              Button.content (
                  Grid.create
                      [ Grid.columnDefinitions "Auto, *, Auto, 70, 70, Auto"
                        Grid.children
                            [ Border.create
                                  [ Grid.column 0
                                    Border.margin (0.0, 0.0, 12.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (playerAvatar p.Name p.Position 36.0) ]
                              StackPanel.create
                                  [ Grid.column 1
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.spacing 2.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text p.Name
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.fontSize 13.0
                                                TextBlock.foreground Theme.TextMain ]
                                          TextBlock.create
                                              [ TextBlock.text clubName
                                                TextBlock.fontSize 11.0
                                                TextBlock.foreground Theme.TextMuted ] ] ]
                              Border.create
                                  [ Grid.column 2
                                    Border.margin (0.0, 0.0, 16.0, 0.0)
                                    Border.background (posColor + "22")
                                    Border.borderBrush (posColor + "55")
                                    Border.borderThickness 1.0
                                    Border.cornerRadius 4.0
                                    Border.padding (6.0, 3.0)
                                    Border.verticalAlignment VerticalAlignment.Center
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text $"%A{p.Position}"
                                              TextBlock.fontSize 10.0
                                              TextBlock.fontWeight FontWeight.Black
                                              TextBlock.foreground posColor ]
                                    ) ]
                              StackPanel.create
                                  [ Grid.column 3
                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                    StackPanel.spacing 2.0
                                    StackPanel.children
                                        [ TextBlock.create
                                              [ TextBlock.text (string p.CurrentSkill)
                                                TextBlock.fontSize 13.0
                                                TextBlock.fontWeight FontWeight.Black
                                                TextBlock.foreground Theme.TextMain
                                                TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                                          caBar p.CurrentSkill ] ]
                              TextBlock.create
                                  [ Grid.column 4
                                    TextBlock.text (formatValue p.Value)
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.foreground Theme.TextSub
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.horizontalAlignment HorizontalAlignment.Right ]
                              Button.create
                                  [ Grid.column 5
                                    Button.margin (8.0, 0.0, 0.0, 0.0)
                                    Button.padding (6.0, 4.0)
                                    Button.cornerRadius 6.0
                                    Button.background (if isWatched then Theme.Warning + "22" else "Transparent")
                                    Button.borderBrush (if isWatched then Theme.Warning + "66" else Theme.Border)
                                    Button.borderThickness 1.0
                                    Button.verticalAlignment VerticalAlignment.Center
                                    Button.onClick (fun e ->
                                        e.Handled <- true
                                        onWatch ())
                                    Button.content (
                                        Icons.iconSm
                                            PlayerIcon.skill
                                            (if isWatched then Theme.Warning else Theme.TextMuted)
                                    ) ] ] ]
              ) ]

    let private statBar (label: string) (value: int) (color: string) =
        Grid.create
            [ Grid.columnDefinitions "90, *, 30"
              Grid.margin (0.0, 3.0)
              Grid.children
                  [ TextBlock.create
                        [ Grid.column 0
                          TextBlock.text label
                          TextBlock.fontSize 11.0
                          TextBlock.foreground Theme.TextMuted
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                    Border.create
                        [ Grid.column 1
                          Border.cornerRadius 3.0
                          Border.background Theme.BgSidebar
                          Border.height 6.0
                          Border.verticalAlignment VerticalAlignment.Center
                          Border.margin (0.0, 0.0, 8.0, 0.0)
                          Border.child (
                              Border.create
                                  [ Border.cornerRadius 3.0
                                    Border.background color
                                    Border.width (float value / 20.0 * 120.0)
                                    Border.height 6.0
                                    Border.horizontalAlignment HorizontalAlignment.Left ]
                          ) ]
                    TextBlock.create
                        [ Grid.column 2
                          TextBlock.text (string value)
                          TextBlock.fontSize 11.0
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground Theme.TextMain
                          TextBlock.horizontalAlignment HorizontalAlignment.Right
                          TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

    let private statGroup (title: string) (stats: (string * int) list) (color: string) =
        StackPanel.create
            [ StackPanel.spacing 4.0
              StackPanel.margin (0.0, 0.0, 0.0, 14.0)
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text (title.ToUpperInvariant())
                          TextBlock.fontSize 9.0
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground Theme.TextMuted
                          TextBlock.lineSpacing 1.5
                          TextBlock.margin (0.0, 0.0, 0.0, 4.0) ]
                    for label, value in stats do
                        statBar label value color ] ]

    let private playerDetailPanel
        (p: Player)
        (clubName: string)
        (isWatched: bool)
        (onWatch: unit -> unit)
        (onOffer: unit -> unit)
        =
        let posColor = positionColor p.Position

        StackPanel.create
            [ StackPanel.spacing 0.0
              StackPanel.children
                  [ Border.create
                        [ Border.background Theme.BgCard
                          Border.padding (20.0, 20.0, 20.0, 16.0)
                          Border.borderBrush Theme.Border
                          Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                          Border.child (
                              StackPanel.create
                                  [ StackPanel.spacing 14.0
                                    StackPanel.children
                                        [ Grid.create
                                              [ Grid.columnDefinitions "Auto, *"
                                                Grid.children
                                                    [ Border.create
                                                          [ Grid.column 0
                                                            Border.margin (0.0, 0.0, 14.0, 0.0)
                                                            Border.child (playerAvatar p.Name p.Position 52.0) ]
                                                      StackPanel.create
                                                          [ Grid.column 1
                                                            StackPanel.verticalAlignment VerticalAlignment.Center
                                                            StackPanel.spacing 4.0
                                                            StackPanel.children
                                                                [ TextBlock.create
                                                                      [ TextBlock.text p.Name
                                                                        TextBlock.fontSize 16.0
                                                                        TextBlock.fontWeight FontWeight.Black
                                                                        TextBlock.foreground Theme.TextMain ]
                                                                  StackPanel.create
                                                                      [ StackPanel.orientation Orientation.Horizontal
                                                                        StackPanel.spacing 8.0
                                                                        StackPanel.children
                                                                            [ Border.create
                                                                                  [ Border.background (posColor + "22")
                                                                                    Border.borderBrush (posColor + "55")
                                                                                    Border.borderThickness 1.0
                                                                                    Border.cornerRadius 4.0
                                                                                    Border.padding (6.0, 2.0)
                                                                                    Border.child (
                                                                                        TextBlock.create
                                                                                            [ TextBlock.text
                                                                                                  $"%A{p.Position}"
                                                                                              TextBlock.fontSize 10.0
                                                                                              TextBlock.fontWeight
                                                                                                  FontWeight.Black
                                                                                              TextBlock.foreground
                                                                                                  posColor ]
                                                                                    ) ]
                                                                              TextBlock.create
                                                                                  [ TextBlock.text clubName
                                                                                    TextBlock.fontSize 11.0
                                                                                    TextBlock.foreground Theme.TextMuted ] ] ] ] ] ] ]
                                          Grid.create
                                              [ Grid.columnDefinitions "*, *, *"
                                                Grid.children
                                                    [ StackPanel.create
                                                          [ Grid.column 0
                                                            StackPanel.spacing 2.0
                                                            StackPanel.children
                                                                [ TextBlock.create
                                                                      [ TextBlock.text "CA / PA"
                                                                        TextBlock.fontSize 9.0
                                                                        TextBlock.fontWeight FontWeight.Bold
                                                                        TextBlock.foreground Theme.TextMuted
                                                                        TextBlock.lineSpacing 1.0 ]
                                                                  TextBlock.create
                                                                      [ TextBlock.text
                                                                            $"{p.CurrentSkill} / {p.PotentialSkill}"
                                                                        TextBlock.fontSize 15.0
                                                                        TextBlock.fontWeight FontWeight.Black
                                                                        TextBlock.foreground Theme.Accent ] ] ]
                                                      StackPanel.create
                                                          [ Grid.column 1
                                                            StackPanel.spacing 2.0
                                                            StackPanel.children
                                                                [ TextBlock.create
                                                                      [ TextBlock.text "VALUE"
                                                                        TextBlock.fontSize 9.0
                                                                        TextBlock.fontWeight FontWeight.Bold
                                                                        TextBlock.foreground Theme.TextMuted
                                                                        TextBlock.lineSpacing 1.0 ]
                                                                  TextBlock.create
                                                                      [ TextBlock.text (formatValue p.Value)
                                                                        TextBlock.fontSize 15.0
                                                                        TextBlock.fontWeight FontWeight.Black
                                                                        TextBlock.foreground Theme.TextMain ] ] ]
                                                      StackPanel.create
                                                          [ Grid.column 2
                                                            StackPanel.spacing 2.0
                                                            StackPanel.children
                                                                [ TextBlock.create
                                                                      [ TextBlock.text "WAGE"
                                                                        TextBlock.fontSize 9.0
                                                                        TextBlock.fontWeight FontWeight.Bold
                                                                        TextBlock.foreground Theme.TextMuted
                                                                        TextBlock.lineSpacing 1.0 ]
                                                                  TextBlock.create
                                                                      [ TextBlock.text (formatValue p.Salary)
                                                                        TextBlock.fontSize 15.0
                                                                        TextBlock.fontWeight FontWeight.Black
                                                                        TextBlock.foreground Theme.TextMain ] ] ] ] ] ] ]
                          ) ]
                    ScrollViewer.create
                        [ ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                          ScrollViewer.content (
                              StackPanel.create
                                  [ StackPanel.margin (20.0, 16.0)
                                    StackPanel.spacing 0.0
                                    StackPanel.children
                                        [ statGroup
                                              "Technical"
                                              [ "Finishing", p.Technical.Finishing
                                                "Dribbling", p.Technical.Dribbling
                                                "Passing", p.Technical.Passing
                                                "Ball Control", p.Technical.BallControl
                                                "Tackling", p.Technical.Tackling
                                                "Marking", p.Technical.Marking ]
                                              Theme.AccentAlt
                                          statGroup
                                              "Physical"
                                              [ "Pace", p.Physical.Pace
                                                "Stamina", p.Physical.Stamina
                                                "Strength", p.Physical.Strength
                                                "Agility", p.Physical.Agility ]
                                              Theme.Accent
                                          statGroup
                                              "Mental"
                                              [ "Vision", p.Mental.Vision
                                                "Composure", p.Mental.Composure
                                                "Positioning", p.Mental.Positioning
                                                "Work Rate", p.Mental.WorkRate ]
                                              Theme.Warning
                                          StackPanel.create
                                              [ StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                                                StackPanel.spacing 8.0
                                                StackPanel.children
                                                    [ Button.create
                                                          [ Button.horizontalAlignment HorizontalAlignment.Stretch
                                                            Button.background (
                                                                if isWatched then
                                                                    Theme.Warning + "22"
                                                                else
                                                                    "Transparent"
                                                            )
                                                            Button.borderBrush (
                                                                if isWatched then Theme.Warning else Theme.Border
                                                            )
                                                            Button.borderThickness 1.0
                                                            Button.cornerRadius 8.0
                                                            Button.padding (0.0, 10.0)
                                                            Button.onClick (fun _ -> onWatch ())
                                                            Button.content (
                                                                StackPanel.create
                                                                    [ StackPanel.orientation Orientation.Horizontal
                                                                      StackPanel.spacing 6.0
                                                                      StackPanel.horizontalAlignment
                                                                          HorizontalAlignment.Center
                                                                      StackPanel.children
                                                                          [ Icons.iconSm
                                                                                PlayerIcon.skill
                                                                                (if isWatched then
                                                                                     Theme.Warning
                                                                                 else
                                                                                     Theme.TextMuted)
                                                                            TextBlock.create
                                                                                [ TextBlock.text (
                                                                                      if isWatched then
                                                                                          "On Watchlist"
                                                                                      else
                                                                                          "Add to Watchlist"
                                                                                  )
                                                                                  TextBlock.fontSize 12.0
                                                                                  TextBlock.fontWeight
                                                                                      FontWeight.SemiBold
                                                                                  TextBlock.foreground (
                                                                                      if isWatched then
                                                                                          Theme.Warning
                                                                                      else
                                                                                          Theme.TextMuted
                                                                                  )
                                                                                  TextBlock.verticalAlignment
                                                                                      VerticalAlignment.Center ] ] ]
                                                            ) ]
                                                      UI.primaryButton "Make Offer" (Some Club.transfer) (fun _ ->
                                                          onOffer ()) ] ] ] ]
                          ) ] ] ]

    let private marketTableHeader () =
        Border.create
            [ Border.padding (16.0, 8.0)
              Border.background Theme.BgSidebar
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
              Border.child (
                  Grid.create
                      [ Grid.columnDefinitions "Auto, *, Auto, 70, 70, Auto"
                        Grid.children
                            [ Border.create [ Grid.column 0; Border.width 48.0 ]
                              TextBlock.create
                                  [ Grid.column 1
                                    TextBlock.text "PLAYER"
                                    TextBlock.fontSize 10.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.lineSpacing 1.0 ]
                              TextBlock.create
                                  [ Grid.column 2
                                    TextBlock.text "POS"
                                    TextBlock.fontSize 10.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.lineSpacing 1.0
                                    TextBlock.margin (0.0, 0.0, 16.0, 0.0) ]
                              TextBlock.create
                                  [ Grid.column 3
                                    TextBlock.text "CA"
                                    TextBlock.fontSize 10.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.lineSpacing 1.0
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                              TextBlock.create
                                  [ Grid.column 4
                                    TextBlock.text "VALUE"
                                    TextBlock.fontSize 10.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.lineSpacing 1.0
                                    TextBlock.horizontalAlignment HorizontalAlignment.Right ]
                              Border.create [ Grid.column 5; Border.width 44.0 ] ] ]
              ) ]

    let private emptyState (icon: Material.Icons.MaterialIconKind) (title: string) (sub: string) =
        Border.create
            [ Border.padding (40.0, 60.0)
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 10.0
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children
                            [ Border.create
                                  [ Border.horizontalAlignment HorizontalAlignment.Center
                                    Border.child (Icons.icon icon 40.0 Theme.TextMuted) ]
                              TextBlock.create
                                  [ TextBlock.text title
                                    TextBlock.fontSize 15.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground Theme.TextSub
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ]
                              TextBlock.create
                                  [ TextBlock.text sub
                                    TextBlock.fontSize 12.0
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ] ] ]
              ) ]

    let private loadingState () =
        Border.create
            [ Border.padding (40.0, 80.0)
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 12.0
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children
                            [ Border.create
                                  [ Border.horizontalAlignment HorizontalAlignment.Center
                                    Border.child (Icons.icon UI.refresh 36.0 Theme.Accent) ]
                              TextBlock.create
                                  [ TextBlock.text "Loading transfer market..."
                                    TextBlock.fontSize 14.0
                                    TextBlock.foreground Theme.TextMuted
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center ] ] ]
              ) ]

    let private paginationBar (page: int) (total: int) (dispatch: Msg -> unit) =
        let totalPages = max 1 ((total + pageSize - 1) / pageSize)

        if totalPages <= 1 then
            Border.create [ Border.height 0.0 ] :> IView
        else
            Border.create
                [ Border.background Theme.BgSidebar
                  Border.borderBrush Theme.Border
                  Border.borderThickness (0.0, 1.0, 0.0, 0.0)
                  Border.padding (16.0, 10.0)
                  Border.child (
                      StackPanel.create
                          [ StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 8.0
                            StackPanel.horizontalAlignment HorizontalAlignment.Center
                            StackPanel.children
                                [ Button.create
                                      [ Button.content (Icons.iconSm Nav.prev Theme.TextMuted)
                                        Button.background "Transparent"
                                        Button.borderThickness 0.0
                                        Button.isEnabled (page > 0)
                                        Button.onClick (fun _ -> dispatch (TransferMsg(PageChange(page - 1)))) ]
                                  TextBlock.create
                                      [ TextBlock.text $"{page + 1} / {totalPages}"
                                        TextBlock.fontSize 12.0
                                        TextBlock.foreground Theme.TextMuted
                                        TextBlock.verticalAlignment VerticalAlignment.Center ]
                                  Button.create
                                      [ Button.content (Icons.iconSm Nav.next Theme.TextMuted)
                                        Button.background "Transparent"
                                        Button.borderThickness 0.0
                                        Button.isEnabled (page < totalPages - 1)
                                        Button.onClick (fun _ -> dispatch (TransferMsg(PageChange(page + 1)))) ] ] ]
                  ) ]
            :> IView

    let transfersView (state: State) (dispatch: Msg -> unit) =
        let ts = state.Transfer

        let filteredPlayers =
            ts.CachedPlayers
            |> List.filter (fun p ->
                let matchesSearch =
                    ts.SearchQuery = ""
                    || p.Name.ToLowerInvariant().Contains(ts.SearchQuery.ToLowerInvariant())

                let matchesPos =
                    match ts.PositionFilter with
                    | AllPositions -> true
                    | Goalkeepers -> p.Position = GK
                    | Defenders ->
                        match p.Position with
                        | DC
                        | DL
                        | DR
                        | WBL
                        | WBR -> true
                        | _ -> false
                    | Midfielders ->
                        match p.Position with
                        | DM
                        | MC
                        | ML
                        | MR
                        | AMC
                        | AML
                        | AMR -> true
                        | _ -> false
                    | Attackers -> p.Position = ST

                matchesSearch && matchesPos)
            |> List.sortWith (fun a b ->
                let cmp =
                    match ts.SortBy with
                    | ByName -> compare a.Name b.Name
                    | ByCA -> compare a.CurrentSkill b.CurrentSkill
                    | ByValue -> compare a.Value b.Value
                    | ByAge -> compare a.Birthday b.Birthday
                    | ByPosition -> compare (sprintf "%A" a.Position) (sprintf "%A" b.Position)

                if ts.SortAsc then cmp else -cmp)

        let pagedPlayers =
            filteredPlayers |> List.skip (ts.Page * pageSize) |> List.truncate pageSize

        let getClubName (pid: PlayerId) =
            ts.ClubNameCache |> Map.tryFind pid |> Option.defaultValue "Unknown"

        let userBudget =
            state.GameState.Clubs
            |> Map.tryFind state.GameState.UserClubId
            |> Option.map _.Budget
            |> Option.defaultValue 0m

        let topBar =
            Border.create
                [ Border.background Theme.BgSidebar
                  Border.borderBrush Theme.Border
                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                  Border.padding (20.0, 14.0)
                  Border.child (
                      DockPanel.create
                          [ DockPanel.children
                                [ StackPanel.create
                                      [ DockPanel.dock Dock.Right
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 6.0
                                        StackPanel.verticalAlignment VerticalAlignment.Center
                                        StackPanel.children
                                            [ Icons.iconSm Club.finances Theme.Accent
                                              TextBlock.create
                                                  [ TextBlock.text "BUDGET"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.Bold
                                                    TextBlock.foreground Theme.TextMuted
                                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                                              TextBlock.create
                                                  [ TextBlock.text (formatValue userBudget)
                                                    TextBlock.fontSize 14.0
                                                    TextBlock.fontWeight FontWeight.Black
                                                    TextBlock.foreground Theme.Accent
                                                    TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                  StackPanel.create
                                      [ StackPanel.spacing 4.0
                                        StackPanel.children
                                            [ TextBlock.create
                                                  [ TextBlock.text "TRANSFERS"
                                                    TextBlock.fontSize 18.0
                                                    TextBlock.fontWeight FontWeight.Black
                                                    TextBlock.foreground Theme.TextMain
                                                    TextBlock.lineSpacing 2.0 ]
                                              TextBlock.create
                                                  [ TextBlock.text "Recruit, negotiate, and build your squad"
                                                    TextBlock.fontSize 11.0
                                                    TextBlock.foreground Theme.TextMuted ] ] ] ] ]
                  ) ]

        let tabBar =
            Border.create
                [ Border.background Theme.BgCard
                  Border.borderBrush Theme.Border
                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                  Border.padding (20.0, 0.0)
                  Border.child (
                      StackPanel.create
                          [ StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 2.0
                            StackPanel.children
                                [ for tab in
                                      [ MarketSearch; MyWatchlist; IncomingOffers; OutgoingOffers; TransferHistory ] do
                                      let isActive = ts.ActiveTab = tab

                                      Button.create
                                          [ Button.padding (14.0, 12.0)
                                            Button.background "Transparent"
                                            Button.borderThickness (
                                                Avalonia.Thickness(0.0, 0.0, 0.0, if isActive then 2.0 else 0.0)
                                            )
                                            Button.borderBrush (if isActive then Theme.Accent else "Transparent")
                                            Button.cornerRadius 0.0
                                            Button.onClick (fun _ -> dispatch (TransferMsg(TabChange tab)))
                                            Button.content (
                                                StackPanel.create
                                                    [ StackPanel.orientation Orientation.Horizontal
                                                      StackPanel.spacing 6.0
                                                      StackPanel.children
                                                          [ Icons.iconSm
                                                                (tabIcon tab)
                                                                (if isActive then Theme.Accent else Theme.TextMuted)
                                                            TextBlock.create
                                                                [ TextBlock.text (tabLabel tab)
                                                                  TextBlock.fontSize 12.0
                                                                  TextBlock.fontWeight (
                                                                      if isActive then
                                                                          FontWeight.SemiBold
                                                                      else
                                                                          FontWeight.Normal
                                                                  )
                                                                  TextBlock.foreground (
                                                                      if isActive then Theme.Accent else Theme.TextMuted
                                                                  )
                                                                  TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                            ) ] ] ]
                  ) ]

        let searchAndFilters =
            Border.create
                [ Border.background Theme.BgSidebar
                  Border.borderBrush Theme.Border
                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                  Border.padding (16.0, 10.0)
                  Border.child (
                      Grid.create
                          [ Grid.columnDefinitions "*, Auto"
                            Grid.children
                                [ Border.create
                                      [ Grid.column 0
                                        Border.background Theme.BgCard
                                        Border.borderBrush Theme.Border
                                        Border.borderThickness 1.0
                                        Border.cornerRadius 8.0
                                        Border.margin (0.0, 0.0, 12.0, 0.0)
                                        Border.child (
                                            Grid.create
                                                [ Grid.columnDefinitions "Auto, *"
                                                  Grid.children
                                                      [ Border.create
                                                            [ Grid.column 0
                                                              Border.padding (10.0, 0.0, 4.0, 0.0)
                                                              Border.verticalAlignment VerticalAlignment.Center
                                                              Border.child (Icons.iconSm UI.search Theme.TextMuted) ]
                                                        TextBox.create
                                                            [ Grid.column 1
                                                              TextBox.text ts.SearchQuery
                                                              TextBox.onTextChanged (fun q ->
                                                                  dispatch (TransferMsg(Search q)))
                                                              TextBox.watermark "Search players..."
                                                              TextBox.background "Transparent"
                                                              TextBox.borderThickness 0.0
                                                              TextBox.padding (4.0, 10.0)
                                                              TextBox.fontSize 13.0 ] ] ]
                                        ) ]
                                  StackPanel.create
                                      [ Grid.column 1
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 6.0
                                        StackPanel.children
                                            [ for f in [ AllPositions; Goalkeepers; Defenders; Midfielders; Attackers ] do
                                                  let isActive = ts.PositionFilter = f

                                                  Button.create
                                                      [ Button.padding (10.0, 6.0)
                                                        Button.cornerRadius 6.0
                                                        Button.background (
                                                            if isActive then Theme.AccentLight else "Transparent"
                                                        )
                                                        Button.borderBrush (
                                                            if isActive then Theme.Accent else Theme.Border
                                                        )
                                                        Button.borderThickness 1.0
                                                        Button.onClick (fun _ -> dispatch (TransferMsg(FilterChange f)))
                                                        Button.content (
                                                            TextBlock.create
                                                                [ TextBlock.text (positionLabel f)
                                                                  TextBlock.fontSize 11.0
                                                                  TextBlock.fontWeight FontWeight.SemiBold
                                                                  TextBlock.foreground (
                                                                      if isActive then Theme.Accent else Theme.TextMuted
                                                                  ) ]
                                                        ) ] ] ] ] ]
                  ) ]

        let playerListPanel =
            if ts.IsLoading then
                loadingState () :> IView
            else
                StackPanel.create
                    [ StackPanel.children
                          [ marketTableHeader ()
                            if pagedPlayers.IsEmpty then
                                emptyState UI.search "No players found" "Try adjusting your search or filters"
                            else
                                ScrollViewer.create
                                    [ ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                      ScrollViewer.content (
                                          StackPanel.create
                                              [ StackPanel.children
                                                    [ for p in pagedPlayers do
                                                          playerRow
                                                              p
                                                              (getClubName p.Id)
                                                              (ts.SelectedPlayerId = Some p.Id)
                                                              (List.contains p.Id ts.WatchlistIds)
                                                              (fun () -> dispatch (TransferMsg(PlayerSelect p.Id)))
                                                              (fun () -> dispatch (TransferMsg(WatchToggle p.Id))) ] ]
                                      ) ]
                            paginationBar ts.Page filteredPlayers.Length dispatch ] ]
                :> IView

        let detailPanel =
            match ts.SelectedPlayerId with
            | None -> emptyState PlayerIcon.position "No player selected" "Click a player to view details" :> IView
            | Some pid ->
                match ts.CachedPlayers |> List.tryFind (fun p -> p.Id = pid) with
                | None -> emptyState UI.error "Player not found" "" :> IView
                | Some p ->
                    playerDetailPanel
                        p
                        (getClubName pid)
                        (List.contains pid ts.WatchlistIds)
                        (fun () -> dispatch (TransferMsg(WatchToggle pid)))
                        (fun () -> ())
                    :> IView

        let marketContent =
            Grid.create
                [ Grid.columnDefinitions "*, 280"
                  Grid.children
                      [ Border.create
                            [ Grid.column 0
                              Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 0.0, 1.0, 0.0)
                              Border.child playerListPanel ]
                        Border.create [ Grid.column 1; Border.child detailPanel ] ] ]

        let watchlistContent =
            match ts.WatchlistIds with
            | [] ->
                emptyState PlayerIcon.skill "Your watchlist is empty" "Star players in the market to track them here"
                :> IView
            | ids ->
                let watched = ts.CachedPlayers |> List.filter (fun p -> List.contains p.Id ids)

                StackPanel.create
                    [ StackPanel.children
                          [ marketTableHeader ()
                            ScrollViewer.create
                                [ ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                                  ScrollViewer.content (
                                      StackPanel.create
                                          [ StackPanel.children
                                                [ for p in watched do
                                                      playerRow
                                                          p
                                                          (getClubName p.Id)
                                                          (ts.SelectedPlayerId = Some p.Id)
                                                          true
                                                          (fun () -> dispatch (TransferMsg(PlayerSelect p.Id)))
                                                          (fun () -> dispatch (TransferMsg(WatchToggle p.Id))) ] ]
                                  ) ] ] ]
                :> IView

        let tabContent =
            match ts.ActiveTab with
            | MarketSearch -> marketContent :> IView
            | MyWatchlist -> watchlistContent
            | IncomingOffers ->
                emptyState UI.add "No incoming offers" "Other clubs haven't made any offers yet" :> IView
            | OutgoingOffers ->
                emptyState Club.transfer "No outgoing offers" "Make an offer from the market to see it here" :> IView
            | TransferHistory ->
                emptyState PlayerIcon.contract "No transfer history" "Completed deals will appear here" :> IView

        Grid.create
            [ Grid.background Theme.BgMain
              Grid.rowDefinitions "Auto, Auto, Auto, *"
              Grid.children
                  [ Border.create [ Grid.row 0; Border.child topBar ]
                    Border.create [ Grid.row 1; Border.child tabBar ]
                    Border.create
                        [ Grid.row 2
                          Border.isVisible (ts.ActiveTab = MarketSearch || ts.ActiveTab = MyWatchlist)
                          Border.child searchAndFilters ]
                    Border.create [ Grid.row 3; Border.child tabContent ] ] ]
