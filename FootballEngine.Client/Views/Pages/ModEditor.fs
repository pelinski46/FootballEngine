namespace FootballEngine.Pages

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open FootballEngine
open FootballEngine.AppMsgs
open FootballEngine.AppTypes
open FootballEngine.Data
open FootballEngine.ModEditorTypes
open FootballEngine.Icons
open FootballEngine.Components


module ModEditor =

    // ─── Helpers ──────────────────────────────────────────────────────────────

    /// Label + input stacked with consistent spacing — replaces the scattered
    /// TextBlock / TextBox pairs throughout the original.
    let private field (label: string) (input: IView) : IView =
        StackPanel.create
            [ StackPanel.spacing 4.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text (label.ToUpperInvariant())
                          TextBlock.fontSize 10.0
                          TextBlock.fontWeight FontWeight.SemiBold
                          TextBlock.foreground Theme.TextSub ]
                    input ] ]

    /// Two-column field row using a Grid.
    let private fieldRow (left: IView) (right: IView) : IView =
        Grid.create
            [ Grid.columnDefinitions "*, 16, *"
              Grid.children
                  [ Border.create [ Grid.column 0; Border.child left ]
                    Border.create [ Grid.column 2; Border.child right ] ] ]

    /// Destructive action — outlined red button, smaller than primary.
    let private dangerButton (label: string) onClick =
        Button.create
            [ Button.content label
              Button.foreground Theme.Danger
              Button.background "Transparent"
              Button.borderBrush (Theme.Danger + "55")
              Button.borderThickness 1.0
              Button.padding (12.0, 6.0)
              Button.cornerRadius 6.0
              Button.fontSize 12.0
              Button.onClick onClick ]

    /// Stat pill shown in the Overview header.
    let private statPill (label: string) (value: string) =
        Border.create
            [ Border.background Theme.BgCard
              Border.borderBrush Theme.Border
              Border.borderThickness 1.0
              Border.cornerRadius 8.0
              Border.padding (16.0, 10.0)
              Border.margin (0.0, 0.0, 12.0, 0.0)
              Border.child (
                  StackPanel.create
                      [ StackPanel.spacing 2.0
                        StackPanel.children
                            [ TextBlock.create
                                  [ TextBlock.text value
                                    TextBlock.fontSize 22.0
                                    TextBlock.fontWeight FontWeight.Black
                                    TextBlock.foreground Theme.Accent ]
                              TextBlock.create
                                  [ TextBlock.text (label.ToUpperInvariant())
                                    TextBlock.fontSize 10.0
                                    TextBlock.foreground Theme.TextSub ] ] ]
              ) ]

    // ─── Tab bar ──────────────────────────────────────────────────────────────

    let private topTab activeTab tab label dispatch =
        let isActive = activeTab = tab

        Button.create
            [ Button.onClick (fun _ -> dispatch (ModEditorMsg(SetTab tab)))
              Button.background "Transparent"
              Button.foreground (if isActive then Theme.Accent else Theme.TextSub)
              Button.borderBrush (if isActive then Theme.Accent else "Transparent")
              Button.borderThickness (0.0, 0.0, 0.0, 2.0)
              Button.padding (16.0, 10.0)
              Button.content (
                  TextBlock.create
                      [ TextBlock.text label
                        TextBlock.fontSize 13.0
                        TextBlock.fontWeight (if isActive then FontWeight.SemiBold else FontWeight.Normal) ]
              ) ]

    // ─── Overview tab ─────────────────────────────────────────────────────────

    let private overviewTab (state: ModEditorState) dispatch =
        let totalClubs =
            state.Countries |> Map.toSeq |> Seq.sumBy (fun (_, c) -> c.Clubs |> List.length)

        let totalLeagues =
            state.Countries
            |> Map.toSeq
            |> Seq.sumBy (fun (_, c) -> c.LeagueNames |> List.length)

        StackPanel.create
            [ StackPanel.spacing 24.0
              StackPanel.children
                  [

                    // ── Stats strip ──
                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.children
                              [ statPill "Countries" (string (state.Countries |> Map.count))
                                statPill "Leagues" (string totalLeagues)
                                statPill "Clubs" (string totalClubs) ] ]

                    // ── Manifest form ──
                    UI.sectionContainer
                        "Mod Manifest"
                        (StackPanel.create
                            [ StackPanel.spacing 16.0
                              StackPanel.margin (Thickness 16.0)
                              StackPanel.children
                                  [ fieldRow
                                        (field
                                            "Name"
                                            (TextBox.create
                                                [ TextBox.text state.Manifest.Name
                                                  TextBox.onTextChanged (fun t ->
                                                      dispatch (
                                                          ModEditorMsg(UpdateManifest(fun m -> { m with Name = t }))
                                                      )) ]))
                                        (field
                                            "Author"
                                            (TextBox.create
                                                [ TextBox.text state.Manifest.Author
                                                  TextBox.onTextChanged (fun t ->
                                                      dispatch (
                                                          ModEditorMsg(UpdateManifest(fun m -> { m with Author = t }))
                                                      )) ]))
                                    fieldRow
                                        (field
                                            "Version"
                                            (TextBox.create
                                                [ TextBox.text state.Manifest.Version
                                                  TextBox.onTextChanged (fun t ->
                                                      dispatch (
                                                          ModEditorMsg(UpdateManifest(fun m -> { m with Version = t }))
                                                      )) ]))
                                        (field
                                            "Merge Strategy"
                                            (ComboBox.create
                                                [ ComboBox.dataItems [ "Replace"; "Append"; "Patch" ]
                                                  ComboBox.selectedItem (
                                                      state.Manifest.MergeStrategy |> Option.defaultValue "Append"
                                                  )
                                                  ComboBox.onSelectedItemChanged (fun obj ->
                                                      match obj with
                                                      | :? string as s ->
                                                          dispatch (
                                                              ModEditorMsg(
                                                                  UpdateManifest(fun m ->
                                                                      { m with MergeStrategy = Some s })
                                                              )
                                                          )
                                                      | _ -> ()) ]))
                                    field
                                        "Description"
                                        (TextBox.create
                                            [ TextBox.text state.Manifest.Description
                                              TextBox.acceptsReturn true
                                              TextBox.height 80.0
                                              TextBox.onTextChanged (fun t ->
                                                  dispatch (
                                                      ModEditorMsg(UpdateManifest(fun m -> { m with Description = t }))
                                                  )) ]) ] ]) ] ]

    // ─── Country sidebar ──────────────────────────────────────────────────────

    let private countrySidebar (state: ModEditorState) dispatch =
        let filtered =
            state.Countries
            |> Map.toList
            |> List.map snd
            |> List.filter (fun c ->
                state.SearchQuery = ""
                || c.Country.Name.ToLower().Contains(state.SearchQuery.ToLower())
                || c.Country.Code.ToLower().Contains(state.SearchQuery.ToLower()))

        Border.create
            [ Border.width 220.0
              Border.borderBrush Theme.Border
              Border.borderThickness (0.0, 0.0, 1.0, 0.0)
              Border.child (
                  DockPanel.create
                      [ DockPanel.children
                            [

                              // ── Search + Add ──
                              StackPanel.create
                                  [ DockPanel.dock Dock.Top
                                    StackPanel.margin (12.0, 12.0, 12.0, 8.0)
                                    StackPanel.spacing 8.0
                                    StackPanel.children
                                        [ TextBox.create
                                              [ TextBox.watermark "Search countries…"
                                                TextBox.text state.SearchQuery
                                                TextBox.onTextChanged (fun t -> dispatch (ModEditorMsg(SearchMod t))) ]
                                          UI.primaryButton "Add Country" (Some IconName.add) (fun _ ->
                                              let newCountry = UpdateModEditor.createEmptyCountry "NEW" "New Country"
                                              dispatch (ModEditorMsg(AddCountry newCountry))) ] ]

                              // ── Count label ──
                              Border.create
                                  [ DockPanel.dock Dock.Top
                                    Border.padding (12.0, 4.0, 12.0, 8.0)
                                    Border.child (
                                        TextBlock.create
                                            [ TextBlock.text $"%d{filtered.Length} countries"
                                              TextBlock.fontSize 10.0
                                              TextBlock.foreground Theme.TextSub ]
                                    ) ]

                              // ── List ──
                              ListBox.create
                                  [ ListBox.background "Transparent"
                                    ListBox.dataItems filtered
                                    ListBox.selectedItem (
                                        state.SelectedCountryCode
                                        |> Option.bind (fun code -> state.Countries |> Map.tryFind code)
                                    )
                                    ListBox.onSelectedItemChanged (fun item ->
                                        match item with
                                        | :? CountryDataDto as c ->
                                            dispatch (ModEditorMsg(SelectCountry(Some c.Country.Code)))
                                        | _ -> ())
                                    ListBox.itemTemplate (
                                        DataTemplateView<CountryDataDto>.create (fun c ->
                                            StackPanel.create
                                                [ StackPanel.orientation Orientation.Horizontal
                                                  StackPanel.spacing 10.0
                                                  StackPanel.margin (4.0, 2.0)
                                                  StackPanel.children
                                                      [ Border.create
                                                            [ Border.background Theme.AccentLight
                                                              Border.cornerRadius 4.0
                                                              Border.padding (5.0, 2.0)
                                                              Border.child (
                                                                  TextBlock.create
                                                                      [ TextBlock.text c.Country.Code
                                                                        TextBlock.fontSize 10.0
                                                                        TextBlock.fontWeight FontWeight.Bold
                                                                        TextBlock.foreground Theme.Accent ]
                                                              ) ]
                                                        TextBlock.create
                                                            [ TextBlock.text c.Country.Name
                                                              TextBlock.fontSize 13.0
                                                              TextBlock.verticalAlignment VerticalAlignment.Center ] ] ])
                                    ) ] ] ]
              ) ]

    // ─── Country detail sub-tabs ──────────────────────────────────────────────

    let private generalSubTab (state: ModEditorState) (country: CountryDataDto) dispatch =
        UI.sectionContainer
            "Country Metadata"
            (StackPanel.create
                [ StackPanel.spacing 16.0
                  StackPanel.margin (Thickness 16.0)
                  StackPanel.children
                      [ fieldRow
                            (field
                                "Name"
                                (TextBox.create
                                    [ TextBox.text country.Country.Name
                                      TextBox.onTextChanged (fun t ->
                                          dispatch (
                                              ModEditorMsg(
                                                  UpdateCountry(
                                                      country.Country.Code,
                                                      fun c ->
                                                          { c with
                                                              Country = { c.Country with Name = t } }
                                                  )
                                              )
                                          )) ]))
                            (field
                                "Code (3 letters)"
                                (TextBox.create
                                    [ TextBox.text country.Country.Code
                                      TextBox.maxLength 3
                                      TextBox.onTextChanged (fun t ->
                                          if t.Length <= 3 then
                                              dispatch (
                                                  ModEditorMsg(
                                                      UpdateCountry(
                                                          country.Country.Code,
                                                          fun c ->
                                                              { c with
                                                                  Country = { c.Country with Code = t } }
                                                      )
                                                  )
                                              )) ]))
                        field
                            "Confederation"
                            (TextBox.create
                                [ TextBox.text country.Country.Confederation
                                  TextBox.maxWidth 240.0
                                  TextBox.horizontalAlignment HorizontalAlignment.Left
                                  TextBox.onTextChanged (fun t ->
                                      dispatch (
                                          ModEditorMsg(
                                              UpdateCountry(
                                                  country.Country.Code,
                                                  fun c ->
                                                      { c with
                                                          Country = { c.Country with Confederation = t } }
                                              )
                                          )
                                      )) ])
                        Border.create
                            [ Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 1.0, 0.0, 0.0)
                              Border.padding (0.0, 12.0, 0.0, 0.0)
                              Border.child (
                                  dangerButton "Delete Country" (fun _ ->
                                      dispatch (ModEditorMsg(RemoveCountry country.Country.Code)))
                              ) ] ] ])

    let private leaguesSubTab (state: ModEditorState) (country: CountryDataDto) dispatch =
        let updateCountry f =
            dispatch (ModEditorMsg(UpdateCountry(country.Country.Code, f)))

        UI.sectionContainer
            "Leagues"
            (StackPanel.create
                [ StackPanel.spacing 0.0
                  StackPanel.children
                      [ for i in 0 .. country.LeagueNames.Length - 1 do
                            let name = country.LeagueNames.[i]
                            let rules = country.LeagueRules.[i]

                            Border.create
                                [ Border.borderBrush Theme.Border
                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                  Border.padding (16.0, 14.0)
                                  Border.child (
                                      StackPanel.create
                                          [ StackPanel.spacing 12.0
                                            StackPanel.children
                                                [

                                                  // ── Name row ──
                                                  DockPanel.create
                                                      [ DockPanel.children
                                                            [ dangerButton "Remove" (fun _ ->
                                                                  updateCountry (fun c ->
                                                                      { c with
                                                                          LeagueNames =
                                                                              c.LeagueNames
                                                                              |> List.indexed
                                                                              |> List.filter (fun (j, _) -> j <> i)
                                                                              |> List.map snd
                                                                          LeagueRules =
                                                                              c.LeagueRules
                                                                              |> List.indexed
                                                                              |> List.filter (fun (j, _) -> j <> i)
                                                                              |> List.map snd }))
                                                              |> fun v ->
                                                                  DockPanel.dock Dock.Right |> ignore
                                                                  v

                                                              TextBox.create
                                                                  [ TextBox.text name
                                                                    TextBox.fontWeight FontWeight.SemiBold
                                                                    TextBox.fontSize 14.0
                                                                    TextBox.onTextChanged (fun t ->
                                                                        updateCountry (fun c ->
                                                                            { c with
                                                                                LeagueNames =
                                                                                    c.LeagueNames
                                                                                    |> List.mapi (fun j n ->
                                                                                        if i = j then t else n) })) ] ] ]

                                                  // ── Points row ──
                                                  StackPanel.create
                                                      [ StackPanel.orientation Orientation.Horizontal
                                                        StackPanel.spacing 16.0
                                                        StackPanel.children
                                                            [ field
                                                                  "Points for Win"
                                                                  (NumericUpDown.create
                                                                      [ NumericUpDown.value (decimal rules.PointsForWin)
                                                                        NumericUpDown.onValueChanged (fun v ->
                                                                            let pts =
                                                                                v
                                                                                |> Option.ofNullable
                                                                                |> Option.map int
                                                                                |> Option.defaultValue 0

                                                                            updateCountry (fun c ->
                                                                                { c with
                                                                                    LeagueRules =
                                                                                        c.LeagueRules
                                                                                        |> List.mapi (fun j r ->
                                                                                            if i = j then
                                                                                                { r with
                                                                                                    PointsForWin =
                                                                                                        pts }
                                                                                            else
                                                                                                r) })) ])
                                                              field
                                                                  "Points for Draw"
                                                                  (NumericUpDown.create
                                                                      [ NumericUpDown.value (
                                                                            decimal rules.PointsForDraw
                                                                        )
                                                                        NumericUpDown.onValueChanged (fun v ->
                                                                            let pts =
                                                                                v
                                                                                |> Option.ofNullable
                                                                                |> Option.map int
                                                                                |> Option.defaultValue 0

                                                                            updateCountry (fun c ->
                                                                                { c with
                                                                                    LeagueRules =
                                                                                        c.LeagueRules
                                                                                        |> List.mapi (fun j r ->
                                                                                            if i = j then
                                                                                                { r with
                                                                                                    PointsForDraw =
                                                                                                        pts }
                                                                                            else
                                                                                                r) })) ]) ] ] ] ]
                                  ) ]

                        Border.create
                            [ Border.padding (16.0, 12.0)
                              Border.child (
                                  UI.primaryButton "Add League" (Some IconName.add) (fun _ ->
                                      updateCountry (fun c ->
                                          { c with
                                              LeagueNames = c.LeagueNames @ [ "New League" ]
                                              LeagueRules =
                                                  c.LeagueRules @ [ UpdateModEditor.createEmptyLeague "New League" ] }))
                              ) ] ] ])

    let private clubsSubTab (state: ModEditorState) (country: CountryDataDto) dispatch =
        let updateCountry f =
            dispatch (ModEditorMsg(UpdateCountry(country.Country.Code, f)))

        UI.sectionContainer
            "Clubs"
            (StackPanel.create
                [ StackPanel.spacing 0.0
                  StackPanel.children
                      [

                        // ── Column headers ──
                        Border.create
                            [ Border.background Theme.BgMain
                              Border.padding (12.0, 8.0)
                              Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                              Border.child (
                                  Grid.create
                                      [ Grid.columnDefinitions "*, 80, 120, 72"
                                        Grid.children
                                            [ TextBlock.create
                                                  [ Grid.column 0
                                                    TextBlock.text "CLUB NAME"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ]
                                              TextBlock.create
                                                  [ Grid.column 1
                                                    TextBlock.text "DIVISION"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ]
                                              TextBlock.create
                                                  [ Grid.column 2
                                                    TextBlock.text "REPUTATION"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ] ] ]
                              ) ]

                        // ── Rows ──
                        for i in 0 .. country.Clubs.Length - 1 do
                            let club = country.Clubs.[i]

                            Border.create
                                [ Border.borderBrush Theme.Border
                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                  Border.padding (12.0, 6.0)
                                  Border.child (
                                      Grid.create
                                          [ Grid.columnDefinitions "*, 80, 120, 72"
                                            Grid.children
                                                [ TextBox.create
                                                      [ Grid.column 0
                                                        TextBox.text club.Name
                                                        TextBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        TextBox.onTextChanged (fun t ->
                                                            updateCountry (fun c ->
                                                                { c with
                                                                    Clubs =
                                                                        c.Clubs
                                                                        |> List.mapi (fun j cl ->
                                                                            if i = j then
                                                                                { cl with Name = t }
                                                                            else
                                                                                cl) })) ]
                                                  NumericUpDown.create
                                                      [ Grid.column 1
                                                        NumericUpDown.value (decimal club.LeagueLevel)
                                                        NumericUpDown.margin (0.0, 0.0, 8.0, 0.0)
                                                        NumericUpDown.onValueChanged (fun v ->
                                                            let level =
                                                                v
                                                                |> Option.ofNullable
                                                                |> Option.map int
                                                                |> Option.defaultValue 0

                                                            updateCountry (fun c ->
                                                                { c with
                                                                    Clubs =
                                                                        c.Clubs
                                                                        |> List.mapi (fun j cl ->
                                                                            if i = j then
                                                                                { cl with LeagueLevel = level }
                                                                            else
                                                                                cl) })) ]
                                                  NumericUpDown.create
                                                      [ Grid.column 2
                                                        NumericUpDown.value (
                                                            decimal (club.Reputation |> Option.defaultValue 5000)
                                                        )
                                                        NumericUpDown.margin (0.0, 0.0, 8.0, 0.0)
                                                        NumericUpDown.onValueChanged (fun v ->
                                                            let rep =
                                                                v
                                                                |> Option.ofNullable
                                                                |> Option.map int
                                                                |> Option.defaultValue 5000

                                                            updateCountry (fun c ->
                                                                { c with
                                                                    Clubs =
                                                                        c.Clubs
                                                                        |> List.mapi (fun j cl ->
                                                                            if i = j then
                                                                                { cl with Reputation = Some rep }
                                                                            else
                                                                                cl) })) ]
                                                  Border.create
                                                      [ Grid.column 3
                                                        Border.child (
                                                            dangerButton "✕" (fun _ ->
                                                                updateCountry (fun c ->
                                                                    { c with
                                                                        Clubs =
                                                                            c.Clubs
                                                                            |> List.indexed
                                                                            |> List.filter (fun (j, _) -> j <> i)
                                                                            |> List.map snd }))
                                                        ) ] ] ]
                                  ) ]

                        Border.create
                            [ Border.padding (12.0, 12.0)
                              Border.child (
                                  UI.primaryButton "Add Club" (Some IconName.add) (fun _ ->
                                      updateCountry (fun c ->
                                          { c with
                                              Clubs = c.Clubs @ [ UpdateModEditor.createEmptyClub "New Club" 0 ] }))
                              ) ] ] ])

    let private cupsSubTab (state: ModEditorState) (country: CountryDataDto) dispatch =
        let updateCup i f =
            dispatch (ModEditorMsg(UpdateCup(country.Country.Code, i, f)))

        UI.sectionContainer
            "Cups"
            (StackPanel.create
                [ StackPanel.spacing 0.0
                  StackPanel.children
                      [ // Column headers
                        Border.create
                            [ Border.background Theme.BgMain
                              Border.padding (12.0, 8.0)
                              Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                              Border.child (
                                  Grid.create
                                      [ Grid.columnDefinitions "*, 120, 72"
                                        Grid.children
                                            [ TextBlock.create
                                                  [ Grid.column 0
                                                    TextBlock.text "CUP NAME"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ]
                                              TextBlock.create
                                                  [ Grid.column 1
                                                    TextBlock.text "FORMAT"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ] ] ]
                              ) ]

                        for i in 0 .. country.Cups.Length - 1 do
                            let cup = country.Cups.[i]

                            Border.create
                                [ Border.borderBrush Theme.Border
                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                  Border.padding (12.0, 6.0)
                                  Border.child (
                                      Grid.create
                                          [ Grid.columnDefinitions "*, 120, 72"
                                            Grid.children
                                                [ TextBox.create
                                                      [ Grid.column 0
                                                        TextBox.text cup.Name
                                                        TextBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        TextBox.onTextChanged (fun t ->
                                                            updateCup i (fun c -> { c with Name = t })) ]
                                                  ComboBox.create
                                                      [ Grid.column 1
                                                        ComboBox.dataItems [ "Knockout"; "GroupsKnockout" ]
                                                        ComboBox.selectedItem cup.Format
                                                        ComboBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        ComboBox.onSelectedItemChanged (fun obj ->
                                                            match obj with
                                                            | :? string as s ->
                                                                updateCup i (fun c -> { c with Format = s })
                                                            | _ -> ()) ]
                                                  Border.create
                                                      [ Grid.column 2
                                                        Border.child (
                                                            dangerButton "✕" (fun _ ->
                                                                dispatch (
                                                                    ModEditorMsg(RemoveCup(country.Country.Code, i))
                                                                ))
                                                        ) ] ] ]
                                  ) ]

                        Border.create
                            [ Border.padding (12.0, 12.0)
                              Border.child (
                                  UI.primaryButton "Add Cup" (Some IconName.add) (fun _ ->
                                      dispatch (
                                          ModEditorMsg(
                                              AddCup(
                                                  country.Country.Code,
                                                  { Name = "New Cup"
                                                    Format = "Knockout" }
                                              )
                                          )
                                      ))
                              ) ] ] ])

    let private namePoolSubTab (country: CountryDataDto) dispatch =
        UI.sectionContainer
            "Name Pool"
            (Grid.create
                [ Grid.columnDefinitions "*, 1, *"
                  Grid.children
                      [ StackPanel.create
                            [ Grid.column 0
                              StackPanel.spacing 8.0
                              StackPanel.margin (Thickness 16.0)
                              StackPanel.children
                                  [ StackPanel.create
                                        [ StackPanel.orientation Orientation.Horizontal
                                          StackPanel.spacing 8.0
                                          StackPanel.children
                                              [ TextBlock.create
                                                    [ TextBlock.text "FIRST NAMES"
                                                      TextBlock.fontSize 10.0
                                                      TextBlock.fontWeight FontWeight.SemiBold
                                                      TextBlock.foreground Theme.TextSub
                                                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                                                UI.countBadge country.Names.FirstNames.Length ] ]
                                    TextBox.create
                                        [ TextBox.acceptsReturn true
                                          TextBox.height 400.0
                                          TextBox.text (String.concat "\n" country.Names.FirstNames)
                                          TextBox.onTextChanged (fun text ->
                                              let names =
                                                  text.Split(
                                                      [| '\n'; '\r' |],
                                                      System.StringSplitOptions.RemoveEmptyEntries
                                                  )
                                                  |> Array.toList

                                              dispatch (
                                                  ModEditorMsg(
                                                      UpdateCountry(
                                                          country.Country.Code,
                                                          fun c ->
                                                              { c with
                                                                  Names = { c.Names with FirstNames = names } }
                                                      )
                                                  )
                                              )) ] ] ]

                        Border.create [ Grid.column 1; Border.background Theme.Border; Border.width 1.0 ]

                        StackPanel.create
                            [ Grid.column 2
                              StackPanel.spacing 8.0
                              StackPanel.margin (Thickness 16.0)
                              StackPanel.children
                                  [ StackPanel.create
                                        [ StackPanel.orientation Orientation.Horizontal
                                          StackPanel.spacing 8.0
                                          StackPanel.children
                                              [ TextBlock.create
                                                    [ TextBlock.text "LAST NAMES"
                                                      TextBlock.fontSize 10.0
                                                      TextBlock.fontWeight FontWeight.SemiBold
                                                      TextBlock.foreground Theme.TextSub
                                                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                                                UI.countBadge country.Names.LastNames.Length ] ]
                                    TextBox.create
                                        [ TextBox.acceptsReturn true
                                          TextBox.height 400.0
                                          TextBox.text (String.concat "\n" country.Names.LastNames)
                                          TextBox.onTextChanged (fun text ->
                                              let names =
                                                  text.Split(
                                                      [| '\n'; '\r' |],
                                                      System.StringSplitOptions.RemoveEmptyEntries
                                                  )
                                                  |> Array.toList

                                              dispatch (
                                                  ModEditorMsg(
                                                      UpdateCountry(
                                                          country.Country.Code,
                                                          fun c ->
                                                              { c with
                                                                  Names = { c.Names with LastNames = names } }
                                                      )
                                                  )
                                              )) ] ] ] ] ])

    // ─── Country detail ───────────────────────────────────────────────────────

    let private countryDetail (state: ModEditorState) (country: CountryDataDto) dispatch =
        StackPanel.create
            [ StackPanel.spacing 20.0
              StackPanel.children
                  [

                    // ── Country heading ──
                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 12.0
                          StackPanel.children
                              [ Border.create
                                    [ Border.background Theme.AccentLight
                                      Border.cornerRadius 6.0
                                      Border.padding (8.0, 4.0)
                                      Border.child (
                                          TextBlock.create
                                              [ TextBlock.text country.Country.Code
                                                TextBlock.fontWeight FontWeight.Black
                                                TextBlock.fontSize 16.0
                                                TextBlock.foreground Theme.Accent ]
                                      ) ]
                                TextBlock.create
                                    [ TextBlock.text country.Country.Name
                                      TextBlock.fontSize 20.0
                                      TextBlock.fontWeight FontWeight.Bold
                                      TextBlock.foreground Theme.TextMain
                                      TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]

                    // ── Sub-tab bar ──
                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 4.0
                          StackPanel.children
                              [ UI.tabButton "General" (state.ActiveSubTab = General) (fun _ ->
                                    dispatch (ModEditorMsg(SetSubTab General)))
                                UI.tabButton "Leagues" (state.ActiveSubTab = Leagues) (fun _ ->
                                    dispatch (ModEditorMsg(SetSubTab Leagues)))
                                UI.tabButton "Clubs" (state.ActiveSubTab = Clubs) (fun _ ->
                                    dispatch (ModEditorMsg(SetSubTab Clubs)))
                                UI.tabButton "Cups" (state.ActiveSubTab = Cups) (fun _ ->
                                    dispatch (ModEditorMsg(SetSubTab Cups)))
                                UI.tabButton "Names" (state.ActiveSubTab = NamePool) (fun _ ->
                                    dispatch (ModEditorMsg(SetSubTab NamePool))) ] ]

                    // ── Sub-tab content ──
                    match state.ActiveSubTab with
                    | General -> generalSubTab state country dispatch
                    | Leagues -> leaguesSubTab state country dispatch
                    | Clubs -> clubsSubTab state country dispatch
                    | NamePool -> namePoolSubTab country dispatch
                    | Cups -> cupsSubTab state country dispatch ] ]

    // ─── Countries tab ────────────────────────────────────────────────────────

    let private countriesTab (state: ModEditorState) dispatch =
        Grid.create
            [ Grid.columnDefinitions "Auto, *"
              Grid.children
                  [ Border.create [ Grid.column 0; Border.child (countrySidebar state dispatch) ]

                    match
                        state.SelectedCountryCode
                        |> Option.bind (fun code -> state.Countries |> Map.tryFind code)
                    with
                    | Some country ->
                        ScrollViewer.create
                            [ Grid.column 1
                              ScrollViewer.padding (24.0, 20.0)
                              ScrollViewer.content (countryDetail state country dispatch) ]
                    | None ->
                        Border.create
                            [ Grid.column 1
                              Border.child (
                                  TextBlock.create
                                      [ TextBlock.text "Select a country to edit or create a new one."
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.foreground Theme.TextSub ]
                              ) ] ] ]

    let private internationalTab (state: ModEditorState) dispatch =
        let updateIntComp i f =
            dispatch (ModEditorMsg(UpdateInternationalComp(i, f)))

        UI.sectionContainer
            "International Competitions"
            (StackPanel.create
                [ StackPanel.spacing 0.0
                  StackPanel.children
                      [ // Column headers
                        Border.create
                            [ Border.background Theme.BgMain
                              Border.padding (12.0, 8.0)
                              Border.borderBrush Theme.Border
                              Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                              Border.child (
                                  Grid.create
                                      [ Grid.columnDefinitions "*, 120, 100, 72"
                                        Grid.children
                                            [ TextBlock.create
                                                  [ Grid.column 0
                                                    TextBlock.text "COMPETITION NAME"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ]
                                              TextBlock.create
                                                  [ Grid.column 1
                                                    TextBlock.text "CONFEDERATION"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ]
                                              TextBlock.create
                                                  [ Grid.column 2
                                                    TextBlock.text "FORMAT"
                                                    TextBlock.fontSize 10.0
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.foreground Theme.TextSub ] ] ]
                              ) ]

                        for i in 0 .. state.InternationalComps.Length - 1 do
                            let comp = state.InternationalComps.[i]

                            Border.create
                                [ Border.borderBrush Theme.Border
                                  Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                                  Border.padding (12.0, 6.0)
                                  Border.child (
                                      Grid.create
                                          [ Grid.columnDefinitions "*, 120, 100, 72"
                                            Grid.children
                                                [ TextBox.create
                                                      [ Grid.column 0
                                                        TextBox.text comp.Name
                                                        TextBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        TextBox.onTextChanged (fun t ->
                                                            updateIntComp i (fun c -> { c with Name = t })) ]
                                                  ComboBox.create
                                                      [ Grid.column 1
                                                        ComboBox.dataItems
                                                            [ "UEFA"; "CONMEBOL"; "CONCACAF"; "CAF"; "AFC"; "OFC" ]
                                                        ComboBox.selectedItem (
                                                            comp.Confederation |> Option.defaultValue "UEFA"
                                                        )
                                                        ComboBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        ComboBox.onSelectedItemChanged (fun obj ->
                                                            match obj with
                                                            | :? string as s ->
                                                                updateIntComp i (fun c ->
                                                                    { c with Confederation = Some s })
                                                            | _ -> ()) ]
                                                  TextBox.create
                                                      [ Grid.column 2
                                                        TextBox.text comp.Format
                                                        TextBox.margin (0.0, 0.0, 8.0, 0.0)
                                                        TextBox.onTextChanged (fun t ->
                                                            updateIntComp i (fun c -> { c with Format = t })) ]
                                                  Border.create
                                                      [ Grid.column 3
                                                        Border.child (
                                                            dangerButton "✕" (fun _ ->
                                                                dispatch (ModEditorMsg(RemoveInternationalComp i)))
                                                        ) ] ] ]
                                  ) ]

                        Border.create
                            [ Border.padding (12.0, 12.0)
                              Border.child (
                                  UI.primaryButton "Add Competition" (Some IconName.add) (fun _ ->
                                      dispatch (
                                          ModEditorMsg(
                                              AddInternationalComp
                                                  { Name = "New Competition"
                                                    Confederation = Some "UEFA"
                                                    Format = "GroupsKnockout"
                                                    Qualification = [] }
                                          )
                                      ))
                              ) ] ] ])

    let private globalNamesTab (state: ModEditorState) dispatch =
        TextBlock.create
            [ TextBlock.text
                  "Global Names editor will allow defining generic first/last names shared across all countries."
              TextBlock.foreground Theme.TextSub
              TextBlock.margin (Thickness 16.0) ]
        :> IView

    // ─── Validation tab ───────────────────────────────────────────────────────

    let private validationTab (state: ModEditorState) dispatch =
        StackPanel.create
            [ StackPanel.spacing 20.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text "Validation Report"
                          TextBlock.fontSize 18.0
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground Theme.TextMain ]

                    if state.Errors.IsEmpty then
                        UI.statusBanner Theme.Success IconName.success "No errors found. Your mod is valid!"
                    else
                        for error in state.Errors do
                            Border.create
                                [ Border.background (Theme.Danger + "18")
                                  Border.borderBrush Theme.Danger
                                  Border.borderThickness 1.0
                                  Border.padding 15.0
                                  Border.cornerRadius 8.0
                                  Border.margin (0.0, 0.0, 0.0, 4.0)
                                  Border.child (
                                      StackPanel.create
                                          [ StackPanel.orientation Orientation.Horizontal
                                            StackPanel.spacing 10.0
                                            StackPanel.children
                                                [ Icons.iconSm IconName.error Theme.Danger
                                                  TextBlock.create
                                                      [ TextBlock.text $"%A{error}"
                                                        TextBlock.foreground Theme.TextMain
                                                        TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
                                  ) ] ] ]

    // ─── Export tab ───────────────────────────────────────────────────────────

    let private exportTab (state: ModEditorState) dispatch =
        StackPanel.create
            [ StackPanel.spacing 20.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text "Export Mod"
                          TextBlock.fontSize 18.0
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground Theme.TextMain ]

                    UI.sectionContainer
                        "Export to Mods Folder"
                        (StackPanel.create
                            [ StackPanel.spacing 14.0
                              StackPanel.margin (Thickness 16.0)
                              StackPanel.children
                                  [ TextBlock.create
                                        [ TextBlock.text $"Output path:  Mods/%s{state.Manifest.Name}/"
                                          TextBlock.foreground Theme.TextSub
                                          TextBlock.fontSize 13.0 ]

                                    if not state.Errors.IsEmpty then
                                        UI.statusBanner
                                            Theme.Warning
                                            IconName.warning
                                            "Your mod has validation errors and may not load correctly."

                                    UI.primaryButton "Export Now" (Some IconName.save) (fun _ ->
                                        dispatch (ModEditorMsg ModEditorMsg.Export))

                                    if not state.IsDirty then
                                        UI.statusBanner Theme.Success IconName.success "Mod exported successfully!" ] ]) ] ]

    // ─── Root view ────────────────────────────────────────────────────────────

    let view (state: State) dispatch =
        let modState = state.ModEditor

        // Computed outside ScrollViewer property list to avoid inline pattern match.
        let contentPadding =
            match modState.ActiveTab with
            | Countries -> Thickness 0.0
            | _ -> Thickness 24.0

        DockPanel.create
            [ DockPanel.children
                  [

                    // ── Top tab bar ──
                    Border.create
                        [ DockPanel.dock Dock.Top
                          Border.background Theme.BgCard
                          Border.borderBrush Theme.Border
                          Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                          Border.child (
                              DockPanel.create
                                  [ DockPanel.children
                                        [

                                          // Undo / Redo flush right
                                          StackPanel.create
                                              [ DockPanel.dock Dock.Right
                                                StackPanel.orientation Orientation.Horizontal
                                                StackPanel.spacing 4.0
                                                StackPanel.margin (8.0, 0.0)
                                                StackPanel.children
                                                    [ UI.ghostButton "Undo" (Some IconName.refresh) (fun _ ->
                                                          dispatch (ModEditorMsg UndoMod))
                                                      UI.ghostButton "Redo" (Some IconName.refresh) (fun _ ->
                                                          dispatch (ModEditorMsg RedoMod)) ] ]

                                          // Tabs
                                          StackPanel.create
                                              [ StackPanel.orientation Orientation.Horizontal
                                                StackPanel.margin (12.0, 0.0)
                                                StackPanel.children
                                                    [ topTab modState.ActiveTab Overview "Overview" dispatch
                                                      topTab modState.ActiveTab Countries "Countries" dispatch
                                                      topTab modState.ActiveTab International "International" dispatch
                                                      topTab modState.ActiveTab GlobalNames "Global Names" dispatch
                                                      topTab modState.ActiveTab Validation "Validation" dispatch
                                                      topTab modState.ActiveTab Export "Export" dispatch ] ] ] ]
                          ) ]

                    // ── Content area ──
                    ScrollViewer.create
                        [ ScrollViewer.padding contentPadding
                          ScrollViewer.content (
                              match modState.ActiveTab with
                              | Overview -> overviewTab modState dispatch :> IView
                              | Countries -> countriesTab modState dispatch :> IView
                              | Validation -> validationTab modState dispatch :> IView
                              | Export -> exportTab modState dispatch :> IView
                              | International -> internationalTab modState dispatch :> IView
                              | GlobalNames -> globalNamesTab modState dispatch
                          ) ] ] ]
