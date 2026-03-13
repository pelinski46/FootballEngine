namespace FootballEngine

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open FootballEngine
open FootballEngine.Components
open FootballEngine.Components.Header
open FootballEngine.Components.Sidebar
open FootballEngine.Pages.Home
open FootballEngine.Pages.Setup.Setup
open FootballEngine.Pages.Squad
open FootballEngine.Pages.Tactics
open FootballEngine.Test.MatchEngineTests

module Views =
    open AppState

    let mainView (state: State) dispatch =
        DockPanel.create
            [ DockPanel.background Theme.BgMain
              DockPanel.children
                  [ sidebar state dispatch
                    DockPanel.create
                        [ DockPanel.lastChildFill true
                          DockPanel.children
                              [ header state dispatch
                                |> fun h -> Border.create [ Border.dock Dock.Top; Border.child h ]
                                match state.CurrentPage with
                                | Setup -> setupView state dispatch
                                | Home ->
                                    homeView state state.SelectedLeagueId (fun leagueId ->
                                        dispatch (ChangeLeague leagueId))
                                | Inbox -> failwith "todo"
                                | Squad -> squadView state dispatch
                                | Tactics -> tacticView state dispatch
                                | Training -> failwith "todo"
                                | Scouting -> failwith "todo"
                                | Transfers -> failwith "todo"
                                | Finances -> failwith "todo"
                                | Settings -> failwith "todo"
                                | MatchLab -> MatchLabView.view state dispatch ] ] ] ]

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Football Engine 2026"
        base.WindowState <- WindowState.Maximized

        Elmish.Program.mkProgram AppState.init AppState.update Views.mainView
        |> Elmish.Program.withHost this
        |> Elmish.Program.runWithAvaloniaSyncDispatch ()

type App() =
    inherit Application()
    override this.Initialize() = this.Styles.Add(FluentTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- MainWindow()
        | _ -> ()

module Program =
    [<EntryPoint>]
    let main args =
        Db.initTables ()
        runAll () |> ignore
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime(args)
