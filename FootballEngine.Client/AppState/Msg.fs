namespace FootballEngine

open FootballEngine.Data
open FootballEngine.Domain

open AppTypes
open FootballEngine.World
open FootballEngine.World.WorldRunner

module AppMsgs =

    type SetupMsg =
        | GoToStep of SetupStep
        | SelectPrimaryCountry of CountryCode
        | ToggleSecondaryCountry of CountryCode
        | UpdateManagerName of string
        | ConfirmClub of ClubId
        | StartNewGame

    type SimMsg =
        | Advance of days: int
        | AdvanceDone of DayResult
        | AdvanceSeason
        | SeasonAdvanceDone of Result<SeasonResult, SeasonError>
        | SaveGame
        | SimulateSeason
        | SimulateUserFixture
        | UserMatchDone of Result<UserMatchResult, string>

    type TransferMsg =
        | Load
        | Loaded of players: Player list * clubNames: Map<PlayerId, string>
        | TabChange of TransferTab
        | Search of string
        | ApplySearch of string
        | FilterChange of TransferFilter
        | SortChange of SortField
        | PlayerSelect of PlayerId
        | WatchToggle of PlayerId
        | PageChange of int
        | MakeOffer of playerId: PlayerId * fee: decimal * salary: decimal
        | SubmitOffer
        | WithdrawOffer of negotiationId: int
        | ClearNegotiation
        | CounterOffer of negotiationId: int * newFee: decimal * newSalary: decimal option

    type NotificationMsg =
        | DismissNotification of id: int
        | DismissAll
        | PushNotification of Notification

    type InboxMsg =
        | SelectMessage of messageId: int
        | MarkAsRead of messageId: int
        | MarkActionTaken of messageId: int

    type ModEditorMsg =
        | SetTab of ModEditorTypes.ModEditorTab
        | SetSubTab of ModEditorTypes.ModEditorSubTab
        | UpdateManifest of (ModManifestDto -> ModManifestDto)
        | SelectCountry of string option
        | AddCountry of CountryDataDto
        | UpdateCountry of string * (CountryDataDto -> CountryDataDto)
        | RemoveCountry of string
        | AddCup of string * CupEntryDto
        | UpdateCup of string * int * (CupEntryDto -> CupEntryDto)
        | RemoveCup of string * int
        | AddInternationalComp of InternationalCompDto
        | UpdateInternationalComp of int * (InternationalCompDto -> InternationalCompDto)
        | RemoveInternationalComp of int
        | UndoMod
        | RedoMod
        | Export
        | Validate
        | SearchMod of string

    type Msg =
        | SetupMsg of SetupMsg
        | SimMsg of SimMsg
        | TransferMsg of TransferMsg
        | NotificationMsg of NotificationMsg
        | InboxMsg of InboxMsg
        | ModEditorMsg of ModEditorMsg
        | GameLoaded of (GameState * WorldClock) option
        | ChangePage of Page
        | SelectPlayer of PlayerId
        | DropPlayerInSlot of slotIndex: int * playerId: int
        | SortPlayersBy of string
        | SetTactics of Formation
        | SetTeamTactics of TeamTactics
        | SetMentality of int
        | SetDefensiveLine of int
        | SetPressingIntensity of int
        | ChangeLeague of CompetitionId
        | SetProcessing of bool
        | NoOp
        | StepActiveMatch of delta: int
        | CloseActiveMatch
        | SetPlayerTrainingSchedule of playerId: PlayerId * TrainingSchedule
        | TogglePlayback
        | SetPlaybackSpeed of int
        | TickInterpolation

module SimHelpers =
    let primaryLeagueId (gs: GameState) =
        gs.Competitions
        |> Map.tryFindKey (fun _ comp ->
            match comp.Type, comp.Country with
            | NationalLeague(LeagueLevel 0, _), Some c when c = gs.PrimaryCountry -> true
            | _ -> false)
        |> Option.defaultWith (fun () ->
            gs.Competitions
            |> Map.tryFindKey (fun _ comp ->
                match comp.Type with
                | NationalLeague _ -> true
                | _ -> false)
            |> Option.defaultValue 1)

    let saveCmd (gs: GameState) (clock: WorldClock) : Elmish.Cmd<AppMsgs.Msg> =
        Elmish.Cmd.OfTask.either (fun () -> Db.saveGameAsync gs clock) () (fun () -> AppMsgs.NoOp) (fun ex ->
            AppMsgs.NotificationMsg(
                AppMsgs.PushNotification
                    { Id = 0
                      Icon = Material.Icons.MaterialIconKind.AlertCircleOutline
                      Title = "Save failed"
                      Body = ex.Message
                      IsRead = false }
            ))
