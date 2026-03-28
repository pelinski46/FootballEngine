namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Engine
open AppTypes

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
        | UserMatchDone of Result<UserMatchDayResult, string>

    type TransferMsg =
        | Load
        | Loaded of players: Player list * clubNames: Map<PlayerId, string>
        | TabChange of TransferTab
        | Search of string
        | FilterChange of TransferFilter
        | SortChange of SortField
        | PlayerSelect of PlayerId
        | WatchToggle of PlayerId
        | PageChange of int
        | MakeOffer of playerId: PlayerId * fee: decimal
        | UpdateOfferedFee of fee: decimal
        | SubmitOffer
        | OfferCounterSalary of salary: decimal * years: int
        | AcceptContract
        | WithdrawOffer of offerId: int
        | ClearNegotiation

    type NotificationMsg =
        | DismissNotification of id: int
        | DismissAll
        | PushNotification of Notification

    type Msg =
        | SetupMsg of SetupMsg
        | SimMsg of SimMsg
        | TransferMsg of TransferMsg
        | NotificationMsg of NotificationMsg
        | GameLoaded of GameState option
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

    let saveCmd (gs: GameState) : Elmish.Cmd<AppMsgs.Msg> =
        Elmish.Cmd.OfTask.either Db.saveGameAsync gs (fun () -> AppMsgs.NoOp) (fun ex ->
            AppMsgs.NotificationMsg(
                AppMsgs.PushNotification
                    { Id = 0
                      Kind = Info
                      Title = "Save failed"
                      Body = ex.Message
                      IsRead = false }
            ))
