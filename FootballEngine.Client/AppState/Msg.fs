namespace FootballEngine

open FootballEngine.Domain
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
        | AdvanceDay
        | AdvanceDayDone of GameState * (MatchId * MatchFixture) list
        | SimulateMatch
        | SimulateNextFixture
        | SimulateAllToday

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

    type MatchLabMsg =
        | SelectHome of ClubId
        | SelectAway of ClubId
        | Run
        | Step of int

    type Msg =
        | SetupMsg of SetupMsg
        | SimMsg of SimMsg
        | TransferMsg of TransferMsg
        | MatchLabMsg of MatchLabMsg
        | GameLoaded of GameState option
        | ChangePage of Page
        | SelectPlayer of PlayerId
        | DropPlayerInSlot of slotIndex: int * playerId: int
        | SortPlayersBy of string
        | SetTactics of Formation
        | ChangeLeague of CompetitionId
        | SaveGame
        | SetProcessing of bool
