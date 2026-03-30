namespace FootballEngine

open FootballEngine.Domain

module AppTypes =

    type Page =
        | Loading
        | Setup
        | HomePage
        | Inbox
        | Squad
        | Tactics
        | Training
        | Scouting
        | Transfers
        | Finances
        | Settings
        | Match

    type SetupStep =
        | MainMenu
        | CountrySelection
        | ClubSelection
        | ManagerNaming

    type TransferTab =
        | MarketSearch
        | MyWatchlist
        | IncomingOffers
        | OutgoingOffers
        | TransferHistory

    type TransferFilter =
        | AllPositions
        | Goalkeepers
        | Defenders
        | Midfielders
        | Attackers

    type SortField =
        | ByName
        | ByCA
        | ByValue
        | ByAge
        | ByPosition

    type NotificationKind =
        | MatchResult
        | SeasonEnd
        | Transfer
        | Info

    type Notification =
        { Id: int
          Icon: Material.Icons.MaterialIconKind
          Title: string
          Body: string
          IsRead: bool }

    type SetupState =
        { Step: SetupStep
          SelectedCountry: CountryCode option
          SecondaryCountries: CountryCode list
          ManagerName: string }

    type NegotiationStep =
        | MakingOffer
        | OfferRejected of reason: string
        | NegotiatingContract of offeredSalary: decimal * offeredYears: int
        | ContractRejected
        | NegotiationComplete

    type ActiveNegotiation =
        { PlayerId: PlayerId
          OfferedFee: decimal
          Step: NegotiationStep }

    type TransferState =
        { ActiveTab: TransferTab
          SearchQuery: string
          PositionFilter: TransferFilter
          SortBy: SortField
          SortAsc: bool
          SelectedPlayerId: PlayerId option
          WatchlistIds: PlayerId list
          CachedPlayers: Player list
          FilteredPlayers: Player list
          ClubNameCache: Map<PlayerId, string>
          IsLoading: bool
          Page: int
          OutgoingOffers: TransferOffer list
          TransferHistory: TransferRecord list
          NextOfferId: int
          ActiveNegotiation: ActiveNegotiation option }

    type MatchLabState =
        { HomeClubId: ClubId option
          AwayClubId: ClubId option
          Result: MatchReplay option
          Snapshot: int }

    type InboxState = { SelectedMessageId: int option }

    type ManagerEmployment =
        | Employed of clubId: ClubId
        | NotEmployed

    type AppMode =
        | Initializing
        | NoSave
        | InGame of GameState * ManagerEmployment

    type State =
        { Mode: AppMode
          CurrentPage: Page
          IsProcessing: bool
          LogMessages: string list
          Notifications: Notification list
          NextNotificationId: int
          SelectedPlayer: PlayerId option
          SelectedTactics: Formation
          SelectedLeagueId: CompetitionId
          DraggedPlayer: PlayerId option
          PlayerSortBy: string
          Setup: SetupState
          Transfer: TransferState
          ActiveMatchReplay: MatchReplay option
          ActiveMatchSnapshot: int
          IsPlaying: bool
          PlaybackSpeed: int
          InterpolationT: float
          Inbox: InboxState
          PrevUserClubSkills: Map<PlayerId, int> option
          PrevUserClubStatus: Map<PlayerId, PlayerStatus> option }

    let initSetupState =
        { Step = MainMenu
          SelectedCountry = None
          SecondaryCountries = []
          ManagerName = "" }

    let initTransferState =
        { ActiveTab = MarketSearch
          SearchQuery = ""
          PositionFilter = AllPositions
          SortBy = ByCA
          SortAsc = false
          SelectedPlayerId = None
          WatchlistIds = []
          CachedPlayers = []
          FilteredPlayers = []
          ClubNameCache = Map.empty
          IsLoading = false
          Page = 0
          OutgoingOffers = []
          TransferHistory = []
          NextOfferId = 1
          ActiveNegotiation = None }

    let initMatchLabState =
        { HomeClubId = None
          AwayClubId = None
          Result = None
          Snapshot = 0 }

    let initInboxState = { SelectedMessageId = None }

    let managerEmployment (gs: GameState) : ManagerEmployment =
        GameState.userManager gs
        |> Option.bind (fun s -> if Staff.isEmployed s then Staff.clubId s else None)
        |> Option.map Employed
        |> Option.defaultValue NotEmployed

    let addLog msg (state: State) =
        { state with
            LogMessages = msg :: state.LogMessages |> List.truncate 30 }

    let pushNotification icon title body (state: State) =
        let note =
            { Id = state.NextNotificationId
              Icon = icon
              Title = title
              Body = body
              IsRead = false }

        { state with
            Notifications = note :: state.Notifications |> List.truncate 20
            NextNotificationId = state.NextNotificationId + 1 }
