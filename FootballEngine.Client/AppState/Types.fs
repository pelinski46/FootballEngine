namespace FootballEngine

open FootballEngine.Domain

module AppTypes =

    type Page =
        | Setup
        | Home
        | Inbox
        | Squad
        | Tactics
        | Training
        | Scouting
        | Transfers
        | Finances
        | Settings
        | MatchLab

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
          Kind: NotificationKind
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

    type State =
        { GameState: GameState
          CurrentPage: Page
          IsProcessing: bool
          LogMessages: string list
          Notifications: Notification list
          NextNotificationId: int
          SelectedPlayer: Player option
          SelectedTactics: Formation
          SelectedLeagueId: CompetitionId
          DraggedPlayer: PlayerId option
          PlayerSortBy: string
          Setup: SetupState
          Transfer: TransferState
          MatchLab: MatchLabState }

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
