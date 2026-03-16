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

    type SetupState =
        { Step: SetupStep
          SelectedCountry: CountryCode option
          SecondaryCountries: CountryCode list
          ManagerName: string }

    type TransferState =
        { ActiveTab: TransferTab
          SearchQuery: string
          PositionFilter: TransferFilter
          SortBy: SortField
          SortAsc: bool
          SelectedPlayerId: PlayerId option
          WatchlistIds: PlayerId list
          CachedPlayers: Player list
          ClubNameCache: Map<PlayerId, string>
          IsLoading: bool
          Page: int }

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
          ClubNameCache = Map.empty
          IsLoading = false
          Page = 0 }

    let initMatchLabState =
        { HomeClubId = None
          AwayClubId = None
          Result = None
          Snapshot = 0 }
