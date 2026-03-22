namespace FootballEngine.Domain

open System

type GameState =
    { CurrentDate: DateTime
      Season: int
      Clubs: Map<ClubId, Club>
      Players: Map<PlayerId, Player>
      Competitions: Map<CompetitionId, Competition>
      Countries: Map<CountryCode, Country>
      UserClubId: ClubId
      ManagerName: string
      PrimaryCountry: CountryCode }

module GameState =

    let getSquad (clubId: ClubId) (gs: GameState) : Player list =
        gs.Clubs
        |> Map.tryFind clubId
        |> Option.map (fun club -> club.PlayerIds |> List.choose gs.Players.TryFind)
        |> Option.defaultValue []

    let freeAgents (gs: GameState) : Player seq =
        gs.Players |> Map.values |> Seq.filter (fun p -> p.Affiliation = FreeAgent)

    let updatePlayer (p: Player) (gs: GameState) : GameState =
        { gs with
            Players = gs.Players |> Map.add p.Id p }

    let clubOf (p: Player) : ClubId option =
        match p.Affiliation with
        | Contracted(clubId, _) -> Some clubId
        | YouthProspect clubId -> Some clubId
        | FreeAgent
        | Retired -> None

    let contractOf (p: Player) : ContractInfo option =
        match p.Affiliation with
        | Contracted(_, contract) -> Some contract
        | _ -> None
