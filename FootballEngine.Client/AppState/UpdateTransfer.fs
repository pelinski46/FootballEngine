namespace FootballEngine

open System.Threading.Tasks
open Elmish
open FootballEngine.Domain
open FootballEngine.Domain.TransferNegotiation
open AppTypes
open AppMsgs

module UpdateTransfer =

    let private buildClubNameCache (gs: GameState) : Map<PlayerId, string> =
        gs.Players
        |> Map.toSeq
        |> Seq.choose (fun (_, p) -> gs.Clubs |> Map.tryFind p.ClubId |> Option.map (fun c -> p.Id, c.Name))
        |> Map.ofSeq

    let private matchesFilter (filter: TransferFilter) (p: Player) =
        match filter with
        | AllPositions -> true
        | Goalkeepers -> p.Position = GK
        | Defenders -> p.Position = DC || p.Position = DL || p.Position = DR
        | Midfielders -> p.Position = DM || p.Position = MC || p.Position = AMC
        | Attackers -> p.Position = AML || p.Position = AMR || p.Position = ST

    let private applySort (sortBy: SortField) (asc: bool) (players: Player list) =
        match sortBy, asc with
        | ByName, true -> players |> List.sortBy _.Name
        | ByName, false -> players |> List.sortByDescending _.Name
        | ByCA, true -> players |> List.sortBy _.CurrentSkill
        | ByCA, false -> players |> List.sortByDescending _.CurrentSkill
        | ByValue, true -> players |> List.sortBy _.Value
        | ByValue, false -> players |> List.sortByDescending _.Value
        | ByAge, true -> players |> List.sortByDescending _.Birthday
        | ByAge, false -> players |> List.sortBy _.Birthday
        | ByPosition, true -> players |> List.sortBy _.Position
        | ByPosition, false -> players |> List.sortByDescending _.Position

    let private applyFilters (t: TransferState) =
        let query = t.SearchQuery.Trim().ToLowerInvariant()

        t.CachedPlayers
        |> List.filter (fun p ->
            matchesFilter t.PositionFilter p
            && (query = "" || p.Name.ToLowerInvariant().Contains(query)))
        |> applySort t.SortBy t.SortAsc

    let private withFilteredPlayers (t: TransferState) =
        { t with
            FilteredPlayers = applyFilters t
            Page = 0 }

    let private buildCacheAsync (gs: GameState) =
        Task.Run(fun () ->
            let clubNameCache = buildClubNameCache gs

            let players =
                gs.Players
                |> Map.toList
                |> List.map snd
                |> List.filter (fun p -> p.ClubId <> gs.UserClubId)

            players, clubNameCache)

    let private applyTransferToGameState
        (buyer: Club)
        (seller: Club)
        (p: Player)
        (fee: decimal)
        (salary: decimal)
        (years: int)
        (gs: GameState)
        : GameState =
        let moved =
            { p with
                ClubId = buyer.Id
                Salary = salary
                ContractExpiry = gs.Season + years
                Morale = min 100 (p.Morale + 10) }

        { gs with
            Players = gs.Players |> Map.add p.Id moved
            Clubs =
                gs.Clubs
                |> Map.add
                    buyer.Id
                    { buyer with
                        Budget = buyer.Budget - fee
                        Players = moved :: (buyer.Players |> List.filter (fun x -> x.Id <> p.Id)) }
                |> Map.add
                    seller.Id
                    { seller with
                        Budget = seller.Budget + fee
                        Players = seller.Players |> List.filter (fun x -> x.Id <> p.Id) } }

    let private recordTransfer
        (p: Player)
        (buyer: Club)
        (seller: Club)
        (fee: decimal)
        (season: int)
        (t: TransferState)
        =
        let record =
            { PlayerId = p.Id
              PlayerName = p.Name
              FromClubName = seller.Name
              ToClubName = buyer.Name
              Fee = fee
              Season = season }

        { t with
            TransferHistory = record :: t.TransferHistory }

    let private pushNotification = AppTypes.pushNotification

    let handle (msg: TransferMsg) (state: State) : State * Cmd<Msg> =
        let gs = state.GameState
        let t = state.Transfer

        match msg with
        | Load ->
            { state with
                State.Transfer.IsLoading = true },
            Cmd.OfTask.perform (fun () -> buildCacheAsync gs) () (fun (players, cache) ->
                TransferMsg(Loaded(players, cache)))

        | Loaded(players, cache) ->
            let updated =
                { t with
                    CachedPlayers = players
                    ClubNameCache = cache
                    IsLoading = false }
                |> withFilteredPlayers

            { state with Transfer = updated }, Cmd.none

        | TabChange tab ->
            { state with
                Transfer = { t with ActiveTab = tab; Page = 0 } },
            Cmd.none

        | Search query ->
            { state with
                Transfer = { t with SearchQuery = query } |> withFilteredPlayers },
            Cmd.none

        | FilterChange f ->
            { state with
                Transfer = { t with PositionFilter = f } |> withFilteredPlayers },
            Cmd.none

        | SortChange s ->
            let asc = if t.SortBy = s then not t.SortAsc else false

            { state with
                Transfer = { t with SortBy = s; SortAsc = asc } |> withFilteredPlayers },
            Cmd.none

        | PlayerSelect pid ->
            { state with
                State.Transfer.SelectedPlayerId = Some pid },
            Cmd.none

        | WatchToggle pid ->
            let updated =
                if List.contains pid t.WatchlistIds then
                    t.WatchlistIds |> List.filter ((<>) pid)
                else
                    pid :: t.WatchlistIds

            { state with
                State.Transfer.WatchlistIds = updated },
            Cmd.none

        | PageChange p -> { state with State.Transfer.Page = p }, Cmd.none

        | MakeOffer(playerId, fee) ->
            let negotiation =
                { PlayerId = playerId
                  OfferedFee = fee
                  Step = MakingOffer }

            { state with
                Transfer =
                    { t with
                        ActiveNegotiation = Some negotiation
                        ActiveTab = MarketSearch } },
            Cmd.none

        | UpdateOfferedFee fee ->
            let updated =
                t.ActiveNegotiation |> Option.map (fun n -> { n with OfferedFee = fee })

            { state with
                State.Transfer.ActiveNegotiation = updated },
            Cmd.none

        | SubmitOffer ->
            match t.ActiveNegotiation with
            | None -> state, Cmd.none
            | Some neg ->
                match gs.Players |> Map.tryFind neg.PlayerId with
                | None -> state, Cmd.none
                | Some p ->
                    let buyer = gs.Clubs[gs.UserClubId]
                    let seller = gs.Clubs[p.ClubId]

                    if not (canAfford buyer neg.OfferedFee (suggestedSalary p)) then
                        let updated =
                            { neg with
                                Step = OfferRejected "Insufficient budget" }

                        { state with
                            State.Transfer.ActiveNegotiation = Some updated },
                        Cmd.none
                    else
                        let response = clubResponse buyer seller p neg.OfferedFee

                        match response with
                        | RejectedByClub ->
                            let updated =
                                { neg with
                                    Step = OfferRejected $"{seller.Name} rejected the offer" }

                            { state with
                                State.Transfer.ActiveNegotiation = Some updated },
                            Cmd.none

                        | AcceptedByClub ->
                            let salary = suggestedSalary p

                            let updated =
                                { neg with
                                    Step = NegotiatingContract(salary, 3) }

                            let offer =
                                { Id = t.NextOfferId
                                  PlayerId = p.Id
                                  SellerClubId = seller.Id
                                  Fee = neg.OfferedFee
                                  Status = AcceptedByClub
                                  CreatedOnDate = gs.CurrentDate }

                            { state with
                                Transfer =
                                    { t with
                                        ActiveNegotiation = Some updated
                                        OutgoingOffers = offer :: t.OutgoingOffers
                                        NextOfferId = t.NextOfferId + 1 } },
                            Cmd.none

                        | _ -> state, Cmd.none

        | OfferCounterSalary(salary, years) ->
            match t.ActiveNegotiation with
            | None -> state, Cmd.none
            | Some neg ->
                let updated =
                    { neg with
                        Step = NegotiatingContract(salary, years) }

                { state with
                    State.Transfer.ActiveNegotiation = Some updated },
                Cmd.none

        | AcceptContract ->
            match t.ActiveNegotiation with
            | None -> state, Cmd.none
            | Some neg ->
                match neg.Step with
                | NegotiatingContract(salary, years) ->
                    match gs.Players |> Map.tryFind neg.PlayerId with
                    | None -> state, Cmd.none
                    | Some p ->
                        let buyer = gs.Clubs[gs.UserClubId]
                        let seller = gs.Clubs[p.ClubId]
                        let playerResp = playerResponse buyer p salary

                        match playerResp with
                        | ContractRejectedByPlayer ->
                            let updated = { neg with Step = ContractRejected }

                            { state with
                                State.Transfer.ActiveNegotiation = Some updated },
                            Cmd.none

                        | ContractOffered _ ->
                            let newGs = applyTransferToGameState buyer seller p neg.OfferedFee salary years gs

                            let updatedTransfer =
                                { t with
                                    ActiveNegotiation = Some { neg with Step = NegotiationComplete }
                                    OutgoingOffers =
                                        t.OutgoingOffers
                                        |> List.map (fun o ->
                                            if o.PlayerId = p.Id then
                                                { o with Status = Completed }
                                            else
                                                o)
                                    CachedPlayers = t.CachedPlayers |> List.filter (fun x -> x.Id <> p.Id)
                                    FilteredPlayers = t.FilteredPlayers |> List.filter (fun x -> x.Id <> p.Id) }
                                |> recordTransfer p buyer seller neg.OfferedFee gs.Season

                            let nextState =
                                { state with
                                    GameState = newGs
                                    Transfer = updatedTransfer }
                                |> pushNotification
                                    Transfer
                                    "Transfer Complete"
                                    $"{p.Name} signed for {buyer.Name} — fee: ${neg.OfferedFee:N0}"

                            nextState, SimHelpers.saveCmd newGs

                        | _ -> state, Cmd.none
                | _ -> state, Cmd.none

        | WithdrawOffer offerId ->
            let updated =
                t.OutgoingOffers
                |> List.map (fun o -> if o.Id = offerId then { o with Status = Withdrawn } else o)

            let clearNeg =
                t.ActiveNegotiation
                |> Option.bind (fun n ->
                    t.OutgoingOffers
                    |> List.tryFind (fun o -> o.Id = offerId && o.PlayerId = n.PlayerId)
                    |> Option.map (fun _ -> None)
                    |> Option.defaultValue (Some n))

            { state with
                Transfer =
                    { t with
                        OutgoingOffers = updated
                        ActiveNegotiation = clearNeg } },
            Cmd.none

        | ClearNegotiation ->
            { state with
                State.Transfer.ActiveNegotiation = None },
            Cmd.none
