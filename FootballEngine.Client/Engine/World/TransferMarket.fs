namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Domain.TransferNegotiation

module NegotiationEngine =

    let private sellerFloor (p: Player) = suggestedFee p * 0.85m
    let private buyerCeiling (maxFee: decimal) = maxFee

    let private advanceStage
        (buyer: Club)
        (seller: Club)
        (p: Player)
        (maxFee: decimal)
        (stage: NegotiationStage)
        : NegotiationStage =
        match stage with
        | Approaching ->
            let initialOffer = suggestedFee p * 0.88m

            if canAfford buyer initialOffer (suggestedSalary p) then
                OfferMade initialOffer
            else
                Collapsed "buyer cannot afford"

        | OfferMade fee ->
            match clubResponse buyer seller p fee with
            | AcceptedByClub -> ContractDiscussion(fee, suggestedSalary p)
            | _ ->
                let counter = fee * 1.1m

                if counter > buyerCeiling maxFee then
                    Collapsed "fee exceeds budget"
                else
                    CounterReceived(counter, 1)

        | CounterReceived(_fee, round) when round >= 4 -> Collapsed "max rounds reached"

        | CounterReceived(fee, round) ->
            let meetingPoint = fee * 0.97m

            if
                meetingPoint >= sellerFloor p
                && canAfford buyer meetingPoint (suggestedSalary p)
            then
                ContractDiscussion(meetingPoint, suggestedSalary p)
            else
                CounterReceived(fee * 1.06m, round + 1)

        | ContractDiscussion(fee, salary) ->
            match playerResponse buyer p salary with
            | ContractOffered(s, y) -> Agreed(fee, s, y)
            | _ ->
                let bump = salary * 1.08m

                if canAfford buyer fee bump then
                    ContractDiscussion(fee, bump)
                else
                    Collapsed "player wage demands too high"

        | terminal -> terminal

    let step (gs: GameState) (neg: Negotiation) : Negotiation =
        match
            Map.tryFind neg.BuyerClubId gs.Clubs,
            Map.tryFind neg.SellerClubId gs.Clubs,
            Map.tryFind neg.PlayerId gs.Players
        with
        | Some buyer, Some seller, Some p ->
            let maxFee =
                match neg.Stage with
                | OfferMade fee
                | CounterReceived(fee, _) -> fee * 1.5m
                | _ -> buyer.Budget * 0.5m

            { neg with
                Stage = advanceStage buyer seller p maxFee neg.Stage }
        | _ ->
            { neg with
                Stage = Collapsed "entity not found" }

    let private applyAgreed
        (neg: Negotiation)
        (fee: decimal)
        (salary: decimal)
        (years: int)
        (gs: GameState)
        : GameState =
        match
            Map.tryFind neg.PlayerId gs.Players,
            Map.tryFind neg.BuyerClubId gs.Clubs,
            Map.tryFind neg.SellerClubId gs.Clubs
        with
        | Some p, Some buyer, Some seller ->
            let moved =
                { p with
                    Affiliation =
                        Contracted(
                            neg.BuyerClubId,
                            { Salary = salary
                              ExpiryYear = gs.Season + years }
                        )
                    Morale = min 100 (p.Morale + 5) }

            { gs with
                Players = gs.Players |> Map.add p.Id moved
                Clubs =
                    gs.Clubs
                    |> Map.add
                        neg.BuyerClubId
                        { buyer with
                            Budget = max 0m (buyer.Budget - fee)
                            PlayerIds = moved.Id :: (buyer.PlayerIds |> List.filter ((<>) moved.Id)) }
                    |> Map.add
                        neg.SellerClubId
                        { seller with
                            Budget = seller.Budget + fee
                            PlayerIds = seller.PlayerIds |> List.filter ((<>) p.Id) } }
        | _ -> gs

    let resolveAndPrune (gs: GameState) (negotiations: Negotiation list) : GameState * Negotiation list =
        negotiations
        |> List.fold
            (fun (state, active) neg ->
                match neg.Stage with
                | Agreed(fee, salary, years) -> applyAgreed neg fee salary years state, active
                | Collapsed _ -> state, active
                | _ -> state, neg :: active)
            (gs, [])
        |> fun (s, active) -> s, List.rev active

module TransferMarket =

    let private alreadyNegotiating (playerId: PlayerId) (window: TransferWindow) =
        window.ActiveNegotiations |> List.exists (fun n -> n.PlayerId = playerId)

    let private openFromIntents
        (intents: ManagerIntent list)
        (window: TransferWindow)
        (gs: GameState)
        : TransferWindow =
        let newNegs, nextId =
            intents
            |> List.collect (fun intent ->
                intent.Targets
                |> List.filter (fun t -> not (alreadyNegotiating t.PlayerId window))
                |> List.choose (fun target ->
                    match Map.tryFind target.PlayerId gs.Players with
                    | None -> None
                    | Some p ->
                        match p.Affiliation with
                        | FreeAgent -> Some(intent.ClubId, intent.ClubId, p)
                        | Contracted(sellerId, _) when sellerId <> intent.ClubId -> Some(intent.ClubId, sellerId, p)
                        | _ -> None))
            |> List.fold
                (fun (acc, id) (buyerId, sellerId, p) ->
                    let neg =
                        { Id = id
                          BuyerClubId = buyerId
                          SellerClubId = sellerId
                          PlayerId = p.Id
                          Stage = Approaching
                          Deadline = window.Closes }

                    neg :: acc, id + 1)
                ([], window.NextId)
            |> fun (negs, id) -> List.rev negs, id

        { window with
            ActiveNegotiations = window.ActiveNegotiations @ newNegs
            NextId = nextId }

    let private stepAll (gs: GameState) (window: TransferWindow) : GameState * TransferWindow =
        let stepped =
            window.ActiveNegotiations
            |> Array.ofList
            |> Array.Parallel.map (NegotiationEngine.step gs)
            |> Array.toList

        let gs2, remaining = NegotiationEngine.resolveAndPrune gs stepped

        gs2,
        { window with
            ActiveNegotiations = remaining }

    let private expireOverdue (date: DateTime) (window: TransferWindow) : TransferWindow =
        { window with
            ActiveNegotiations = window.ActiveNegotiations |> List.filter (fun n -> date <= n.Deadline) }

    let runWeek
        (date: DateTime)
        (intents: ManagerIntent list)
        (window: TransferWindow)
        (gs: GameState)
        : GameState * TransferWindow =
        if not (TransferWindow.isOpen date window) then
            gs, window
        else
            let w1 = openFromIntents intents window gs
            let gs2, w2 = stepAll gs w1
            gs2, expireOverdue date w2

    let simulateSummerWindow (gs: GameState) : GameState =
        let window = TransferWindow.summer gs.Season
        let intents = ManagerAI.buildAllIntents gs

        let rec loop (date: DateTime) (w: TransferWindow) (state: GameState) =
            if not (TransferWindow.isOpen date w) then
                state
            else
                let s2, w2 = runWeek date intents w state
                loop (date.AddDays 7.0) w2 s2

        loop window.Opens window gs
