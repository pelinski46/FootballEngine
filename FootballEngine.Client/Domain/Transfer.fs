namespace FootballEngine.Domain

open System

type OfferStatus =
    | Pending
    | AcceptedByClub
    | RejectedByClub
    | ContractOffered of salary: decimal * years: int
    | ContractRejectedByPlayer
    | Completed
    | Withdrawn

type TransferOffer =
    { Id: int
      PlayerId: PlayerId
      SellerClubId: ClubId
      Fee: decimal
      Status: OfferStatus
      CreatedOnDate: DateTime }

type TransferRecord =
    { PlayerId: PlayerId
      PlayerName: string
      FromClubName: string
      ToClubName: string
      Fee: decimal
      Season: int }

module TransferNegotiation =

    let private minAcceptableMultiplier = 0.85m
    let private clubBudgetSafetyFactor = 1.2m
    let private reputationGapThreshold = 2000

    let private playerWillAccept (club: Club) (p: Player) (salary: decimal) =
        let isBetterClub = club.Reputation >= p.Reputation - 500
        let isFairSalary = salary >= p.Salary * 0.9m
        isBetterClub && isFairSalary

    let clubResponse (buyer: Club) (seller: Club) (p: Player) (fee: decimal) : OfferStatus =
        let isEnoughFee = fee >= p.Value * minAcceptableMultiplier

        let buyerReputationOk =
            buyer.Reputation >= seller.Reputation - reputationGapThreshold

        if isEnoughFee && buyerReputationOk then
            AcceptedByClub
        else
            RejectedByClub

    let playerResponse (buyer: Club) (p: Player) (salary: decimal) : OfferStatus =
        if playerWillAccept buyer p salary then
            ContractOffered(salary, 3)
        else
            ContractRejectedByPlayer

    let suggestedFee (p: Player) = p.Value

    let suggestedSalary (p: Player) = p.Salary

    let canAfford (buyer: Club) (fee: decimal) (salary: decimal) =
        buyer.Budget >= fee * clubBudgetSafetyFactor && buyer.Budget >= salary * 12m
