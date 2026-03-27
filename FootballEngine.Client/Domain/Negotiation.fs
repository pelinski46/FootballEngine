namespace FootballEngine.Domain

open System

type NegotiationStage =
    | Approaching
    | OfferMade of fee: decimal
    | CounterReceived of fee: decimal * round: int
    | ContractDiscussion of fee: decimal * salary: decimal
    | Agreed of fee: decimal * salary: decimal * years: int
    | Collapsed of reason: string

type Negotiation =
    { Id: int
      BuyerClubId: ClubId
      SellerClubId: ClubId
      PlayerId: PlayerId
      Stage: NegotiationStage
      Deadline: DateTime }

type TransferWindow =
    { Opens: DateTime
      Closes: DateTime
      ActiveNegotiations: Negotiation list
      NextId: int }

module TransferWindow =

    let summer (season: int) =
        { Opens = DateTime(season, 6, 1)
          Closes = DateTime(season, 8, 31)
          ActiveNegotiations = []
          NextId = 1 }

    let winter (season: int) =
        { Opens = DateTime(season, 1, 1)
          Closes = DateTime(season, 1, 31)
          ActiveNegotiations = []
          NextId = 1 }

    let isOpen (date: DateTime) (window: TransferWindow) =
        date >= window.Opens && date <= window.Closes
