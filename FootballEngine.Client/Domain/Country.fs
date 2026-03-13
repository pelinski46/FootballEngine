namespace FootballEngine.Domain

open FootballEngine.DomainTypes

type Confederation =
    | UEFA
    | CONMEBOL
    | CONCACAF
    | CAF
    | AFC
    | OFC

type Country =
    { Code: CountryCode
      Name: string
      Confederation: Confederation }
