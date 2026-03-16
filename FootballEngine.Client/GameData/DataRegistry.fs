module FootballEngine.Data.DataRegistry

open FootballEngine.Domain
open FootballEngine.Data

let allCountries: CountryData list =
    [ Countries.ARG.data
      Countries.ENG.data
      Countries.ESP.data
      Countries.BRA.data ]

let findCountry (code: CountryCode) =
    allCountries |> List.find (fun c -> c.Country.Code = code)

let tryFindCountry (code: CountryCode) =
    allCountries |> List.tryFind (fun c -> c.Country.Code = code)

let countries =
    allCountries |> List.map (fun cd -> cd.Country.Code, cd.Country) |> Map.ofList
