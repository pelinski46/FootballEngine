namespace FootballEngine.Data

open FootballEngine.Domain
open ModTypes

/// Runtime-populated registry. Loaded by ModLoader at startup.
/// Mutable by design — initialized once, read-only thereafter.
module DataRegistry =

    let mutable private _countries = Map.empty<CountryCode, CountryData>
    let mutable private _intlComps: (string * CompetitionType) list = []
    let mutable private _mods: ModManifest list = []
    let mutable private _loadErrors: ModError list = []

    let internal setLoadedData (data: LoadedData) =
        _countries <- data.Countries
        _intlComps <- data.InternationalComps
        _mods <- data.ActiveMods
        _loadErrors <- data.Errors

    let allCountries: CountryData list =
        _countries |> Map.values |> Seq.toList

    let findCountry (code: CountryCode) : CountryData =
        match _countries |> Map.tryFind code with
        | Some cd -> cd
        | None -> failwithf $"Country '{code}' not found in loaded data"

    let tryFindCountry (code: CountryCode) : CountryData option =
        _countries |> Map.tryFind code

    let countries: Map<CountryCode, Country> =
        _countries |> Map.map (fun _ cd -> cd.Country)

    let internationalComps: (string * CompetitionType) list =
        _intlComps

    let activeMods: ModManifest list =
        _mods

    let loadErrors: ModError list =
        _loadErrors
