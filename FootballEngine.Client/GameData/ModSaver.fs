namespace FootballEngine.Data

open System.IO
open System.Text.Json

module ModSaver =

    let private options = JsonSerializerOptions(WriteIndented = true)

    let saveMod (basePath: string) (manifest: ModManifestDto) (countries: Map<string, CountryDataDto>) =
        try
            let modDir = Path.Combine(basePath, manifest.Name)

            if not (Directory.Exists modDir) then
                Directory.CreateDirectory modDir |> ignore

            // Save manifest
            let manifestPath = Path.Combine(modDir, "mod.json")
            let manifestJson = JsonSerializer.Serialize(manifest, options)
            File.WriteAllText(manifestPath, manifestJson)

            // Save countries
            let countriesDir = Path.Combine(modDir, "countries")

            if not (Directory.Exists countriesDir) then
                Directory.CreateDirectory countriesDir |> ignore

            for KeyValue(code, data) in countries do
                let countryPath = Path.Combine(countriesDir, $"country_{code}.json")

                // Wrap in envelope
                let envelope =
                    { SchemaVersion = SchemaVersion.Current
                      DataType = "CountryData"
                      RawJson = None }

                // We need to serialize the whole thing.
                // In our system, the envelope is just a header, but the actual file is the whole CountryDataDto.
                // Wait, let's check how ModLoader loads it.

                let json = JsonSerializer.Serialize(data, options)
                File.WriteAllText(countryPath, json)

            Ok()
        with ex ->
            Error ex.Message
