namespace FootballEngine

open System
open FootballEngine.Data
open FootballEngine.Domain
open FootballEngine.Generation
open FootballEngine.Stats

module YouthAcademy =



    let private youthPositions = [| GK; DC; MC; ST; AML; AMR |]

    let generateYouth (rng: Random) (gs: GameState) : GameState =
        let mutable nextId =
            gs.Players
            |> Map.toList
            |> List.map fst
            |> fun ids -> if ids.IsEmpty then 1 else List.max ids + 1

        let newPlayers =
            gs.Clubs
            |> Map.toList
            |> List.map (fun (clubId, club) ->
                let countryData =
                    gs.Countries
                    |> Map.tryFind club.Nationality
                    |> Option.map (fun c -> DataRegistry.findCountry c.Code)
                    |> Option.defaultWith (fun () -> DataRegistry.findCountry club.Nationality)

                let pos = youthPositions[rng.Next(youthPositions.Length)]
                let raw = PlayerGen.create nextId pos clubId countryData 2 gs.Season

                let youth =
                    { raw with
                        CurrentSkill = clamp 30 70 raw.CurrentSkill }

                nextId <- nextId + 1
                clubId, youth)

        newPlayers
        |> List.fold
            (fun state (clubId, p) ->
                { state with
                    Players = state.Players |> Map.add p.Id p
                    Clubs =
                        state.Clubs
                        |> Map.add
                            clubId
                            { state.Clubs[clubId] with
                                PlayerIds = p.Id :: state.Clubs[clubId].PlayerIds } })
            gs
