namespace FootballEngine

open System
open FootballEngine.Domain
open FSharp.Stats.Distributions

module ContractManager =

    let private renewOrRelease (season: int) (squad: Player list) (p: Player) (clubId: ClubId) (c: ContractInfo) : Player =
        let avg =
            if squad.IsEmpty then 0.0
            else squad |> List.averageBy (fun x -> float x.CurrentSkill)

        let isEssential = float p.CurrentSkill >= avg - 5.0
        let roll = Continuous.Uniform.Sample 2.0 5.0
        let newExpiry = if isEssential then season + int roll else season

        { p with Affiliation = Contracted(clubId, { c with ExpiryYear = newExpiry }) }

    let processContracts (gs: GameState) : GameState =
        let expiring =
            gs.Players
            |> Map.toList
            |> List.choose (fun (_, p) ->
                match p.Affiliation with
                | Contracted(clubId, c) when c.ExpiryYear <= gs.Season && clubId <> gs.UserClubId -> Some(p, clubId, c)
                | _ -> None)

        expiring
        |> List.fold
            (fun state (p, clubId, c) ->
                let squad   = GameState.getSquad clubId state
                let updated = renewOrRelease gs.Season squad p clubId c
                GameState.updatePlayer updated state)
            gs
