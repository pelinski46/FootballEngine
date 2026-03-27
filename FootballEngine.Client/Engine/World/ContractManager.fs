namespace FootballEngine

open System
open FootballEngine.Domain

module ContractManager =

    let private renewOrRelease (rng: Random) (season: int) (squad: Player list) (p: Player) (clubId: ClubId) (c: ContractInfo) : Player =
        let avg =
            if squad.IsEmpty then 0.0
            else squad |> List.averageBy (fun x -> float x.CurrentSkill)

        let isEssential = float p.CurrentSkill >= avg - 5.0
        let newExpiry = if isEssential then season + rng.Next(2, 5) else season

        { p with Affiliation = Contracted(clubId, { c with ExpiryYear = newExpiry }) }

    let processContracts (rng: Random) (gs: GameState) : GameState =
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
                let updated = renewOrRelease rng gs.Season squad p clubId c
                GameState.updatePlayer updated state)
            gs
