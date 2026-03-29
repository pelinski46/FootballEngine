namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Lineup
open FootballEngine.Stats

module SeasonManager =



    let applyLeagueConsequences (gs: GameState) : GameState =
        let clubLevel =
            gs.Competitions
            |> Map.toList
            |> List.choose (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague(LeagueLevel lvl, _), Some _ -> Some(lvl, comp.ClubIds)
                | _ -> None)
            |> List.collect (fun (lvl, ids) -> ids |> List.map (fun id -> id, lvl))
            |> Map.ofList

        let budgetBonus =
            function
            | 0 -> 50_000_000m
            | 1 -> 15_000_000m
            | _ -> 3_000_000m

        let repDelta =
            function
            | 0 -> 300
            | 1 -> 0
            | _ -> -200

        { gs with
            Clubs =
                gs.Clubs
                |> Map.map (fun id club ->
                    match Map.tryFind id clubLevel with
                    | None -> club
                    | Some lvl ->
                        { club with
                            Budget = club.Budget + budgetBonus lvl * 0.3m
                            Reputation = clamp 100 9999 (club.Reputation + repDelta lvl) }) }

    let resetConditions (gs: GameState) : GameState =
        { gs with
            Players =
                gs.Players
                |> Map.map (fun _ p ->
                    { p with
                        Condition = 100
                        MatchFitness = 100
                        Status =
                            match p.Status with
                            | Suspended _ -> Available
                            | other -> other }) }

    let refreshAiLineups (gs: GameState) : GameState =
        gs.Clubs
        |> Map.keys
        |> Seq.filter (fun id -> id <> gs.UserClubId)
        |> Seq.fold
            (fun state clubId ->
                match GameState.headCoach clubId state with
                | None -> state
                | Some coach ->
                    let squad = GameState.getSquad clubId state
                    let updated = autoLineup coach squad (bestFormation squad)
                    GameState.updateStaff updated state)
            gs

    let computeSeasonSummary (gs: GameState) : string list =
        let clubName id =
            gs.Clubs |> Map.tryFind id |> Option.map _.Name |> Option.defaultValue "Unknown"

        let topOf (comp: Competition) =
            Competition.leader comp

        let bottomN n (comp: Competition) =
            Competition.bottomN n comp

        let countRelegation =
            List.sumBy (function
                | AutomaticRelegation n
                | PlayoffRelegation n -> n)

        let countPromotion =
            List.sumBy (function
                | AutomaticPromotion n
                | PlayoffPromotion n -> n)

        gs.Competitions
        |> Map.toList
        |> List.collect (fun (_, comp) ->
            match comp.Type with
            | NationalLeague(LeagueLevel 0, rules) ->
                [ match topOf comp with
                  | Some id -> yield $"{comp.Name} champion: {clubName id}"
                  | None -> ()
                  for id in bottomN (countRelegation rules.Relegation) comp do
                      yield $"Relegated from {comp.Name}: {clubName id}" ]

            | NationalLeague(_, rules) ->
                [ for id, _ in
                      Competition.rankedStandings comp
                      |> List.truncate (countPromotion rules.Promotion) do
                      yield $"Promoted from {comp.Name}: {clubName id}" ]

            | NationalCup _
            | InternationalCup _ ->
                [ match topOf comp with
                  | Some id -> yield $"{comp.Name} winner: {clubName id}"
                  | None -> () ])
