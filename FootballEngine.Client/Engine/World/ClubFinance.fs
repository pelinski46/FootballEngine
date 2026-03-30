namespace FootballEngine

open FootballEngine.Domain

type FinancialReport =
    { ClubId: ClubId
      Revenue: decimal
      WageBill: decimal
      NetChange: decimal }

module ClubFinance =

    let private tvRevenue (rep: int) : decimal =
        let r = rep / 100
        decimal (r * r * 12_000 + 400_000)

    let private sponsorshipRevenue (rep: int) : decimal =
        decimal rep * 150m

    let private matchDayRevenue (rep: int) (matchesPlayed: int) : decimal =
        decimal rep * 20m * decimal matchesPlayed

    let private leaguePositionBonus (clubId: ClubId) (comps: Map<CompetitionId, Competition>) : decimal =
        comps
        |> Map.toList
        |> List.sumBy (fun (_, comp) ->
            match comp.Type with
            | NationalLeague _ ->
                comp.Standings
                |> Map.tryFind clubId
                |> Option.map (fun _ ->
                    comp.Standings
                    |> Map.toList
                    |> List.sortByDescending (fun (_, s) -> s.Points)
                    |> List.findIndex (fun (id, _) -> id = clubId)
                    |> (+) 1
                    |> function
                        | 1 -> 3_000_000m
                        | 2 -> 1_500_000m
                        | 3 -> 800_000m
                        | n when n <= 6 -> 400_000m
                        | _ -> 100_000m)
                |> Option.defaultValue 0m
            | _ -> 0m)

    let private cupParticipationRevenue (clubId: ClubId) (comps: Map<CompetitionId, Competition>) : decimal =
        comps
        |> Map.values
        |> Seq.sumBy (fun comp ->
            match comp.Type with
            | NationalCup _ when comp.ClubIds |> List.contains clubId -> 150_000m
            | InternationalCup _ when comp.ClubIds |> List.contains clubId -> 500_000m
            | _ -> 0m)

    let private matchesPlayedCount (clubId: ClubId) (comps: Map<CompetitionId, Competition>) : int =
        comps
        |> Map.values
        |> Seq.sumBy (fun comp ->
            comp.Fixtures
            |> Map.values
            |> Seq.filter (fun f -> MatchFixture.isPlayed f && MatchFixture.involves clubId f)
            |> Seq.length)

    let private playerWageBill (players: Player list) : decimal =
        players
        |> List.sumBy (fun p ->
            Player.contractOf p |> Option.map _.Salary |> Option.defaultValue 0m)
        |> (*) 12m

    let private staffWageBill (staff: Staff list) : decimal =
        staff
        |> List.sumBy (fun s ->
            match s.Contract with
            | Some c -> c.Salary
            | None -> 0m)
        |> (*) 12m

    let computeReport
        (clubId: ClubId)
        (club: Club)
        (players: Player list)
        (staff: Staff list)
        (comps: Map<CompetitionId, Competition>)
        : FinancialReport =
        let played = matchesPlayedCount clubId comps
        let revenue =
            tvRevenue club.Reputation
            + sponsorshipRevenue club.Reputation
            + matchDayRevenue club.Reputation played
            + leaguePositionBonus clubId comps
            + cupParticipationRevenue clubId comps
        let wages = playerWageBill players + staffWageBill staff
        { ClubId = clubId
          Revenue = revenue
          WageBill = wages
          NetChange = revenue - wages }

    let distributeRevenue (gs: GameState) : GameState =
        let reports =
            gs.Clubs
            |> Map.toList
            |> List.map (fun (id, club) ->
                let players = GameState.getSquad id gs
                let staff = GameState.getStaff id gs
                computeReport id club players staff gs.Competitions)

        let deltas = reports |> List.map (fun r -> r.ClubId, r.NetChange) |> Map.ofList

        { gs with
            Clubs =
                gs.Clubs
                |> Map.map (fun id club ->
                    match Map.tryFind id deltas with
                    | Some delta -> Club.adjustBudget delta club
                    | None -> club) }
