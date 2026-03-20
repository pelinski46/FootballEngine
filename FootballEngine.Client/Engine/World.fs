namespace FootballEngine

open System
open FootballEngine.Data
open FootballEngine.Domain
open FootballEngine.Domain.TransferNegotiation
open MatchStats

module World =

    let private clamp (min: int) (max: int) (value: int) : int = Math.Clamp(value, min, max)
    let private age (season: int) (p: Player) = season - p.Birthday.Year

    let private skillDelta (age: int) (skill: int) (potential: int) : int =
        let gap = potential - skill

        match age with
        | a when a <= 20 -> nextNormalInt (float (min gap 4)) 1.5 0 (min gap 5)
        | a when a <= 24 -> nextNormalInt (float (min gap 2)) 1.5 -1 (min gap 3)
        | a when a <= 27 -> nextNormalInt 0.5 1.0 -1 2
        | a when a <= 30 -> nextNormalInt -0.5 1.0 -2 1
        | a when a <= 33 -> nextNormalInt -1.5 1.0 -3 0
        | _ -> nextNormalInt -2.5 1.0 -4 -1

    let private maybeStat (rng: Random) (delta: int) (stat: int) : int =
        let threshold =
            if delta > 0 then
                0.35 * float delta
            else
                0.50 * float (abs delta)

        if rng.NextDouble() < threshold then
            clamp 1 20 (stat + (if delta > 0 then 1 else -1))
        else
            stat

    let private developStats (rng: Random) (delta: int) (pos: Position) (p: Player) : Player =
        let phys =
            if age 0 p > 28 then
                maybeStat rng (min delta 0)
            else
                maybeStat rng delta

        let tech = maybeStat rng delta
        let mental = maybeStat rng delta
        let gkOnly = if pos = GK then maybeStat rng delta else id

        let physical =
            match pos with
            | GK
            | DC
            | DM ->
                { p.Physical with
                    Strength = phys p.Physical.Strength
                    Stamina = phys p.Physical.Stamina }
            | ST
            | AML
            | AMR ->
                { p.Physical with
                    Pace = phys p.Physical.Pace
                    Agility = phys p.Physical.Agility }
            | _ ->
                { p.Physical with
                    Stamina = phys p.Physical.Stamina }

        let technical =
            match pos with
            | GK -> p.Technical
            | ST
            | AML
            | AMR ->
                { p.Technical with
                    Finishing = tech p.Technical.Finishing
                    Dribbling = tech p.Technical.Dribbling }
            | DC
            | DM ->
                { p.Technical with
                    Tackling = tech p.Technical.Tackling
                    Marking = tech p.Technical.Marking }
            | MC
            | AMC ->
                { p.Technical with
                    Passing = tech p.Technical.Passing
                    BallControl = tech p.Technical.BallControl }
            | _ ->
                { p.Technical with
                    Passing = tech p.Technical.Passing }

        { p with
            Physical = physical
            Technical = technical
            Mental =
                { p.Mental with
                    Vision = mental p.Mental.Vision
                    Positioning = mental p.Mental.Positioning }
            Goalkeeping =
                { p.Goalkeeping with
                    Reflexes = gkOnly p.Goalkeeping.Reflexes
                    OneOnOne = gkOnly p.Goalkeeping.OneOnOne
                    Handling = gkOnly p.Goalkeeping.Handling } }

    let developPlayer (rng: Random) (season: int) (p: Player) : Player =
        let a = age season p
        let delta = skillDelta a p.CurrentSkill p.PotentialSkill
        let newCA = clamp 1 200 (p.CurrentSkill + delta)

        let updated =
            { p with
                CurrentSkill = newCA
                Value = Player.playerValue newCA
                Salary =
                    if newCA > p.CurrentSkill then
                        Player.playerSalary newCA
                    else
                        p.Salary }

        developStats rng delta p.Position updated

    let developAllPlayers (rng: Random) (state: GameState) : GameState =
        let updated = state.Players |> Map.map (fun _ p -> developPlayer rng state.Season p)

        { state with
            Players = updated
            Clubs =
                state.Clubs
                |> Map.map (fun _ c ->
                    { c with
                        Players =
                            c.Players
                            |> List.map (fun p -> updated |> Map.tryFind p.Id |> Option.defaultValue p) }) }

    let updateMorale
        (homeScore: int)
        (awayScore: int)
        (homeId: ClubId)
        (awayId: ClubId)
        (state: GameState)
        : GameState =
        let winnerId =
            if homeScore > awayScore then Some homeId
            elif awayScore > homeScore then Some awayId
            else None

        let clubDelta id =
            if winnerId = Some id then 3
            elif winnerId.IsNone then 0
            else -2

        let playerDelta id =
            if winnerId = Some id then 2
            elif winnerId.IsNone then 0
            else -1

        { state with
            Clubs =
                state.Clubs
                |> Map.map (fun id c ->
                    if id = homeId || id = awayId then
                        { c with
                            Morale = clamp 0 100 (c.Morale + clubDelta id) }
                    else
                        c)
            Players =
                state.Players
                |> Map.map (fun _ p ->
                    if p.ClubId = homeId || p.ClubId = awayId then
                        { p with
                            Morale = clamp 0 100 (p.Morale + playerDelta p.ClubId) }
                    else
                        p) }

    let private baseRevenue (rep: int) : decimal =
        decimal (rep / 100 |> fun r -> r * r * 10_000 + 500_000)

    let private leagueFinishBonus (clubId: ClubId) (comps: Map<CompetitionId, Competition>) : decimal =
        comps
        |> Map.toList
        |> List.sumBy (fun (_, comp) ->
            match comp.Standings |> Map.tryFind clubId with
            | None -> 0m
            | Some _ ->
                comp.Standings
                |> Map.toList
                |> List.sortByDescending (fun (_, st) -> st.Points)
                |> List.findIndex (fun (id, _) -> id = clubId)
                |> (+) 1
                |> function
                    | 1 -> 2_000_000m
                    | 2 -> 1_000_000m
                    | 3 -> 500_000m
                    | _ -> 100_000m)

    let distributeRevenue (state: GameState) : GameState =
        { state with
            Clubs =
                state.Clubs
                |> Map.map (fun id c ->
                    { c with
                        Budget = c.Budget + baseRevenue c.Reputation + leagueFinishBonus id state.Competitions }) }

    let private isKeyPlayer (club: Club) (p: Player) =
        let avg = club.Players |> List.averageBy (fun x -> float x.CurrentSkill) |> int
        p.CurrentSkill >= avg - 5

    let processContracts (rng: Random) (state: GameState) : GameState =
        let expiring =
            state.Players
            |> Map.toList
            |> List.filter (fun (_, p) -> p.ContractExpiry <= state.Season && p.ClubId <> state.UserClubId)

        (state, expiring)
        ||> List.fold (fun acc (id, p) ->
            let newExpiry =
                if isKeyPlayer acc.Clubs[p.ClubId] p then
                    state.Season + rng.Next(2, 5)
                else
                    state.Season

            { acc with
                Players = acc.Players |> Map.add id { p with ContractExpiry = newExpiry } })

    // ── AI Transfer market ────────────────────────────────────────────────

    let private positionGroups =
        [ [ GK ]; [ DC; DL; DR; WBL; WBR; DM ]; [ ML; MC; MR; AML; AMR; AMC ]; [ ST ] ]

    let private squadStrengthByGroup (club: Club) (group: Position list) =
        club.Players
        |> List.filter (fun p -> List.contains p.Position group && p.Status = Available)
        |> List.sortByDescending _.CurrentSkill
        |> List.truncate 3
        |> List.sumBy _.CurrentSkill

    let private weakestPositionGroup (club: Club) =
        positionGroups |> List.minBy (squadStrengthByGroup club)

    let private isFreeAgent (season: int) (p: Player) = p.ContractExpiry <= season

    let private isTransferTarget (season: int) (p: Player) =
        isFreeAgent season p || p.ContractExpiry = season + 1

    let private applyTransfer
        (buyerId: ClubId)
        (sellerId: ClubId)
        (p: Player)
        (fee: decimal)
        (salary: decimal)
        (years: int)
        (state: GameState)
        : GameState =
        let buyer = state.Clubs[buyerId]
        let seller = state.Clubs[sellerId]

        let moved =
            { p with
                ClubId = buyerId
                Salary = salary
                ContractExpiry = state.Season + years
                Morale = clamp 0 100 (p.Morale + 5) }

        { state with
            Players = state.Players |> Map.add p.Id moved
            Clubs =
                state.Clubs
                |> Map.add
                    buyerId
                    { buyer with
                        Budget = buyer.Budget - fee
                        Players = moved :: (buyer.Players |> List.filter (fun x -> x.Id <> p.Id)) }
                |> Map.add
                    sellerId
                    { seller with
                        Budget = seller.Budget + fee
                        Players = seller.Players |> List.filter (fun x -> x.Id <> p.Id) } }

    let private tryAiSignFreeAgent (buyerId: ClubId) (p: Player) (state: GameState) : GameState =
        let buyer = state.Clubs[buyerId]
        let salary = suggestedSalary p

        if canAfford buyer 0m salary then
            applyTransfer buyerId p.ClubId p 0m salary 3 state
        else
            state

    let private tryAiBuyPlayer (rng: Random) (buyerId: ClubId) (p: Player) (state: GameState) : GameState =
        let buyer = state.Clubs[buyerId]
        let seller = state.Clubs[p.ClubId]
        let fee = p.Value * (0.9m + decimal (rng.NextDouble() * 0.2))
        let salary = suggestedSalary p

        if not (canAfford buyer fee salary) then
            state
        else
            match clubResponse buyer seller p fee with
            | AcceptedByClub ->
                match playerResponse buyer p salary with
                | ContractOffered(s, y) -> applyTransfer buyerId p.ClubId p fee s y state
                | _ -> state
            | _ -> state

    let private runClubTransfers (rng: Random) (buyerId: ClubId) (state: GameState) : GameState =
        let neededPositions = weakestPositionGroup state.Clubs[buyerId]

        let candidates =
            state.Players
            |> Map.toList
            |> List.map snd
            |> List.filter (fun p ->
                p.ClubId <> buyerId
                && List.contains p.Position neededPositions
                && p.Status = Available
                && isTransferTarget state.Season p)
            |> List.sortByDescending _.CurrentSkill
            |> List.truncate 3

        let freeAgents, forSale = candidates |> List.partition (isFreeAgent state.Season)

        let stateAfterFree =
            (state, freeAgents |> List.truncate 2)
            ||> List.fold (fun acc p -> tryAiSignFreeAgent buyerId p acc)

        (stateAfterFree, forSale |> List.truncate 1)
        ||> List.fold (fun acc p -> tryAiBuyPlayer rng buyerId p acc)

    let processTransfers (rng: Random) (state: GameState) : GameState =
        state.Clubs
        |> Map.toList
        |> List.map fst
        |> List.filter (fun id -> id <> state.UserClubId)
        |> List.sortBy (fun _ -> rng.Next())
        |> List.fold (fun acc id -> runClubTransfers rng id acc) state


    let private budgetForLevel =
        function
        | 0 -> 50_000_000m
        | 1 -> 15_000_000m
        | _ -> 3_000_000m

    let private reputationDeltaForLevel =
        function
        | 0 -> 300 // promoted to top flight — reputation boost
        | 1 -> 0 // second tier — neutral
        | _ -> -200 // lower leagues — slight reputation drop

    let applyLeagueConsequences (state: GameState) : GameState =
        // Build a map: clubId -> league level they're in after promotion/relegation
        let clubLevel =
            state.Competitions
            |> Map.toList
            |> List.choose (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague(LeagueLevel lvl, _), Some _ -> Some(lvl, comp.ClubIds)
                | _ -> None)
            |> List.collect (fun (lvl, ids) -> ids |> List.map (fun id -> id, lvl))
            |> Map.ofList

        { state with
            Clubs =
                state.Clubs
                |> Map.map (fun id club ->
                    match Map.tryFind id clubLevel with
                    | None -> club
                    | Some lvl ->
                        let budgetBonus = budgetForLevel lvl * 0.3m // 30% of tier budget as injection
                        let repDelta = reputationDeltaForLevel lvl

                        { club with
                            Budget = club.Budget + budgetBonus
                            Reputation = clamp 100 9999 (club.Reputation + repDelta) }) }

    // ── Youth generation ─────────────────────────────────────────────────

    let private youthPosition (rng: Random) =
        let positions = [| GK; DC; MC; ST; AML; AMR |]
        positions[rng.Next(positions.Length)]

    let generateYouth (rng: Random) (state: GameState) : GameState =
        let mutable nextId =
            state.Players
            |> Map.toList
            |> List.map fst
            |> fun ids -> if ids.IsEmpty then 1 else List.max ids + 1

        let newPlayers =
            state.Clubs
            |> Map.toList
            |> List.map (fun (clubId, club) ->
                let countryData =
                    state.Countries
                    |> Map.tryFind club.Nationality
                    |> Option.map (fun c -> DataRegistry.findCountry c.Code)
                    |> Option.defaultWith (fun () -> DataRegistry.findCountry club.Nationality)

                let raw =
                    GameGenerator.createPlayer rng nextId (youthPosition rng) clubId countryData 2 state.Season

                let youth =
                    { raw with
                        CurrentSkill = clamp 30 70 raw.CurrentSkill }

                nextId <- nextId + 1
                clubId, youth)

        (state, newPlayers)
        ||> List.fold (fun acc (clubId, p) ->
            { acc with
                Players = acc.Players |> Map.add p.Id p
                Clubs =
                    acc.Clubs
                    |> Map.add
                        clubId
                        { acc.Clubs[clubId] with
                            Players = p :: acc.Clubs[clubId].Players } })

    let private resetConditions (state: GameState) : GameState =
        { state with
            Players =
                state.Players
                |> Map.map (fun _ p ->
                    { p with
                        Condition = 100
                        MatchFitness = 100
                        Status =
                            match p.Status with
                            | Suspended _ -> Available
                            | other -> other }) }

    let computeSeasonSummary (state: GameState) : string list =
        let clubName id =
            state.Clubs
            |> Map.tryFind id
            |> Option.map _.Name
            |> Option.defaultValue "Unknown"

        let topOf (comp: Competition) =
            comp.Standings
            |> Map.toList
            |> List.sortByDescending (fun (_, s) -> s.Points, s.Won, -(s.GoalsAgainst - s.GoalsFor))
            |> List.tryHead
            |> Option.map fst

        let bottomN n (comp: Competition) =
            comp.Standings
            |> Map.toList
            |> List.sortBy (fun (_, s) -> s.Points, s.Won)
            |> List.truncate n
            |> List.map fst

        let countRelegation =
            List.sumBy (function
                | AutomaticRelegation n -> n
                | PlayoffRelegation n -> n)

        let countPromotion =
            List.sumBy (function
                | AutomaticPromotion n -> n
                | PlayoffPromotion n -> n)

        state.Competitions
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
                      comp.Standings
                      |> Map.toList
                      |> List.sortByDescending (fun (_, s) -> s.Points)
                      |> List.truncate (countPromotion rules.Promotion) do
                      yield $"Promoted from {comp.Name}: {clubName id}" ]

            | NationalCup _
            | InternationalCup _ ->
                [ match topOf comp with
                  | Some id -> yield $"{comp.Name} winner: {clubName id}"
                  | None -> () ])

    let refreshAiLineups (state: GameState) : GameState =
        { state with
            Clubs =
                state.Clubs
                |> Map.map (fun id club ->
                    if id = state.UserClubId then
                        club
                    else
                        Lineup.autoLineup club (Lineup.bestFormation club)) }

    let advanceSeason (rng: Random) (state: GameState) : GameState =
        state
        |> distributeRevenue
        |> developAllPlayers rng
        |> processContracts rng
        |> processTransfers rng
        |> generateYouth rng
        |> resetConditions
        |> refreshAiLineups
        |> fun s ->
            { s with
                Season = s.Season + 1
                CurrentDate = DateTime(s.Season + 1, 7, 1) }
