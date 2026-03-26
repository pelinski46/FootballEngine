namespace FootballEngine

open System
open FootballEngine.Data
open FootballEngine.Domain
open FootballEngine.Generation
open FootballEngine.Lineup
open FootballEngine.Stats

module World =

    let private clamp (lo: int) (hi: int) (v: int) = Math.Clamp(v, lo, hi)
    let private age (season: int) (p: Player) = season - p.Birthday.Year

    let private skillDelta (age: int) (skill: int) (potential: int) : int =
        let gap = potential - skill

        match age with
        | a when a <= 20 -> normalInt (float (min gap 4)) 1.5 0 (min gap 5)
        | a when a <= 24 -> normalInt (float (min gap 2)) 1.5 -1 (min gap 3)
        | a when a <= 27 -> normalInt 0.5 1.0 -1 2
        | a when a <= 30 -> normalInt -0.5 1.0 -2 1
        | a when a <= 33 -> normalInt -1.5 1.0 -3 0
        | _ -> normalInt -2.5 1.0 -4 -1

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

        let updatedAffiliation =
            match p.Affiliation with
            | Contracted(clubId, c) when newCA > p.CurrentSkill ->
                Contracted(
                    clubId,
                    { c with
                        Salary = Player.playerSalary newCA }
                )
            | other -> other

        { p with
            CurrentSkill = newCA
            Affiliation = updatedAffiliation }
        |> developStats rng delta p.Position

    let developAllPlayers (rng: Random) (gs: GameState) : GameState =
        { gs with
            Players = gs.Players |> Map.map (fun _ p -> developPlayer rng gs.Season p) }

    let updateMorale (homeScore: int) (awayScore: int) (homeId: ClubId) (awayId: ClubId) (gs: GameState) : GameState =
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

        { gs with
            Clubs =
                gs.Clubs
                |> Map.map (fun id c ->
                    if id = homeId || id = awayId then
                        { c with
                            Morale = clamp 0 100 (c.Morale + clubDelta id) }
                    else
                        c)
            Players =
                gs.Players
                |> Map.map (fun _ p ->
                    match GameState.clubOf p with
                    | Some cid when cid = homeId || cid = awayId ->
                        { p with
                            Morale = clamp 0 100 (p.Morale + playerDelta cid) }
                    | _ -> p) }

    let private renewOrRelease
        (rng: Random)
        (season: int)
        (squad: Player list)
        (p: Player)
        (clubId: ClubId)
        (c: ContractInfo)
        : Player =
        let avg =
            if squad.IsEmpty then
                0.0
            else
                squad |> List.averageBy (fun x -> float x.CurrentSkill)

        let isEssential = float p.CurrentSkill >= avg - 5.0

        let newExpiry = if isEssential then season + rng.Next(2, 5) else season

        { p with
            Affiliation = Contracted(clubId, { c with ExpiryYear = newExpiry }) }

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
                let squad = GameState.getSquad clubId state
                let updated = renewOrRelease rng gs.Season squad p clubId c
                GameState.updatePlayer updated state)
            gs

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

    let private resetConditions (gs: GameState) : GameState =
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

    let advanceSeason (rng: Random) (gs: GameState) : GameState =
        gs
        |> ClubFinance.distributeRevenue
        |> TransferMarket.simulateSummerWindow
        |> developAllPlayers rng
        |> processContracts rng
        |> generateYouth rng
        |> applyLeagueConsequences
        |> BoardAI.runEndOfSeason
        |> resetConditions
        |> refreshAiLineups
        |> fun s ->
            { s with
                Season = s.Season + 1
                CurrentDate = DateTime(s.Season + 1, 7, 1) }
