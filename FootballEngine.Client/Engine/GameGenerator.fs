namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Data
open MatchStats

module GameGenerator =

    type private Counters =
        { mutable ClubId: int
          mutable CompId: int
          mutable MatchId: int
          mutable PlayerId: int }

    let private squadBlueprint =
        [ GK, 2
          DC, 4
          DL, 2
          DR, 2
          WBL, 1
          WBR, 1
          DM, 2
          MC, 4
          AML, 2
          AMR, 2
          AMC, 2
          ST, 4 ]
        |> List.collect (fun (pos, n) -> List.replicate n pos)

    let private positionBias (pos: Position) (base': float) =
        let t v = nextNormalInt v 3.0 1 20
        let hi v = t (base' + v)
        let lo v = t (base' - v)

        fun (cat: string) (attr: string) ->
            match cat, attr, pos with
            | "tech", "Finishing", ST -> hi 4.0
            | "tech", "Finishing", _ -> lo 2.0
            | "tech", "Crossing", (AML | AMR) -> hi 3.0
            | "tech", "Tackling", (DC | DM) -> hi 4.0
            | "tech", "Tackling", _ -> lo 3.0
            | "tech", "Marking", DC -> hi 4.0
            | "tech", "Marking", _ -> lo 3.0
            | _ -> t base'

    let private buildPhysical (bias: string -> string -> int) =
        { Acceleration = bias "phys" "Acceleration"
          Pace = bias "phys" "Pace"
          Agility = bias "phys" "Agility"
          Balance = bias "phys" "Balance"
          JumpingReach = bias "phys" "JumpingReach"
          Stamina = bias "phys" "Stamina"
          Strength = bias "phys" "Strength" }

    let private buildTechnical (bias: string -> string -> int) =
        { Finishing = bias "tech" "Finishing"
          LongShots = bias "tech" "LongShots"
          Dribbling = bias "tech" "Dribbling"
          BallControl = bias "tech" "BallControl"
          Passing = bias "tech" "Passing"
          Crossing = bias "tech" "Crossing"
          Tackling = bias "tech" "Tackling"
          Marking = bias "tech" "Marking"
          Heading = bias "tech" "Heading"
          FreeKick = nextNormalInt 8.0 4.0 1 20
          Penalty = nextNormalInt 10.0 3.0 1 20 }

    let private buildMental (base': float) =
        { Aggression = nextNormalInt 10.0 5.0 1 20
          Composure = nextNormalInt base' 3.0 1 20
          Vision = nextNormalInt base' 4.0 1 20
          Positioning = nextNormalInt base' 3.0 1 20
          Bravery = nextNormalInt 10.0 4.0 1 20
          WorkRate = nextNormalInt 12.0 3.0 1 20
          Concentration = nextNormalInt base' 2.0 1 20
          Leadership = nextNormalInt 5.0 5.0 1 20 }

    let private buildGoalkeeping (pos: Position) (base': float) =
        let mean = if pos = GK then base' + 5.0 else 3.0

        { Reflexes = nextNormalInt mean 2.0 1 20
          Handling = nextNormalInt mean 2.0 1 20
          Kicking = nextNormalInt mean 4.0 1 20
          OneOnOne = nextNormalInt mean 3.0 1 20
          AerialReach = nextNormalInt mean 3.0 1 20 }

    let private playerValue (ca: int) =
        let x = max 0 (ca - 30) in decimal (x * x * 800)

    let private playerSalary (ca: int) =
        let x = max 0 (ca - 20) in decimal (x * x * 5 + 500)

    let private randomAge () = nextNormalInt 25.0 4.5 16 38

    let private contractExpiry (year: int) (age: int) (rnd: Random) =
        year + 1 + rnd.Next(max 1 (min 5 (38 - age)))

    let private caParamsForLevel =
        function
        | 0 -> 130.0, 18.0
        | 1 -> 105.0, 15.0
        | _ -> 85.0, 15.0

    let createPlayer
        (rnd: Random)
        (id: int)
        (pos: Position)
        (clubId: ClubId)
        (countryData: CountryData)
        (level: int)
        (year: int)
        : Player =
        let meanCa, sdCa = caParamsForLevel level
        let ca = nextNormalInt meanCa sdCa 40 180
        let potential = nextNormalInt (float ca + 15.0) 12.0 ca 200
        let base' = float ca / 10.0
        let bias = positionBias pos base'
        let age = randomAge ()
        let f = countryData.Names.FirstNames
        let l = countryData.Names.LastNames

        { Id = id
          ClubId = clubId
          Name = $"{f[rnd.Next(f.Length)]} {l[rnd.Next(l.Length)]}"
          Birthday = DateTime(year - age, rnd.Next(1, 13), rnd.Next(1, 29))
          Nationality = countryData.Country.Code
          Position = pos
          PreferredFoot = if rnd.NextDouble() > 0.3 then Right else Left
          Height = 170 + rnd.Next(25)
          Weight = 65 + rnd.Next(25)
          Physical = buildPhysical bias
          Technical = buildTechnical bias
          Mental = buildMental base'
          Goalkeeping = buildGoalkeeping pos base'
          Condition = 100
          MatchFitness = 100
          Morale = 70
          Status = Available
          CurrentSkill = ca
          PotentialSkill = potential
          Reputation = ca * 5
          Value = playerValue ca
          Salary = playerSalary ca
          ContractExpiry = contractExpiry year age rnd }

    // ── Fixture generation ────────────────────────────────────────────────

    let private makeFixture
        (firstMatchId: int)
        (idx: int)
        (compId: CompetitionId)
        (startDate: DateTime)
        (roundIdx: int)
        (homeId: ClubId)
        (awayId: ClubId)
        : MatchFixture =
        { Id = firstMatchId + idx
          CompetitionId = compId
          Round = None
          HomeClubId = homeId
          AwayClubId = awayId
          ScheduledDate = startDate.AddDays(float roundIdx * 7.0)
          Played = false
          HomeScore = None
          AwayScore = None
          Events = [] }

    let private singleRoundRobinPairs (clubIds: ClubId list) : (int * ClubId * ClubId) list =
        let teams =
            if clubIds.Length % 2 = 0 then
                Array.ofList clubIds
            else
                Array.append (Array.ofList clubIds) [| -1 |]

        let n = teams.Length
        let mutable rotating = teams[1..]
        let mutable result = []

        for round in 0 .. n - 2 do
            let pool = Array.append [| teams[0] |] rotating

            let pairs =
                [ for i in 0 .. n / 2 - 1 do
                      let h, a = pool[i], pool[n - 1 - i]

                      if h <> -1 && a <> -1 then
                          yield round, h, a ]

            result <- result @ pairs
            rotating <- Array.append [| rotating[rotating.Length - 1] |] rotating[.. rotating.Length - 2]

        result

    let private roundRobinPairs (clubIds: ClubId list) : (int * ClubId * ClubId) list =
        let firstLeg = singleRoundRobinPairs clubIds

        let rounds =
            (if clubIds.Length % 2 = 0 then
                 clubIds.Length
             else
                 clubIds.Length + 1)
            - 1

        [ for (round, h, a) in firstLeg do
              yield round, h, a
              yield round + rounds, a, h ]
        |> List.sortBy (fun (r, _, _) -> r)

    let generateLeagueFixtures
        (compId: CompetitionId)
        (clubIds: ClubId list)
        (startDate: DateTime)
        (firstMatchId: int)
        : MatchFixture list * int =
        let pairs = roundRobinPairs clubIds

        let fixtures =
            pairs
            |> List.mapi (fun i (roundIdx, h, a) -> makeFixture firstMatchId i compId startDate roundIdx h a)

        fixtures, firstMatchId + fixtures.Length

    let generateGroupStageFixtures
        (compId: CompetitionId)
        (clubIds: ClubId list)
        (groupRules: GroupRules)
        (startDate: DateTime)
        (firstMatchId: int)
        : MatchFixture list * int =
        let groupCount = groupRules.GroupCount

        let groups =
            clubIds
            |> List.chunkBySize (max 1 (clubIds.Length / groupCount))
            |> List.truncate groupCount

        let mutable nextId = firstMatchId
        let mutable allFixtures = []

        for groupIdx, group in groups |> List.indexed do
            let pairs = singleRoundRobinPairs group

            for (roundIdx, h, a) in pairs do
                let f =
                    { makeFixture nextId 0 compId (startDate.AddDays(float groupIdx * 1.0)) roundIdx h a with
                        Round = Some(GroupStage groupIdx) }

                allFixtures <- f :: allFixtures
                nextId <- nextId + 1

        List.rev allFixtures, nextId

    let private fixturesForFormat
        (compId: CompetitionId)
        (clubIds: ClubId list)
        (format: CupFormat)
        (startDate: DateTime)
        (firstMatchId: int)
        : MatchFixture list * int =
        match format with
        | StraightKnockout _ -> generateLeagueFixtures compId clubIds startDate firstMatchId
        | GroupThenKnockout(groupRules, _) ->
            generateGroupStageFixtures compId clubIds groupRules startDate firstMatchId

    // ── Club generation ───────────────────────────────────────────────────

    let private budgetForLevel =
        function
        | 0 -> 50_000_000m
        | 1 -> 15_000_000m
        | _ -> 3_000_000m

    let private reputationForClub (entry: ClubEntry) (rankPercentile: float) =
        match entry.Reputation with
        | Some r -> r
        | None -> nextNormalInt (9500.0 - rankPercentile * 8500.0) 400.0 500 9999

    let private generateClub
        (rnd: Random)
        (clubId: ClubId)
        (entry: ClubEntry)
        (countryData: CountryData)
        (reputation: int)
        (year: int)
        (counters: Counters)
        =
        let players =
            squadBlueprint
            |> List.map (fun pos ->
                let p =
                    createPlayer rnd counters.PlayerId pos clubId countryData entry.LeagueLevel year

                counters.PlayerId <- counters.PlayerId + 1
                p)

        { Id = clubId
          Name = entry.Name
          Nationality = countryData.Country.Code
          Reputation = reputation
          Players = players
          CurrentLineup = None
          Budget = budgetForLevel entry.LeagueLevel
          Morale = 70 },
        players |> List.map (fun p -> p.Id, p) |> Map.ofList

    // ── World accumulator ─────────────────────────────────────────────────

    type private WorldAcc =
        { Clubs: Map<ClubId, Club>
          Players: Map<PlayerId, Player>
          Competitions: Map<CompetitionId, Competition> }

    let private emptyWorld =
        { Clubs = Map.empty
          Players = Map.empty
          Competitions = Map.empty }

    let private addCompetition (comp: Competition) (acc: WorldAcc) =
        { acc with
            Competitions = acc.Competitions |> Map.add comp.Id comp }

    // ── League generation ─────────────────────────────────────────────────

    let private generateLeagues
        (rnd: Random)
        (year: int)
        (countryData: CountryData)
        (counters: Counters)
        (acc: WorldAcc)
        : WorldAcc =
        let clubsByLevel =
            countryData.Clubs |> List.groupBy _.LeagueLevel |> List.sortBy fst

        let totalClubs = float countryData.Clubs.Length

        (acc, clubsByLevel)
        ||> List.fold (fun acc (levelIdx, entries) ->
            let compId = counters.CompId
            counters.CompId <- counters.CompId + 1

            let accAfterClubs, leagueClubIds =
                ((acc, []), entries)
                ||> List.fold (fun (innerAcc, ids) entry ->
                    let cid = counters.ClubId
                    counters.ClubId <- counters.ClubId + 1

                    let rankPct =
                        countryData.Clubs
                        |> List.tryFindIndex (fun c -> c.Name = entry.Name)
                        |> Option.map (fun i -> float i / totalClubs)
                        |> Option.defaultValue 0.5

                    let club, players =
                        generateClub rnd cid entry countryData (reputationForClub entry rankPct) year counters

                    let clubWithLineup =
                        FootballEngine.Client.AI.ManagerAI.ensureLineup
                            club
                            (FootballEngine.Client.AI.ManagerAI.pickBestFormation club)

                    { innerAcc with
                        Clubs = innerAcc.Clubs |> Map.add cid clubWithLineup
                        Players = Map.foldBack Map.add players innerAcc.Players },
                    ids @ [ cid ])

            let fixtures, nextMatchId =
                generateLeagueFixtures compId leagueClubIds (DateTime(year, 8, 1)) counters.MatchId

            counters.MatchId <- nextMatchId

            let comp =
                { Id = compId
                  Name = countryData.LeagueNames[levelIdx]
                  Type = NationalLeague(LeagueLevel levelIdx, countryData.LeagueRules[levelIdx])
                  Country = Some countryData.Country.Code
                  Season = year
                  ClubIds = leagueClubIds
                  Fixtures = fixtures |> List.map (fun f -> f.Id, f) |> Map.ofList
                  Standings = Map.empty
                  KnockoutTies = Map.empty }

            addCompetition comp accAfterClubs)

    // ── National cups ─────────────────────────────────────────────────────

    let private generateNationalCups
        (year: int)
        (countryData: CountryData)
        (counters: Counters)
        (acc: WorldAcc)
        : WorldAcc =
        let leagueClubIds =
            acc.Competitions
            |> Map.toList
            |> List.collect (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague _, Some code when code = countryData.Country.Code -> comp.ClubIds
                | _ -> [])

        if leagueClubIds.IsEmpty then
            acc
        else

            (acc, countryData.Cups |> List.indexed)
            ||> List.fold (fun acc (cupIdx, cupFormat) ->
                let compId = counters.CompId
                counters.CompId <- counters.CompId + 1

                let slots = [ LeaguePosition(LeagueLevel 0, 1, leagueClubIds.Length) ]

                let fixtures, nextMatchId =
                    fixturesForFormat
                        compId
                        leagueClubIds
                        cupFormat
                        (DateTime(year, 10, 1).AddDays(float cupIdx * 3.0))
                        counters.MatchId

                counters.MatchId <- nextMatchId

                let cupName =
                    match countryData.Country.Code with
                    | "ARG" -> "Copa Argentina"
                    | "ENG" -> "FA Cup"
                    | "ESP" -> "Copa del Rey"
                    | "BRA" -> "Copa do Brasil"
                    | code -> $"Cup {code}"

                let comp =
                    { Id = compId
                      Name = cupName
                      Type = NationalCup(cupFormat, slots)
                      Country = Some countryData.Country.Code
                      Season = year
                      ClubIds = leagueClubIds
                      Fixtures = fixtures |> List.map (fun f -> f.Id, f) |> Map.ofList
                      Standings = Map.empty
                      KnockoutTies = Map.empty }

                addCompetition comp acc)

    // ── International cups ────────────────────────────────────────────────

    let private resolveSlots
        (slots: QualificationSlot list)
        (confOpt: Confederation option)
        (allCountryData: CountryData list)
        (competitions: Map<CompetitionId, Competition>)
        : ClubId list =
        let countryInConf (code: CountryCode) =
            match confOpt with
            | None -> true
            | Some conf ->
                allCountryData
                |> List.exists (fun c -> c.Country.Code = code && c.Country.Confederation = conf)

        let cupWinnerIds =
            competitions
            |> Map.toList
            |> List.choose (fun (_, comp) ->
                match comp.Type with
                | NationalCup _ ->
                    match comp.Country with
                    | Some code when countryInConf code -> comp.ClubIds |> List.tryHead
                    | _ -> None
                | _ -> None)

        let titleHolderIds =
            competitions
            |> Map.toList
            |> List.choose (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague(LeagueLevel 0, _), Some code when countryInConf code -> comp.ClubIds |> List.tryHead
                | _ -> None)

        slots
        |> List.collect (fun slot ->
            match slot with
            | LeaguePosition(LeagueLevel lvl, fromPos, toPos) ->
                competitions
                |> Map.toList
                |> List.choose (fun (_, comp) ->
                    match comp.Type, comp.Country with
                    | NationalLeague(LeagueLevel l, _), Some code when l = lvl && countryInConf code ->
                        comp.ClubIds |> List.truncate toPos |> List.skip (max 0 (fromPos - 1)) |> Some
                    | _ -> None)
                |> List.concat
            | TitleHolder -> titleHolderIds
            | CupWinner _ -> cupWinnerIds
            | ConfederationSlot _ -> [])
        |> List.distinct

    let private generateInternational
        (year: int)
        (name: string)
        (compType: CompetitionType)
        (allCountryData: CountryData list)
        (counters: Counters)
        (acc: WorldAcc)
        : WorldAcc =
        let compId = counters.CompId
        counters.CompId <- counters.CompId + 1

        let qualifyingClubIds, format =
            match compType with
            | InternationalCup(confOpt, fmt, slots) -> resolveSlots slots confOpt allCountryData acc.Competitions, fmt
            | _ -> [], StraightKnockout(SingleLeg ExtraTimeThenPenalties)

        if qualifyingClubIds.IsEmpty then
            acc
        else

            let fixtures, nextMatchId =
                fixturesForFormat compId qualifyingClubIds format (DateTime(year, 2, 1)) counters.MatchId

            counters.MatchId <- nextMatchId

            let comp =
                { Id = compId
                  Name = name
                  Type = compType
                  Country = None
                  Season = year
                  ClubIds = qualifyingClubIds
                  Fixtures = fixtures |> List.map (fun f -> f.Id, f) |> Map.ofList
                  Standings = Map.empty
                  KnockoutTies = Map.empty }

            addCompetition comp acc

    // ── Entry point ───────────────────────────────────────────────────────

    let generateNewGame
        (rnd: Random)
        (primaryCountry: CountryCode)
        (managerName: string)
        (secondaryCountries: CountryCode list)
        : GameState =
        let allCountryCodes = (primaryCountry :: secondaryCountries) |> List.distinct
        let allCountryData = allCountryCodes |> List.map DataRegistry.findCountry
        let year = DateTime.Today.Year

        let counters =
            { ClubId = 1
              CompId = 1
              MatchId = 1
              PlayerId = 1 }

        let world =
            (emptyWorld, allCountryData)
            ||> List.fold (fun acc cd -> generateLeagues rnd year cd counters acc)

        let world =
            (world, allCountryData)
            ||> List.fold (fun acc cd -> generateNationalCups year cd counters acc)

        let intlComps =
            [ "Copa Libertadores", International.CONMEBOL.libertadores
              "Copa Sudamericana", International.CONMEBOL.sudamericana
              "UEFA Champions League", International.UEFA.championsLeague
              "UEFA Europa League", International.UEFA.europaLeague ]

        let world =
            (world, intlComps)
            ||> List.fold (fun acc (name, ct) -> generateInternational year name ct allCountryData counters acc)

        let userClubId =
            world.Competitions
            |> Map.toSeq
            |> Seq.tryPick (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague(LeagueLevel 0, _), Some c when c = primaryCountry -> comp.ClubIds |> List.tryHead
                | _ -> None)
            |> Option.defaultWith (fun () -> failwithf $"No first-division competition found for '{primaryCountry}'")

        { CurrentDate = DateTime(year, 7, 1)
          Season = year
          Clubs = world.Clubs
          Players = world.Players
          Competitions = world.Competitions
          Countries = DataRegistry.countries
          UserClubId = userClubId
          ManagerName = managerName
          PrimaryCountry = primaryCountry }
