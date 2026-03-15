namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.DomainTypes
open MatchStats


module GameGenerator =

    // ── Squad shape ──────────────────────────────────────────────────────────

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

    // ── Stat helpers ─────────────────────────────────────────────────────────

    let private positionBias (pos: Position) (base': float) =
        let t v = nextNormalInt v 3.0 1 20
        let hi v = t (base' + v)
        let lo v = t (base' - v)

        fun (category: string) (attr: string) ->
            match category, attr, pos with
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
        let m = if pos = GK then base' + 5.0 else 3.0

        { Reflexes = nextNormalInt m 2.0 1 20
          Handling = nextNormalInt m 2.0 1 20
          Kicking = nextNormalInt m 4.0 1 20
          OneOnOne = nextNormalInt m 3.0 1 20
          AerialReach = nextNormalInt m 3.0 1 20 }

    // ── Value / salary curves ────────────────────────────────────────────────

    let private playerValue (ca: int) : decimal =
        let x = max 0 (ca - 30)
        decimal (x * x * 800)

    let private playerSalary (ca: int) : decimal =
        let x = max 0 (ca - 20)
        decimal (x * x * 5 + 500)

    // ── Age / contract ───────────────────────────────────────────────────────

    let private randomAge () : int = nextNormalInt 25.0 4.5 16 38

    let private contractExpiry (year: int) (age: int) (rnd: Random) : int =
        let maxYears = max 1 (min 5 (38 - age))
        year + 1 + rnd.Next(maxYears)

    // ── CA target by level ───────────────────────────────────────────────────

    let private caParamsForLevel (levelIdx: int) : float * float =
        match levelIdx with
        | 0 -> 130.0, 18.0
        | 1 -> 105.0, 15.0
        | _ -> 85.0, 15.0

    // ── Player creation ──────────────────────────────────────────────────────

    let createPlayer
        (rnd: Random)
        (id: int)
        (pos: Position)
        (clubId: ClubId)
        (nationality: CountryCode)
        (levelIdx: int)
        (year: int)
        : Player =

        let meanCa, sdCa = caParamsForLevel levelIdx
        let ca = nextNormalInt meanCa sdCa 40 180
        let potential = nextNormalInt (float ca + 15.0) 12.0 ca 200
        let base' = float ca / 10.0
        let bias = positionBias pos base'
        let age = randomAge ()

        { Id = id
          ClubId = clubId
          Name =
            let f = GameData.firstNames[nationality]
            let l = GameData.lastNames[nationality]
            $"{f[rnd.Next(f.Length)]} {l[rnd.Next(l.Length)]}"
          Birthday = DateTime(year - age, rnd.Next(1, 13), rnd.Next(1, 29))
          Nationality = nationality
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
          ContractExpiry = contractExpiry year age rnd
          TeamId = clubId }

    // ── Fixture generation ───────────────────────────────────────────────────

    let generateRoundRobinFixtures
        (competitionId: CompetitionId)
        (clubIds: ClubId list)
        (startDate: DateTime)
        (firstMatchId: int)
        : MatchFixture list * int =

        let teams =
            if clubIds.Length % 2 = 0 then
                Array.ofList clubIds
            else
                Array.append (Array.ofList clubIds) [| -1 |]

        let n = teams.Length
        let rounds = n - 1

        let rotate (pool: int[]) =
            Array.append [| pool[pool.Length - 1] |] pool[1..] |> Array.append [| pool[0] |]

        let roundPairs (pool: int[]) =
            [| yield pool[0], pool[n - 1]
               for i in 1 .. n / 2 - 1 do
                   yield pool[i], pool[n - 1 - i] |]
            |> Array.choose (fun (a, b) -> if a = -1 || b = -1 then None else Some(a, b))

        let firstLegRounds =
            (([], Array.tail teams), [ 0 .. rounds - 1 ])
            ||> List.fold (fun (acc, pool) round ->
                let fullPool = Array.append [| teams[0] |] pool
                let pairs = roundPairs fullPool |> Array.toList |> List.map (fun p -> round, p)
                pairs @ acc, rotate pool)
            |> fst
            |> List.rev

        let allPairs =
            [ for round, (home, away) in firstLegRounds do
                  yield round, home, away
                  yield round + rounds, away, home ]
            |> List.sortBy (fun (round, _, _) -> round)

        let toFixture idx (roundIdx, homeId, awayId) =
            { Id = firstMatchId + idx
              CompetitionId = competitionId
              Round = None
              HomeClubId = homeId
              AwayClubId = awayId
              ScheduledDate = startDate.AddDays(float roundIdx * 7.0)
              Played = false
              HomeScore = None
              AwayScore = None
              Events = [] }

        let fixtures = allPairs |> List.mapi toFixture

        fixtures, firstMatchId + fixtures.Length

    // ── Club generation ──────────────────────────────────────────────────────

    let private budgetForLevel (levelIdx: int) : decimal =
        match levelIdx with
        | 0 -> 50_000_000m
        | 1 -> 15_000_000m
        | _ -> 3_000_000m

    let generateClub
        (rnd: Random)
        (clubId: ClubId)
        (levelIdx: int)
        (name: string)
        (country: CountryCode)
        (reputation: int)
        (year: int)
        (playerIdCounter: int ref)
        : Club * Map<PlayerId, Player> =

        let players =
            squadBlueprint
            |> List.map (fun pos ->
                let p = createPlayer rnd playerIdCounter.Value pos clubId country levelIdx year
                playerIdCounter.Value <- playerIdCounter.Value + 1
                p)

        let club =
            { Id = clubId
              Name = name
              Nationality = country
              Reputation = reputation
              Players = players
              CurrentLineup = None
              Budget = budgetForLevel levelIdx
              Morale = 70
              Wins = 0
              Draws = 0
              Losses = 0
              GoalsFor = 0
              GoalsAgainst = 0 }

        club, players |> List.map (fun p -> p.Id, p) |> Map.ofList

    // ── World accumulator ────────────────────────────────────────────────────

    type private WorldAcc =
        { Clubs: Map<ClubId, Club>
          Players: Map<PlayerId, Player>
          Competitions: Map<CompetitionId, Competition>
          Fixtures: Map<MatchId, MatchFixture> }

    let private emptyWorld =
        { Clubs = Map.empty
          Players = Map.empty
          Competitions = Map.empty
          Fixtures = Map.empty }

    // ── World generation ─────────────────────────────────────────────────────

    let generateNewGame
        (rnd: Random)
        (primaryCountry: CountryCode)
        (managerName: string)
        (secondaryCountries: CountryCode list)
        : GameState =

        let allCountries = (primaryCountry :: secondaryCountries) |> List.distinct
        let year = DateTime.Today.Year
        let playerIdCounter = ref 1
        let mutable clubId = 1
        let mutable compId = 1
        let mutable matchId = 1

        let world =
            (emptyWorld, allCountries)
            ||> List.fold (fun acc country ->
                let leaguesData = GameData.teamsByCountry[country]
                let leagueNames = GameData.leagueNames[country]
                let allNames = leaguesData |> List.concat
                let totalClubs = float allNames.Length

                (acc, List.indexed leaguesData)
                ||> List.fold (fun acc (levelIdx, teamNames) ->
                    let competitionId = compId
                    compId <- compId + 1

                    let mutable leagueClubIds = []

                    let acc =
                        (acc, teamNames)
                        ||> List.fold (fun acc name ->
                            let currentClubId = clubId
                            clubId <- clubId + 1

                            let rankPercentile =
                                match allNames |> List.tryFindIndex ((=) name) with
                                | Some idx -> float idx / totalClubs
                                | None -> 0.5

                            let reputation = nextNormalInt (9500.0 - rankPercentile * 8500.0) 400.0 500 9999

                            let club, players =
                                generateClub rnd currentClubId levelIdx name country reputation year playerIdCounter

                            let clubWithLineup =
                                FootballEngine.Client.AI.ManagerAI.ensureLineup
                                    club
                                    (FootballEngine.Client.AI.ManagerAI.pickBestFormation club)

                            leagueClubIds <- currentClubId :: leagueClubIds

                            { acc with
                                Clubs = acc.Clubs |> Map.add currentClubId clubWithLineup
                                Players = Map.foldBack Map.add players acc.Players })

                    let competition: Competition =
                        { Id = competitionId
                          Name = leagueNames[levelIdx]
                          Type = NationalLeague(if levelIdx = 0 then First else Second)
                          Country = Some country
                          Season = year
                          ClubIds = leagueClubIds
                          Settings = Map.empty }

                    let fixtures, nextId =
                        generateRoundRobinFixtures competitionId leagueClubIds (DateTime(year, 8, 1)) matchId

                    matchId <- nextId

                    { acc with
                        Competitions = acc.Competitions |> Map.add competitionId competition
                        Fixtures = (acc.Fixtures, fixtures) ||> List.fold (fun m f -> Map.add f.Id f m) }))

        let userClubId =
            world.Competitions
            |> Map.toSeq
            |> Seq.tryPick (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague First, Some c when c = primaryCountry -> Some comp.ClubIds.Head
                | _ -> None)
            |> Option.defaultWith (fun () -> failwithf $"No first-division competition found for '%s{primaryCountry}'")

        { CurrentDate = DateTime(year, 7, 1)
          Season = year
          Clubs = world.Clubs
          Players = world.Players
          Competitions = world.Competitions
          Fixtures = world.Fixtures
          KnockoutTies = Map.empty
          Countries = Map.empty
          UserClubId = userClubId
          ManagerName = managerName
          PrimaryCountry = primaryCountry }
