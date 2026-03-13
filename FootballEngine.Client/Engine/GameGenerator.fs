namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.DomainTypes
open MatchStats


module GameGenerator =
    let createPlayer (seedRnd: Random) (id: int) (pos: Position) (clubId: ClubId) (nationality: CountryCode) : Player =
        let targetCA = nextNormalInt 110.0 20.0 40 180
        let potential = nextNormalInt (float targetCA + 10.0) 10.0 targetCA 200
        let b = float targetCA / 10.0

        let firsts = GameData.firstNames[nationality]
        let lasts = GameData.lastNames[nationality]

        let fullName =
            $"{firsts[seedRnd.Next(firsts.Length)]} {lasts[seedRnd.Next(lasts.Length)]}"

        let physical () =
            { Acceleration = nextNormalInt b 3.0 1 20
              Pace = nextNormalInt b 3.0 1 20
              Agility = nextNormalInt b 3.0 1 20
              Balance = nextNormalInt b 3.0 1 20
              JumpingReach = nextNormalInt b 4.0 1 20
              Stamina = nextNormalInt b 3.0 1 20
              Strength = nextNormalInt b 4.0 1 20 }

        let technical () =
            let t v = nextNormalInt v 3.0 1 20

            { Finishing = if pos = ST then t (b + 4.0) else t (b - 2.0)
              LongShots = t b
              Dribbling = t b
              BallControl = t b
              Passing = t b
              Crossing = if pos = AML || pos = AMR then t (b + 3.0) else t b
              Tackling = if pos = DC || pos = DM then t (b + 4.0) else t (b - 3.0)
              Marking = if pos = DC then t (b + 4.0) else t (b - 3.0)
              Heading = t b
              FreeKick = nextNormalInt 8.0 4.0 1 20
              Penalty = nextNormalInt 10.0 3.0 1 20 }

        let mental () =
            { Aggression = nextNormalInt 10.0 5.0 1 20
              Composure = nextNormalInt b 3.0 1 20
              Vision = nextNormalInt b 4.0 1 20
              Positioning = nextNormalInt b 3.0 1 20
              Bravery = nextNormalInt 10.0 4.0 1 20
              WorkRate = nextNormalInt 12.0 3.0 1 20
              Concentration = nextNormalInt b 2.0 1 20
              Leadership = nextNormalInt 5.0 5.0 1 20 }

        let goalkeeping () =
            let m = if pos = GK then b + 5.0 else 3.0

            { Reflexes = nextNormalInt m 2.0 1 20
              Handling = nextNormalInt m 2.0 1 20
              Kicking = nextNormalInt m 4.0 1 20
              OneOnOne = nextNormalInt m 3.0 1 20
              AerialReach = nextNormalInt m 3.0 1 20 }

        { Id = id
          ClubId = clubId
          Name = fullName
          Birthday = DateTime.Now.AddYears(-(17 + seedRnd.Next(18)))
          Nationality = nationality
          Position = pos
          PreferredFoot = if seedRnd.NextDouble() > 0.3 then Right else Left
          Height = 170 + seedRnd.Next(25)
          Weight = 65 + seedRnd.Next(25)
          Physical = physical ()
          Technical = technical ()
          Mental = mental ()
          Goalkeeping = goalkeeping ()
          Condition = 100
          MatchFitness = 100
          Morale = 70
          Status = Available
          CurrentSkill = targetCA
          PotentialSkill = potential
          Reputation = targetCA * 5
          Value = decimal (targetCA * targetCA * 1000)
          Salary = decimal (targetCA * 100)
          ContractExpiry = 2030
          TeamId = clubId }

    // ------------------------------------------------------------------ //
    //  Fixture generation                                                  //
    // ------------------------------------------------------------------ //

    /// Returns (fixtures, nextMatchId) — no mutable ref escaping.
    let generateRoundRobinFixtures
        (competitionId: CompetitionId)
        (clubIds: ClubId list)
        (startDate: DateTime)
        (firstMatchId: int)
        : MatchFixture list * int =

        let clubs =
            if clubIds.Length % 2 <> 0 then
                clubIds @ [ -1 ]
            else
                clubIds

        let n = clubs.Length
        let rounds = n - 1
        let half = n / 2
        let pool = Array.ofList clubs
        let mutable fixtures = []
        let mutable nextId = firstMatchId

        for round in 0 .. rounds - 1 do
            for i in 0 .. half - 1 do
                let home = pool[i]
                let away = pool[n - 1 - i]

                if home <> -1 && away <> -1 then
                    fixtures <-
                        { Id = nextId
                          CompetitionId = competitionId
                          Round = None
                          HomeClubId = home
                          AwayClubId = away
                          ScheduledDate = startDate.AddDays(float (round * 7))
                          Played = false
                          HomeScore = None
                          AwayScore = None
                          Events = [] }
                        :: fixtures

                    nextId <- nextId + 1

            // Rotate pool (keep pool[0] fixed)
            let last = pool[n - 1]

            for j in n - 1 .. -1 .. 2 do
                pool[j] <- pool[j - 1]

            pool[1] <- last

        List.rev fixtures, nextId

    // ------------------------------------------------------------------ //
    //  Club generation                                                     //
    // ------------------------------------------------------------------ //

    let private squadPositions =
        [ GK, 4
          DL, 4
          DC, 4
          DR, 4
          WBL, 4
          WBR, 4
          DM, 4
          MC, 4
          AML, 4
          AMR, 4
          ST, 4 ]
        |> List.collect (fun (pos, n) -> List.replicate n pos)

    let generateClub
        (seedRnd: Random)
        (clubId: ClubId)
        (levelIdx: int)
        (name: string)
        (country: CountryCode)
        (reputation: int)
        (playerIdStart: int ref)
        : Club * Map<PlayerId, Player> =

        let players =
            squadPositions
            |> List.map (fun pos ->
                let p = createPlayer seedRnd playerIdStart.Value pos clubId country
                playerIdStart.Value <- playerIdStart.Value + 1
                p)

        let club =
            { Id = clubId
              Name = name
              Nationality = country
              Reputation = reputation
              Players = players
              CurrentLineup = None
              Budget = if levelIdx = 0 then 50_000_000m else 10_000_000m
              Morale = 70
              Wins = 0
              Draws = 0
              Losses = 0
              GoalsFor = 0
              GoalsAgainst = 0 }

        let playerMap = players |> List.map (fun p -> p.Id, p) |> Map.ofList

        club, playerMap

    // ------------------------------------------------------------------ //
    //  World generation                                                    //
    // ------------------------------------------------------------------ //

    let generateNewGame
        (seedRnd: Random)
        (primaryCountry: CountryCode)
        (managerName: string)
        (secondaryCountries: CountryCode list)
        : GameState =

        let allCountries = (primaryCountry :: secondaryCountries) |> List.distinct
        let year = DateTime.Today.Year

        // Shared counters — local to this function, threaded explicitly.
        let playerIdCounter = ref 1
        let mutable clubIdCounter = 1
        let mutable competitionIdCounter = 1
        let mutable nextMatchId = 1

        let mutable allClubs: Map<ClubId, Club> = Map.empty
        let mutable allPlayers: Map<PlayerId, Player> = Map.empty
        let mutable allCompetitions: Map<CompetitionId, Competition> = Map.empty
        let mutable allFixtures: Map<MatchId, MatchFixture> = Map.empty

        for country in allCountries do
            let leaguesData = GameData.teamsByCountry[country]
            let leagueNames = GameData.leagueNames[country]
            let countryClubsFlat = leaguesData |> List.concat
            let totalClubs = float countryClubsFlat.Length

            for levelIdx, teamNames in List.indexed leaguesData do
                let competitionId = competitionIdCounter
                competitionIdCounter <- competitionIdCounter + 1
                let mutable leagueClubIds = []

                for name in teamNames do
                    let clubId = clubIdCounter
                    clubIdCounter <- clubIdCounter + 1

                    let rankPercentile =
                        float (countryClubsFlat |> List.findIndex ((=) name)) / totalClubs

                    let reputation = nextNormalInt (9500.0 - rankPercentile * 8500.0) 400.0 500 9999

                    let club, players =
                        generateClub seedRnd clubId levelIdx name country reputation playerIdCounter

                    let clubWithLineup =
                        FootballEngine.Client.AI.ManagerAI.ensureLineup
                            club
                            (FootballEngine.Client.AI.ManagerAI.pickBestFormation club)

                    leagueClubIds <- clubId :: leagueClubIds
                    allClubs <- Map.add clubId clubWithLineup allClubs
                    allPlayers <- Map.foldBack Map.add players allPlayers

                let level = if levelIdx = 0 then First else Second

                let competition: Competition =
                    { Id = competitionId
                      Name = leagueNames[levelIdx]
                      Type = NationalLeague level
                      Country = Some country
                      Season = year
                      ClubIds = leagueClubIds
                      Settings = Map.empty }

                allCompetitions <- Map.add competitionId competition allCompetitions

                let fixtures, nextId =
                    generateRoundRobinFixtures competitionId leagueClubIds (DateTime(year, 8, 1)) nextMatchId

                nextMatchId <- nextId

                for f in fixtures do
                    allFixtures <- Map.add f.Id f allFixtures

        let firstClubId =
            allCompetitions
            |> Map.toSeq
            |> Seq.tryPick (fun (_, comp) ->
                match comp.Type, comp.Country with
                | NationalLeague First, Some c when c = primaryCountry -> Some comp.ClubIds.Head
                | _ -> None)
            |> Option.defaultWith (fun () ->
                failwithf $"No first-division competition found for country '%s{primaryCountry}'")

        { CurrentDate = DateTime(year, 7, 1)
          Season = year
          Clubs = allClubs
          Players = allPlayers
          Competitions = allCompetitions
          Fixtures = allFixtures
          KnockoutTies = Map.empty
          Countries = Map.empty
          UserClubId = firstClubId
          ManagerName = managerName
          PrimaryCountry = primaryCountry }
