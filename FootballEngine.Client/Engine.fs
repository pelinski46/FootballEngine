namespace FootballEngine

open System
open FootballEngine.Domain
open FSharp.Stats.Distributions

module Engine =

    let rnd = Random()


    let nextNormalInt mean stdDev min max =
        let sample = Continuous.Normal.Sample mean stdDev
        Math.Clamp(int (Math.Round(sample)), min, max)


    let createRegen id pos clubId (nationality: CountryCode) =



        let targetCA = nextNormalInt 80.0 25.0 40 180
        let potential = nextNormalInt (float targetCA + 15.0) 15.0 targetCA 200
        let b = float targetCA / 10.0


        let firsts = GameData.firstNames[nationality]
        let lasts = GameData.lastNames[nationality]

        let fullName = $"{firsts[rnd.Next(firsts.Length)]} {lasts[rnd.Next(lasts.Length)]}"

        let genPhysical () =
            { Acceleration = nextNormalInt b 3.0 1 20
              Pace = nextNormalInt b 3.0 1 20
              Agility = nextNormalInt b 3.0 1 20
              Balance = nextNormalInt b 3.0 1 20
              JumpingReach = nextNormalInt b 4.0 1 20
              Stamina = nextNormalInt b 3.0 1 20
              Strength = nextNormalInt b 4.0 1 20 }

        let genTechnical () =
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

        let genMental () =
            { Aggression = nextNormalInt 10.0 5.0 1 20
              Composure = nextNormalInt b 3.0 1 20
              Vision = nextNormalInt b 4.0 1 20
              Positioning = nextNormalInt b 3.0 1 20
              Bravery = nextNormalInt 10.0 4.0 1 20
              WorkRate = nextNormalInt 12.0 3.0 1 20
              Concentration = nextNormalInt b 2.0 1 20
              Leadership = nextNormalInt 5.0 5.0 1 20 }

        let genGK () =
            let m = if pos = GK then b + 5.0 else 3.0

            { Reflexes = nextNormalInt m 2.0 1 20
              Handling = nextNormalInt m 2.0 1 20
              Kicking = nextNormalInt m 4.0 1 20
              OneOnOne = nextNormalInt m 3.0 1 20
              AerialReach = nextNormalInt m 3.0 1 20 }

        let p =
            { Id = id
              ClubId = clubId
              Name = fullName
              Birthday = DateTime.Now.AddYears(-(17 + rnd.Next(18)))
              Nationality = nationality
              Position = pos
              PreferredFoot = if rnd.NextDouble() > 0.3 then Right else Left
              Height = 170 + rnd.Next(25)
              Weight = 65 + rnd.Next(25)
              Physical = genPhysical ()
              Technical = genTechnical ()
              Mental = genMental ()
              Goalkeeping = genGK ()
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

        { p with
            CurrentSkill = Player.calculateCurrentAbility p }

    let pickStartingXI (club: Club) =
        let getBest n pos =
            club.Players
            |> List.filter (fun p -> p.Position = pos)
            |> List.sortByDescending _.CurrentSkill
            |> List.take n

        let gk = getBest 1 GK
        let df = getBest 4 DC
        let md = getBest 4 MC
        let st = getBest 2 ST

        gk @ df @ md @ st
        |> List.take (Math.Min(11, club.Players.Length))
        |> List.toArray

    let inline private effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let baseValue =
            float stat * (float condition / 100.0) * (0.8 + (float morale / 500.0))

        Continuous.Normal.Sample baseValue sigma

    let simulateMatch (home: Club) (away: Club) =

        let h11 = pickStartingXI home
        let a11 = pickStartingXI away


        let hDefenders =
            h11
            |> Array.filter (fun p -> p.Position = DC || p.Position = DL || p.Position = DR || p.Position = DM)


        let aDefenders =
            a11
            |> Array.filter (fun p -> p.Position = DC || p.Position = DL || p.Position = DR || p.Position = DM)


        let hMidfielders =
            h11
            |> Array.filter (fun p -> p.Position = MC || p.Position = AMC || p.Position = AML || p.Position = AMR)


        let aMidfielders =
            a11
            |> Array.filter (fun p -> p.Position = MC || p.Position = AMC || p.Position = AML || p.Position = AMR)


        let hAttackers =
            h11
            |> Array.filter (fun p -> p.Position = ST || p.Position = AML || p.Position = AMR)


        let aAttackers =
            a11
            |> Array.filter (fun p -> p.Position = ST || p.Position = AML || p.Position = AMR)


        let hDMs = h11 |> Array.filter (fun p -> p.Position = MC || p.Position = DM)

        let aDMs = a11 |> Array.filter (fun p -> p.Position = MC || p.Position = DM)

        let hCBs =
            h11
            |> Array.filter (fun p -> p.Position = DC || p.Position = DL || p.Position = DR)


        let aCBs =
            a11
            |> Array.filter (fun p -> p.Position = DC || p.Position = DL || p.Position = DR)


        let hGK = h11 |> Array.find (fun p -> p.Position = GK)
        let aGK = a11 |> Array.find (fun p -> p.Position = GK)


        let mutable hScore, aScore = 0, 0
        let events = ResizeArray<MatchEvent>(10)
        let mutable ballZone, possessionHome = 50.0, true
        let mutable momentum = 0.0


        let hCondition = Array.init 11 (fun i -> h11[i].Condition)
        let aCondition = Array.init 11 (fun i -> a11[i].Condition)

        let hIdToIdx = h11 |> Array.mapi (fun i p -> p.Id, i) |> dict
        let aIdToIdx = a11 |> Array.mapi (fun i p -> p.Id, i) |> dict

        for min in 1..95 do
            if min % 3 = 0 then
                let homePressing = ballZone > 70.0
                let awayPressing = ballZone < 30.0

                for i in 0..10 do
                    let hp = h11[i]
                    let baseFatigue = (100 - hp.Physical.Stamina) / 20
                    let workRateFatigue = hp.Mental.WorkRate / 15
                    let multiplier = if homePressing then 1.5 else 1.0
                    let totalFatigue = float (baseFatigue + workRateFatigue) * multiplier
                    hCondition[i] <- Math.Max(0, hCondition[i] - int totalFatigue)

                    let ap = a11[i]
                    let baseFatigue2 = (100 - ap.Physical.Stamina) / 20
                    let workRateFatigue2 = ap.Mental.WorkRate / 15
                    let multiplier2 = if awayPressing then 1.5 else 1.0
                    let totalFatigue2 = float (baseFatigue2 + workRateFatigue2) * multiplier2
                    aCondition[i] <- Math.Max(0, aCondition[i] - int totalFatigue2)

            let att, def, attCond, defCond, phase =
                let attackingTeam = if possessionHome then h11 else a11
                let defendingTeam = if possessionHome then a11 else h11
                let attCondArr = if possessionHome then hCondition else aCondition
                let defCondArr = if possessionHome then aCondition else hCondition

                match ballZone with
                | z when z <= 30.0 || z >= 70.0 ->
                    let attPlayers = if possessionHome then hDefenders else aDefenders
                    let defPlayers = if possessionHome then aDMs else hDMs

                    let att =
                        if attPlayers.Length > 0 then
                            attPlayers |> Array.maxBy (fun p -> p.Technical.Passing)
                        else
                            attackingTeam[4]

                    let def =
                        if defPlayers.Length > 0 then
                            defPlayers |> Array.maxBy (fun p -> p.Mental.Positioning)
                        else
                            defendingTeam[5]

                    let attIdx =
                        if possessionHome then
                            hIdToIdx[att.Id]
                        else
                            aIdToIdx[att.Id]

                    let defIdx =
                        if possessionHome then
                            aIdToIdx[def.Id]
                        else
                            hIdToIdx[def.Id]

                    att, def, attCondArr[attIdx], defCondArr[defIdx], "buildup"

                | z when z > 30.0 && z < 70.0 ->
                    let attPlayers = if possessionHome then hMidfielders else aMidfielders
                    let defPlayers = if possessionHome then aDMs else hDMs

                    let att =
                        if attPlayers.Length > 0 then
                            attPlayers |> Array.maxBy (fun p -> p.Mental.Vision + p.Technical.Passing)
                        else
                            attackingTeam[6]

                    let def =
                        if defPlayers.Length > 0 then
                            defPlayers |> Array.maxBy (fun p -> p.Technical.Tackling + p.Mental.Positioning)
                        else
                            defendingTeam[5]

                    let attIdx =
                        if possessionHome then
                            hIdToIdx[att.Id]
                        else
                            aIdToIdx[att.Id]

                    let defIdx =
                        if possessionHome then
                            aIdToIdx[def.Id]
                        else
                            hIdToIdx[def.Id]

                    att, def, attCondArr[attIdx], defCondArr[defIdx], "midfield"

                | _ ->
                    let attPlayers = if possessionHome then hAttackers else aAttackers
                    let defPlayers = if possessionHome then aCBs else hCBs

                    let att =
                        if attPlayers.Length > 0 then
                            attPlayers |> Array.maxBy (fun p -> p.Physical.Pace + p.Technical.Dribbling)
                        else
                            attackingTeam[9]

                    let def =
                        if defPlayers.Length > 0 then
                            defPlayers |> Array.maxBy (fun p -> p.Physical.Pace + p.Technical.Marking)
                        else
                            defendingTeam[2]

                    let attIdx =
                        if possessionHome then
                            hIdToIdx[att.Id]
                        else
                            aIdToIdx[att.Id]

                    let defIdx =
                        if possessionHome then
                            aIdToIdx[def.Id]
                        else
                            hIdToIdx[def.Id]

                    att, def, attCondArr[attIdx], defCondArr[defIdx], "attack"

            let homeBonus =
                if
                    (possessionHome && home.Id = att.ClubId)
                    || (not possessionHome && home.Id = def.ClubId)
                then
                    2.0
                else
                    1.0

            let momentumBonus = if possessionHome then momentum else -momentum

            let attackEffort =
                match phase with
                | "buildup" ->
                    effectiveStat att.Technical.Passing attCond att.Morale 2.0
                    + effectiveStat att.Mental.Vision attCond att.Morale 1.5
                    + effectiveStat att.Mental.Composure attCond att.Morale 1.0
                | "midfield" ->
                    effectiveStat att.Technical.BallControl attCond att.Morale 2.0
                    + effectiveStat att.Mental.Vision attCond att.Morale 2.0
                    + effectiveStat att.Technical.Dribbling attCond att.Morale 1.5
                | "attack" ->
                    effectiveStat att.Technical.Dribbling attCond att.Morale 2.5
                    + effectiveStat att.Physical.Pace attCond att.Morale 2.0
                    + effectiveStat att.Physical.Agility attCond att.Morale 1.5
                | _ -> 10.0

            let defenseEffort =
                effectiveStat def.Technical.Tackling defCond def.Morale 2.0
                + effectiveStat def.Mental.Positioning defCond def.Morale 2.0
                + effectiveStat def.Physical.Strength defCond def.Morale 1.0
                + effectiveStat def.Mental.Concentration defCond def.Morale 1.0

            let totalAttack = attackEffort + homeBonus + momentumBonus
            let duelDiff = totalAttack - defenseEffort

            if duelDiff > 2.0 then
                let move = Continuous.Normal.Sample 12.0 6.0 |> Math.Abs

                ballZone <-
                    if possessionHome then
                        Math.Min(100.0, ballZone + move)
                    else
                        Math.Max(0.0, ballZone - move)

                momentum <- Math.Clamp(momentum + (if possessionHome then 0.5 else -0.5), -10.0, 10.0)
            elif duelDiff > -2.0 then
                let move = Continuous.Normal.Sample 5.0 3.0
                ballZone <- Math.Clamp(ballZone + (if possessionHome then move else -move), 0.0, 100.0)
            else
                possessionHome <- not possessionHome
                momentum <- Math.Clamp(momentum + (if possessionHome then -1.0 else 1.0), -10.0, 10.0)
                let move = Continuous.Normal.Sample 8.0 4.0 |> Math.Abs

                ballZone <-
                    if possessionHome then
                        Math.Min(100.0, ballZone + move)
                    else
                        Math.Max(0.0, ballZone - move)

            if (possessionHome && ballZone >= 88.0) || (not possessionHome && ballZone <= 12.0) then
                let shooterTeam = if possessionHome then hAttackers else aAttackers
                let gk = if possessionHome then aGK else hGK
                let shooterCond = if possessionHome then hCondition else aCondition
                let gkCond = if possessionHome then aCondition else hCondition
                let currentClub = if possessionHome then home else away

                if shooterTeam.Length > 0 then
                    let shooter = shooterTeam |> Array.maxBy (fun p -> p.Technical.Finishing)

                    let shooterIdx =
                        if possessionHome then
                            hIdToIdx[shooter.Id]
                        else
                            aIdToIdx[shooter.Id]

                    let gkIdx = if possessionHome then aIdToIdx[gk.Id] else hIdToIdx[gk.Id]

                    let distance = if ballZone >= 88.0 then 100.0 - ballZone else ballZone
                    let distancePenalty = distance * 0.3

                    let shotPower =
                        effectiveStat shooter.Technical.Finishing shooterCond[shooterIdx] shooter.Morale 3.0
                        + effectiveStat shooter.Mental.Composure shooterCond[shooterIdx] shooter.Morale 2.5
                        + effectiveStat shooter.Technical.LongShots shooterCond[shooterIdx] shooter.Morale 1.5
                        - distancePenalty

                    let defenders = if possessionHome then aCBs else hCBs
                    let defCond = if possessionHome then aCondition else hCondition

                    let defenderPressure =
                        defenders
                        |> Array.sumBy (fun p ->
                            let idx = if possessionHome then aIdToIdx[p.Id] else hIdToIdx[p.Id]
                            effectiveStat p.Technical.Marking defCond[idx] p.Morale 0.5)

                    let savePower =
                        effectiveStat gk.Goalkeeping.Reflexes gkCond[gkIdx] gk.Morale 3.0
                        + effectiveStat gk.Goalkeeping.OneOnOne gkCond[gkIdx] gk.Morale 2.5
                        + effectiveStat gk.Mental.Positioning gkCond[gkIdx] gk.Morale 2.0
                        + defenderPressure

                    let goalThreshold = Continuous.Normal.Sample 2.5 3.0

                    if shotPower > savePower + goalThreshold then
                        if possessionHome then
                            hScore <- hScore + 1
                        else
                            aScore <- aScore + 1

                        events.Add(
                            { Minute = min
                              PlayerId = shooter.Id
                              ClubId = currentClub.Id
                              Type = Goal }
                        )

                        ballZone <- 50.0
                        possessionHome <- not possessionHome
                        momentum <- if possessionHome then momentum - 3.0 else momentum + 3.0
                    else
                        ballZone <-
                            if possessionHome then
                                Continuous.Normal.Sample 75.0 5.0
                            else
                                Continuous.Normal.Sample 25.0 5.0

                        possessionHome <- not possessionHome
                else
                    ballZone <- if possessionHome then 75.0 else 25.0
                    possessionHome <- not possessionHome

        (hScore, aScore, Seq.toList events)



    let generateRoundRobinFixture (clubIds: ClubId list) (startDate: DateTime) =
        let n = clubIds.Length
        let clubs = if n % 2 <> 0 then clubIds @ [ -1 ] else clubIds
        let numTeams = clubs.Length
        let rounds = numTeams - 1
        let half = numTeams / 2
        let mutable fixtures = []
        let mutable matchId = 1
        let pool = List.toArray clubs

        for round in 0 .. rounds - 1 do
            for i in 0 .. half - 1 do
                let home = pool[i]
                let away = pool[numTeams - 1 - i]

                if home <> -1 && away <> -1 then
                    fixtures <-
                        { Id = matchId
                          HomeClubId = home
                          AwayClubId = away
                          ScheduledDate = startDate.AddDays(float (round * 7))
                          Played = false
                          HomeScore = None
                          AwayScore = None
                          Events = [] }
                        :: fixtures

                    matchId <- matchId + 1

            let last = pool[numTeams - 1]

            for j in numTeams - 1 .. -1 .. 2 do
                pool[j] <- pool[j - 1]

            pool[1] <- last

        fixtures |> List.rev



    let generateNewGame (primaryCountry: CountryCode) (managerName: string) (secondaryCountries: CountryCode list) =
        let mutable playerIdCounter = 1
        let mutable clubIdCounter = 1
        let mutable leagueIdCounter = 1
        let mutable matchIdCounter = 1

        let allCountries = primaryCountry :: secondaryCountries |> List.distinct
        let mutable allClubs = Map.empty
        let mutable allPlayers = Map.empty
        let mutable allLeagues = Map.empty
        let mutable allFixtures = Map.empty

        for country in allCountries do
            let leaguesData = GameData.teamsByCountry[country]
            let leagueNames = GameData.leagueNames[country]
            let countryClubsFlat = leaguesData |> List.concat
            let totalClubs = float countryClubsFlat.Length

            for levelIdx, teamNames in List.indexed leaguesData do
                let leagueId = leagueIdCounter
                leagueIdCounter <- leagueIdCounter + 1
                let mutable leagueClubIds = []

                for name in teamNames do
                    let clubId = clubIdCounter
                    clubIdCounter <- clubIdCounter + 1
                    leagueClubIds <- clubId :: leagueClubIds
                    let rankPos = countryClubsFlat |> List.findIndex (fun n -> n = name) |> float
                    let rankPercentile = rankPos / totalClubs
                    let targetRepMean = 9500.0 - (rankPercentile * 8500.0)
                    let clubRep = nextNormalInt targetRepMean 400.0 500 9999

                    let squadDistribution =
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

                    let expandPositions dist =
                        dist |> List.collect (fun (pos, n) -> List.replicate n pos)

                    let positions = expandPositions squadDistribution

                    let clubPlayers =
                        positions
                        |> List.map (fun pos ->
                            let p = createRegen playerIdCounter pos clubId country
                            playerIdCounter <- playerIdCounter + 1
                            allPlayers <- Map.add p.Id p allPlayers
                            p)

                    let club =
                        { Id = clubId
                          Name = name
                          Nationality = country
                          Reputation = clubRep
                          Players = clubPlayers
                          CurrentLineup = None
                          Budget = (if levelIdx = 0 then 50_000_000m else 10_000_000m)
                          Morale = 70
                          Wins = 0
                          Draws = 0
                          Losses = 0
                          GoalsFor = 0
                          GoalsAgainst = 0 }

                    allClubs <- Map.add clubId club allClubs

                let league =
                    { Id = leagueId
                      Name = leagueNames[levelIdx]
                      Level = (if levelIdx = 0 then First else Second)
                      ClubIds = leagueClubIds
                      Season = 2026
                      Nationality = country
                      IsPlayable = (country = primaryCountry) }

                allLeagues <- Map.add leagueId league allLeagues

                let startDate = DateTime(2026, 8, 1)
                let fixtures = generateRoundRobinFixture leagueClubIds startDate

                for f in fixtures do
                    let finalF = { f with Id = matchIdCounter }
                    allFixtures <- Map.add matchIdCounter finalF allFixtures
                    matchIdCounter <- matchIdCounter + 1

        let firstClubId =
            allLeagues
            |> Map.toSeq
            |> Seq.filter (fun (_, l) -> l.Nationality = primaryCountry && l.Level = First)
            |> Seq.head
            |> snd
            |> (fun l -> l.ClubIds.Head)

        { CurrentDate = DateTime(2026, 7, 1)
          Clubs = allClubs
          Players = allPlayers
          Leagues = allLeagues
          Fixtures = allFixtures
          UserClubId = firstClubId
          ManagerName = managerName
          PrimaryCountry = primaryCountry }



    let updateDaily (state: GameState) =
        let updatedPlayers =
            state.Players
            |> Map.map (fun _ p ->
                let recovered = Math.Clamp(p.Condition + 10, 0, 100)
                let age = Player.age state.CurrentDate p

                let devFactor =
                    if age < 23 then 0.05
                    elif age > 31 then -0.05
                    else 0.01

                let newSkill =
                    if rnd.NextDouble() < abs devFactor && p.CurrentSkill < p.PotentialSkill then
                        p.CurrentSkill + (if devFactor > 0.0 then 1 else -1)
                    else
                        p.CurrentSkill

                { p with
                    Condition = recovered
                    CurrentSkill = newSkill })

        { state with
            CurrentDate = state.CurrentDate.AddDays(1.0)
            Players = updatedPlayers }

    let simulateFixture (fixture: MatchFixture) (clubs: Map<ClubId, Club>) =
        let home = clubs[fixture.HomeClubId]
        let away = clubs[fixture.AwayClubId]
        let hScore, aScore, events = simulateMatch home away

        let updatedFixture =
            { fixture with
                Played = true
                HomeScore = Some hScore
                AwayScore = Some aScore
                Events = events }

        let update (c: Club) myS oppS =
            if myS > oppS then
                { c with
                    Wins = c.Wins + 1
                    GoalsFor = c.GoalsFor + myS
                    GoalsAgainst = c.GoalsAgainst + oppS }
            elif myS < oppS then
                { c with
                    Losses = c.Losses + 1
                    GoalsFor = c.GoalsFor + myS
                    GoalsAgainst = c.GoalsAgainst + oppS }
            else
                { c with
                    Draws = c.Draws + 1
                    GoalsFor = c.GoalsFor + myS
                    GoalsAgainst = c.GoalsAgainst + oppS }

        (updatedFixture, update home hScore aScore, update away aScore hScore)
