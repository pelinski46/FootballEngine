namespace FootballEngine

open System
open FootballEngine.Client.AI
open FootballEngine.Domain
open FSharp.Stats.Distributions


module MatchEngine =


    let nextNormalInt mean stdDev min max =
        let sample = Continuous.Normal.Sample mean stdDev
        Math.Clamp(int (Math.Round(sample)), min, max)

    let inline private effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let baseValue =
            float stat * (float condition / 100.0) * (0.8 + (float morale / 500.0))

        Continuous.Normal.Sample baseValue sigma

    let isDefender p =
        match p.Position with
        | DC
        | DL
        | DR
        | DM -> true
        | _ -> false

    let isMidfielder p =
        match p.Position with
        | MC
        | AMC
        | AML
        | AMR -> true
        | _ -> false

    let isAttacker p =
        match p.Position with
        | ST
        | AML
        | AMR -> true
        | _ -> false

    let private isCenterBack p =
        match p.Position with
        | DC
        | DL
        | DR -> true
        | _ -> false

    let private isDefensiveMid p =
        match p.Position with
        | MC
        | DM -> true
        | _ -> false

    type Possession =
        | Home
        | Away

    type MatchPhase =
        | BuildUp
        | Midfield
        | Attack

    type MatchState =
        { Home: Club
          Away: Club
          Second: int
          HomeScore: int
          AwayScore: int
          BallPosition: float * float
          Possession: Possession
          Momentum: float
          EventsRev: MatchEvent list }

    let inline private addEvent (ev: MatchEvent) (s: MatchState) =
        { s with EventsRev = ev :: s.EventsRev }

    let private flipPossession =
        function
        | Home -> Away
        | Away -> Home

    let inline private clampBallZone z = Math.Clamp(z, 0.0, 100.0)

    let inline private phaseFromBallZone (z: float) =
        if z < 30.0 || z > 70.0 then BuildUp
        elif z >= 40.0 && z <= 60.0 then Midfield
        else Attack


    let private calculateAttackEffort phase att attCond =
        match phase with
        | BuildUp ->
            effectiveStat att.Technical.Passing attCond att.Morale 2.0
            + effectiveStat att.Mental.Vision attCond att.Morale 1.5
            + effectiveStat att.Mental.Composure attCond att.Morale 1.0
        | Midfield ->
            effectiveStat att.Technical.BallControl attCond att.Morale 2.0
            + effectiveStat att.Mental.Vision attCond att.Morale 2.0
            + effectiveStat att.Technical.Dribbling attCond att.Morale 1.5
        | Attack ->
            effectiveStat att.Technical.Dribbling attCond att.Morale 2.5
            + effectiveStat att.Physical.Pace attCond att.Morale 2.0
            + effectiveStat att.Physical.Agility attCond att.Morale 1.5

    let private calculateDefenseEffort def defCond =
        effectiveStat def.Technical.Tackling defCond def.Morale 2.0
        + effectiveStat def.Mental.Positioning defCond def.Morale 2.0
        + effectiveStat def.Physical.Strength defCond def.Morale 1.0
        + effectiveStat def.Mental.Concentration defCond def.Morale 1.0

    let simulateMatch (home: Club) (away: Club) =
        let getLineupPlayers (club: Club) =
            match club.CurrentLineup with
            | None -> failwithf $"El club %s{club.Name} no tiene alineación definida"
            | Some lineup ->
                lineup.Slots
                |> List.choose (fun slot ->
                    slot.PlayerId
                    |> Option.bind (fun pid ->
                        club.Players
                        |> List.tryFind (fun p -> p.Id = pid)
                        |> Option.map (fun p -> (p, slot.X, slot.Y))))
                |> List.toArray

        let homePlayersWithPos = getLineupPlayers home
        let awayPlayersWithPos = getLineupPlayers away

        if homePlayersWithPos.Length <> 11 || awayPlayersWithPos.Length <> 11 then
            failwith "Ambos equipos deben tener exactamente 11 jugadores en la alineación"

        // Separar jugadores y posiciones
        let homePlayers = homePlayersWithPos |> Array.map (fun (p, _, _) -> p)
        let awayPlayers = awayPlayersWithPos |> Array.map (fun (p, _, _) -> p)

        let homePos =
            homePlayersWithPos
            |> Array.map (fun (_, x, y) ->
                let engineX = (1.0 - y) * 100.0
                let engineY = x * 100.0
                (engineX, engineY))

        let awayPos =
            awayPlayersWithPos
            |> Array.map (fun (_, x, y) ->
                let engineX = y * 100.0
                let engineY = x * 100.0
                (engineX, engineY))

        // Condiciones mutables
        let hCondition = homePlayers |> Array.map _.Condition
        let aCondition = awayPlayers |> Array.map _.Condition

        let getCondition playerId pos =
            match pos with
            | Home ->
                let idx = homePlayers |> Array.findIndex (fun p -> p.Id = playerId)
                hCondition[idx]
            | Away ->
                let idx = awayPlayers |> Array.findIndex (fun p -> p.Id = playerId)
                aCondition[idx]

        let applyFatigue (ballZone: float) =
            let homePressing = ballZone > 70.0
            let awayPressing = ballZone < 30.0

            let updateTeamFatigue (team: Player array) (condition: int array) isPressing =
                for i in 0..10 do
                    let p = team[i]
                    let baseFatigue = (100 - p.Physical.Stamina) / 20
                    let workRateFatigue = p.Mental.WorkRate / 15
                    let multiplier = if isPressing then 1.5 else 1.0
                    let totalFatigue = float (baseFatigue + workRateFatigue) * multiplier
                    condition[i] <- Math.Max(0, condition[i] - int totalFatigue)

            updateTeamFatigue homePlayers hCondition homePressing
            updateTeamFatigue awayPlayers aCondition awayPressing

        let distance (x1, y1) (x2, y2) =
            sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)

        let pickDuel (s: MatchState) =
            let ball = s.BallPosition
            let homeDists = homePos |> Array.map (distance ball)
            let awayDists = awayPos |> Array.map (distance ball)

            match s.Possession with
            | Home ->
                let attIdx = homeDists |> Array.mapi (fun i d -> i, d) |> Array.minBy snd |> fst
                let defIdx = awayDists |> Array.mapi (fun i d -> i, d) |> Array.minBy snd |> fst
                let att = homePlayers[attIdx]
                let def = awayPlayers[defIdx]
                (att, def, attIdx, defIdx)
            | Away ->
                let attIdx = awayDists |> Array.mapi (fun i d -> i, d) |> Array.minBy snd |> fst
                let defIdx = homeDists |> Array.mapi (fun i d -> i, d) |> Array.minBy snd |> fst
                let att = awayPlayers[attIdx]
                let def = homePlayers[defIdx]
                (att, def, attIdx, defIdx)

        let resolveDuel (att: Player, def: Player, attIdx, defIdx) (s: MatchState) =
            let attPos =
                if s.Possession = Home then
                    homePos[attIdx]
                else
                    awayPos[attIdx]

            let defPos =
                if s.Possession = Home then
                    awayPos[defIdx]
                else
                    homePos[defIdx]

            let attCond = getCondition att.Id s.Possession
            let defCond = getCondition def.Id (flipPossession s.Possession)

            let homeBonus =
                match s.Possession with
                | Home when home.Id = att.ClubId -> 3.0
                | Away when home.Id = def.ClubId -> 3.0
                | _ -> 0.0

            let momentumBonus = if s.Possession = Home then s.Momentum else -s.Momentum

            let attackEffort =
                calculateAttackEffort (phaseFromBallZone (fst s.BallPosition)) att attCond

            let defenseEffort = calculateDefenseEffort def defCond
            let duelDiff = (attackEffort + homeBonus + momentumBonus) - defenseEffort

            if duelDiff > 2.0 then
                // Atacante gana claramente: la pelota se mueve hacia su posición
                let moveX =
                    (fst attPos - fst s.BallPosition) * 0.5 + Continuous.Normal.Sample 0.0 10.0

                let moveY =
                    (snd attPos - snd s.BallPosition) * 0.5 + Continuous.Normal.Sample 0.0 10.0

                let newX = Math.Clamp(fst s.BallPosition + moveX, 0.0, 100.0)
                let newY = Math.Clamp(snd s.BallPosition + moveY, 0.0, 100.0)

                { s with
                    BallPosition = (newX, newY)
                    Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
            elif duelDiff > -2.0 then
                // Equilibrado: movimiento aleatorio pequeño
                let moveX = Continuous.Normal.Sample 0.0 3.0
                let moveY = Continuous.Normal.Sample 0.0 3.0
                let newX = clampBallZone (fst s.BallPosition + moveX)
                let newY = Math.Clamp(snd s.BallPosition + moveY, 0.0, 100.0)
                { s with BallPosition = (newX, newY) }
            else
                // Defensa gana: robo y pelota hacia el defensor
                let newPoss = flipPossession s.Possession

                let moveX =
                    (fst defPos - fst s.BallPosition) * 0.5 + Continuous.Normal.Sample 0.0 2.0

                let moveY =
                    (snd defPos - snd s.BallPosition) * 0.5 + Continuous.Normal.Sample 0.0 2.0

                let newX = Math.Clamp(fst s.BallPosition + moveX, 0.0, 100.0)
                let newY = Math.Clamp(snd s.BallPosition + moveY, 0.0, 100.0)

                { s with
                    Possession = newPoss
                    BallPosition = (newX, newY)
                    Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }

        let tryShot (s: MatchState) (attacker: Player) =
            let ballX, ballY = s.BallPosition

            let inChance =
                (s.Possession = Home && ballX >= 70.0) || (s.Possession = Away && ballX <= 30.0)

            if not inChance then
                s
            else
                let shooter = attacker
                // Encontrar al arquero rival
                let gkIdxRival =
                    if s.Possession = Home then
                        awayPlayers |> Array.findIndex (fun p -> p.Position = GK)
                    else
                        homePlayers |> Array.findIndex (fun p -> p.Position = GK)

                let gk =
                    if s.Possession = Home then
                        awayPlayers[gkIdxRival]
                    else
                        homePlayers[gkIdxRival]

                // Índices para condición
                let shooterIdx =
                    if s.Possession = Home then
                        homePlayers |> Array.findIndex (fun p -> p.Id = shooter.Id)
                    else
                        awayPlayers |> Array.findIndex (fun p -> p.Id = shooter.Id)

                let gkIdx =
                    if s.Possession = Home then
                        awayPlayers |> Array.findIndex (fun p -> p.Id = gk.Id)
                    else
                        homePlayers |> Array.findIndex (fun p -> p.Id = gk.Id)

                let shooterCond = if s.Possession = Home then hCondition else aCondition
                let gkCond = if s.Possession = Home then aCondition else hCondition

                let distanceToGoal = if s.Possession = Home then 100.0 - ballX else ballX
                let distancePenalty = distanceToGoal * 0.15

                let shotPower =
                    effectiveStat shooter.Technical.Finishing shooterCond[shooterIdx] shooter.Morale 3.5
                    + effectiveStat shooter.Mental.Composure shooterCond[shooterIdx] shooter.Morale 3.0
                    + effectiveStat shooter.Technical.LongShots shooterCond[shooterIdx] shooter.Morale 2.0
                    + effectiveStat shooter.Physical.Pace shooterCond[shooterIdx] shooter.Morale 1.5
                    - distancePenalty

                // Defensores rivales más cercanos al tirador (usamos los 3 más cercanos)
                let defendersWithPos =
                    if s.Possession = Home then
                        awayPlayers
                        |> Array.mapi (fun i p -> (p, awayPos[i]))
                        |> Array.filter (fun (p, _) -> isDefender p)
                    else
                        homePlayers
                        |> Array.mapi (fun i p -> (p, homePos[i]))
                        |> Array.filter (fun (p, _) -> isDefender p)

                let defCondArr = if s.Possession = Home then aCondition else hCondition

                let defenderPressure =
                    defendersWithPos
                    |> Array.map (fun (p, pos) -> (p, pos, distance (ballX, ballY) pos))
                    |> Array.sortBy (fun (_, _, d) -> d)
                    |> Array.truncate 1
                    |> Array.sumBy (fun (p, _, _) ->
                        let idx =
                            if s.Possession = Home then
                                awayPlayers |> Array.findIndex (fun p2 -> p2.Id = p.Id)
                            else
                                homePlayers |> Array.findIndex (fun p2 -> p2.Id = p.Id)

                        effectiveStat p.Technical.Marking defCondArr[idx] p.Morale 0.5)

                let savePower =
                    effectiveStat gk.Goalkeeping.Reflexes gkCond[gkIdx] gk.Morale 1.5
                    + effectiveStat gk.Goalkeeping.OneOnOne gkCond[gkIdx] gk.Morale 2.0

                    + defenderPressure

                let goalThreshold = Continuous.Normal.Sample 0.0 2.0

                if shotPower > savePower + goalThreshold then
                    // Gol
                    let clubId = shooter.ClubId

                    let s' =
                        { s with
                            HomeScore = if s.Possession = Home then s.HomeScore + 1 else s.HomeScore
                            AwayScore = if s.Possession = Away then s.AwayScore + 1 else s.AwayScore
                            BallPosition = (50.0, 50.0)
                            Possession = flipPossession s.Possession
                            Momentum =
                                if s.Possession = Home then
                                    s.Momentum + 3.0
                                else
                                    s.Momentum - 3.0 }
                        |> addEvent
                            { Second = s.Second
                              PlayerId = shooter.Id
                              ClubId = clubId
                              Type = Goal }

                    s'
                else
                    // Rechace, la pelota sale hacia un lateral
                    let newX =
                        if s.Possession = Home then
                            Continuous.Normal.Sample 75.0 5.0
                        else
                            Continuous.Normal.Sample 25.0 5.0

                    let newY = Math.Clamp(Continuous.Normal.Sample ballY 10.0, 0.0, 100.0)

                    { s with
                        BallPosition = (newX, newY)
                        Possession = flipPossession s.Possession }


        let mutable s =
            { Home = home
              Away = away
              Second = 0
              HomeScore = 0
              AwayScore = 0
              BallPosition = (50.0, 50.0)
              Possession = Home
              Momentum = 0.0
              EventsRev = [] }

        let totalSeconds = 95 * 60

        for second in 1..totalSeconds do
            s <- { s with Second = second }

            if second % 180 = 0 then
                applyFatigue (fst s.BallPosition)

            if second % 30 = 0 then
                let att, def, attIdx, defIdx = pickDuel s
                let sAfterDuel = resolveDuel (att, def, attIdx, defIdx) s

                let sAfterShot =
                    if sAfterDuel.Possession = s.Possession then
                        tryShot sAfterDuel att
                    else
                        sAfterDuel

                s <- sAfterShot

        (s.HomeScore, s.AwayScore, s.EventsRev)


open MatchEngine

module Engine =
    let createRegen (seedRnd: Random) id pos clubId (nationality: CountryCode) =
        let targetCA = nextNormalInt 80.0 25.0 40 180
        let potential = nextNormalInt (float targetCA + 15.0) 15.0 targetCA 200
        let b = float targetCA / 10.0


        let firsts = GameData.firstNames[nationality]
        let lasts = GameData.lastNames[nationality]

        let fullName =
            $"{firsts[seedRnd.Next(firsts.Length)]} {lasts[seedRnd.Next(lasts.Length)]}"

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
              Birthday = DateTime.Now.AddYears(-(17 + seedRnd.Next(18)))
              Nationality = nationality
              Position = pos
              PreferredFoot = if seedRnd.NextDouble() > 0.3 then Right else Left
              Height = 170 + seedRnd.Next(25)
              Weight = 65 + seedRnd.Next(25)
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



    let generateNewGame
        (seedRnd: Random)
        (primaryCountry: CountryCode)
        (managerName: string)
        (secondaryCountries: CountryCode list)
        =
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
                            let p = createRegen seedRnd playerIdCounter pos clubId country
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

                    let clubWithLineup = ManagerAI.ensureLineup club (ManagerAI.pickBestFormation club)
                    allClubs <- Map.add clubId clubWithLineup allClubs

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
            |> _.ClubIds.Head

        { CurrentDate = DateTime(2026, 7, 1)
          Clubs = allClubs
          Players = allPlayers
          Leagues = allLeagues
          Fixtures = allFixtures
          UserClubId = firstClubId
          ManagerName = managerName
          PrimaryCountry = primaryCountry }

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

    let simulateFixtures (gameState: GameState) fixtures =
        let results =
            fixtures
            |> List.toArray
            |> Array.Parallel.map (fun (id, fixture) ->
                let updatedFixture, updatedHome, updatedAway =
                    simulateFixture fixture gameState.Clubs

                let hScore = updatedFixture.HomeScore |> Option.defaultValue 0
                let aScore = updatedFixture.AwayScore |> Option.defaultValue 0

                id,
                updatedFixture,
                updatedHome,
                updatedAway,
                $"⚽ {updatedHome.Name} {hScore}-{aScore} {updatedAway.Name}")

        results
        |> Array.fold
            (fun (gs, logs) (id, fixture, home, away, log) ->
                { gs with
                    Fixtures = gs.Fixtures |> Map.add id fixture
                    Clubs = gs.Clubs |> Map.add home.Id home |> Map.add away.Id away },
                log :: logs)
            (gameState, [])
