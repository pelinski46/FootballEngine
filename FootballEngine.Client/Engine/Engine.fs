namespace FootballEngine

open FootballEngine.Domain
open MatchSimulator

module Engine =

    let createPlayer = GameGenerator.createPlayer
    let generateNewGame = GameGenerator.generateNewGame
    let advanceSeason = World.advanceSeason
    let regenerateSeasonFixtures = GameGenerator.regenerateSeasonFixtures
    let computeSeasonSummary = World.computeSeasonSummary

    let private ensureLineup (userClubId: ClubId) (club: Club) =
        let isComplete =
            club.CurrentLineup
            |> Option.map (fun lu -> lu.Slots |> List.filter (fun s -> s.PlayerId.IsSome) |> List.length = 11)
            |> Option.defaultValue false

        if isComplete || club.Id = userClubId then
            club
        else
            Lineup.autoLineup club (Lineup.bestFormation club)

    let updateStanding (standing: LeagueStanding) myScore oppScore =
        let base' =
            { standing with
                Played = standing.Played + 1
                GoalsFor = standing.GoalsFor + myScore
                GoalsAgainst = standing.GoalsAgainst + oppScore }

        if myScore > oppScore then
            { base' with
                Won = standing.Won + 1
                Points = standing.Points + 3 }
        elif myScore < oppScore then
            { base' with Lost = standing.Lost + 1 }
        else
            { base' with
                Drawn = standing.Drawn + 1
                Points = standing.Points + 1 }

    let private emptyStanding (clubId: ClubId) =
        { ClubId = clubId
          Played = 0
          Won = 0
          Drawn = 0
          Lost = 0
          GoalsFor = 0
          GoalsAgainst = 0
          Points = 0 }

    let simulateFixture (fixture: MatchFixture) (clubs: Map<ClubId, Club>) =
        trySimulateMatch clubs[fixture.HomeClubId] clubs[fixture.AwayClubId]
        |> Result.map (fun (h, a, evs) ->
            { fixture with
                Played = true
                HomeScore = Some h
                AwayScore = Some a
                Events = evs },
            h,
            a)

    let private simulateFixtureWithFallback (userClubId: ClubId) (fixture: MatchFixture) (clubs: Map<ClubId, Club>) =
        let home = clubs[fixture.HomeClubId] |> ensureLineup userClubId
        let away = clubs[fixture.AwayClubId] |> ensureLineup userClubId

        trySimulateMatch home away
        |> Result.map (fun (h, a, evs) ->
            { fixture with
                Played = true
                HomeScore = Some h
                AwayScore = Some a
                Events = evs },
            h,
            a)

    type BatchResult =
        { GameState: GameState
          Logs: string list
          Errors: (MatchId * SimulationError) list }

    type private MatchOutcome =
        { FixtureId: MatchId
          Fixture: MatchFixture
          HomeScore: int
          AwayScore: int }

    let private applyOutcomes
        (fixtureToComp: Map<MatchId, CompetitionId>)
        (outcomes: MatchOutcome[])
        (gs: GameState)
        : GameState =

        let updatedComps =
            (gs.Competitions, outcomes)
            ||> Array.fold (fun comps o ->
                match Map.tryFind o.FixtureId fixtureToComp with
                | None -> comps
                | Some compId ->
                    let comp = comps[compId]

                    let hs =
                        comp.Standings
                        |> Map.tryFind o.Fixture.HomeClubId
                        |> Option.defaultWith (fun () -> emptyStanding o.Fixture.HomeClubId)

                    let as' =
                        comp.Standings
                        |> Map.tryFind o.Fixture.AwayClubId
                        |> Option.defaultWith (fun () -> emptyStanding o.Fixture.AwayClubId)

                    comps
                    |> Map.add
                        compId
                        { comp with
                            Fixtures = comp.Fixtures |> Map.add o.FixtureId o.Fixture
                            Standings =
                                comp.Standings
                                |> Map.add o.Fixture.HomeClubId (updateStanding hs o.HomeScore o.AwayScore)
                                |> Map.add o.Fixture.AwayClubId (updateStanding as' o.AwayScore o.HomeScore) })

        let gsWithComps = { gs with Competitions = updatedComps }

        (gsWithComps, outcomes)
        ||> Array.fold (fun acc o ->
            World.updateMorale o.HomeScore o.AwayScore o.Fixture.HomeClubId o.Fixture.AwayClubId acc)

    let simulateFixtures (gameState: GameState) (fixtures: (MatchId * MatchFixture) list) : BatchResult =
        let fixtureToComp =
            gameState.Competitions
            |> Map.toList
            |> List.collect (fun (compId, comp) ->
                comp.Fixtures |> Map.toList |> List.map (fun (fid, _) -> fid, compId))
            |> Map.ofList

        let unplayed =
            fixtures
            |> List.filter (fun (id, _) ->
                gameState.Competitions
                |> Map.exists (fun _ comp ->
                    match comp.Fixtures |> Map.tryFind id with
                    | Some f -> not f.Played
                    | None -> false))

        let simResults =
            unplayed
            |> List.toArray
            |> Array.Parallel.map (fun (id, fixture) ->
                id, simulateFixtureWithFallback gameState.UserClubId fixture gameState.Clubs)

        let outcomes, errors, logs =
            simResults
            |> Array.fold
                (fun (outs, errs, ls) (id, result) ->
                    match result with
                    | Error e -> outs, (id, e) :: errs, ls
                    | Ok(fixture, h, a) ->
                        let home = gameState.Clubs[fixture.HomeClubId]
                        let away = gameState.Clubs[fixture.AwayClubId]

                        let out =
                            { FixtureId = id
                              Fixture = fixture
                              HomeScore = h
                              AwayScore = a }

                        out :: outs, errs, $"{home.Name} {h}-{a} {away.Name}" :: ls)
                ([], [], [])

        if errors.IsEmpty then
            { GameState = applyOutcomes fixtureToComp (List.toArray outcomes) gameState
              Logs = logs
              Errors = [] }
        else
            { GameState = gameState
              Logs = []
              Errors = errors }
