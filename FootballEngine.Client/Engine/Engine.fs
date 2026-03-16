namespace FootballEngine

open FootballEngine.Domain
open MatchSimulator

module Engine =

    let createPlayer = GameGenerator.createPlayer
    let generateNewGame = GameGenerator.generateNewGame

    let updateStanding (standing: LeagueStanding) myScore oppScore =
        if myScore > oppScore then
            { standing with
                Won = standing.Won + 1
                GoalsFor = standing.GoalsFor + myScore
                GoalsAgainst = standing.GoalsAgainst + oppScore
                Points = standing.Points + 3
                Played = standing.Played + 1 }
        elif myScore < oppScore then
            { standing with
                Lost = standing.Lost + 1
                GoalsFor = standing.GoalsFor + myScore
                GoalsAgainst = standing.GoalsAgainst + oppScore
                Played = standing.Played + 1 }
        else
            { standing with
                Drawn = standing.Drawn + 1
                GoalsFor = standing.GoalsFor + myScore
                GoalsAgainst = standing.GoalsAgainst + oppScore
                Points = standing.Points + 1
                Played = standing.Played + 1 }

    let private defaultStanding (clubId: ClubId) =
        { ClubId = clubId
          Played = 0
          Won = 0
          Drawn = 0
          Lost = 0
          GoalsFor = 0
          GoalsAgainst = 0
          Points = 0 }

    let simulateFixture
        (fixture: MatchFixture)
        (clubs: Map<ClubId, Club>)
        : Result<MatchFixture * int * int, SimulationError> =

        let home = clubs[fixture.HomeClubId]
        let away = clubs[fixture.AwayClubId]

        trySimulateMatch home away
        |> Result.map (fun (hScore, aScore, events) ->
            let updatedFixture =
                { fixture with
                    Played = true
                    HomeScore = Some hScore
                    AwayScore = Some aScore
                    Events = events }

            updatedFixture, hScore, aScore)

    type BatchResult =
        { GameState: GameState
          Logs: string list
          Errors: (MatchId * SimulationError) list }

    let simulateFixtures (gameState: GameState) (fixtures: (MatchId * MatchFixture) list) : BatchResult =
        let unplayed =
            fixtures
            |> List.filter (fun (id, _) ->
                gameState.Competitions
                |> Map.exists (fun _ comp ->
                    comp.Fixtures
                    |> Map.tryFind id
                    |> Option.map (fun f -> not f.Played)
                    |> Option.defaultValue false))

        let results =
            unplayed
            |> List.toArray
            |> Array.Parallel.map (fun (id, fixture) -> id, simulateFixture fixture gameState.Clubs)

        results
        |> Array.fold
            (fun (acc: BatchResult) (id, result) ->
                match result with
                | Error e ->
                    { acc with
                        Errors = (id, e) :: acc.Errors }

                | Ok(fixture, hScore, aScore) ->
                    let home = acc.GameState.Clubs[fixture.HomeClubId]
                    let away = acc.GameState.Clubs[fixture.AwayClubId]
                    let log = $"⚽ {home.Name} {hScore}-{aScore} {away.Name}"

                    let updatedComps =
                        acc.GameState.Competitions
                        |> Map.map (fun _ comp ->
                            if not (Map.containsKey id comp.Fixtures) then
                                comp
                            else
                                let homeStanding =
                                    comp.Standings
                                    |> Map.tryFind fixture.HomeClubId
                                    |> Option.defaultWith (fun () -> defaultStanding fixture.HomeClubId)

                                let awayStanding =
                                    comp.Standings
                                    |> Map.tryFind fixture.AwayClubId
                                    |> Option.defaultWith (fun () -> defaultStanding fixture.AwayClubId)

                                { comp with
                                    Fixtures = comp.Fixtures |> Map.add id fixture
                                    Standings =
                                        comp.Standings
                                        |> Map.add fixture.HomeClubId (updateStanding homeStanding hScore aScore)
                                        |> Map.add fixture.AwayClubId (updateStanding awayStanding aScore hScore) })

                    { acc with
                        BatchResult.GameState.Competitions = updatedComps
                        Logs = log :: acc.Logs })
            { GameState = gameState
              Logs = []
              Errors = [] }

    let isSeasonOver (gameState: GameState) =
        gameState.Competitions
        |> Map.forall (fun _ comp -> comp.Fixtures |> Map.forall (fun _ f -> f.Played))
