namespace FootballEngine

open FootballEngine.Domain
open MatchSimulator

module Engine =

    let createPlayer = GameGenerator.createPlayer
    let generateNewGame = GameGenerator.generateNewGame
    let advanceSeason = World.advanceSeason
    let regenerateSeasonFixtures = GameGenerator.regenerateSeasonFixtures
    let computeSeasonSummary = World.computeSeasonSummary

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

        unplayed
        |> List.toArray
        |> Array.Parallel.map (fun (id, fixture) -> id, simulateFixture fixture gameState.Clubs)
        |> Array.fold
            (fun (acc: BatchResult) (id, result) ->
                match result with
                | Error e ->
                    { acc with
                        Errors = (id, e) :: acc.Errors }
                | Ok(fixture, h, a) ->
                    let stateWithMorale =
                        World.updateMorale h a fixture.HomeClubId fixture.AwayClubId acc.GameState

                    let home = stateWithMorale.Clubs[fixture.HomeClubId]
                    let away = stateWithMorale.Clubs[fixture.AwayClubId]

                    let updatedComps =
                        stateWithMorale.Competitions
                        |> Map.map (fun _ comp ->
                            match comp.Fixtures |> Map.tryFind id with
                            | None
                            | Some { Played = true } -> comp
                            | _ ->
                                let hs =
                                    comp.Standings
                                    |> Map.tryFind fixture.HomeClubId
                                    |> Option.defaultWith (fun () -> emptyStanding fixture.HomeClubId)

                                let as' =
                                    comp.Standings
                                    |> Map.tryFind fixture.AwayClubId
                                    |> Option.defaultWith (fun () -> emptyStanding fixture.AwayClubId)

                                { comp with
                                    Fixtures = comp.Fixtures |> Map.add id fixture
                                    Standings =
                                        comp.Standings
                                        |> Map.add fixture.HomeClubId (updateStanding hs h a)
                                        |> Map.add fixture.AwayClubId (updateStanding as' a h) })

                    { acc with
                        GameState =
                            { stateWithMorale with
                                Competitions = updatedComps }
                        Logs = $"{home.Name} {h}-{a} {away.Name}" :: acc.Logs })
            { GameState = gameState
              Logs = []
              Errors = [] }
