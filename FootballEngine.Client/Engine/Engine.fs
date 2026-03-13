namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.DomainTypes
open MatchSimulator

/// Public API consumed by AppState. Delegates to focused sub-modules.
/// This file should stay thin — only orchestration, no logic.
module Engine =

    // Re-export so call sites don't need to change their open statements.
    let createPlayer = GameGenerator.createPlayer
    let generateNewGame = GameGenerator.generateNewGame

    // ------------------------------------------------------------------ //
    //  Single-fixture simulation                                           //
    // ------------------------------------------------------------------ //

    let private updateClub (c: Club) myScore oppScore =
        if myScore > oppScore then
            { c with
                Wins = c.Wins + 1
                GoalsFor = c.GoalsFor + myScore
                GoalsAgainst = c.GoalsAgainst + oppScore }
        elif myScore < oppScore then
            { c with
                Losses = c.Losses + 1
                GoalsFor = c.GoalsFor + myScore
                GoalsAgainst = c.GoalsAgainst + oppScore }
        else
            { c with
                Draws = c.Draws + 1
                GoalsFor = c.GoalsFor + myScore
                GoalsAgainst = c.GoalsAgainst + oppScore }

    /// Returns Ok with updated fixture + clubs, or Error with a SimulationError
    /// describing exactly what was missing (no lineup, incomplete lineup, etc.).
    let simulateFixture
        (fixture: MatchFixture)
        (clubs: Map<ClubId, Club>)
        : Result<MatchFixture * Club * Club, SimulationError> =

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

            updatedFixture, updateClub home hScore aScore, updateClub away aScore hScore)

    // ------------------------------------------------------------------ //
    //  Batch simulation                                                    //
    // ------------------------------------------------------------------ //

    type BatchResult =
        {
            GameState: GameState
            Logs: string list
            /// Fixtures that failed to simulate and why.
            Errors: (MatchId * SimulationError) list
        }

    /// Simulates all given fixtures in parallel.
    /// Failed fixtures are collected in BatchResult.Errors and skipped
    /// rather than aborting the whole batch.
    let simulateFixtures (gameState: GameState) (fixtures: (MatchId * MatchFixture) list) : BatchResult =
        let results =
            fixtures
            |> List.toArray
            |> Array.Parallel.map (fun (id, fixture) -> id, simulateFixture fixture gameState.Clubs)

        results
        |> Array.fold
            (fun (acc: BatchResult) (id, result) ->
                match result with
                | Error e ->
                    { acc with
                        Errors = (id, e) :: acc.Errors }

                | Ok(fixture, home, away) ->
                    let h = fixture.HomeScore |> Option.defaultValue 0
                    let a = fixture.AwayScore |> Option.defaultValue 0
                    let log = $"⚽ {home.Name} {h}-{a} {away.Name}"

                    { acc with
                        GameState =
                            { acc.GameState with
                                Fixtures = acc.GameState.Fixtures |> Map.add id fixture
                                Clubs = acc.GameState.Clubs |> Map.add home.Id home |> Map.add away.Id away }
                        Logs = log :: acc.Logs })
            { GameState = gameState
              Logs = []
              Errors = [] }

    // ------------------------------------------------------------------ //
    //  Season helpers                                                      //
    // ------------------------------------------------------------------ //

    let isSeasonOver (gameState: GameState) =
        gameState.Fixtures |> Map.forall (fun _ f -> f.Played)
