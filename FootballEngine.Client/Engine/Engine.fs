namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Generation
open MatchSimulator
open MatchOutcome

module Engine =

    let private simulateFixtureList (gs: GameState) (fixtures: (MatchId * MatchFixture) list) =
        let gsReady = Lineup.ensureForFixtures fixtures gs

        let results =
            fixtures
            |> List.toArray
            |> Array.Parallel.map (fun (id, fixture) ->
                let home = gsReady.Clubs[fixture.HomeClubId]
                let away = gsReady.Clubs[fixture.AwayClubId]
                id, fixture, home, away, trySimulateMatch home away gsReady.Players gsReady.Staff)

        let outs, errs, ls =
            results
            |> Array.fold
                (fun (outs, errs, ls) (id, fixture, home, away, result) ->
                    match result with
                    | Error e -> outs, (id, e) :: errs, ls
                    | Ok(h, a, evs) ->
                        let updatedFixture =
                            { fixture with
                                Played = true
                                HomeScore = Some h
                                AwayScore = Some a
                                Events = evs }

                        { FixtureId = id
                          Fixture = updatedFixture
                          HomeScore = h
                          AwayScore = a }
                        :: outs,
                        errs,
                        $"{home.Name} {h}-{a} {away.Name}" :: ls)
                ([], [], [])

        outs, errs, ls, gsReady

    type DayResult =
        { GameState: GameState
          PlayedFixtures: (MatchId * MatchFixture) list
          Logs: string list
          SeasonComplete: bool }

    let private isSeasonComplete (gs: GameState) =
        let hasUnplayed (comp: Competition) =
            comp.Fixtures |> Map.exists (fun _ f -> not f.Played)

        gs.Competitions
        |> Map.tryFindKey (fun _ comp ->
            match comp.Type, comp.Country with
            | NationalLeague(LeagueLevel 0, _), Some c when c = gs.PrimaryCountry -> true
            | _ -> false)
        |> Option.map (fun id ->
            not (hasUnplayed gs.Competitions[id])
            && gs.Competitions
               |> Map.forall (fun _ comp ->
                   match comp.Type with
                   | NationalLeague _ -> not (hasUnplayed comp)
                   | _ -> true))
        |> Option.defaultValue false

    let private advanceSingleDay (gs: GameState) : DayResult =
        let gs =
            { gs with
                CurrentDate = gs.CurrentDate.AddDays(1.0) }

        let todayFixtures =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) ->
                comp.Fixtures
                |> Map.toSeq
                |> Seq.filter (fun (_, f) ->
                    f.ScheduledDate.Date = gs.CurrentDate.Date
                    && not f.Played
                    && f.HomeClubId <> gs.UserClubId
                    && f.AwayClubId <> gs.UserClubId))
            |> List.ofSeq

        if todayFixtures.IsEmpty then
            { GameState = gs
              PlayedFixtures = []
              Logs = []
              SeasonComplete = isSeasonComplete gs }
        else
            let outcomes, errors, logs, gsReady = simulateFixtureList gs todayFixtures

            let newGs =
                if errors.IsEmpty then
                    applyOutcomes (fixtureToCompMap gsReady) (List.toArray outcomes) gsReady
                else
                    gsReady

            { GameState = newGs
              PlayedFixtures = todayFixtures
              Logs =
                if errors.IsEmpty then
                    logs
                else
                    errors |> List.map (fun (id, e) -> $"Fixture {id} failed: {e}")
              SeasonComplete = isSeasonComplete newGs }

    let advanceDays (days: int) (gs: GameState) : DayResult =
        let rec loop remaining current =
            if remaining = 0 then
                current
            else
                let next = advanceSingleDay current.GameState

                let merged =
                    { next with
                        PlayedFixtures = current.PlayedFixtures @ next.PlayedFixtures
                        Logs = current.Logs @ next.Logs }

                if merged.SeasonComplete then
                    merged
                else
                    loop (remaining - 1) merged

        loop
            days
            { GameState = gs
              PlayedFixtures = []
              Logs = []
              SeasonComplete = false }

    type SeasonResult =
        { Summary: string list
          SeasonFinalGs: GameState
          NewGs: GameState }

    type SeasonError =
        | NoFixturesToPlay
        | SimulationErrors of string

    let private runSeasonPipeline (gs: GameState) =
        let rng = Random()

        gs
        |> ClubFinance.distributeRevenue
        |> TransferMarket.simulateSummerWindow
        |> PlayerDevelopment.developAll rng
        |> ContractManager.processContracts rng
        |> YouthAcademy.generateYouth rng
        |> SeasonManager.applyLeagueConsequences
        |> BoardAI.runEndOfSeason
        |> SeasonManager.resetConditions
        |> SeasonManager.refreshAiLineups
        |> fun s ->
            { s with
                Season = s.Season + 1
                CurrentDate = DateTime(s.Season + 1, 7, 1) }
        |> SeasonGen.regenerateFixtures

    let advanceSeason (gs: GameState) : SeasonResult =
        { Summary = SeasonManager.computeSeasonSummary gs
          SeasonFinalGs = gs
          NewGs = runSeasonPipeline gs }

    let simulateAndAdvanceSeason (gs: GameState) : Result<SeasonResult, SeasonError> =
        let allUnplayed =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq |> Seq.filter (fun (_, f) -> not f.Played))
            |> List.ofSeq

        if allUnplayed.IsEmpty then
            Error NoFixturesToPlay
        else
            let outcomes, errors, _logs, gsReady = simulateFixtureList gs allUnplayed

            if not errors.IsEmpty then
                errors
                |> List.map (fun (_, e) -> string e)
                |> String.concat ", "
                |> SimulationErrors
                |> Error
            else
                let lastMatchDate =
                    allUnplayed |> List.map (fun (_, f) -> f.ScheduledDate) |> List.max

                let finalGs =
                    { applyOutcomes (fixtureToCompMap gsReady) (List.toArray outcomes) gsReady with
                        CurrentDate = lastMatchDate }

                Ok
                    { Summary = SeasonManager.computeSeasonSummary finalGs
                      SeasonFinalGs = finalGs
                      NewGs = runSeasonPipeline finalGs }

    type UserMatchDayResult =
        { DayResult: DayResult
          UserMatchReplay: MatchReplay }

    let hasUserFixtureToday (gs: GameState) : bool =
        gs.Competitions
        |> Map.exists (fun _ comp ->
            comp.Fixtures
            |> Map.exists (fun _ f ->
                f.ScheduledDate.Date = gs.CurrentDate.Date
                && not f.Played
                && (f.HomeClubId = gs.UserClubId || f.AwayClubId = gs.UserClubId)))

    let simulateUserFixtureForDay (gs: GameState) : Result<UserMatchDayResult, string> =
        let userFixture =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
            |> Seq.tryFind (fun (_, f) ->
                f.ScheduledDate.Date = gs.CurrentDate.Date
                && not f.Played
                && (f.HomeClubId = gs.UserClubId || f.AwayClubId = gs.UserClubId))

        match userFixture with
        | None -> Error "No user fixture today"
        | Some(fixtureId, fixture) ->
            let gsReady =
                gs
                |> Lineup.ensureForClub fixture.HomeClubId
                |> Lineup.ensureForClub fixture.AwayClubId

            let home = gsReady.Clubs[fixture.HomeClubId]
            let away = gsReady.Clubs[fixture.AwayClubId]

            match trySimulateMatchFull home away gsReady.Players gsReady.Staff with
            | Error e -> Error $"Match simulation failed: {e}"
            | Ok replay ->
                let h = replay.Final.HomeScore
                let a = replay.Final.AwayScore

                let updatedFixture =
                    { fixture with
                        Played = true
                        HomeScore = Some h
                        AwayScore = Some a
                        Events = List.rev replay.Final.EventsRev }

                let outcome =
                    { FixtureId = fixtureId
                      Fixture = updatedFixture
                      HomeScore = h
                      AwayScore = a }

                let finalGs = applyOutcomes (fixtureToCompMap gsReady) [| outcome |] gsReady

                let dayResult =
                    { GameState = finalGs
                      PlayedFixtures = [ fixtureId, updatedFixture ]
                      Logs = [ $"{home.Name} {h}-{a} {away.Name}" ]
                      SeasonComplete = isSeasonComplete finalGs }

                Ok
                    { DayResult = dayResult
                      UserMatchReplay = replay }
