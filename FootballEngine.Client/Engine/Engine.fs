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
                    | Ok(h, a, _evs, finalState) ->
                        // Extract only injured players before releasing the large MatchState
                        let injured =
                            finalState.HomeSide.Sidelined
                            |> Map.toSeq
                            |> Seq.append (finalState.AwaySide.Sidelined |> Map.toSeq)
                            |> Seq.choose (fun (pid, status) -> if status = SidelinedByInjury then Some pid else None)
                            |> Set.ofSeq

                        let updatedFixture =
                            { fixture with
                                Played = true
                                HomeScore = Some h
                                AwayScore = Some a
                                Events = [] } // AI matches don't need event history

                        { FixtureId = id
                          Fixture = updatedFixture
                          HomeScore = h
                          AwayScore = a
                          InjuredPlayers = injured }
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

    let private applyWeeklyTrainingIfNeeded (gs: GameState) : GameState =
        let seasonStart = DateTime(gs.Season, 7, 1)
        let dayOfSeason = int (gs.CurrentDate - seasonStart).TotalDays

        if dayOfSeason > 0 && dayOfSeason % 7 = 0 then
            { gs with
                TrainingWeeksApplied = gs.TrainingWeeksApplied + 1
                Players =
                    gs.Players
                    |> Map.map (fun _ player ->
                        match player.Affiliation with
                        | Contracted _ ->
                            TrainingEngine.applyWeeklyTraining gs.CurrentDate player.TrainingSchedule player
                        | _ -> player) }
        else
            gs

    let private processDayFixtures (gs: GameState) (fixtures: (MatchId * MatchFixture) list) =
        fixtures
        |> function
            | [] -> Ok(gs, [])
            | fixes ->
                let outcomes, errors, logs, gsReady = simulateFixtureList gs fixes

                if not errors.IsEmpty then
                    Error(errors)
                else
                    gsReady
                    |> applyOutcomes (fixtureToCompMap gsReady) (List.toArray outcomes)
                    |> fun newGs -> Ok(newGs, logs)

    let private advanceSingleDay (gs: GameState) : DayResult =
        let gs =
            { gs with
                CurrentDate = gs.CurrentDate.AddDays(1.0) }

        let gs = applyWeeklyTrainingIfNeeded gs

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

        match processDayFixtures gs todayFixtures with
        | Ok(newGs, logs) ->
            { GameState = newGs
              PlayedFixtures = todayFixtures
              Logs = logs
              SeasonComplete = isSeasonComplete newGs }
        | Error errors ->
            { GameState = gs
              PlayedFixtures = todayFixtures
              Logs = errors |> List.map (fun (id, e) -> $"Fixture {id} failed: {e}")
              SeasonComplete = isSeasonComplete gs }

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

    let private seasonWeeks (gs: GameState) =
        let seasonStart = DateTime(gs.Season, 7, 1)

        let seasonEnd =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, c) -> c.Fixtures |> Map.toSeq |> Seq.map (fun (_, f) -> f.ScheduledDate))
            |> Seq.toList
            |> function
                | [] -> DateTime(gs.Season + 1, 5, 1)
                | dates -> List.max dates

        int (seasonEnd - seasonStart).TotalDays / 7

    let private runSeasonPipeline (gs: GameState) =
        let totalWeeks = seasonWeeks gs

        gs
        |> ClubFinance.distributeRevenue
        |> TransferMarket.simulateSummerWindow
        |> TrainingEngine.applyRemainingSeasonTraining gs.CurrentDate gs.TrainingWeeksApplied totalWeeks
        |> PlayerDevelopment.developAll
        |> ContractManager.processContracts
        |> YouthAcademy.generateYouth
        |> SeasonManager.applyLeagueConsequences
        |> BoardAI.runEndOfSeason
        |> SeasonManager.resetConditions
        |> SeasonManager.refreshAiLineups
        |> fun s ->
            { s with
                Season = s.Season + 1
                CurrentDate = DateTime(s.Season + 1, 7, 1)
                TrainingWeeksApplied = 0 }
        |> SeasonGen.regenerateFixtures

    let advanceSeason (gs: GameState) : SeasonResult =
        { Summary = SeasonManager.computeSeasonSummary gs
          SeasonFinalGs = gs
          NewGs = runSeasonPipeline gs }

    let simulateAndAdvanceSeason (gs: GameState) : Result<SeasonResult, SeasonError> =
        let fixturesByDate =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq |> Seq.filter (fun (_, f) -> not f.Played))
            |> Seq.groupBy (fun (_, f) -> f.ScheduledDate.Date)
            |> Seq.map (fun (date, fixtures) -> date, List.ofSeq fixtures)
            |> Map.ofSeq

        if fixturesByDate.IsEmpty then
            Error NoFixturesToPlay
        else
            let sortedDates = fixturesByDate |> Map.keys |> Seq.sort |> Array.ofSeq
            let mutable currentGs = gs
            let mutable simError: SeasonError option = None
            let mutable i = 0

            while i < sortedDates.Length && simError.IsNone do
                let date = sortedDates[i]
                let dayFixtures = fixturesByDate.[date]

                if not dayFixtures.IsEmpty then
                    match processDayFixtures currentGs dayFixtures with
                    | Ok(updatedGs, _) -> currentGs <- { updatedGs with CurrentDate = date }
                    | Error errors ->
                        simError <-
                            errors
                            |> List.map (fun (_, e) -> string e)
                            |> String.concat ", "
                            |> SimulationErrors
                            |> Some

                i <- i + 1

            match simError with
            | Some e -> Error e
            | None ->
                Ok
                    { Summary = SeasonManager.computeSeasonSummary currentGs
                      SeasonFinalGs = currentGs
                      NewGs = runSeasonPipeline currentGs }

    type UserMatchDayResult =
        { DayResult: DayResult
          UserMatchReplay: MatchReplay }

    let hasUserFixtureToday (gs: GameState) : bool =
        gs.Competitions
        |> Map.exists (fun _ comp ->
            comp.Fixtures
            |> Map.exists (fun _ f ->
                f.ScheduledDate.Date = gs.CurrentDate.Date
                && MatchFixture.isPending f
                && MatchFixture.involves gs.UserClubId f))

    let simulateUserFixtureForDay (gs: GameState) : Result<UserMatchDayResult, string> =
        let userFixture =
            gs.Competitions
            |> Map.toSeq
            |> Seq.collect (fun (_, comp) -> comp.Fixtures |> Map.toSeq)
            |> Seq.tryFind (fun (_, f) ->
                f.ScheduledDate.Date = gs.CurrentDate.Date
                && MatchFixture.isPending f
                && MatchFixture.involves gs.UserClubId f)

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

                let injured =
                    replay.Final.HomeSide.Sidelined
                    |> Map.toSeq
                    |> Seq.append (replay.Final.AwaySide.Sidelined |> Map.toSeq)
                    |> Seq.choose (fun (pid, status) -> if status = SidelinedByInjury then Some pid else None)
                    |> Set.ofSeq

                let updatedFixture =
                    { fixture with
                        Played = true
                        HomeScore = Some h
                        AwayScore = Some a
                        Events = replay.Events }

                let outcome =
                    { FixtureId = fixtureId
                      Fixture = updatedFixture
                      HomeScore = h
                      AwayScore = a
                      InjuredPlayers = injured }

                let finalGs = applyOutcomes (fixtureToCompMap gsReady) [| outcome |] gsReady

                let dayResult =
                    { GameState = finalGs
                      PlayedFixtures = [ fixtureId, updatedFixture ]
                      Logs = [ $"{home.Name} {h}-{a} {away.Name}" ]
                      SeasonComplete = isSeasonComplete finalGs }

                Ok
                    { DayResult = dayResult
                      UserMatchReplay = replay }
