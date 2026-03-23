namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Generation
open MatchSimulator

module Engine =

    let private ensureLineup (players: Map<PlayerId, Player>) (club: Club) =
        let isComplete =
            club.CurrentLineup
            |> Option.map (fun lu -> lu.Slots |> List.filter (fun s -> s.PlayerId.IsSome) |> List.length = 11)
            |> Option.defaultValue false

        if isComplete then
            club
        else
            let squad = club.PlayerIds |> List.choose players.TryFind
            Lineup.autoLineup club squad (Lineup.bestFormation squad)

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

    let simulateFixture (fixture: MatchFixture) (gs: GameState) =
        let home = gs.Clubs[fixture.HomeClubId] |> ensureLineup gs.Players
        let away = gs.Clubs[fixture.AwayClubId] |> ensureLineup gs.Players

        trySimulateMatch home away gs.Players
        |> Result.map (fun (h, a, evs) ->
            { fixture with
                Played = true
                HomeScore = Some h
                AwayScore = Some a
                Events = evs },
            h,
            a)

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

        ({ gs with Competitions = updatedComps }, outcomes)
        ||> Array.fold (fun acc o ->
            World.updateMorale o.HomeScore o.AwayScore o.Fixture.HomeClubId o.Fixture.AwayClubId acc)

    let private fixtureToCompMap (gs: GameState) =
        gs.Competitions
        |> Map.toList
        |> List.collect (fun (compId, comp) -> comp.Fixtures |> Map.toList |> List.map (fun (fid, _) -> fid, compId))
        |> Map.ofList

    let private simulateFixtureList (gs: GameState) (fixtures: (MatchId * MatchFixture) list) =
        fixtures
        |> List.toArray
        |> Array.Parallel.map (fun (id, fixture) -> id, simulateFixture fixture gs)
        |> Array.fold
            (fun (outs, errs, ls) (id, result) ->
                match result with
                | Error e -> outs, (id, e) :: errs, ls
                | Ok(fixture, h, a) ->
                    { FixtureId = id
                      Fixture = fixture
                      HomeScore = h
                      AwayScore = a }
                    :: outs,
                    errs,
                    $"{gs.Clubs[fixture.HomeClubId].Name} {h}-{a} {gs.Clubs[fixture.AwayClubId].Name}"
                    :: ls)
            ([], [], [])

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
            let outcomes, errors, logs = simulateFixtureList gs todayFixtures

            let newGs =
                if errors.IsEmpty then
                    applyOutcomes (fixtureToCompMap gs) (List.toArray outcomes) gs
                else
                    gs

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
        gs
        |> World.advanceSeason (Random())
        |> SeasonGen.regenerateFixtures
        |> World.applyLeagueConsequences

    let advanceSeason (gs: GameState) : SeasonResult =
        { Summary = World.computeSeasonSummary gs
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
            let outcomes, errors, _logs = simulateFixtureList gs allUnplayed

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
                    { applyOutcomes (fixtureToCompMap gs) (List.toArray outcomes) gs with
                        CurrentDate = lastMatchDate }

                Ok
                    { Summary = World.computeSeasonSummary finalGs
                      SeasonFinalGs = finalGs
                      NewGs = runSeasonPipeline finalGs }

    // ── Match-day flow ────────────────────────────────────────────────────

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

    /// Simulates only the user's fixture for the current date with full snapshot replay.
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
            let home = gs.Clubs[fixture.HomeClubId] |> ensureLineup gs.Players
            let away = gs.Clubs[fixture.AwayClubId] |> ensureLineup gs.Players

            match trySimulateMatchFull home away gs.Players with
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

                let finalGs = applyOutcomes (fixtureToCompMap gs) [| outcome |] gs

                let dayResult =
                    { GameState = finalGs
                      PlayedFixtures = [ fixtureId, updatedFixture ]
                      Logs = [ $"{home.Name} {h}-{a} {away.Name}" ]
                      SeasonComplete = isSeasonComplete finalGs }

                Ok
                    { DayResult = dayResult
                      UserMatchReplay = replay }
