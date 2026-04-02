namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats

module MatchOutcome =

    let private emptyStanding (clubId: ClubId) =
        { ClubId = clubId
          Played = 0
          Won = 0
          Drawn = 0
          Lost = 0
          GoalsFor = 0
          GoalsAgainst = 0
          Points = 0 }

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

    let updateAfterMatch
        (homeScore: int)
        (awayScore: int)
        (homeId: ClubId)
        (awayId: ClubId)
        (gs: GameState)
        : GameState =
        let winnerId =
            if homeScore > awayScore then Some homeId
            elif awayScore > homeScore then Some awayId
            else None

        let clubDelta id =
            if winnerId = Some id then 3
            elif winnerId.IsNone then 0
            else -2

        let playerDelta id =
            if winnerId = Some id then 2
            elif winnerId.IsNone then 0
            else -1

        { gs with
            Clubs =
                gs.Clubs
                |> Map.map (fun id c ->
                    if id = homeId || id = awayId then
                        { c with
                            Morale = clamp 0 100 (c.Morale + clubDelta id) }
                    else
                        c)
            Players =
                gs.Players
                |> Map.map (fun _ p ->
                    match Player.clubOf p with
                    | Some cid when cid = homeId || cid = awayId ->
                        { p with
                            Morale = clamp 0 100 (p.Morale + playerDelta cid) }
                    | _ -> p) }

    let fixtureToCompMap (gs: GameState) =
        gs.Competitions
        |> Map.toList
        |> List.collect (fun (compId, comp) -> comp.Fixtures |> Map.toList |> List.map (fun (fid, _) -> fid, compId))
        |> Map.ofList

    type FixtureOutcome =
        { FixtureId: MatchId
          Fixture: MatchFixture
          HomeScore: int
          AwayScore: int
          InjuredPlayers: Set<PlayerId> }

    let private injurySeverityWeights =
        [ 0.30, Minor; 0.40, Moderate; 0.20, Major; 0.10, Severe ]

    let applyOutcomes
        (fixtureToComp: Map<MatchId, CompetitionId>)
        (outcomes: FixtureOutcome[])
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

        let withUpdatedMorale =
            ({ gs with Competitions = updatedComps }, outcomes)
            ||> Array.fold (fun acc o ->
                updateAfterMatch o.HomeScore o.AwayScore o.Fixture.HomeClubId o.Fixture.AwayClubId acc)

        let injuredPlayers =
            outcomes
            |> Array.collect (fun o -> o.InjuredPlayers |> Set.toArray)
            |> Set.ofSeq

        if injuredPlayers.IsEmpty then
            withUpdatedMorale
        else
            let currentDate = gs.CurrentDate

            let applyInjury (players: Map<PlayerId, Player>) (pid: PlayerId) =
                players
                |> Map.change pid (fun playerOpt ->
                    playerOpt
                    |> Option.map (fun p ->
                        let severity = pickWeighted injurySeverityWeights

                        let until =
                            match severity with
                            | Minor -> currentDate.AddDays 7.0
                            | Moderate -> currentDate.AddDays 21.0
                            | Major -> currentDate.AddDays 60.0
                            | Severe -> currentDate.AddDays 120.0

                        { p with
                            Status = Injured(severity, until) }))

            { withUpdatedMorale with
                Players = injuredPlayers |> Seq.fold applyInjury withUpdatedMorale.Players }
