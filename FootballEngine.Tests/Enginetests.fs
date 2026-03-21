module FootballEngine.Tests.EngineTests

open Expecto
open FootballEngine.Domain
open FootballEngine.Engine
open FootballEngine.Tests.Helpers

let private unplayedFixtures (game: GameState) limit =
    game.Competitions
    |> Map.toList
    |> List.collect (fun (_, comp) -> comp.Fixtures |> Map.toList)
    |> List.filter (fun (_, f) -> not f.Played)
    |> List.truncate limit

let batchTests =
    testList
        "Batch & Standings Contracts"
        [ testCase "batch simulates fixtures correctly"
          <| fun () ->
              let game = loadGame ()
              let fixtures = unplayedFixtures game 20

              if fixtures.IsEmpty then
                  failtest "No unplayed fixtures found — start a new game"

              let result = simulateFixtures game fixtures
              let simulated = fixtures.Length - result.Errors.Length
              Expect.isTrue (simulated > 0) "all fixtures failed to simulate"
              Expect.isEmpty result.Errors $"%d{result.Errors.Length} errors"
              Expect.equal result.Logs.Length simulated "log count doesn't match simulated count"

          testCase "standings are mathematically sane"
          <| fun () ->
              let game = loadGame ()
              let result = simulateFixtures game (unplayedFixtures game 20)

              let sane =
                  result.GameState.Competitions
                  |> Map.forall (fun _ comp ->
                      comp.Standings
                      |> Map.forall (fun _ s ->
                          s.Played >= 0
                          && s.Won >= 0
                          && s.Drawn >= 0
                          && s.Lost >= 0
                          && s.Won + s.Drawn + s.Lost = s.Played
                          && s.Points = s.Won * 3 + s.Drawn
                          && s.GoalsFor >= 0
                          && s.GoalsAgainst >= 0))

              Expect.isTrue sane "W+D+L ≠ Played or Points ≠ 3W+D or negative values"

          testCase "all played fixtures have scores"
          <| fun () ->
              let game = loadGame ()
              let result = simulateFixtures game (unplayedFixtures game 20)

              Expect.isTrue
                  (result.GameState.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Fixtures
                       |> Map.forall (fun _ f -> not f.Played || (f.HomeScore.IsSome && f.AwayScore.IsSome))))
                  "played fixture missing HomeScore or AwayScore"

          testCase "Played never exceeds 2*(clubs-1)"
          <| fun () ->
              let game = loadGame ()
              let result = simulateFixtures game (unplayedFixtures game 20)

              Expect.isTrue
                  (result.GameState.Competitions
                   |> Map.forall (fun _ comp ->
                       comp.Standings |> Map.forall (fun _ s -> s.Played <= comp.ClubIds.Length * 2)))
                  "a club has played more games than possible"

          testCase "GoalsFor = GoalsAgainst across all standings"
          <| fun () ->
              let game = loadGame ()
              let result = simulateFixtures game (unplayedFixtures game 20)

              let symmetric =
                  result.GameState.Competitions
                  |> Map.forall (fun _ comp ->
                      let totalFor = comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsFor)

                      let totalAgainst =
                          comp.Standings |> Map.toList |> List.sumBy (snd >> _.GoalsAgainst)

                      totalFor = totalAgainst)

              Expect.isTrue symmetric "total goals for ≠ total goals against" ]

let doubleSimGuardTests =
    testList
        "Double-Simulation Guard"
        [ testCase "standings unchanged after re-simulating played fixtures"
          <| fun () ->
              let game = loadGame ()

              let gameWithLineups =
                  { game with
                      Clubs = game.Clubs |> Map.map (fun _ c -> makeReadyClub c) }

              let fixtures = unplayedFixtures gameWithLineups 5

              if fixtures.IsEmpty then
                  failtest "No unplayed fixtures to test"

              let result1 = simulateFixtures gameWithLineups fixtures
              let result2 = simulateFixtures result1.GameState fixtures

              let collectStandings (gs: GameState) =
                  gs.Competitions
                  |> Map.toList
                  |> List.collect (fun (compId, c) ->
                      c.Standings |> Map.toList |> List.map (fun (clubId, s) -> (compId, clubId), s))

              let standingsAfter1 = collectStandings result1.GameState
              let standingsAfter2 = collectStandings result2.GameState

              let unchanged =
                  standingsAfter1
                  |> List.forall (fun (key, s1) ->
                      standingsAfter2
                      |> List.tryFind (fun (k, _) -> k = key)
                      |> Option.map (fun (_, s2) -> s1.Played = s2.Played && s1.Won = s2.Won && s1.Points = s2.Points)
                      |> Option.defaultValue true)

              let maxPts1 = standingsAfter1 |> List.map (snd >> _.Points) |> List.max
              let maxPts2 = standingsAfter2 |> List.map (snd >> _.Points) |> List.max
              Expect.isTrue unchanged "standings changed — fixture was processed twice"
              Expect.equal maxPts2 maxPts1 "points doubled — alreadyPlayed guard is not working"
              Expect.isEmpty result2.Logs "second batch added log entries for already-played fixtures"
              Expect.isEmpty result2.Errors "second batch produced unexpected errors" ]

let standingUpdateTests =
    testList
        "Standing Update Contracts"
        [ testCase "standing arithmetic is correct for one fixture"
          <| fun () ->
              let game = loadGame ()
              let unplayed = unplayedFixtures game 1

              if unplayed.IsEmpty then
                  failtest "No unplayed fixture to use"

              let fixtureId, fixture = unplayed[0]

              let comp =
                  game.Competitions
                  |> Map.toList
                  |> List.tryFind (fun (_, c) -> Map.containsKey fixtureId c.Fixtures)
                  |> Option.map snd

              match comp with
              | None -> failtest "Could not find competition for fixture"
              | Some comp ->
                  let result = simulateFixtures game unplayed

                  let homeAfter =
                      result.GameState.Competitions
                      |> Map.toList
                      |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.HomeClubId)

                  let awayAfter =
                      result.GameState.Competitions
                      |> Map.toList
                      |> List.tryPick (fun (_, c) -> c.Standings |> Map.tryFind fixture.AwayClubId)

                  let homeBefore =
                      comp.Standings
                      |> Map.tryFind fixture.HomeClubId
                      |> Option.defaultValue (emptyStanding fixture.HomeClubId)

                  let awayBefore =
                      comp.Standings
                      |> Map.tryFind fixture.AwayClubId
                      |> Option.defaultValue (emptyStanding fixture.AwayClubId)

                  match homeAfter, awayAfter with
                  | Some h, Some a ->
                      Expect.isTrue
                          (h.Played = homeBefore.Played + 1 && a.Played = awayBefore.Played + 1)
                          "Played not incremented by 1"

                      let hPts = h.Points - homeBefore.Points
                      let aPts = a.Points - awayBefore.Points

                      Expect.isTrue
                          ((hPts = 3 && aPts = 0) || (hPts = 0 && aPts = 3) || (hPts = 1 && aPts = 1))
                          "points distribution formula wrong"

                      Expect.isTrue
                          (h.GoalsFor - homeBefore.GoalsFor >= 0 && a.GoalsFor - awayBefore.GoalsFor >= 0)
                          "GoalsFor not updated"

                      let hGoalsAdded = h.GoalsFor - homeBefore.GoalsFor
                      let aGoalsAdded = a.GoalsFor - awayBefore.GoalsFor
                      let hConceded = h.GoalsAgainst - homeBefore.GoalsAgainst
                      let aConceded = a.GoalsAgainst - awayBefore.GoalsAgainst

                      Expect.isTrue
                          (hGoalsAdded = aConceded && aGoalsAdded = hConceded)
                          "goals asymmetry: home.GoalsFor ≠ away.GoalsAgainst"
                  | _ -> failtest "standings not found after simulation" ]

let fixtureIntegrityTests =
    testList
        "Fixture Integrity"
        [ testCase "each club plays exactly n-1*2 matches in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let n = comp.ClubIds.Length
                  let expected = (n - 1) * 2

                  let gamesPerClub =
                      comp.Fixtures
                      |> Map.toList
                      |> List.collect (fun (_, f) -> [ f.HomeClubId; f.AwayClubId ])
                      |> List.countBy id

                  gamesPerClub
                  |> List.iter (fun (clubId, count) ->
                      Expect.equal count expected $"{comp.Name}: club {clubId} played {count} (expected {expected})"))

          testCase "no duplicate fixtures in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let keys =
                      comp.Fixtures
                      |> Map.toList
                      |> List.map (fun (_, f) -> f.HomeClubId, f.AwayClubId)

                  Expect.equal keys.Length (keys |> List.distinct).Length $"{comp.Name}: duplicate fixtures detected")

          testCase "total fixtures = n*(n-1) in league"
          <| fun () ->
              let game = loadGame ()

              game.Competitions
              |> Map.toList
              |> List.choose (fun (_, comp) ->
                  match comp.Type with
                  | NationalLeague _ -> Some comp
                  | _ -> None)
              |> List.iter (fun comp ->
                  let n = comp.ClubIds.Length

                  Expect.equal
                      comp.Fixtures.Count
                      (n * (n - 1))
                      $"{comp.Name}: expected {n * (n - 1)} fixtures, got {comp.Fixtures.Count}") ]
