namespace FootballEngine.Database

open System
open FootballEngine.Domain
open FootballEngine.Database.Serializers

module Mappers =

    let toPlayerEntity (p: Player) : PlayerEntity =
        let statusType, statusInt, statusDate =
            match p.Status with
            | Available -> "Available", 0, DateTime.MinValue
            | Suspended n -> "Suspended", n, DateTime.MinValue
            | Injured(severity, dt) ->
                let severityInt =
                    match severity with
                    | Minor -> 0
                    | Moderate -> 1
                    | Major -> 2
                    | Severe -> 3

                "Injured", severityInt, dt

        { Id = p.Id
          ClubId = p.ClubId
          Name = p.Name
          Birthday = p.Birthday
          Nationality = p.Nationality
          Position = $"%A{p.Position}"
          PreferredFoot = $"%A{p.PreferredFoot}"
          Height = p.Height
          Weight = p.Weight
          Acceleration = p.Physical.Acceleration
          Pace = p.Physical.Pace
          Agility = p.Physical.Agility
          Balance = p.Physical.Balance
          JumpingReach = p.Physical.JumpingReach
          Stamina = p.Physical.Stamina
          Strength = p.Physical.Strength
          Finishing = p.Technical.Finishing
          LongShots = p.Technical.LongShots
          Dribbling = p.Technical.Dribbling
          BallControl = p.Technical.BallControl
          Passing = p.Technical.Passing
          Crossing = p.Technical.Crossing
          Tackling = p.Technical.Tackling
          Marking = p.Technical.Marking
          Heading = p.Technical.Heading
          FreeKick = p.Technical.FreeKick
          Penalty = p.Technical.Penalty
          Aggression = p.Mental.Aggression
          Composure = p.Mental.Composure
          Vision = p.Mental.Vision
          Positioning = p.Mental.Positioning
          Bravery = p.Mental.Bravery
          WorkRate = p.Mental.WorkRate
          Concentration = p.Mental.Concentration
          Leadership = p.Mental.Leadership
          Reflexes = p.Goalkeeping.Reflexes
          Handling = p.Goalkeeping.Handling
          Kicking = p.Goalkeeping.Kicking
          OneOnOne = p.Goalkeeping.OneOnOne
          AerialReach = p.Goalkeeping.AerialReach
          Condition = p.Condition
          MatchFitness = p.MatchFitness
          Morale = p.Morale
          StatusType = statusType
          StatusParamInt = statusInt
          StatusParamDate = statusDate
          CurrentSkill = p.CurrentSkill
          PotentialSkill = p.PotentialSkill
          Reputation = p.Reputation
          Value = p.Value
          Salary = p.Salary
          ContractExpiry = p.ContractExpiry }

    let toPlayerDomain (e: PlayerEntity) : Player =
        let status =
            match e.StatusType with
            | "Suspended" -> Suspended e.StatusParamInt
            | "Injured" ->
                let severity =
                    match e.StatusParamInt with
                    | 0 -> Minor
                    | 1 -> Moderate
                    | 2 -> Major
                    | _ -> Severe

                Injured(severity, e.StatusParamDate)
            | _ -> Available

        { Id = e.Id
          ClubId = e.ClubId
          Name = e.Name
          Birthday = e.Birthday
          Nationality = e.Nationality
          Position = parsePosition e.Position
          PreferredFoot = parseFoot e.PreferredFoot
          Height = e.Height
          Weight = e.Weight
          Physical =
            { Acceleration = e.Acceleration
              Pace = e.Pace
              Agility = e.Agility
              Balance = e.Balance
              JumpingReach = e.JumpingReach
              Stamina = e.Stamina
              Strength = e.Strength }
          Technical =
            { Finishing = e.Finishing
              LongShots = e.LongShots
              Dribbling = e.Dribbling
              BallControl = e.BallControl
              Passing = e.Passing
              Crossing = e.Crossing
              Tackling = e.Tackling
              Marking = e.Marking
              Heading = e.Heading
              FreeKick = e.FreeKick
              Penalty = e.Penalty }
          Mental =
            { Aggression = e.Aggression
              Composure = e.Composure
              Vision = e.Vision
              Positioning = e.Positioning
              Bravery = e.Bravery
              WorkRate = e.WorkRate
              Concentration = e.Concentration
              Leadership = e.Leadership }
          Goalkeeping =
            { Reflexes = e.Reflexes
              Handling = e.Handling
              Kicking = e.Kicking
              OneOnOne = e.OneOnOne
              AerialReach = e.AerialReach }
          Condition = e.Condition
          MatchFitness = e.MatchFitness
          Morale = e.Morale
          Status = status
          CurrentSkill = e.CurrentSkill
          PotentialSkill = e.PotentialSkill
          Reputation = e.Reputation
          Value = e.Value
          Salary = e.Salary
          ContractExpiry = e.ContractExpiry }

    let toClubEntity (c: Club) : ClubEntity =
        { Id = c.Id
          Name = c.Name
          Nationality = c.Nationality
          Reputation = c.Reputation
          Budget = c.Budget
          Morale = c.Morale }

    let toClubDomain
        (players: Map<PlayerId, Player>)
        (lineupSlots: LineupSlotEntity list)
        (ce: ClubEntity)
        : ClubId * Club =
        let squad =
            players |> Map.values |> Seq.filter (fun p -> p.ClubId = ce.Id) |> List.ofSeq

        let slots =
            lineupSlots
            |> List.filter (fun ls -> ls.ClubId = ce.Id)
            |> List.sortBy _.SlotIndex

        let lineup =
            match slots with
            | [] -> None
            | first :: _ ->
                Some
                    { Formation = parseFormation first.FormationName
                      TeamTactics = first.TacticsName
                      Slots =
                        slots
                        |> List.map (fun s ->
                            { Index = s.SlotIndex
                              Role = parsePosition s.Role
                              X = s.X
                              Y = s.Y
                              PlayerId = if s.PlayerId = -1 then None else Some s.PlayerId }) }

        ce.Id,
        { Id = ce.Id
          Name = ce.Name
          Nationality = ce.Nationality
          Reputation = ce.Reputation
          Players = squad
          CurrentLineup = lineup
          Budget = ce.Budget
          Morale = ce.Morale }

    let toCompetitionEntity (c: Competition) : CompetitionEntity =
        let tag, p1, p2 = competitionTypeToStrings c.Type

        { Id = c.Id
          Name = c.Name
          TypeTag = tag
          TypeParam1 = p1
          TypeParam2 = p2
          Country = c.Country |> Option.defaultValue ""
          Season = c.Season }

    let toFixtureEntity (f: MatchFixture) : MatchFixtureEntity =
        { Id = f.Id
          CompetitionId = f.CompetitionId
          Round = f.Round |> Option.map roundToString |> Option.defaultValue ""
          HomeClubId = f.HomeClubId
          AwayClubId = f.AwayClubId
          ScheduledDate = f.ScheduledDate
          HomeScore = f.HomeScore |> Option.defaultValue -1
          AwayScore = f.AwayScore |> Option.defaultValue -1
          Played = f.Played }

    let toFixtureDomain (e: MatchFixtureEntity) : MatchFixture =
        { Id = e.Id
          CompetitionId = e.CompetitionId
          Round =
            if String.IsNullOrEmpty(e.Round) then
                None
            else
                Some(parseRound e.Round)
          HomeClubId = e.HomeClubId
          AwayClubId = e.AwayClubId
          ScheduledDate = e.ScheduledDate
          HomeScore = if e.HomeScore = -1 then None else Some e.HomeScore
          AwayScore = if e.AwayScore = -1 then None else Some e.AwayScore
          Played = e.Played
          Events = [] }

    let toKnockoutTieEntity (compId: CompetitionId) (t: KnockoutTie) : KnockoutTieEntity =
        let aggHome, aggAway = t.AggregateScore |> Option.defaultValue (-1, -1)

        { TieId = t.TieId
          CompetitionId = compId
          Round = roundToString t.Round
          HomeClubId = t.HomeClubId
          AwayClubId = t.AwayClubId
          Leg1FixtureId = t.Leg1FixtureId |> Option.defaultValue -1
          Leg2FixtureId = t.Leg2FixtureId |> Option.defaultValue -1
          AggHome = aggHome
          AggAway = aggAway
          WinnerId = t.WinnerId |> Option.defaultValue -1 }

    let toKnockoutTieDomain (e: KnockoutTieEntity) : KnockoutTie =
        { TieId = e.TieId
          Round = parseRound e.Round
          HomeClubId = e.HomeClubId
          AwayClubId = e.AwayClubId
          Leg1FixtureId = if e.Leg1FixtureId = -1 then None else Some e.Leg1FixtureId
          Leg2FixtureId = if e.Leg2FixtureId = -1 then None else Some e.Leg2FixtureId
          AggregateScore = if e.AggHome = -1 then None else Some(e.AggHome, e.AggAway)
          WinnerId = if e.WinnerId = -1 then None else Some e.WinnerId }

    let toStandingEntity (compId: CompetitionId) (s: LeagueStanding) : LeagueStandingEntity =
        { Id = 0
          CompetitionId = compId
          ClubId = s.ClubId
          Played = s.Played
          Won = s.Won
          Drawn = s.Drawn
          Lost = s.Lost
          GoalsFor = s.GoalsFor
          GoalsAgainst = s.GoalsAgainst
          Points = s.Points }

    let toStandingDomain (e: LeagueStandingEntity) : LeagueStanding =
        { ClubId = e.ClubId
          Played = e.Played
          Won = e.Won
          Drawn = e.Drawn
          Lost = e.Lost
          GoalsFor = e.GoalsFor
          GoalsAgainst = e.GoalsAgainst
          Points = e.Points }

    let toCompetitionDomain
        (compClubs: CompetitionClubEntity list)
        (allFixtures: MatchFixtureEntity list)
        (allStandings: LeagueStandingEntity list)
        (allTies: KnockoutTieEntity list)
        (ce: CompetitionEntity)
        : CompetitionId * Competition =
        let clubIds =
            compClubs
            |> List.filter (fun cc -> cc.CompetitionId = ce.Id)
            |> List.map _.ClubId

        let fixtures =
            allFixtures
            |> List.filter (fun f -> f.CompetitionId = ce.Id)
            |> List.map (fun f -> f.Id, toFixtureDomain f)
            |> Map.ofList

        let standings =
            allStandings
            |> List.filter (fun s -> s.CompetitionId = ce.Id)
            |> List.map (fun s -> s.ClubId, toStandingDomain s)
            |> Map.ofList

        let knockoutTies =
            allTies
            |> List.filter (fun t -> t.CompetitionId = ce.Id)
            |> List.map (fun t -> t.TieId, toKnockoutTieDomain t)
            |> Map.ofList

        ce.Id,
        { Id = ce.Id
          Name = ce.Name
          Type = parseCompetitionType ce.TypeTag ce.TypeParam1 ce.TypeParam2
          Country =
            if String.IsNullOrEmpty(ce.Country) then
                None
            else
                Some ce.Country
          Season = ce.Season
          ClubIds = clubIds
          Fixtures = fixtures
          Standings = standings
          KnockoutTies = knockoutTies }

    let toCountryEntity (c: Country) : CountryEntity =
        { Code = c.Code
          Name = c.Name
          Confederation = confederationToString c.Confederation }

    let toCountryDomain (e: CountryEntity) : Country =
        { Code = e.Code
          Name = e.Name
          Confederation = parseConfederation e.Confederation }
