namespace FootballEngine

open System
open SQLite
open System.IO
open FootballEngine.Domain

module Db =

    // --- PERSISTENCE ENTITIES ---

    [<CLIMutable>]
    type PlayerEntity =
        { [<PrimaryKey>]
          Id: int
          ClubId: int
          Name: string
          Birthday: DateTime
          Nationality: string
          Position: string
          PreferredFoot: string
          Height: int
          Weight: int
          // Physical
          Acceleration: int
          Pace: int
          Agility: int
          Balance: int
          JumpingReach: int
          Stamina: int
          Strength: int
          // Technical
          Finishing: int
          LongShots: int
          Dribbling: int
          BallControl: int
          Passing: int
          Crossing: int
          Tackling: int
          Marking: int
          Heading: int
          FreeKick: int
          Penalty: int
          // Mental
          Aggression: int
          Composure: int
          Vision: int
          Positioning: int
          Bravery: int
          WorkRate: int
          Concentration: int
          Leadership: int
          // Goalkeeping
          Reflexes: int
          Handling: int
          Kicking: int
          OneOnOne: int
          AerialReach: int
          // State
          Condition: int
          MatchFitness: int
          Morale: int
          StatusType: string
          StatusParamInt: int
          StatusParamDate: DateTime
          // Global
          CurrentSkill: int
          PotentialSkill: int
          Reputation: int
          Value: decimal
          Salary: decimal
          ContractExpiry: int }

    [<CLIMutable>]
    type ClubEntity =
        { [<PrimaryKey>]
          Id: int
          Name: string
          Nationality: string
          Reputation: int
          Budget: decimal
          Morale: int
          Wins: int
          Draws: int
          Losses: int
          GoalsFor: int
          GoalsAgainst: int }

    [<CLIMutable>]
    type LineupSlotEntity =
        { [<PrimaryKey; AutoIncrement>]
          Id: int
          ClubId: int
          FormationName: string
          TacticsName: string
          SlotIndex: int
          Role: string
          X: float
          Y: float
          PlayerId: int }

    [<CLIMutable>]
    type LeagueEntity =
        { [<PrimaryKey>]
          Id: int
          Name: string
          Level: int
          Season: int
          Nationality: string
          IsPlayable: bool }

    [<CLIMutable>]
    type LeagueClubEntity =
        { [<PrimaryKey; AutoIncrement>]
          Id: int
          LeagueId: int
          ClubId: int }

    [<CLIMutable>]
    type MatchFixtureEntity =
        { [<PrimaryKey>]
          Id: int
          HomeClubId: int
          AwayClubId: int
          ScheduledDate: DateTime
          HomeScore: int
          AwayScore: int
          Played: bool }

    [<CLIMutable>]
    type GameSaveMeta =
        { [<PrimaryKey>]
          Id: int
          CurrentDate: DateTime
          UserClubId: int
          ManagerName: string
          PrimaryCountry: string }

    let private parsePosition (s: string) =
        match s.Trim().ToUpper() with
        | "GK" -> GK
        | "DL" -> DL
        | "DC" -> DC
        | "DR" -> DR
        | "WBL" -> WBL
        | "DM" -> DM
        | "WBR" -> WBR
        | "ML" -> ML
        | "MC" -> MC
        | "MR" -> MR
        | "AML" -> AML
        | "AMC" -> AMC
        | "AMR" -> AMR
        | "ST" -> ST
        | _ -> MC

    let private parseFoot (s: string) =
        match s.Trim().ToUpper() with
        | "LEFT" -> Left
        | "RIGHT" -> Right
        | "BOTH" -> Both
        | _ -> Right

    let private formationToString =
        function
        | F442 -> "4-4-2"
        | F442Diamond -> "4-4-2 Diamond"
        | F433 -> "4-3-3"
        | F433Flat -> "4-3-3 Flat"
        | F451 -> "4-5-1"
        | F4141 -> "4-1-4-1"
        | F4231 -> "4-2-3-1"
        | F4312 -> "4-3-1-2"
        | F4321 -> "4-3-2-1"
        | F352 -> "3-5-2"
        | F343 -> "3-4-3"
        | F3421 -> "3-4-2-1"
        | F532 -> "5-3-2"
        | F541 -> "5-4-1"
        | F523 -> "5-2-3"


    let private parseFormation =
        function
        | "4-4-2" -> F442
        | "4-4-2 Diamond" -> F442Diamond
        | "4-3-3" -> F433
        | "4-3-3 Flat" -> F433Flat
        | "4-5-1" -> F451
        | "4-1-4-1" -> F4141
        | "4-2-3-1" -> F4231
        | "4-3-1-2" -> F4312
        | "4-3-2-1" -> F4321
        | "3-5-2" -> F352
        | "3-4-3" -> F343
        | "3-4-2-1" -> F3421
        | "5-3-2" -> F532
        | "5-4-1" -> F541
        | "5-2-3" -> F523
        | _ -> F442

    let private toPlayerEntity (p: Player) : PlayerEntity =
        let statusStr, sInt, sDate =
            match p.Status with
            | Available -> "Available", 0, DateTime.MinValue
            | Suspended n -> "Suspended", n, DateTime.MinValue
            | Injured(_, dt) -> "Injured", 0, dt

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
          StatusType = statusStr
          StatusParamInt = sInt
          StatusParamDate = sDate
          CurrentSkill = p.CurrentSkill
          PotentialSkill = p.PotentialSkill
          Reputation = p.Reputation
          Value = p.Value
          Salary = p.Salary
          ContractExpiry = p.ContractExpiry }

    let private toPlayerDomain (e: PlayerEntity) : Player =
        let status =
            match e.StatusType with
            | "Suspended" -> Suspended e.StatusParamInt
            | "Injured" -> Injured(Moderate, e.StatusParamDate) // Simplificado severidad
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
          ContractExpiry = e.ContractExpiry
          TeamId = e.ClubId }

    let private toClubEntity (c: Club) : ClubEntity =
        { Id = c.Id
          Name = c.Name
          Nationality = c.Nationality
          Reputation = c.Reputation
          Budget = c.Budget
          Morale = c.Morale
          Wins = c.Wins
          Draws = c.Draws
          Losses = c.Losses
          GoalsFor = c.GoalsFor
          GoalsAgainst = c.GoalsAgainst }

    let private toFixtureEntity (f: MatchFixture) : MatchFixtureEntity =
        { Id = f.Id
          HomeClubId = f.HomeClubId
          AwayClubId = f.AwayClubId
          ScheduledDate = f.ScheduledDate
          HomeScore = f.HomeScore |> Option.defaultValue -1
          AwayScore = f.AwayScore |> Option.defaultValue -1
          Played = f.Played }

    let private toFixtureDomain (e: MatchFixtureEntity) : MatchFixture =
        { Id = e.Id
          HomeClubId = e.HomeClubId
          AwayClubId = e.AwayClubId
          ScheduledDate = e.ScheduledDate
          HomeScore = if e.HomeScore = -1 then None else Some e.HomeScore
          AwayScore = if e.AwayScore = -1 then None else Some e.AwayScore
          Played = e.Played
          Events = [] }

    // --- DATABASE CORE ---

    let private dbPath =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "football_engine.db3")

    let private getConnection () = new SQLiteConnection(dbPath)

    let saveGame (state: GameState) =
        use db = getConnection ()

        db.RunInTransaction(fun () ->
            db.InsertOrReplace(
                { Id = 1
                  CurrentDate = state.CurrentDate
                  UserClubId = state.UserClubId
                  ManagerName = state.ManagerName
                  PrimaryCountry = state.PrimaryCountry }
            )
            |> ignore

            for KeyValue(_, club) in state.Clubs do
                db.InsertOrReplace(toClubEntity club) |> ignore
                // Guardar Lineup si existe
                match club.CurrentLineup with
                | Some lineup ->
                    db.Execute("DELETE FROM LineupSlotEntity WHERE ClubId = ?", club.Id) |> ignore

                    for slot in lineup.Slots do
                        db.Insert(
                            { Id = 0
                              ClubId = club.Id
                              FormationName = formationToString lineup.Formation
                              TacticsName = lineup.TeamTactics
                              SlotIndex = slot.Index
                              Role = string slot.Role
                              X = slot.X
                              Y = slot.Y
                              PlayerId = (slot.PlayerId |> Option.defaultValue -1) }
                        )
                        |> ignore
                | None -> ()

            for KeyValue(_, player) in state.Players do
                db.InsertOrReplace(toPlayerEntity player) |> ignore

            db.DeleteAll<LeagueClubEntity>() |> ignore

            for KeyValue(_, league) in state.Leagues do
                db.InsertOrReplace(
                    { Id = league.Id
                      Name = league.Name
                      Level =
                        (match league.Level with
                         | First -> 0
                         | Second -> 1)
                      Season = league.Season
                      Nationality = league.Nationality
                      IsPlayable = league.IsPlayable }
                )
                |> ignore

                for clubId in league.ClubIds do
                    db.Insert(
                        { Id = 0
                          LeagueId = league.Id
                          ClubId = clubId }
                    )
                    |> ignore

            for KeyValue(_, fixture) in state.Fixtures do
                db.InsertOrReplace(toFixtureEntity fixture) |> ignore)

    let saveDailyProgress (state: GameState) =
        use db = getConnection ()

        db.RunInTransaction(fun () ->
            db.InsertOrReplace(
                { Id = 1
                  CurrentDate = state.CurrentDate
                  UserClubId = state.UserClubId
                  ManagerName = state.ManagerName
                  PrimaryCountry = state.PrimaryCountry }
            )
            |> ignore

            for KeyValue(_, club) in state.Clubs do
                db.InsertOrReplace(toClubEntity club) |> ignore

            for KeyValue(_, player) in state.Players do
                db.InsertOrReplace(toPlayerEntity player) |> ignore

            for KeyValue(_, fixture) in state.Fixtures do
                db.InsertOrReplace(toFixtureEntity fixture) |> ignore)

    let loadGame () : GameState option =
        if not (File.Exists dbPath) then
            None
        else
            use db = getConnection ()
            let meta = db.Table<GameSaveMeta>().FirstOrDefault()

            if box meta = null then
                None
            else
                let players =
                    db.Table<PlayerEntity>().ToList()
                    |> Seq.map toPlayerDomain
                    |> Seq.map (fun p -> p.Id, p)
                    |> Map.ofSeq

                let clubEntities = db.Table<ClubEntity>().ToList() |> List.ofSeq
                let leagueClubs = db.Table<LeagueClubEntity>().ToList() |> List.ofSeq
                let lineupSlots = db.Table<LineupSlotEntity>().ToList() |> List.ofSeq

                let clubs =
                    clubEntities
                    |> Seq.map (fun ce ->
                        let squad =
                            players
                            |> Map.toSeq
                            |> Seq.map snd
                            |> Seq.filter (fun p -> p.ClubId = ce.Id)
                            |> List.ofSeq

                        let slots =
                            lineupSlots
                            |> List.filter (fun ls -> ls.ClubId = ce.Id)
                            |> List.sortBy _.SlotIndex

                        let lineup =
                            if slots.IsEmpty then
                                None
                            else
                                let first = slots.Head

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
                          Morale = ce.Morale
                          Wins = ce.Wins
                          Draws = ce.Draws
                          Losses = ce.Losses
                          GoalsFor = ce.GoalsFor
                          GoalsAgainst = ce.GoalsAgainst })
                    |> Map.ofSeq

                let leagues =
                    db.Table<LeagueEntity>().ToList()
                    |> Seq.map (fun le ->
                        let cIds =
                            leagueClubs
                            |> List.filter (fun lc -> lc.LeagueId = le.Id)
                            |> List.map _.ClubId

                        le.Id,
                        { Id = le.Id
                          Name = le.Name
                          Level = (if le.Level = 0 then First else Second)
                          ClubIds = cIds
                          Season = le.Season
                          Nationality = le.Nationality
                          IsPlayable = le.IsPlayable })
                    |> Map.ofSeq

                let fixtures =
                    db.Table<MatchFixtureEntity>().ToList()
                    |> Seq.map toFixtureDomain
                    |> Seq.map (fun f -> f.Id, f)
                    |> Map.ofSeq

                Some
                    { CurrentDate = meta.CurrentDate
                      Clubs = clubs
                      Players = players
                      Leagues = leagues
                      Fixtures = fixtures
                      UserClubId = meta.UserClubId
                      ManagerName = meta.ManagerName
                      PrimaryCountry = meta.PrimaryCountry }

    let initTables () =
        use db = getConnection ()
        db.CreateTable<PlayerEntity>() |> ignore
        db.CreateTable<ClubEntity>() |> ignore
        db.CreateTable<LeagueEntity>() |> ignore
        db.CreateTable<LeagueClubEntity>() |> ignore
        db.CreateTable<MatchFixtureEntity>() |> ignore
        db.CreateTable<GameSaveMeta>() |> ignore
        db.CreateTable<LineupSlotEntity>() |> ignore
