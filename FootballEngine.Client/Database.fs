namespace FootballEngine

open System
open FootballEngine.DomainTypes
open FootballEngine.Domain
open SQLite
open System.IO

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

    /// Persists every Competition (league, cup, international).
    /// CompetitionType is stored as a discriminated-union tag + payload strings
    /// so no child table is needed for the type itself.
    [<CLIMutable>]
    type CompetitionEntity =
        {
            [<PrimaryKey>]
            Id: int
            Name: string
            /// "NationalLeague" | "NationalCup" | "InternationalCup"
            TypeTag: string
            /// For NationalLeague: "First" | "Second"
            /// For NationalCup / InternationalCup: serialised CupFormat (see below)
            TypeParam1: string
            /// For InternationalCup: confederation code or "" if None
            TypeParam2: string
            Country: string // "" when None
            Season: int
            /// Pipe-separated key=value pairs for Settings map
            Settings: string
        }

    [<CLIMutable>]
    type CompetitionClubEntity =
        { [<PrimaryKey; AutoIncrement>]
          Id: int
          CompetitionId: int
          ClubId: int }

    [<CLIMutable>]
    type KnockoutTieEntity =
        { [<PrimaryKey>]
          TieId: int
          Round: string
          HomeClubId: int
          AwayClubId: int
          Leg1FixtureId: int // -1 = None
          Leg2FixtureId: int // -1 = None
          AggHome: int // -1 = None
          AggAway: int // -1 = None
          WinnerId: int } // -1 = None

    [<CLIMutable>]
    type MatchFixtureEntity =
        { [<PrimaryKey>]
          Id: int
          CompetitionId: int
          Round: string // serialised Round option; "" = None
          HomeClubId: int
          AwayClubId: int
          ScheduledDate: DateTime
          HomeScore: int // -1 = None
          AwayScore: int // -1 = None
          Played: bool }

    [<CLIMutable>]
    type CountryEntity =
        { [<PrimaryKey>]
          Code: string
          Name: string
          Confederation: string }

    [<CLIMutable>]
    type GameSaveMeta =
        { [<PrimaryKey>]
          Id: int
          CurrentDate: DateTime
          Season: int
          UserClubId: int
          ManagerName: string
          PrimaryCountry: string }

    // --- HELPERS: Position / Foot / Formation ---

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

    // --- HELPERS: Round / CompetitionType / CupFormat ---

    let private roundToString (r: Round) =
        match r with
        | GroupStage g -> $"GroupStage:{g}"
        | RoundOf32 -> "RoundOf32"
        | RoundOf16 -> "RoundOf16"
        | QuarterFinal -> "QuarterFinal"
        | SemiFinal -> "SemiFinal"
        | Final -> "Final"
        | ThirdPlace -> "ThirdPlace"

    let private parseRound (s: string) : Round =
        if s.StartsWith("GroupStage:") then
            GroupStage(int (s.Substring(11)))
        else
            match s with
            | "RoundOf32" -> RoundOf32
            | "RoundOf16" -> RoundOf16
            | "QuarterFinal" -> QuarterFinal
            | "SemiFinal" -> SemiFinal
            | "Final" -> Final
            | "ThirdPlace" -> ThirdPlace
            | _ -> Final

    let private cupFormatToString (f: CupFormat) =
        match f with
        | SingleMatch -> "SingleMatch"
        | TwoLegs -> "TwoLegs"
        | GroupThenKnockout(gs, tpg) -> $"GroupThenKnockout:{gs}:{tpg}"
        | StraightKnockout legs -> $"StraightKnockout:{legs}"

    let private parseCupFormat (s: string) : CupFormat =
        if s.StartsWith("GroupThenKnockout:") then
            let parts = s.Split(':')
            GroupThenKnockout(int parts.[1], int parts.[2])
        elif s.StartsWith("StraightKnockout:") then
            StraightKnockout(int (s.Substring(17)))
        elif s = "TwoLegs" then
            TwoLegs
        else
            SingleMatch

    let private confederationToString (c: Confederation) =
        match c with
        | UEFA -> "UEFA"
        | CONMEBOL -> "CONMEBOL"
        | CONCACAF -> "CONCACAF"
        | CAF -> "CAF"
        | AFC -> "AFC"
        | OFC -> "OFC"

    let private parseConfederation (s: string) : Confederation =
        match s with
        | "CONMEBOL" -> CONMEBOL
        | "CONCACAF" -> CONCACAF
        | "CAF" -> CAF
        | "AFC" -> AFC
        | "OFC" -> OFC
        | _ -> UEFA

    let private competitionTypeToStrings (ct: CompetitionType) : string * string * string =
        match ct with
        | NationalLeague First -> "NationalLeague", "First", ""
        | NationalLeague Second -> "NationalLeague", "Second", ""
        | NationalCup fmt -> "NationalCup", cupFormatToString fmt, ""
        | InternationalCup(confOpt, fmt) ->
            let confStr = confOpt |> Option.map confederationToString |> Option.defaultValue ""
            "InternationalCup", cupFormatToString fmt, confStr

    let private parseCompetitionType (tag: string) (p1: string) (p2: string) : CompetitionType =
        match tag with
        | "NationalLeague" -> NationalLeague(if p1 = "Second" then Second else First)
        | "NationalCup" -> NationalCup(parseCupFormat p1)
        | "InternationalCup" ->
            let conf =
                if String.IsNullOrEmpty(p2) then
                    None
                else
                    Some(parseConfederation p2)

            InternationalCup(conf, parseCupFormat p1)
        | _ -> NationalLeague First

    let private settingsToString (m: Map<string, string>) =
        m |> Map.toSeq |> Seq.map (fun (k, v) -> $"{k}={v}") |> String.concat "|"

    let private parseSettings (s: string) : Map<string, string> =
        if String.IsNullOrEmpty(s) then
            Map.empty
        else
            s.Split('|')
            |> Array.choose (fun kv ->
                let idx = kv.IndexOf('=')

                if idx > 0 then
                    Some(kv.[.. idx - 1], kv.[idx + 1 ..])
                else
                    None)
            |> Map.ofArray

    // --- ENTITY <-> DOMAIN CONVERSIONS ---

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
            | "Injured" -> Injured(Moderate, e.StatusParamDate)
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

    let private toCompetitionEntity (c: Competition) : CompetitionEntity =
        let tag, p1, p2 = competitionTypeToStrings c.Type

        { Id = c.Id
          Name = c.Name
          TypeTag = tag
          TypeParam1 = p1
          TypeParam2 = p2
          Country = c.Country |> Option.defaultValue ""
          Season = c.Season
          Settings = settingsToString c.Settings }

    let private toCompetitionDomain (e: CompetitionEntity) (clubIds: ClubId list) : Competition =
        { Id = e.Id
          Name = e.Name
          Type = parseCompetitionType e.TypeTag e.TypeParam1 e.TypeParam2
          Country =
            if String.IsNullOrEmpty(e.Country) then
                None
            else
                Some e.Country
          Season = e.Season
          ClubIds = clubIds
          Settings = parseSettings e.Settings }

    let private toFixtureEntity (f: MatchFixture) : MatchFixtureEntity =
        { Id = f.Id
          CompetitionId = f.CompetitionId
          Round = f.Round |> Option.map roundToString |> Option.defaultValue ""
          HomeClubId = f.HomeClubId
          AwayClubId = f.AwayClubId
          ScheduledDate = f.ScheduledDate
          HomeScore = f.HomeScore |> Option.defaultValue -1
          AwayScore = f.AwayScore |> Option.defaultValue -1
          Played = f.Played }

    let private toFixtureDomain (e: MatchFixtureEntity) : MatchFixture =
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

    let private toKnockoutTieEntity (t: KnockoutTie) : KnockoutTieEntity =
        let aggHome, aggAway =
            match t.AggregateScore with
            | Some(h, a) -> h, a
            | None -> -1, -1

        { TieId = t.TieId
          Round = roundToString t.Round
          HomeClubId = t.HomeClubId
          AwayClubId = t.AwayClubId
          Leg1FixtureId = t.Leg1FixtureId |> Option.defaultValue -1
          Leg2FixtureId = t.Leg2FixtureId |> Option.defaultValue -1
          AggHome = aggHome
          AggAway = aggAway
          WinnerId = t.WinnerId |> Option.defaultValue -1 }

    let private toKnockoutTieDomain (e: KnockoutTieEntity) : KnockoutTie =
        { TieId = e.TieId
          Round = parseRound e.Round
          HomeClubId = e.HomeClubId
          AwayClubId = e.AwayClubId
          Leg1FixtureId = if e.Leg1FixtureId = -1 then None else Some e.Leg1FixtureId
          Leg2FixtureId = if e.Leg2FixtureId = -1 then None else Some e.Leg2FixtureId
          AggregateScore = if e.AggHome = -1 then None else Some(e.AggHome, e.AggAway)
          WinnerId = if e.WinnerId = -1 then None else Some e.WinnerId }

    let private toCountryEntity (c: Country) : CountryEntity =
        { Code = c.Code
          Name = c.Name
          Confederation = confederationToString c.Confederation }

    let private toCountryDomain (e: CountryEntity) : Country =
        { Code = e.Code
          Name = e.Name
          Confederation = parseConfederation e.Confederation }

    // --- DATABASE CORE ---

    let private dbPath =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "football_engine.db3")

    let private getConnection () = new SQLiteConnection(dbPath)

    // --- SAVE ---

    let saveGame (state: GameState) =
        use db = getConnection ()

        db.RunInTransaction(fun () ->

            // Meta
            db.InsertOrReplace(
                { GameSaveMeta.Id = 1
                  CurrentDate = state.CurrentDate
                  Season = state.Season
                  UserClubId = state.UserClubId
                  ManagerName = state.ManagerName
                  PrimaryCountry = state.PrimaryCountry }
            )
            |> ignore

            // Clubs + lineups
            for KeyValue(_, club) in state.Clubs do
                db.InsertOrReplace(toClubEntity club) |> ignore

                match club.CurrentLineup with
                | Some lineup ->
                    db.Execute("DELETE FROM LineupSlotEntity WHERE ClubId = ?", club.Id) |> ignore

                    for slot in lineup.Slots do
                        db.Insert(
                            { LineupSlotEntity.Id = 0
                              ClubId = club.Id
                              FormationName = formationToString lineup.Formation
                              TacticsName = lineup.TeamTactics
                              SlotIndex = slot.Index
                              Role = string slot.Role
                              X = slot.X
                              Y = slot.Y
                              PlayerId = slot.PlayerId |> Option.defaultValue -1 }
                        )
                        |> ignore
                | None -> ()

            // Players
            for KeyValue(_, player) in state.Players do
                db.InsertOrReplace(toPlayerEntity player) |> ignore

            // Competitions + club membership
            db.DeleteAll<CompetitionClubEntity>() |> ignore

            for KeyValue(_, comp) in state.Competitions do
                db.InsertOrReplace(toCompetitionEntity comp) |> ignore

                for clubId in comp.ClubIds do
                    db.Insert(
                        { CompetitionClubEntity.Id = 0
                          CompetitionId = comp.Id
                          ClubId = clubId }
                    )
                    |> ignore

            // Fixtures
            for KeyValue(_, fixture) in state.Fixtures do
                db.InsertOrReplace(toFixtureEntity fixture) |> ignore


            for KeyValue(_, tie) in state.KnockoutTies do
                db.InsertOrReplace(toKnockoutTieEntity tie) |> ignore


            for KeyValue(_, country) in state.Countries do
                db.InsertOrReplace(toCountryEntity country) |> ignore)


    let loadGame () : GameState option =
        if not (File.Exists dbPath) then
            None
        else
            use db = getConnection ()
            let meta = db.Table<GameSaveMeta>().FirstOrDefault()

            if box meta = null then
                None
            else
                // Players
                let players: Map<PlayerId, Player> =
                    db.Table<PlayerEntity>().ToList()
                    |> Seq.map toPlayerDomain
                    |> Seq.map (fun p -> p.Id, p)
                    |> Map.ofSeq

                // Clubs
                let clubEntities = db.Table<ClubEntity>().ToList() |> List.ofSeq
                let lineupSlots = db.Table<LineupSlotEntity>().ToList() |> List.ofSeq

                let clubs: Map<ClubId, Club> =
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
                            |> List.sortBy (fun ls -> ls.SlotIndex)

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


                let compClubs = db.Table<CompetitionClubEntity>().ToList() |> List.ofSeq

                let competitions: Map<CompetitionId, Competition> =
                    db.Table<CompetitionEntity>().ToList()
                    |> Seq.map (fun ce ->
                        let clubIds =
                            compClubs
                            |> List.filter (fun cc -> cc.CompetitionId = ce.Id)
                            |> List.map (fun cc -> cc.ClubId)

                        ce.Id, toCompetitionDomain ce clubIds)
                    |> Map.ofSeq


                let fixtures: Map<MatchId, MatchFixture> =
                    db.Table<MatchFixtureEntity>().ToList()
                    |> Seq.map toFixtureDomain
                    |> Seq.map (fun f -> f.Id, f)
                    |> Map.ofSeq


                let knockoutTies: Map<int, KnockoutTie> =
                    db.Table<KnockoutTieEntity>().ToList()
                    |> Seq.map toKnockoutTieDomain
                    |> Seq.map (fun t -> t.TieId, t)
                    |> Map.ofSeq

                // Countries
                let countries: Map<CountryCode, Country> =
                    db.Table<CountryEntity>().ToList()
                    |> Seq.map toCountryDomain
                    |> Seq.map (fun c -> c.Code, c)
                    |> Map.ofSeq

                Some
                    { CurrentDate = meta.CurrentDate
                      Season = meta.Season
                      Clubs = clubs
                      Players = players
                      Competitions = competitions
                      Fixtures = fixtures
                      KnockoutTies = knockoutTies
                      UserClubId = meta.UserClubId
                      ManagerName = meta.ManagerName
                      PrimaryCountry = meta.PrimaryCountry
                      Countries = countries }

    // --- SCHEMA INIT ---

    let initTables () =
        use db = getConnection ()
        db.CreateTable<PlayerEntity>() |> ignore
        db.CreateTable<ClubEntity>() |> ignore
        db.CreateTable<LineupSlotEntity>() |> ignore
        db.CreateTable<CompetitionEntity>() |> ignore
        db.CreateTable<CompetitionClubEntity>() |> ignore
        db.CreateTable<MatchFixtureEntity>() |> ignore
        db.CreateTable<KnockoutTieEntity>() |> ignore
        db.CreateTable<CountryEntity>() |> ignore
        db.CreateTable<GameSaveMeta>() |> ignore
