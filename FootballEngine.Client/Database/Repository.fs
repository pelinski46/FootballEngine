namespace FootballEngine

open System
open System.IO
open FootballEngine.Domain
open TacticalInstructions
open FootballEngine.Database
open FootballEngine.Database.Mappers
open SQLite

module Db =

    let private dbPath =
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "football_engine.db3")

    let private db = lazy SQLiteAsyncConnection(dbPath)

    let saveGameAsync (state: GameState) =
        task {
            do!
                db.Value.RunInTransactionAsync(fun (conn: SQLiteConnection) ->

                    conn.InsertOrReplace(
                        { GameSaveMeta.Id = 1
                          CurrentDate = state.CurrentDate
                          Season = state.Season
                          UserClubId = state.UserClubId
                          UserStaffId = state.UserStaffId
                          PrimaryCountry = state.PrimaryCountry }
                    )
                    |> ignore

                    for club in state.Clubs.Values do
                        conn.InsertOrReplace(toClubEntity club) |> ignore

                    for player in state.Players.Values do
                        conn.InsertOrReplace(toPlayerEntity player) |> ignore

                    for staff in state.Staff.Values do
                        conn.InsertOrReplace(toStaffEntity staff) |> ignore

                    // Save lineups only for head coaches
                    for staff in state.Staff.Values do
                        match staff.Role, staff.Contract, staff.Attributes.Coaching.Lineup with
                        | HeadCoach, Some contract, Some lineup ->
                            let instructions = lineup.Instructions |> Option.defaultValue TacticalInstructions.defaultInstructions
                            
                            // Delete existing slots for this club before inserting new ones
                            conn.Execute("DELETE FROM LineupSlotEntity WHERE ClubId = ?", contract.ClubId)
                            |> ignore

                            for slot in lineup.Slots do
                                conn.Insert(
                                    { LineupSlotEntity.Id = 0
                                      ClubId = contract.ClubId
                                      FormationName = Serializers.formationToString lineup.Formation
                                      TacticsName = Serializers.tacticsToString lineup.Tactics
                                      Mentality = instructions.Mentality
                                      DefensiveLine = instructions.DefensiveLine
                                      PressingIntensity = instructions.PressingIntensity
                                      SlotIndex = slot.Index
                                      Role = string slot.Role
                                      X = slot.X
                                      Y = slot.Y
                                      PlayerId = slot.PlayerId |> Option.defaultValue -1 }
                                )
                                |> ignore
                        | _ -> ()

                    conn.DeleteAll<CompetitionClubEntity>() |> ignore
                    conn.DeleteAll<LeagueStandingEntity>() |> ignore
                    conn.DeleteAll<MatchFixtureEntity>() |> ignore

                    for comp in state.Competitions.Values do
                        conn.InsertOrReplace(toCompetitionEntity comp) |> ignore

                        for clubId in comp.ClubIds do
                            conn.Insert(
                                { CompetitionClubEntity.Id = 0
                                  CompetitionId = comp.Id
                                  ClubId = clubId }
                            )
                            |> ignore

                        for fixture in comp.Fixtures.Values do
                            conn.Insert(toFixtureEntity fixture) |> ignore

                        for standing in comp.Standings.Values do
                            conn.Insert(toStandingEntity comp.Id standing) |> ignore

                        for tie in comp.KnockoutTies.Values do
                            conn.InsertOrReplace(toKnockoutTieEntity comp.Id tie) |> ignore

                    for country in state.Countries.Values do
                        conn.InsertOrReplace(toCountryEntity country) |> ignore)
        }

    let loadGame () =
        task {
            if not (File.Exists dbPath) then
                return None
            else
                let! meta = db.Value.Table<GameSaveMeta>().FirstOrDefaultAsync()

                if box meta = null then
                    return None
                else
                    let! playerEntities = db.Value.Table<PlayerEntity>().ToListAsync()

                    let players =
                        playerEntities
                        |> Seq.map toPlayerDomain
                        |> Seq.map (fun p -> p.Id, p)
                        |> Map.ofSeq

                    let! staffEntities = db.Value.Table<StaffEntity>().ToListAsync()

                    let staff = staffEntities |> Seq.map toStaffDomain |> Map.ofSeq

                    let! lineupSlots = db.Value.Table<LineupSlotEntity>().ToListAsync()
                    let! compClubs = db.Value.Table<CompetitionClubEntity>().ToListAsync()
                    let! allFixtures = db.Value.Table<MatchFixtureEntity>().ToListAsync()
                    let! allStandings = db.Value.Table<LeagueStandingEntity>().ToListAsync()
                    let! allTies = db.Value.Table<KnockoutTieEntity>().ToListAsync()
                    let! clubEntities = db.Value.Table<ClubEntity>().ToListAsync()

                    let clubs =
                        clubEntities |> Seq.map (fun ce -> toClubDomain players staff ce) |> Map.ofSeq

                    let lineupByClub =
                        lineupSlots
                        |> List.ofSeq
                        |> List.groupBy (fun s -> s.ClubId)
                        |> List.choose (fun (clubId, slots) ->
                            match slots with
                            | [] -> None
                            | first :: _ ->
                                let formation = Serializers.parseFormation first.FormationName
                                let tactics = Serializers.parseTactics first.TacticsName

                                let instructions =
                                    { Mentality = first.Mentality
                                      DefensiveLine = first.DefensiveLine
                                      PressingIntensity = first.PressingIntensity }

                                let slotsList =
                                    slots
                                    |> List.sortBy (fun s -> s.SlotIndex)
                                    |> List.map (fun s ->
                                        { Index = s.SlotIndex
                                          Role = Serializers.parsePosition s.Role
                                          X = s.X
                                          Y = s.Y
                                          PlayerId = if s.PlayerId = -1 then None else Some s.PlayerId })

                                Some(
                                    clubId,
                                    { Formation = formation
                                      Tactics = tactics
                                      Instructions = Some instructions
                                      Slots = slotsList }
                                ))

                    let updatedStaff =
                        lineupByClub
                        |> List.fold
                            (fun (staffMap: Map<StaffId, Staff>) (clubId, lineup) ->
                                let headCoach =
                                    staffMap
                                    |> Map.values
                                    |> Seq.tryFind (fun s ->
                                        s.Role = HeadCoach
                                        && s.Contract |> Option.map (fun c -> c.ClubId) = Some clubId)

                                match headCoach with
                                | Some coach ->
                                    let updatedCoach =
                                        { coach with
                                            Attributes.Coaching.Lineup = Some lineup }

                                    staffMap |> Map.add coach.Id updatedCoach
                                | None -> staffMap)
                            staff

                    let! compEntities = db.Value.Table<CompetitionEntity>().ToListAsync()

                    let competitions =
                        compEntities
                        |> Seq.map (
                            toCompetitionDomain
                                (List.ofSeq compClubs)
                                (List.ofSeq allFixtures)
                                (List.ofSeq allStandings)
                                (List.ofSeq allTies)
                        )
                        |> Map.ofSeq

                    let! countryEntities = db.Value.Table<CountryEntity>().ToListAsync()

                    let countries =
                        countryEntities
                        |> Seq.map toCountryDomain
                        |> Seq.map (fun c -> c.Code, c)
                        |> Map.ofSeq

                    return
                        Some
                            { CurrentDate = meta.CurrentDate
                              Season = meta.Season
                              Clubs = clubs
                              Players = players
                              Staff = updatedStaff
                              Competitions = competitions
                              Countries = countries
                              UserClubId = meta.UserClubId
                              UserStaffId = meta.UserStaffId
                              PrimaryCountry = meta.PrimaryCountry }
        }

    let initTables () =
        task {
            let! _ = db.Value.CreateTableAsync<PlayerEntity>()
            let! _ = db.Value.CreateTableAsync<ClubEntity>()
            let! _ = db.Value.CreateTableAsync<LineupSlotEntity>()
            let! _ = db.Value.CreateTableAsync<StaffEntity>()
            let! _ = db.Value.CreateTableAsync<CompetitionEntity>()
            let! _ = db.Value.CreateTableAsync<CompetitionClubEntity>()
            let! _ = db.Value.CreateTableAsync<LeagueStandingEntity>()
            let! _ = db.Value.CreateTableAsync<MatchFixtureEntity>()
            let! _ = db.Value.CreateTableAsync<KnockoutTieEntity>()
            let! _ = db.Value.CreateTableAsync<CountryEntity>()
            let! _ = db.Value.CreateTableAsync<GameSaveMeta>()
            return ()
        }
