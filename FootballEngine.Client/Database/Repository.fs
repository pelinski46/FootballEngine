namespace FootballEngine

open System
open System.IO
open FootballEngine.Domain
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
                          ManagerName = state.ManagerName
                          PrimaryCountry = state.PrimaryCountry }
                    )
                    |> ignore

                    for KeyValue(_, club) in state.Clubs do
                        conn.InsertOrReplace(toClubEntity club) |> ignore

                        match club.CurrentLineup with
                        | None -> ()
                        | Some lineup ->
                            conn.Execute("DELETE FROM LineupSlotEntity WHERE ClubId = ?", club.Id) |> ignore

                            for slot in lineup.Slots do
                                conn.Insert(
                                    { LineupSlotEntity.Id = 0
                                      ClubId = club.Id
                                      FormationName = Serializers.formationToString lineup.Formation
                                      TacticsName = lineup.TeamTactics
                                      SlotIndex = slot.Index
                                      Role = string slot.Role
                                      X = slot.X
                                      Y = slot.Y
                                      PlayerId = slot.PlayerId |> Option.defaultValue -1 }
                                )
                                |> ignore

                    for KeyValue(_, player) in state.Players do
                        conn.InsertOrReplace(toPlayerEntity player) |> ignore

                    conn.DeleteAll<CompetitionClubEntity>() |> ignore
                    conn.DeleteAll<LeagueStandingEntity>() |> ignore
                    conn.DeleteAll<MatchFixtureEntity>() |> ignore

                    for KeyValue(_, comp) in state.Competitions do
                        conn.InsertOrReplace(toCompetitionEntity comp) |> ignore

                        for clubId in comp.ClubIds do
                            conn.Insert(
                                { CompetitionClubEntity.Id = 0
                                  CompetitionId = comp.Id
                                  ClubId = clubId }
                            )
                            |> ignore

                        for KeyValue(_, fixture) in comp.Fixtures do
                            conn.Insert(toFixtureEntity fixture) |> ignore

                        for KeyValue(_, standing) in comp.Standings do
                            conn.Insert(toStandingEntity comp.Id standing) |> ignore

                        for KeyValue(_, tie) in comp.KnockoutTies do
                            conn.InsertOrReplace(toKnockoutTieEntity comp.Id tie) |> ignore

                    for KeyValue(_, country) in state.Countries do
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

                    let! lineupSlots = db.Value.Table<LineupSlotEntity>().ToListAsync()
                    let! compClubs = db.Value.Table<CompetitionClubEntity>().ToListAsync()
                    let! allFixtures = db.Value.Table<MatchFixtureEntity>().ToListAsync()
                    let! allStandings = db.Value.Table<LeagueStandingEntity>().ToListAsync()
                    let! allTies = db.Value.Table<KnockoutTieEntity>().ToListAsync()
                    let! clubEntities = db.Value.Table<ClubEntity>().ToListAsync()

                    let clubs =
                        clubEntities
                        |> Seq.map (toClubDomain players (List.ofSeq lineupSlots))
                        |> Map.ofSeq

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
                              Competitions = competitions
                              Countries = countries
                              UserClubId = meta.UserClubId
                              ManagerName = meta.ManagerName
                              PrimaryCountry = meta.PrimaryCountry }
        }

    let initTables () =
        task {
            let! _ = db.Value.CreateTableAsync<PlayerEntity>()
            let! _ = db.Value.CreateTableAsync<ClubEntity>()
            let! _ = db.Value.CreateTableAsync<LineupSlotEntity>()
            let! _ = db.Value.CreateTableAsync<CompetitionEntity>()
            let! _ = db.Value.CreateTableAsync<CompetitionClubEntity>()
            let! _ = db.Value.CreateTableAsync<LeagueStandingEntity>()
            let! _ = db.Value.CreateTableAsync<MatchFixtureEntity>()
            let! _ = db.Value.CreateTableAsync<KnockoutTieEntity>()
            let! _ = db.Value.CreateTableAsync<CountryEntity>()
            let! _ = db.Value.CreateTableAsync<GameSaveMeta>()
            return ()
        }
