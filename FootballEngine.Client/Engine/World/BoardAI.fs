namespace FootballEngine

open FootballEngine.Domain

type BoardDecision =
    | MaintainCoach of clubId: ClubId
    | PressureCoach of clubId: ClubId * coachId: StaffId
    | DismissCoach of clubId: ClubId * coachId: StaffId
    | SetObjective of clubId: ClubId * objective: BoardObjective

module ObjectiveEvaluation =

    let private leaguePos (clubId: ClubId) (comp: Competition) : int option =
        comp.Standings
        |> Map.toList
        |> List.sortByDescending (fun (_, s) -> s.Points, s.Won)
        |> List.tryFindIndex (fun (id, _) -> id = clubId)
        |> Option.map ((+) 1)

    let private totalTeams (comp: Competition) = comp.Standings |> Map.count

    let private cupWon (clubId: ClubId) (comp: Competition) =
        comp.KnockoutTies
        |> Map.values
        |> Seq.exists (fun t -> t.Round = Final && t.WinnerId = Some clubId)

    let evaluate (clubId: ClubId) (objective: BoardObjective) (comps: Map<CompetitionId, Competition>) : bool =
        comps
        |> Map.values
        |> Seq.exists (fun comp ->
            match comp.Type, objective with
            | NationalLeague(LeagueLevel 0, _), LeagueObjective WinLeague ->
                leaguePos clubId comp = Some 1

            | NationalLeague(LeagueLevel 0, _), LeagueObjective TopFour ->
                leaguePos clubId comp |> Option.exists (fun p -> p <= 4)

            | NationalLeague(LeagueLevel 0, _), LeagueObjective TopHalf ->
                leaguePos clubId comp |> Option.exists (fun p -> p <= totalTeams comp / 2)

            | NationalLeague(LeagueLevel 0, _), LeagueObjective MidTable ->
                let n = totalTeams comp
                leaguePos clubId comp |> Option.exists (fun p -> p > n / 3 && p <= n * 2 / 3)

            | NationalLeague(LeagueLevel 0, _), LeagueObjective Survival ->
                leaguePos clubId comp |> Option.exists (fun p -> p <= totalTeams comp - 3)

            | NationalLeague(LeagueLevel lvl, _), Promotion when lvl > 0 ->
                leaguePos clubId comp |> Option.exists (fun p -> p <= 2)

            | NationalCup _, CupObjective WinDomesticCup -> cupWon clubId comp
            | InternationalCup _, CupObjective WinChampionsLeague -> cupWon clubId comp
            | InternationalCup _, CupObjective WinContinentalCup -> cupWon clubId comp

            | _ -> false)

module ObjectiveSetting =

    let suggest (club: Club) (comps: Map<CompetitionId, Competition>) : BoardObjective =
        let tier = club.Reputation / 2000

        let isTopFlight =
            comps
            |> Map.values
            |> Seq.exists (fun comp ->
                match comp.Type with
                | NationalLeague(LeagueLevel 0, _) -> comp.ClubIds |> List.contains club.Id
                | _ -> false)

        match tier, isTopFlight with
        | t, true when t >= 4 -> LeagueObjective WinLeague
        | t, true when t >= 3 -> LeagueObjective TopFour
        | t, true when t >= 2 -> LeagueObjective TopHalf
        | _, true -> LeagueObjective Survival
        | _, false -> Promotion

module BoardAI =

    let private evaluateCoach (clubId: ClubId) (coach: Staff) (gs: GameState) : BoardDecision =
        let club = gs.Clubs[clubId]
        let achieved = ObjectiveEvaluation.evaluate clubId club.BoardObjective gs.Competitions
        let updated = Staff.applySeasonOutcome club.BoardObjective achieved coach

        match updated.Status with
        | Sacked -> DismissCoach(clubId, coach.Id)
        | UnderPressure -> PressureCoach(clubId, coach.Id)
        | _ -> MaintainCoach clubId

    let private bestAvailableCoach (gs: GameState) : Staff option =
        GameState.unemployedStaff gs
        |> Seq.filter (fun s -> s.Role = HeadCoach || Staff.canBecomeHeadCoach s)
        |> Seq.sortByDescending Staff.effectiveAbility
        |> Seq.tryHead

    let private hireCoach (clubId: ClubId) (candidate: Staff) (gs: GameState) : GameState =
        let hired =
            Staff.promoteToHeadCoach
                { candidate with
                    Status = Active
                    Contract = Some { ClubId = clubId; Salary = 50_000m; ExpiryYear = gs.Season + 3 } }
        GameState.updateStaff hired gs

    let private applyDecision (gs: GameState) (decision: BoardDecision) : GameState =
        match decision with
        | MaintainCoach _ -> gs

        | PressureCoach(_, coachId) ->
            match Map.tryFind coachId gs.Staff with
            | Some coach -> GameState.updateStaff { coach with Status = UnderPressure } gs
            | None -> gs

        | DismissCoach(clubId, coachId) ->
            let gs2 =
                match Map.tryFind coachId gs.Staff with
                | Some coach -> GameState.updateStaff { coach with Status = Unemployed; Contract = None } gs
                | None -> gs

            match bestAvailableCoach gs2 with
            | Some candidate -> hireCoach clubId candidate gs2
            | None -> gs2

        | SetObjective(clubId, objective) ->
            match Map.tryFind clubId gs.Clubs with
            | Some club -> { gs with Clubs = gs.Clubs |> Map.add clubId { club with BoardObjective = objective } }
            | None -> gs

    let private updateCoachRecord (clubId: ClubId) (gs: GameState) : GameState =
        match GameState.headCoach clubId gs with
        | None -> gs
        | Some coach ->
            let club = gs.Clubs[clubId]
            let achieved = ObjectiveEvaluation.evaluate clubId club.BoardObjective gs.Competitions
            let updated = Staff.applySeasonOutcome club.BoardObjective achieved coach
            GameState.updateStaff updated gs

    let private refreshObjective (clubId: ClubId) (gs: GameState) : GameState =
        match Map.tryFind clubId gs.Clubs with
        | None -> gs
        | Some club ->
            let objective = ObjectiveSetting.suggest club gs.Competitions
            { gs with Clubs = gs.Clubs |> Map.add clubId { club with BoardObjective = objective } }

    let runEndOfSeason (gs: GameState) : GameState =
        let aiClubs =
            gs.Clubs
            |> Map.keys
            |> Seq.filter (fun id -> id <> gs.UserClubId)
            |> List.ofSeq

        let gs2 = aiClubs |> List.fold (fun s id -> updateCoachRecord id s) gs

        let decisions =
            aiClubs
            |> List.choose (fun clubId ->
                match GameState.headCoach clubId gs2 with
                | Some coach -> Some(evaluateCoach clubId coach gs2)
                | None -> None)

        let gs3 = decisions |> List.fold applyDecision gs2
        aiClubs |> List.fold (fun s id -> refreshObjective id s) gs3
