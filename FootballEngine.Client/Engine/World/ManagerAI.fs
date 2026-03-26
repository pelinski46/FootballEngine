namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Domain.TransferNegotiation

type SquadNeed =
    | NeedsPosition of Position
    | NeedsDepth of Position
    | NeedsQualityUpgrade of aboveSkill: int

type TransferTarget =
    { PlayerId: PlayerId
      Priority: float
      MaxFee: decimal }

type ManagerIntent =
    { ClubId: ClubId
      CoachId: StaffId
      Needs: SquadNeed list
      Targets: TransferTarget list
      PlayersToSell: PlayerId list
      BudgetAllocation: decimal }

module ManagerPersonality =

    let private norm (v: int) = float v / 20.0

    let ambition (coach: Staff) =
        (norm coach.Mental.Determination + norm coach.Knowledge.JudgingPlayerPotential) / 2.0

    let riskTolerance (coach: Staff) =
        (norm coach.Mental.Adaptability + norm coach.Mental.Determination) / 2.0

    let youthFocus (coach: Staff) =
        norm coach.Knowledge.JudgingPlayerPotential

    let scoutingAccuracy (coach: Staff) =
        (norm coach.Knowledge.JudgingPlayerAbility + norm coach.Knowledge.JudgingPlayerPotential) / 2.0

    let loyalty (coach: Staff) =
        (norm coach.Mental.LevelOfDiscipline + norm coach.Mental.PeopleManagement) / 2.0

    let spendingAggression (coach: Staff) (budget: decimal) =
        let budgetComfort = min 1.0 (float budget / 10_000_000.0)
        ambition coach * budgetComfort

module SquadAnalysis =

    let private allOutfieldPositions =
        [ DR; DC; DL; WBR; WBL; DM; MR; MC; ML; AMR; AMC; AML; ST ]

    let private countAvailable (pos: Position) (squad: Player list) =
        squad
        |> List.filter (fun p -> p.Position = pos && p.Status = Available)
        |> List.length

    let private averageSkill (squad: Player list) =
        if squad.IsEmpty then 0.0
        else squad |> List.averageBy (fun p -> float p.CurrentSkill)

    let assessNeeds (squad: Player list) : SquadNeed list =
        let avg = averageSkill squad

        let gkNeeds =
            match countAvailable GK squad with
            | 0 -> [ NeedsPosition GK ]
            | 1 -> [ NeedsDepth GK ]
            | _ -> []

        let outfieldNeeds =
            allOutfieldPositions
            |> List.collect (fun pos ->
                match countAvailable pos squad with
                | 0 -> [ NeedsPosition pos ]
                | 1 -> [ NeedsDepth pos ]
                | _ -> [])

        let qualityNeed =
            if avg < 65.0 then [ NeedsQualityUpgrade(int avg) ]
            else []

        gkNeeds @ outfieldNeeds @ qualityNeed

    let assessSales (coach: Staff) (squad: Player list) : PlayerId list =
        let avg = averageSkill squad
        let loyalty = ManagerPersonality.loyalty coach

        squad
        |> List.filter (fun p ->
            let isWeak = float p.CurrentSkill < avg - 15.0
            let isExpiring =
                match p.Affiliation with
                | Contracted(_, c) -> c.ExpiryYear <= 0
                | _ -> false
            isWeak && isExpiring && not (Stats.bernoulli loyalty))
        |> List.map _.Id

module TargetSelection =

    let private perceivedSkill (coach: Staff) (p: Player) : float =
        let accuracy = ManagerPersonality.scoutingAccuracy coach
        float p.CurrentSkill * (0.8 + accuracy * 0.4)

    let private potentialBonus (coach: Staff) (p: Player) : float =
        if ManagerPersonality.youthFocus coach > 0.6 then
            float (p.PotentialSkill - p.CurrentSkill) * 0.3
        else
            0.0

    let private scoreAgainstNeed (coach: Staff) (need: SquadNeed) (p: Player) : float =
        let base' = perceivedSkill coach p + potentialBonus coach p
        match need with
        | NeedsPosition pos when p.Position = pos -> base' + 20.0
        | NeedsDepth pos when p.Position = pos -> base' + 10.0
        | NeedsQualityUpgrade minSkill when p.CurrentSkill > minSkill -> base'
        | _ -> 0.0

    let private maxFeeFor (coach: Staff) (budget: decimal) (p: Player) : decimal =
        let aggression = decimal (ManagerPersonality.spendingAggression coach budget)
        suggestedFee p * (0.85m + aggression * 0.4m)

    let selectTargets
        (coach: Staff)
        (club: Club)
        (needs: SquadNeed list)
        (candidates: Player list)
        : TransferTarget list =
        needs
        |> List.collect (fun need ->
            candidates
            |> List.choose (fun p ->
                let score = scoreAgainstNeed coach need p
                if score > 0.0 then Some(p, score) else None)
            |> List.sortByDescending snd
            |> List.truncate 2
            |> List.map (fun (p, score) ->
                { PlayerId = p.Id
                  Priority = score
                  MaxFee = maxFeeFor coach club.Budget p }))
        |> List.distinctBy _.PlayerId
        |> List.sortByDescending _.Priority
        |> List.truncate 5

module ManagerAI =

    let private budgetAllocation (coach: Staff) (budget: decimal) (needs: SquadNeed list) : decimal =
        let aggression = ManagerPersonality.spendingAggression coach budget
        let urgency =
            needs
            |> List.filter (function NeedsPosition _ -> true | _ -> false)
            |> List.length
            |> float
        budget * decimal (aggression * (1.0 + urgency * 0.1)) * 0.4m

    let private transferCandidates (clubId: ClubId) (gs: GameState) : Player list =
        gs.Players
        |> Map.values
        |> Seq.filter (fun p ->
            p.Status = Available
            && match p.Affiliation with
               | FreeAgent -> true
               | Contracted(cid, _) -> cid <> clubId
               | _ -> false)
        |> List.ofSeq

    let buildIntent (gs: GameState) (clubId: ClubId) : ManagerIntent option =
        match Map.tryFind clubId gs.Clubs, GameState.headCoach clubId gs with
        | Some club, Some coach ->
            let squad = GameState.getSquad clubId gs
            let needs = SquadAnalysis.assessNeeds squad
            let toSell = SquadAnalysis.assessSales coach squad
            let candidates = transferCandidates clubId gs
            let targets = TargetSelection.selectTargets coach club needs candidates
            let budget = budgetAllocation coach club.Budget needs

            Some
                { ClubId = clubId
                  CoachId = coach.Id
                  Needs = needs
                  Targets = targets
                  PlayersToSell = toSell
                  BudgetAllocation = budget }
        | _ -> None

    let buildAllIntents (gs: GameState) : ManagerIntent list =
        gs.Clubs
        |> Map.keys
        |> Seq.filter (fun id -> id <> gs.UserClubId)
        |> Seq.choose (buildIntent gs)
        |> List.ofSeq
