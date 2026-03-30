namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Domain.TransferNegotiation
open FootballEngine.Stats

module ManagerPersonality =

    let private norm (v: int) = float v / 20.0

    let ambition (coach: Staff) =
        (norm coach.Mental.Determination + norm coach.Knowledge.JudgingPlayerPotential)
        / 2.0

    let riskTolerance (coach: Staff) =
        (norm coach.Mental.Adaptability + norm coach.Mental.Determination) / 2.0

    let youthFocus (coach: Staff) =
        norm coach.Knowledge.JudgingPlayerPotential

    let scoutingAccuracy (coach: Staff) =
        (norm coach.Knowledge.JudgingPlayerAbility
         + norm coach.Knowledge.JudgingPlayerPotential)
        / 2.0

    let loyalty (coach: Staff) =
        (norm coach.Mental.LevelOfDiscipline + norm coach.Mental.PeopleManagement) / 2.0

    let spendingAggression (coach: Staff) (budget: decimal) =
        let budgetComfort = min 1.0 (log (1.0 + float budget / 5_000_000.0) / log 11.0)
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

    let private averageAge (currentDate: System.DateTime) (players: Player list) =
        if players.IsEmpty then 0.0
        else players |> List.averageBy (fun p -> Player.age currentDate p |> float)

    let assessNeeds (currentDate: System.DateTime) (squad: Player list) : SquadNeed list =
        let avg = averageSkill squad

        let gkNeeds =
            match countAvailable GK squad with
            | 0 -> [ NeedsPosition GK ]
            | 1 -> [ NeedsDepth GK ]
            | _ -> []

        let outfieldNeeds =
            allOutfieldPositions
            |> List.collect (fun pos ->
                let atPos = squad |> List.filter (fun p -> p.Position = pos && p.Status = Available)
                let count = List.length atPos
                let avgAgeAtPos = averageAge currentDate atPos

                match count with
                | 0 -> [ NeedsPosition pos ]
                | 1 ->
                    let agingOut = avgAgeAtPos > 30.0
                    if agingOut then [ NeedsDepth pos; NeedsPosition pos ]
                    else [ NeedsDepth pos ]
                | _ ->
                    let allAging = avgAgeAtPos > 31.0
                    if allAging then [ NeedsDepth pos ] else [])

        let qualityNeed = if avg < 65.0 then [ NeedsQualityUpgrade(int avg) ] else []

        gkNeeds @ outfieldNeeds @ qualityNeed

    let assessSales (coach: Staff) (currentDate: System.DateTime) (squad: Player list) : PlayerId list =
        let avg = averageSkill squad
        let loyalty = ManagerPersonality.loyalty coach

        let positionCounts =
            allOutfieldPositions
            |> List.map (fun pos ->
                pos, squad |> List.filter (fun p -> p.Position = pos && p.Status = Available) |> List.length)
            |> Map.ofList

        squad
        |> List.filter (fun p ->
            let isWeak = float p.CurrentSkill < avg - 15.0

            let contractExpiry = Player.contractOf p |> Option.map _.ExpiryYear

            let isExpiring = contractExpiry |> Option.exists (fun y -> y <= 0)

            let isOldAndExpiring =
                contractExpiry
                |> Option.exists (fun y -> Player.age currentDate p > 32 && y <= 1)

            let isExcessCoverage =
                let count = positionCounts |> Map.tryFind p.Position |> Option.defaultValue 0
                count >= 3 && float p.CurrentSkill < avg - 5.0

            let sellChance =
                if isWeak && isExpiring then 0.85
                elif isOldAndExpiring then 0.6
                elif isExcessCoverage then loyalty |> fun l -> 0.3 * (1.0 - l)
                else 0.0

            bernoulli sellChance)
        |> List.map _.Id

module TargetSelection =

    let private perceivedSkill (coach: Staff) (p: Player) : float =
        let accuracy = ManagerPersonality.scoutingAccuracy coach
        let mean = float p.CurrentSkill * (0.8 + accuracy * 0.4)
        let stdDev = (1.0 - accuracy) * 12.0
        normalFloat mean stdDev (float p.CurrentSkill * 0.5) (float p.CurrentSkill * 1.5)

    let private perceivedPotential (coach: Staff) (p: Player) : float =
        let accuracy = ManagerPersonality.scoutingAccuracy coach
        let youthFocus = ManagerPersonality.youthFocus coach
        let mean = float p.PotentialSkill * (0.7 + accuracy * 0.3)
        let stdDev = (1.0 - accuracy) * 15.0 * (1.0 - youthFocus * 0.5)
        normalFloat mean stdDev (float p.CurrentSkill) 200.0

    let private potentialBonus (coach: Staff) (p: Player) : float =
        if ManagerPersonality.youthFocus coach > 0.5 then
            let perceived = perceivedPotential coach p
            (perceived - float p.CurrentSkill) * ManagerPersonality.youthFocus coach * 0.4
        else
            0.0

    let private scoreAgainstNeed (coach: Staff) (need: SquadNeed) (p: Player) : float =
        let base' = perceivedSkill coach p + potentialBonus coach p

        match need with
        | NeedsPosition pos when p.Position = pos -> base' + 25.0
        | NeedsDepth pos when p.Position = pos -> base' + 10.0
        | NeedsQualityUpgrade minSkill when p.CurrentSkill > minSkill -> base' * 0.8
        | _ -> 0.0

    let private maxFeeFor (coach: Staff) (budget: decimal) (p: Player) : decimal =
        let aggression = decimal (ManagerPersonality.spendingAggression coach budget)
        let riskMult = decimal (0.85 + ManagerPersonality.riskTolerance coach * 0.3)
        suggestedFee p * riskMult * (0.85m + aggression * 0.4m)

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
        let riskTolerance = ManagerPersonality.riskTolerance coach

        let urgency =
            needs
            |> List.sumBy (function
                | NeedsPosition _ -> 2.0
                | NeedsDepth _ -> 1.0
                | NeedsQualityUpgrade _ -> 0.5)

        let urgencyMult = 1.0 + urgency * 0.08
        let riskMult = 0.3 + riskTolerance * 0.3

        budget * decimal (aggression * urgencyMult * riskMult)
        |> fun b -> min b (budget * 0.7m)

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
            let needs = SquadAnalysis.assessNeeds gs.CurrentDate squad
            let toSell = SquadAnalysis.assessSales coach gs.CurrentDate squad
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

    let decideTrainingForPlayer (currentDate: DateTime) (coach: Staff) (player: Player) : TrainingSchedule =
        let age = Player.age currentDate player
        let potentialGap = player.PotentialSkill - player.CurrentSkill

        let focus =
            match player.Position with
            | GK -> TrainingFocus.TrainingGoalkeeping
            | DC | DM | DR | DL | WBR | WBL ->
                if potentialGap > 15 then TrainingFocus.TrainingPhysical
                elif player.CurrentSkill < 70 then TrainingFocus.TrainingTechnical
                else TrainingFocus.TrainingMental
            | MC | AMC | MR | ML ->
                if potentialGap > 15 then TrainingFocus.TrainingTechnical
                elif player.CurrentSkill < 70 then TrainingFocus.TrainingMental
                else TrainingFocus.TrainingAllRound
            | ST | AML | AMR ->
                if potentialGap > 15 then TrainingFocus.TrainingTechnical
                elif player.CurrentSkill < 70 then TrainingFocus.TrainingPhysical
                else TrainingFocus.TrainingAllRound

        let ambition = ManagerPersonality.ambition coach

        let intensity =
            if age > 30 || player.Condition < 60 then
                TrainingIntensity.TrainingLight
            elif age < 21 && player.Condition > 85 && potentialGap > 10 then
                TrainingIntensity.TrainingHeavy
            elif ambition > 0.7 && player.Condition > 70 then
                TrainingIntensity.TrainingHeavy
            else
                TrainingIntensity.TrainingNormal

        { Focus = focus
          Intensity = intensity }