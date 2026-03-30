namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats

module TrainingEngine =

    let focusMultiplier (focus: TrainingFocus) (position: Position) : float =
        match focus, position with
        | TrainingFocus.TrainingAllRound, _ -> 1.0
        | TrainingFocus.TrainingGoalkeeping, GK -> 2.0
        | TrainingFocus.TrainingGoalkeeping, _ -> 0.5
        | TrainingFocus.TrainingPhysical, (DC | DM | DR | DL | WBR | WBL) -> 2.0
        | TrainingFocus.TrainingPhysical, (MC | MR | ML) -> 1.5
        | TrainingFocus.TrainingPhysical, (GK | ST | AML | AMR | AMC) -> 0.7
        | TrainingFocus.TrainingTechnical, (ST | AML | AMR | AMC) -> 2.0
        | TrainingFocus.TrainingTechnical, (MC | MR | ML) -> 1.5
        | TrainingFocus.TrainingTechnical, (GK | DC | DM | DR | DL | WBR | WBL) -> 0.5
        | TrainingFocus.TrainingMental, _ -> 1.2

    let intensityEffect (intensity: TrainingIntensity) : TrainingIntensityData =
        TrainingIntensityData.get intensity

    let private applyWeeklySkillDevelopment (schedule: TrainingSchedule) (age: int) (currentSkill: int) (potential: int) (position: Position) (p: Player) : Player =
        let gap = potential - currentSkill
        let baseDelta =
            match age with
            | a when a <= 20 -> min gap 4
            | a when a <= 24 -> min gap 3
            | a when a <= 27 -> min gap 2
            | a when a <= 30 -> 1
            | _ -> 0

        if baseDelta <= 0 then p
        else
            let focusMult = focusMultiplier schedule.Focus position
            let intensityMult = intensityEffect schedule.Intensity |> fun e -> e.DeltaMultiplier
            let weeklyDelta = float baseDelta * focusMult * intensityMult / 120.0

            let roll = rollProbability ()
            let shouldIncrease = roll < weeklyDelta
            
            if shouldIncrease then
                { p with
                    CurrentSkill = min potential (p.CurrentSkill + 1)
                    Physical =
                        { p.Physical with
                            Stamina = min 20 (p.Physical.Stamina + 1) }
                    Technical =
                        match position with
                        | ST | AML | AMR | AMC ->
                            { p.Technical with
                                Finishing = min 20 (p.Technical.Finishing + 1)
                                Dribbling = min 20 (p.Technical.Dribbling + 1) }
                        | DC | DM ->
                            { p.Technical with
                                Tackling = min 20 (p.Technical.Tackling + 1)
                                Marking = min 20 (p.Technical.Marking + 1) }
                        | MC | MR | ML ->
                            { p.Technical with
                                Passing = min 20 (p.Technical.Passing + 1)
                                BallControl = min 20 (p.Technical.BallControl + 1) }
                        | GK -> p.Technical
                        | _ ->
                            { p.Technical with
                                Passing = min 20 (p.Technical.Passing + 1) }
                    Mental =
                        { p.Mental with
                            Vision = min 20 (p.Mental.Vision + 1)
                            Positioning = min 20 (p.Mental.Positioning + 1) }
                    Goalkeeping =
                        if position = GK then
                            { p.Goalkeeping with
                                Reflexes = min 20 (p.Goalkeeping.Reflexes + 1)
                                OneOnOne = min 20 (p.Goalkeeping.OneOnOne + 1)
                                Handling = min 20 (p.Goalkeeping.Handling + 1) }
                        else
                            p.Goalkeeping }
            else
                p

    let applyWeeklyTraining (currentDate: DateTime) (schedule: TrainingSchedule) (player: Player) : Player =
        let effect = intensityEffect schedule.Intensity

        let newCondition = System.Math.Clamp(player.Condition + effect.ConditionCost, 0, 100)

        let newMorale = System.Math.Clamp(player.Morale + effect.MoraleChange, 0, 100)

        let injuryRoll = rollProbability ()

        let playerWithCondition =
            if injuryRoll < effect.InjuryRisk then
                { player with
                    Status = Injured(Minor, currentDate.AddDays(7.0))
                    Condition = newCondition
                    Morale = newMorale }
            else
                { player with
                    Condition = newCondition
                    Morale = newMorale }

        let age = Player.age currentDate playerWithCondition
        applyWeeklySkillDevelopment schedule age playerWithCondition.CurrentSkill playerWithCondition.PotentialSkill playerWithCondition.Position playerWithCondition

    let applyRemainingSeasonTraining (currentDate: DateTime) (weeksApplied: int) (totalWeeks: int) (gs: GameState) : GameState =
        let remaining = max 0 (totalWeeks - weeksApplied)

        if remaining = 0 then gs
        else
            { gs with
                Players =
                    gs.Players
                    |> Map.map (fun _ player ->
                        match player.Affiliation with
                        | Contracted _ ->
                            let effect = intensityEffect player.TrainingSchedule.Intensity
                            let totalCost = effect.ConditionCost * remaining
                            let newCondition = System.Math.Clamp(player.Condition + totalCost, 0, 100)
                            let cumulativeRisk = 1.0 - (1.0 - effect.InjuryRisk) ** float remaining
                            let injuryRoll = rollProbability ()
                            let injured = injuryRoll < cumulativeRisk

                            { player with
                                Condition = newCondition
                                Status =
                                    if injured then
                                        Injured(Minor, currentDate.AddDays(14.0))
                                    else
                                        player.Status }
                        | _ -> player) }
