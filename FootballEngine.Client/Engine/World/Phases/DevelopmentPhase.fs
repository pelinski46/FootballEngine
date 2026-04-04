namespace FootballEngine.World.Phases

open System
open FootballEngine.Domain
open FootballEngine.Stats
open FootballEngine.World

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

    let intensityEffect (intensity: TrainingIntensity) : TrainingIntensityData = TrainingIntensityData.get intensity

    let private applyWeeklySkillDevelopment
        (schedule: TrainingSchedule)
        (age: int)
        (currentSkill: int)
        (potential: int)
        (position: Position)
        (p: Player)
        : Player =
        let gap = potential - currentSkill

        let baseDelta =
            match age with
            | a when a <= 20 -> min gap 4
            | a when a <= 24 -> min gap 3
            | a when a <= 27 -> min gap 2
            | a when a <= 30 -> 1
            | _ -> 0

        if baseDelta <= 0 then
            p
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
                        | ST
                        | AML
                        | AMR
                        | AMC ->
                            { p.Technical with
                                Finishing = min 20 (p.Technical.Finishing + 1)
                                Dribbling = min 20 (p.Technical.Dribbling + 1) }
                        | DC
                        | DM ->
                            { p.Technical with
                                Tackling = min 20 (p.Technical.Tackling + 1)
                                Marking = min 20 (p.Technical.Marking + 1) }
                        | MC
                        | MR
                        | ML ->
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

        let newCondition = Math.Clamp(player.Condition + effect.ConditionCost, 0, 100)

        let newMorale = Math.Clamp(player.Morale + effect.MoraleChange, 0, 100)

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

        applyWeeklySkillDevelopment
            schedule
            age
            playerWithCondition.CurrentSkill
            playerWithCondition.PotentialSkill
            playerWithCondition.Position
            playerWithCondition

    let applyRemainingSeasonTraining
        (currentDate: DateTime)
        (weeksApplied: int)
        (totalWeeks: int)
        (gs: GameState)
        : GameState =
        let remaining = max 0 (totalWeeks - weeksApplied)

        if remaining = 0 then
            gs
        else
            { gs with
                Players =
                    gs.Players
                    |> Map.map (fun _ player ->
                        match player.Affiliation with
                        | Contracted _ ->
                            let effect = intensityEffect player.TrainingSchedule.Intensity
                            let totalCost = effect.ConditionCost * remaining
                            let newCondition = Math.Clamp(player.Condition + totalCost, 0, 100)
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

module PlayerDevelopment =

    let private skillDelta
        (age: int)
        (skill: int)
        (potential: int)
        (schedule: TrainingSchedule option)
        (position: Position)
        : int =
        let gap = potential - skill

        match schedule with
        | None ->
            match age with
            | a when a <= 20 -> normalInt (float (min gap 4)) 1.5 0 (min gap 5)
            | a when a <= 24 -> normalInt (float (min gap 2)) 1.5 -1 (min gap 3)
            | a when a <= 27 -> normalInt 0.5 1.0 -1 2
            | a when a <= 30 -> normalInt -0.5 1.0 -2 1
            | a when a <= 33 -> normalInt -1.5 1.0 -3 0
            | _ -> normalInt -2.5 1.0 -4 -1
        | Some sched ->
            let focusMult = TrainingEngine.focusMultiplier sched.Focus position

            let intensityMult =
                TrainingEngine.intensityEffect sched.Intensity |> fun e -> e.DeltaMultiplier

            let combinedMult = focusMult * intensityMult

            match age with
            | a when a <= 20 ->
                normalInt
                    (float (min gap 4) * combinedMult)
                    (1.5 * combinedMult)
                    0
                    (int (float (min gap 5) * combinedMult))
            | a when a <= 24 ->
                normalInt
                    (float (min gap 2) * combinedMult)
                    (1.5 * combinedMult)
                    -1
                    (int (float (min gap 3) * combinedMult))
            | a when a <= 27 -> normalInt (0.5 * combinedMult) (1.0 * combinedMult) -1 (int (float 2 * combinedMult))
            | a when a <= 30 -> normalInt (-0.5 * combinedMult) (1.0 * combinedMult) -2 (int (float 1 * combinedMult))
            | a when a <= 33 -> normalInt (-1.5 * combinedMult) (1.0 * combinedMult) -3 0
            | _ -> normalInt (-2.5 * combinedMult) (1.0 * combinedMult) -4 -1

    let private maybeStat (delta: int) (stat: int) : int =
        let threshold =
            if delta > 0 then
                0.35 * float delta
            else
                0.50 * float (abs delta)

        let roll = rollProbability ()

        if roll < threshold then
            clamp 1 20 (stat + (if delta > 0 then 1 else -1))
        else
            stat

    let private developStats (delta: int) (pos: Position) (currentDate: DateTime) (p: Player) =
        let phys =
            if Player.age currentDate p > 28 then
                maybeStat (min delta 0)
            else
                maybeStat delta

        let tech = maybeStat delta
        let mental = maybeStat delta
        let gkOnly = if pos = GK then maybeStat delta else id

        let physical =
            match pos with
            | GK
            | DC
            | DM ->
                { p.Physical with
                    Strength = phys p.Physical.Strength
                    Stamina = phys p.Physical.Stamina }
            | ST
            | AML
            | AMR ->
                { p.Physical with
                    Pace = phys p.Physical.Pace
                    Agility = phys p.Physical.Agility }
            | _ ->
                { p.Physical with
                    Stamina = phys p.Physical.Stamina }

        let technical =
            match pos with
            | GK -> p.Technical
            | ST
            | AML
            | AMR ->
                { p.Technical with
                    Finishing = tech p.Technical.Finishing
                    Dribbling = tech p.Technical.Dribbling }
            | DC
            | DM ->
                { p.Technical with
                    Tackling = tech p.Technical.Tackling
                    Marking = tech p.Technical.Marking }
            | MC
            | AMC ->
                { p.Technical with
                    Passing = tech p.Technical.Passing
                    BallControl = tech p.Technical.BallControl }
            | _ ->
                { p.Technical with
                    Passing = tech p.Technical.Passing }

        { p with
            Physical = physical
            Technical = technical
            Mental =
                { p.Mental with
                    Vision = mental p.Mental.Vision
                    Positioning = mental p.Mental.Positioning }
            Goalkeeping =
                { p.Goalkeeping with
                    Reflexes = gkOnly p.Goalkeeping.Reflexes
                    OneOnOne = gkOnly p.Goalkeeping.OneOnOne
                    Handling = gkOnly p.Goalkeeping.Handling } }

    let developPlayer (currentDate: DateTime) (schedule: TrainingSchedule option) (p: Player) : Player =
        let a = Player.age currentDate p
        let delta = skillDelta a p.CurrentSkill p.PotentialSkill schedule p.Position
        let newCA = clamp 1 p.PotentialSkill (p.CurrentSkill + delta)

        let updatedAffiliation =
            match p.Affiliation with
            | Contracted(clubId, c) when newCA > p.CurrentSkill ->
                Contracted(
                    clubId,
                    { c with
                        Salary = Player.playerSalary newCA }
                )
            | other -> other

        { p with
            CurrentSkill = newCA
            Affiliation = updatedAffiliation }
        |> developStats delta p.Position currentDate

    let developAll (gs: GameState) : GameState =
        { gs with
            Players =
                gs.Players
                |> Map.map (fun _ p ->
                    let schedule = Some p.TrainingSchedule
                    developPlayer gs.CurrentDate schedule p) }

module DevelopmentPhase =

    let make: WorldPhase =
        { Frequency = Monthly
          Run = fun _clock state -> PlayerDevelopment.developAll state }
