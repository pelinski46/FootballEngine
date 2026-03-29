namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open FSharp.Stats.Distributions

module PlayerDevelopment =

    let private skillDelta (age: int) (skill: int) (potential: int) (schedule: TrainingSchedule option) (position: Position) : int =
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
            let intensityMult = TrainingEngine.intensityEffect sched.Intensity |> fun e -> e.DeltaMultiplier
            let combinedMult = focusMult * intensityMult
            
            match age with
            | a when a <= 20 -> normalInt (float (min gap 4) * combinedMult) (1.5 * combinedMult) 0 (int (float (min gap 5) * combinedMult))
            | a when a <= 24 -> normalInt (float (min gap 2) * combinedMult) (1.5 * combinedMult) -1 (int (float (min gap 3) * combinedMult))
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

        let roll = Continuous.Uniform.Sample 0.0 1.0
        
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
