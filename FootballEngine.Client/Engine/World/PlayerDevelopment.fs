namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats

module PlayerDevelopment =



    let private skillDelta (age: int) (skill: int) (potential: int) : int =
        let gap = potential - skill

        match age with
        | a when a <= 20 -> normalInt (float (min gap 4)) 1.5 0 (min gap 5)
        | a when a <= 24 -> normalInt (float (min gap 2)) 1.5 -1 (min gap 3)
        | a when a <= 27 -> normalInt 0.5 1.0 -1 2
        | a when a <= 30 -> normalInt -0.5 1.0 -2 1
        | a when a <= 33 -> normalInt -1.5 1.0 -3 0
        | _ -> normalInt -2.5 1.0 -4 -1

    let private maybeStat (rng: Random) (delta: int) (stat: int) : int =
        let threshold =
            if delta > 0 then
                0.35 * float delta
            else
                0.50 * float (abs delta)

        if rng.NextDouble() < threshold then
            clamp 1 20 (stat + (if delta > 0 then 1 else -1))
        else
            stat

    let private developStats (rng: Random) (delta: int) (pos: Position) (currentDate: DateTime) (p: Player) =
        let phys =
            if Player.age currentDate p > 28 then
                maybeStat rng (min delta 0)
            else
                maybeStat rng delta

        let tech = maybeStat rng delta
        let mental = maybeStat rng delta
        let gkOnly = if pos = GK then maybeStat rng delta else id

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

    let developPlayer (rng: Random) (currentDate: DateTime) (p: Player) : Player =
        let a = Player.age currentDate p
        let delta = skillDelta a p.CurrentSkill p.PotentialSkill
        let newCA = clamp 1 200 (p.CurrentSkill + delta)

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
        |> developStats rng delta p.Position currentDate

    let developAll (rng: Random) (gs: GameState) : GameState =
        { gs with
            Players = gs.Players |> Map.map (fun _ -> developPlayer rng gs.CurrentDate) }
