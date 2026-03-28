namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FSharp.Stats.Distributions
open MatchState
open MatchStats
open Pitch

module MatchPlayer =

    let private clubIdOf (p: Player) (s: MatchState) =
        if s.HomeSide.Players |> Array.exists (fun x -> x.Id = p.Id) then
            s.Home.Id
        else
            s.Away.Id

    let private attackingClubId (s: MatchState) =
        if s.Possession = Home then s.Home.Id else s.Away.Id

    let resolveDuel (homeId: ClubId) (att: Player, def: Player, ai: int, di: int) (s: MatchState) : MatchState =
        let v = buildView s

        let homeBonus = if s.Home.Id = homeId then 7.0 else -5.0

        // Bonus por calidad de jugador (CurrentSkill) - más impacto
        let skillBonus = float (att.CurrentSkill - def.CurrentSkill) * 0.12

        // Bonus por moral del jugador
        let moraleBonus = float (att.Morale - def.Morale) * 0.05

        // Bonus por condición física
        let conditionBonus = float (att.Condition - def.Condition) * 0.03

        // Bonus por reputación del club del atacante vs defensor
        let attClubId =
            if s.HomeSide.Players |> Array.exists (fun p -> p.Id = att.Id) then
                s.Home.Id
            else
                s.Away.Id

        let defClubId =
            if s.HomeSide.Players |> Array.exists (fun p -> p.Id = def.Id) then
                s.Home.Id
            else
                s.Away.Id

        let attClubRep = (if attClubId = s.Home.Id then s.Home else s.Away).Reputation
        let defClubRep = (if defClubId = s.Home.Id then s.Home else s.Away).Reputation
        let repBonus = float (attClubRep - defClubRep) * 0.002

        let momentum = if s.Possession = Home then s.Momentum else -s.Momentum
        let pressure = (pressureMultiplier v.AttIsHome s - 1.0) * 5.0

        // Apply tactics urgency multiplier - get tactics from the attacking team side
        let attSide = side v.AttIsHome s
        let attTacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
        let u = MatchManager.urgency v.AttIsHome s * attTacticsCfg.UrgencyMultiplier

        let diff =
            attackEffort (phaseFromBallZone (fst s.BallPosition)) att v.ACond[ai] * u
            + homeBonus
            + skillBonus
            + moraleBonus
            + conditionBonus
            + repBonus
            + momentum
            + pressure
            - defenseEffort def v.DCond[di]

        let bX, bY = s.BallPosition
        let aX, aY = v.APos[ai]
        let dX, dY = v.DPos[di]

        if diff > 2.0 then
            { s with
                BallPosition = jitter bX bY aX aY 0.5 10.0 10.0
                Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
        elif diff > -2.0 then
            { s with
                BallPosition = jitter bX bY bX bY 0.0 3.0 3.0 }
        else
            { s with
                Possession = flipPossession s.Possession
                BallPosition = jitter bX bY dX dY 0.5 2.0 2.0
                Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }

    let tryShot (attacker: Player) (s: MatchState) (q: PriorityQueue<ScheduledEvent, int>) (second: int) : MatchState =
        let bX, bY = s.BallPosition

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s
        else
            let v = buildView s
            let dist = (if s.Possession = Home then 100.0 - bX else bX) * 0.15

            let attSide = side v.AttIsHome s
            let attTacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
            let composure = pressureMultiplier v.AttIsHome s * attTacticsCfg.UrgencyMultiplier
            let u = MatchManager.urgency v.AttIsHome s * attTacticsCfg.UrgencyMultiplier

            let shotPower =
                match v.Att |> Array.tryFindIndex (fun p -> p.Id = attacker.Id) with
                | Some i ->
                    let cond = v.ACond[i]
                    let shooter = v.Att[i]

                    let finishingBonus =
                        match shooter.Position with
                        | ST
                        | AMC
                        | AML
                        | AMR -> 2.0
                        | MC -> 1.2
                        | _ -> 0.5

                    effectiveStat shooter.Technical.Finishing cond shooter.Morale finishingBonus
                    + effectiveStat shooter.Mental.Composure cond shooter.Morale (1.5 * composure * u)
                    + effectiveStat shooter.Technical.LongShots cond shooter.Morale 1.0
                    + effectiveStat shooter.Physical.Pace cond shooter.Morale 0.5
                    - dist
                | None ->
                    effectiveStat attacker.Technical.Finishing attacker.Condition attacker.Morale 1.0
                    + effectiveStat attacker.Mental.Composure attacker.Condition attacker.Morale (1.0 * composure * u)
                    - dist

            let nearDefenders =
                v.Def
                |> Array.mapi (fun i p -> p, v.DPos[i], v.DCond[i])
                |> Array.filter (fun (p, _, _) -> playerRole p = Defender)
                |> Array.sortBy (fun (_, pos, _) -> distance (bX, bY) pos)
                |> Array.truncate 2
                |> Array.sumBy (fun (p, _, cond) -> effectiveStat p.Technical.Marking cond p.Morale 0.8)

            let defSide = side (not v.AttIsHome) s
            let defTacticsCfg = tacticsConfig defSide.Tactics defSide.Instructions

            let savePower =
                match v.Def |> Array.tryFindIndex (fun p -> p.Position = GK) with
                | Some i ->
                    let gk = v.Def[i]

                    effectiveStat gk.Goalkeeping.Reflexes v.DCond[i] gk.Morale 2.5
                    + effectiveStat gk.Goalkeeping.OneOnOne v.DCond[i] gk.Morale 3.5
                    + effectiveStat gk.Goalkeeping.Handling v.DCond[i] gk.Morale 2.0
                    + nearDefenders
                    + defTacticsCfg.DefensiveDrop * 0.15
                | None -> nearDefenders + Continuous.Normal.Sample 8.0 3.0

            // Reducir varianza para más consistencia
            if shotPower > savePower + Continuous.Normal.Sample 2.0 6.0 then
                { s with
                    HomeScore = if s.Possession = Home then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if s.Possession = Away then s.AwayScore + 1 else s.AwayScore
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum + (if s.Possession = Home then 3.0 else -3.0), -10.0, 10.0) }
                |> addEvent
                    { Second = s.Second
                      PlayerId = attacker.Id
                      ClubId = attackingClubId s
                      Type = Goal }
            else
                let rebX =
                    if s.Possession = Home then
                        Continuous.Normal.Sample 75.0 5.0
                    else
                        Continuous.Normal.Sample 25.0 5.0
                
                // Corner solo si el rebote va MUY cerca de la línea de fondo (menos frecuente)
                if rebX >= 82.0 || rebX <= 18.0 then
                    q.Enqueue(CornerTaken, second + 5)
                    { s with
                        BallPosition = rebX, Math.Clamp(Continuous.Normal.Sample bY 10.0, 0.0, 100.0)
                        Possession = flipPossession s.Possession }
                else
                    { s with
                        BallPosition = rebX, Math.Clamp(Continuous.Normal.Sample bY 10.0, 0.0, 100.0)
                        Possession = flipPossession s.Possession }

    let processPenaltyKick (kicker: Player) (isHome: bool) (kickNum: int) (s: MatchState) (homeId: ClubId) (players: Map<PlayerId, Player>) =
        let clubId = if isHome then s.Home.Id else s.Away.Id
        let gk = if isHome then s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK) else s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)
        
        let kickerSkill = float kicker.CurrentSkill
        let gkSkill = gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0
        let kickerMorale = float kicker.Morale
        let pressure = 0.85
        let baseChance = 0.75 + (kickerSkill - gkSkill) * 0.002 + kickerMorale * 0.001
        let scored = Continuous.Uniform.Sample 0.0 1.0 < baseChance * pressure
        
        let s' =
            if scored then
                { s with
                    HomeScore = if isHome then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if isHome then s.AwayScore else s.AwayScore + 1
                    BallPosition = 50.0, 50.0
                    Possession = if isHome then Away else Home }
            else
                { s with
                    BallPosition = 50.0, 50.0
                    Possession = if isHome then Away else Home }
        
        s' |> addEvent { Second = 95 * 60 + kickNum; PlayerId = kicker.Id; ClubId = clubId; Type = PenaltyAwarded scored }

    let processFreeKick (kicker: Player) (s: MatchState) (homeId: ClubId) (players: Map<PlayerId, Player>) =
        let clubId = clubIdOf kicker s
        let bX, bY = s.BallPosition
        
        // Determinar el arco que ataca (basado en el club del pateador, no en la posesión)
        let isHomeKicker = clubId = homeId
        let attackingX = if isHomeKicker then 100.0 else 0.0
        
        let kickerSkill = float kicker.CurrentSkill
        let finishingBonus =
            match kicker.Position with
            | ST | AMC | AML | AMR -> 2.0
            | MC -> 1.2
            | _ -> 0.5
        
        let shotPower =
            effectiveStat kicker.Technical.Finishing kicker.Condition kicker.Morale finishingBonus
            + effectiveStat kicker.Mental.Composure kicker.Condition kicker.Morale 1.5
            + effectiveStat kicker.Technical.LongShots kicker.Condition kicker.Morale 1.0
        
        // El arquero es el del equipo contrario al pateador
        let gk = if isHomeKicker then s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK) else s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)
        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> Continuous.Normal.Sample 50.0 10.0
        
        let scored = shotPower > savePower + Continuous.Normal.Sample 2.0 6.0
        
        let s' =
            if scored then
                { s with
                    HomeScore = if isHomeKicker then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if isHomeKicker then s.AwayScore else s.AwayScore + 1
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession }
            else
                { s with
                    BallPosition = (if isHomeKicker then 75.0 else 25.0), bY
                    Possession = if isHomeKicker then Away else Home }
        
        s' |> addEvent { Second = s.Second; PlayerId = kicker.Id; ClubId = clubId; Type = FreeKick scored }

    let processCorner (s: MatchState) (homeId: ClubId) (q: PriorityQueue<ScheduledEvent, int>) (second: int) =
        let isHomeCorner = s.Possession = Home
        let clubId = if isHomeCorner then s.Home.Id else s.Away.Id
        
        // Buscar el mejor cabeceador del equipo
        let squad = if isHomeCorner then s.HomeSide.Players else s.AwaySide.Players
        
        // Guard: si no hay jugadores, no hacer nada
        if squad.Length = 0 then s
        else
            let bestHeader = squad |> Array.maxBy (fun p -> p.Physical.Strength + p.Technical.Finishing)
            
            // Simular el corner
            let headerChance = float bestHeader.CurrentSkill / 200.0 * 0.6
            let scored = Continuous.Uniform.Sample 0.0 1.0 < headerChance
            
            let s' =
                if scored then
                    { s with
                        HomeScore = if isHomeCorner then s.HomeScore + 1 else s.HomeScore
                        AwayScore = if isHomeCorner then s.AwayScore else s.AwayScore + 1
                        BallPosition = 50.0, 50.0
                        Possession = flipPossession s.Possession }
                else
                    { s with
                        BallPosition = (if isHomeCorner then 20.0 else 80.0), 50.0
                        Possession = flipPossession s.Possession }
            
            s' |> addEvent { Second = second; PlayerId = bestHeader.Id; ClubId = clubId; Type = Corner }
