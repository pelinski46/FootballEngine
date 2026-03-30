namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FootballEngine.MatchCalc
open FootballEngine.Stats
open MatchState

open PitchMath

module PlayerMovement =

    let moveSpeed (p: Player) (condition: int) =
        let pace = float p.Physical.Pace
        let accel = float p.Physical.Acceleration
        let cond = float condition / 100.0
        Math.Clamp((pace * 0.6 + accel * 0.4) / 100.0 * cond * 0.45, 0.05, 0.45)

    let decideTarget (p: Player) (baseX: float) (baseY: float) (ballX: float) (ballY: float) (isPossessing: bool) =
        let positioning = float p.Mental.Positioning / 100.0
        let vision = float p.Mental.Vision / 100.0
        let workRate = float p.Mental.WorkRate / 100.0
        let agility = float p.Physical.Agility / 100.0

        let offPush, defPull, lateral =
            match p.Position with
            | GK -> 0.04, 0.02, 0.03
            | DC -> 0.10 + positioning * 0.08, 0.06 + workRate * 0.05, 0.15 + vision * 0.08
            | DL
            | DR -> 0.18 + positioning * 0.10, 0.08 + workRate * 0.06, 0.30 + vision * 0.12
            | WBL
            | WBR -> 0.25 + positioning * 0.12, 0.10 + workRate * 0.08, 0.38 + vision * 0.10
            | DM -> 0.20 + positioning * 0.10, 0.12 + workRate * 0.08, 0.25 + vision * 0.10
            | MC -> 0.28 + positioning * 0.12, 0.18 + workRate * 0.10, 0.28 + vision * 0.12
            | ML
            | MR -> 0.28 + positioning * 0.10, 0.15 + workRate * 0.08, 0.38 + vision * 0.12
            | AML
            | AMR
            | AMC -> 0.38 + positioning * 0.12, 0.22 + workRate * 0.08, 0.32 + vision * 0.10
            | ST -> 0.42 + positioning * 0.10, 0.28 + workRate * 0.06, 0.18 + vision * 0.08

        let push = if isPossessing then offPush else defPull
        let tx = Math.Clamp(baseX + (ballX - baseX) * push, 2.0, 98.0)
        let ty = Math.Clamp(baseY + (ballY - baseY) * lateral, 2.0, 98.0)

        let jitterScale = 0.3 + agility * 0.5

        Math.Clamp(tx + normalSample 0.0 jitterScale, 2.0, 98.0),
        Math.Clamp(ty + normalSample 0.0 jitterScale, 2.0, 98.0)

    let separationForce (myIdx: int) (myPos: float * float) (teamPositions: (float * float)[]) (agility: float) =
        let minDist = 4.0
        let mx, my = myPos

        teamPositions
        |> Array.mapi (fun i pos -> i, pos)
        |> Array.filter (fun (i, _) -> i <> myIdx)
        |> Array.fold
            (fun (fx, fy) (_, (ox, oy)) ->
                let dx, dy = mx - ox, my - oy
                let dist = sqrt (dx * dx + dy * dy)

                if dist < minDist && dist > 0.001 then
                    let strength = (minDist - dist) / minDist * (0.1 + agility * 0.15)
                    fx + dx / dist * strength, fy + dy / dist * strength
                else
                    fx, fy)
            (0.0, 0.0)


module MatchPlayer =

    let private clubIdOf (p: Player) (s: MatchState) =
        if s.HomeSide.Players |> Array.exists (fun x -> x.Id = p.Id) then
            s.Home.Id
        else
            s.Away.Id

    let private attackingClubId (s: MatchState) =
        if s.Possession = Home then s.Home.Id else s.Away.Id

    let resolveDuel (homeId: ClubId) (att: Player, def: Player, ai: int, di: int) (s: MatchState) : MatchState =
        let attIsHome = s.Possession = Home
        let attSide = side attIsHome s
        let defSide = side (not attIsHome) s
        let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions

        let homeBonus = if s.Home.Id = homeId then 15.0 else -10.0
        let skillBonus = float (att.CurrentSkill - def.CurrentSkill) * 0.12
        let moraleBonus = float (att.Morale - def.Morale) * 0.05
        let condBonus = float (att.Condition - def.Condition) * 0.03

        let attClubId =
            if attSide.Players |> Array.exists (fun p -> p.Id = att.Id) then
                s.Home.Id
            else
                s.Away.Id

        let defClubId =
            if attSide.Players |> Array.exists (fun p -> p.Id = def.Id) then
                s.Home.Id
            else
                s.Away.Id

        let repBonus =
            let attRep = (if attClubId = s.Home.Id then s.Home else s.Away).Reputation
            let defRep = (if defClubId = s.Home.Id then s.Home else s.Away).Reputation
            float (attRep - defRep) * 0.002

        let momentum = if s.Possession = Home then s.Momentum else -s.Momentum
        let pressure = (pressureMultiplier attIsHome s - 1.0) * 5.0
        let u = MatchManager.urgency attIsHome s * tacticsCfg.UrgencyMultiplier

        let diff =
            attackEffort (phaseFromBallZone (fst s.BallPosition)) att attSide.Conditions[ai]
            * u
            + homeBonus
            + skillBonus
            + moraleBonus
            + condBonus
            + repBonus
            + momentum
            + pressure
            - defenseEffort def defSide.Conditions[di]

        let bX, bY = s.BallPosition
        let aX, aY = attSide.Positions[ai]
        let dX, dY = defSide.Positions[di]

        if logisticBernoulli diff 0.50 then
            { s with
                BallPosition = jitter bX bY aX aY 0.5 10.0 10.0
                Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
        elif logisticBernoulli (-diff) 0.35 then
            { s with
                Possession = flipPossession s.Possession
                BallPosition = jitter bX bY dX dY 0.5 2.0 2.0
                Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }
        else
            { s with
                BallPosition = jitter bX bY bX bY 0.0 3.0 3.0 }

    let tryShot (attacker: Player) (s: MatchState) (q: PriorityQueue<ScheduledEvent, int>) (second: int) : MatchState =
        let bX, bY = s.BallPosition

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s
        else

            let attIsHome = s.Possession = Home
            let attSide = side attIsHome s
            let defSide = side (not attIsHome) s
            let tacticsCfg = tacticsConfig attSide.Tactics attSide.Instructions
            let defTactics = tacticsConfig defSide.Tactics defSide.Instructions
            let composure = pressureMultiplier attIsHome s * tacticsCfg.UrgencyMultiplier
            let u = MatchManager.urgency attIsHome s * tacticsCfg.UrgencyMultiplier
            let dist = (if attIsHome then 100.0 - bX else bX) * 0.15

            let finishingBonus (pos: Position) =
                match pos with
                | ST
                | AMC
                | AML
                | AMR -> 2.0
                | MC -> 1.2
                | _ -> 0.5

            let shotPower =
                match attSide.Players |> Array.tryFindIndex (fun p -> p.Id = attacker.Id) with
                | Some i ->
                    let shooter = attSide.Players[i]
                    let cond = attSide.Conditions[i]

                    effectiveStat shooter.Technical.Finishing cond shooter.Morale (finishingBonus shooter.Position)
                    + effectiveStat shooter.Mental.Composure cond shooter.Morale (1.5 * composure * u)
                    + effectiveStat shooter.Technical.LongShots cond shooter.Morale 1.0
                    + effectiveStat shooter.Physical.Pace cond shooter.Morale 0.5
                    - dist
                | None ->
                    effectiveStat attacker.Technical.Finishing attacker.Condition attacker.Morale 1.0
                    + effectiveStat attacker.Mental.Composure attacker.Condition attacker.Morale (1.0 * composure * u)
                    - dist

            let nearDefenders =
                defSide.Players
                |> Array.mapi (fun i (p: Player) -> p, defSide.Positions[i], defSide.Conditions[i])
                |> Array.filter (fun (p, _, _) -> playerRole p = Defender)
                |> Array.sortBy (fun (_, pos, _) -> distance (bX, bY) pos)
                |> Array.truncate 2
                |> Array.sumBy (fun (p, _, cond) -> effectiveStat p.Technical.Marking cond p.Morale 0.8)

            let savePower =
                match defSide.Players |> Array.tryFindIndex (fun p -> p.Position = GK) with
                | Some i ->
                    let gk = defSide.Players[i]

                    effectiveStat gk.Goalkeeping.Reflexes defSide.Conditions[i] gk.Morale 2.5
                    + effectiveStat gk.Goalkeeping.OneOnOne defSide.Conditions[i] gk.Morale 3.5
                    + effectiveStat gk.Goalkeeping.Handling defSide.Conditions[i] gk.Morale 2.0
                    + nearDefenders
                    + defTactics.DefensiveDrop * 0.15
                | None -> nearDefenders + normalSample 8.0 3.0

            if shotPower > savePower + 150.0 + normalSample 0.0 30.0 then
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
                        normalSample 75.0 5.0
                    else
                        normalSample 25.0 5.0

                let rebY = Math.Clamp(normalSample bY 10.0, 0.0, 100.0)

                if rebX >= 82.0 || rebX <= 18.0 then
                    q.Enqueue(CornerTaken, second + 5)

                { s with
                    BallPosition = rebX, rebY
                    Possession = flipPossession s.Possession }

    let processPenaltyKick (kicker: Player) (isHome: bool) (kickNum: int) (s: MatchState) =
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let gk =
            if isHome then
                s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK)
            else
                s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)

        let gkSkill =
            gk |> Option.map (fun g -> float g.CurrentSkill) |> Option.defaultValue 50.0

        let score =
            (float kicker.CurrentSkill - gkSkill) * 0.04 + float kicker.Morale * 0.01

        let scored = logisticBernoulli score 0.8

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

        s'
        |> addEvent
            { Second = 95 * 60 + kickNum
              PlayerId = kicker.Id
              ClubId = clubId
              Type = PenaltyAwarded scored }

    let processFreeKick (kicker: Player) (s: MatchState) (homeId: ClubId) =
        let clubId = clubIdOf kicker s
        let _, bY = s.BallPosition
        let isHomeKicker = clubId = homeId

        let finishingBonus =
            match kicker.Position with
            | ST
            | AMC
            | AML
            | AMR -> 2.0
            | MC -> 1.2
            | _ -> 0.5

        let shotPower =
            effectiveStat kicker.Technical.Finishing kicker.Condition kicker.Morale finishingBonus
            + effectiveStat kicker.Mental.Composure kicker.Condition kicker.Morale 1.5
            + effectiveStat kicker.Technical.LongShots kicker.Condition kicker.Morale 1.0

        let gk =
            if isHomeKicker then
                s.AwaySide.Players |> Array.tryFind (fun p -> p.Position = GK)
            else
                s.HomeSide.Players |> Array.tryFind (fun p -> p.Position = GK)

        let savePower =
            match gk with
            | Some g ->
                effectiveStat g.Goalkeeping.Reflexes g.Condition g.Morale 2.5
                + effectiveStat g.Goalkeeping.OneOnOne g.Condition g.Morale 3.5
                + effectiveStat g.Goalkeeping.Handling g.Condition g.Morale 2.0
            | None -> normalSample 50.0 10.0

        let scored = shotPower > savePower + 120.0 + normalSample 0.0 25.0

        let s' =
            if scored then
                { s with
                    HomeScore = if isHomeKicker then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if isHomeKicker then s.AwayScore else s.AwayScore + 1
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession }
                |> addEvent
                    { Second = s.Second
                      PlayerId = kicker.Id
                      ClubId = clubId
                      Type = Goal }
            else
                { s with
                    BallPosition = (if isHomeKicker then 75.0 else 25.0), bY
                    Possession = if isHomeKicker then Away else Home }

        s'
        |> addEvent
            { Second = s.Second
              PlayerId = kicker.Id
              ClubId = clubId
              Type = FreeKick scored }

    let processCorner (s: MatchState) (second: int) =
        let isHomeCorner = s.Possession = Home
        let clubId = if isHomeCorner then s.Home.Id else s.Away.Id

        let squad =
            if isHomeCorner then
                s.HomeSide.Players
            else
                s.AwaySide.Players

        if squad.Length = 0 then
            s
        else
            let bestHeader =
                squad |> Array.maxBy (fun p -> p.Physical.Strength + p.Technical.Heading)

            let defSide = if isHomeCorner then s.AwaySide else s.HomeSide

            let bestDefender =
                defSide.Players
                |> Array.filter (fun p -> p.Position <> GK)
                |> Array.sortByDescending (fun p -> p.Physical.Strength + p.Mental.Positioning)
                |> Array.tryHead

            let attackScore =
                float (bestHeader.Physical.Strength + bestHeader.Technical.Heading) / 200.0

            let defScore =
                bestDefender
                |> Option.map (fun d -> float (d.Physical.Strength + d.Mental.Positioning) / 200.0)
                |> Option.defaultValue 0.5

            let scored = logisticBernoulli (attackScore - defScore) 0.08

            let s' =
                if scored then
                    { s with
                        HomeScore = if isHomeCorner then s.HomeScore + 1 else s.HomeScore
                        AwayScore = if isHomeCorner then s.AwayScore else s.AwayScore + 1
                        BallPosition = 50.0, 50.0
                        Possession = flipPossession s.Possession }
                    |> addEvent
                        { Second = second
                          PlayerId = bestHeader.Id
                          ClubId = clubId
                          Type = Goal }
                else
                    { s with
                        BallPosition = (if isHomeCorner then 20.0 else 80.0), 50.0
                        Possession = flipPossession s.Possession }

            s'
            |> addEvent
                { Second = second
                  PlayerId = bestHeader.Id
                  ClubId = clubId
                  Type = Corner }

    let private findNearestTeammate (attacker: Player) (s: MatchState) (isHome: bool) =
        let side = if isHome then s.HomeSide else s.AwaySide

        match side.Players |> Array.tryFindIndex (fun p -> p.Id = attacker.Id) with
        | None -> None
        | Some attackerIdx ->
            let ax, ay = side.Positions[attackerIdx]

            side.Players
            |> Array.mapi (fun i p -> i, p, side.Positions[i])
            |> Array.filter (fun (i, _, _) -> i <> attackerIdx)
            |> Array.minBy (fun (_, _, (px, py)) ->
                let dx, dy = px - ax, py - ay
                sqrt (dx * dx + dy * dy))
            |> fun (i, p, pos) -> Some(p, i, pos)

    let private findNearestOpponent (attacker: Player) (s: MatchState) (isHome: bool) =
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let defSide = if isHome then s.AwaySide else s.HomeSide

        if defSide.Players.Length = 0 then
            (attacker, 0, attSide.Positions[0])
        else
            match attSide.Players |> Array.tryFindIndex (fun p -> p.Id = attacker.Id) with
            | None -> (defSide.Players[0], 0, defSide.Positions[0])
            | Some attackerIdx ->
                let ax, ay = attSide.Positions[attackerIdx]

                defSide.Players
                |> Array.mapi (fun i p -> i, p, defSide.Positions[i])
                |> Array.filter (fun (_, p, _) -> p.Position <> GK)
                |> Array.minBy (fun (_, _, (px, py)) ->
                    let dx, dy = px - ax, py - ay
                    sqrt (dx * dx + dy * dy))
                |> fun (i, p, pos) -> (p, i, pos)

    let processPassSequence (attacker: Player) (s: MatchState) (q: PriorityQueue<ScheduledEvent, int>) (second: int) =
        let isHome = s.Possession = Home
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let attackerIdx =
            attSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = attacker.Id)
            |> Option.defaultValue 0

        let condition = float attSide.Conditions[attackerIdx] / 100.0

        let passMean =
            0.65
            + float attacker.Technical.Passing / 100.0 * 0.2
            + float attacker.Mental.Vision / 100.0 * 0.1

        let successChance = betaSample passMean (8.0 + condition * 12.0)

        match findNearestTeammate attacker s isHome with
        | Some(teammate, teammateIdx, _) ->
            if bernoulli successChance then
                let s' =
                    { s with
                        BallPosition = attSide.Positions[teammateIdx]
                        Momentum = Math.Clamp(s.Momentum + (if isHome then 0.3 else -0.3), -10.0, 10.0) }
                    |> addEvent
                        { Second = second
                          PlayerId = attacker.Id
                          ClubId = clubId
                          Type = PassCompleted(attacker.Id, teammate.Id) }

                let r = rollProbability ()
                if r < 0.60 then
                    q.Enqueue(ShotAttempt teammate, second + normalInt 4.0 2.0 2 8)
                elif r < 0.70 then
                    q.Enqueue(DribbleAttempt teammate, second + normalInt 8.0 3.0 4 15)
                else
                    q.Enqueue(PassSequence teammate, second + normalInt 6.0 2.0 3 12)
                s'
            else
                let s' =
                    { s with
                        Possession = if isHome then Away else Home
                        Momentum = Math.Clamp(s.Momentum - (if isHome then 0.5 else -0.5), -10.0, 10.0) }
                    |> addEvent
                        { Second = second
                          PlayerId = attacker.Id
                          ClubId = clubId
                          Type = PassIncomplete attacker.Id }

                q.Enqueue(TackleAttempt attacker, second + 2)
                s'

        | None ->
            q.Enqueue(DribbleAttempt attacker, second + 3)
            s

    let processDribble (attacker: Player) (s: MatchState) (q: PriorityQueue<ScheduledEvent, int>) (second: int) =
        let isHome = s.Possession = Home
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let attackerIdx =
            attSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = attacker.Id)
            |> Option.defaultValue 0

        let ax, ay = attSide.Positions[attackerIdx]
        let condition = float attSide.Conditions[attackerIdx] / 100.0

        let attScore =
            float attacker.Technical.Dribbling * 0.5
            + float attacker.Physical.Agility * 0.3
            + float attacker.Physical.Balance * 0.2

        let defender, _, _ = findNearestOpponent attacker s isHome

        let defScore =
            float defender.Technical.Tackling * 0.6 + float defender.Physical.Strength * 0.4

        let duelScore = (attScore - defScore) / 100.0 * condition

        if logisticBernoulli duelScore 1.5 then
            let s' =
                { s with
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.4 else -0.4), -10.0, 10.0) }
                |> addEvent
                    { Second = second
                      PlayerId = attacker.Id
                      ClubId = clubId
                      Type = DribbleSuccess }

            let pushX = if isHome then 5.0 else -5.0
            let newBX = Math.Clamp(ax + pushX, 5.0, 95.0)

            let nextAction =
                pickWeighted
                    [ 0.10, fun () -> q.Enqueue(CrossAttemptEvent attacker, second + normalInt 5.0 2.0 2 10)
                      0.60, fun () -> q.Enqueue(PassSequence attacker, second + normalInt 6.0 2.0 3 12)
                      0.30, fun () -> q.Enqueue(ShotAttempt attacker, second + normalInt 4.0 2.0 2 8) ]

            nextAction ()
            { s' with BallPosition = newBX, ay }
        else
            let s' =
                { s with
                    Possession = if isHome then Away else Home
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.6 else -0.6), -10.0, 10.0) }
                |> addEvent
                    { Second = second
                      PlayerId = attacker.Id
                      ClubId = clubId
                      Type = DribbleFail }

            q.Enqueue(TackleAttempt defender, second + 2)
            s'

    let processCross
        (attacker: Player)
        (s: MatchState)
        (q: PriorityQueue<ScheduledEvent, int>)
        (second: int)
        (homeId: ClubId)
        =
        let isHome = s.Possession = Home
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let defSide = if isHome then s.AwaySide else s.HomeSide
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let attackerIdx =
            attSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = attacker.Id)
            |> Option.defaultValue 0

        let condition = float attSide.Conditions[attackerIdx] / 100.0

        let crossMean =
            0.45
            + float attacker.Technical.Crossing / 100.0 * 0.25
            + float attacker.Mental.Vision / 100.0 * 0.15

        let successChance = betaSample crossMean (6.0 + condition * 10.0)

        let targets =
            attSide.Players
            |> Array.filter (fun p -> p.Position = ST || p.Position = AMC || p.Position = AML || p.Position = AMR)

        if bernoulli successChance && targets.Length > 0 then
            let target =
                targets.[poissonSample (float targets.Length * 0.5)
                         |> fun n -> Math.Clamp(n, 0, targets.Length - 1)]

            let attackPower =
                normalFloat
                    (float target.Technical.Heading * 0.5 + float target.Technical.Finishing * 0.5)
                    10.0
                    0.0
                    100.0

            let bestDef =
                defSide.Players
                |> Array.filter (fun p -> p.Position <> GK)
                |> Array.sortByDescending (fun p -> p.Physical.Strength + p.Mental.Positioning)
                |> Array.tryHead

            let defensePower =
                bestDef
                |> Option.map (fun d ->
                    normalFloat (float d.Physical.Strength * 0.5 + float d.Mental.Positioning * 0.5) 10.0 0.0 100.0)
                |> Option.defaultValue 50.0

            if attackPower > defensePower then
                let conversionChance = 0.08

                if bernoulli conversionChance then
                    let s' =
                        { s with
                            HomeScore = if isHome then s.HomeScore + 1 else s.HomeScore
                            AwayScore = if isHome then s.AwayScore else s.AwayScore + 1
                            BallPosition = 50.0, 50.0
                            Possession = if isHome then Away else Home
                            Momentum = Math.Clamp(s.Momentum + (if isHome then 2.0 else -2.0), -10.0, 10.0) }
                        |> addEvent
                            { Second = second
                              PlayerId = target.Id
                              ClubId = clubId
                              Type = Goal }
                        |> addEvent
                            { Second = second
                              PlayerId = attacker.Id
                              ClubId = clubId
                              Type = Assist }

                    s'
                else
                    let s' =
                        { s with
                            Possession = if isHome then Away else Home
                            Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) }
                        |> addEvent
                            { Second = second
                              PlayerId = attacker.Id
                              ClubId = clubId
                              Type = CrossAttempt true }
                        |> addEvent
                            { Second = second
                              PlayerId = target.Id
                              ClubId = clubId
                              Type = ShotBlocked }

                    q.Enqueue(TackleAttempt target, second + 3)
                    s'
            else
                let s' =
                    { s with
                        Possession = if isHome then Away else Home
                        Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) }
                    |> addEvent
                        { Second = second
                          PlayerId = attacker.Id
                          ClubId = clubId
                          Type = CrossAttempt true }
                    |> addEvent
                        { Second = second
                          PlayerId = target.Id
                          ClubId = clubId
                          Type = ShotBlocked }

                q.Enqueue(TackleAttempt target, second + 3)
                s'
        else
            let s' =
                { s with
                    Possession = if isHome then Away else Home
                    BallPosition = (if isHome then 85.0 else 15.0), 50.0 }
                |> addEvent
                    { Second = second
                      PlayerId = attacker.Id
                      ClubId = clubId
                      Type = CrossAttempt false }

            if bernoulli 0.4 then
                q.Enqueue(CornerTaken, second + 3)

            s'

    let processLongBall (attacker: Player) (s: MatchState) (q: PriorityQueue<ScheduledEvent, int>) (second: int) =
        let isHome = s.Possession = Home
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let attackerIdx =
            attSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = attacker.Id)
            |> Option.defaultValue 0

        let condition = float attSide.Conditions[attackerIdx] / 100.0

        let longMean =
            0.40
            + float attacker.Technical.LongShots / 100.0 * 0.2
            + float attacker.Technical.Passing / 100.0 * 0.2
            + float attacker.Mental.Vision / 100.0 * 0.15

        let successChance = betaSample longMean (5.0 + condition * 10.0)

        let forwards =
            attSide.Players
            |> Array.filter (fun p -> p.Position = ST || p.Position = AML || p.Position = AMR || p.Position = AMC)

        if bernoulli successChance && forwards.Length > 0 then
            let targetIdx =
                int (uniformSample 0.0 (float forwards.Length))
                |> fun n -> Math.Clamp(n, 0, forwards.Length - 1)

            let target = forwards.[targetIdx]

            let forwardIdx =
                attSide.Players
                |> Array.tryFindIndex (fun p -> p.Id = target.Id)
                |> Option.defaultValue attackerIdx

            let s' =
                { s with
                    BallPosition = attSide.Positions[forwardIdx]
                    Momentum = Math.Clamp(s.Momentum + (if isHome then 0.5 else -0.5), -10.0, 10.0) }
                |> addEvent
                    { Second = second
                      PlayerId = attacker.Id
                      ClubId = clubId
                      Type = LongBall true }

            q.Enqueue(DribbleAttempt target, second + normalInt 6.0 2.0 3 10)
            s'
        else
            let s' =
                { s with
                    Possession = if isHome then Away else Home
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.4 else -0.4), -10.0, 10.0) }
                |> addEvent
                    { Second = second
                      PlayerId = attacker.Id
                      ClubId = clubId
                      Type = LongBall false }

            q.Enqueue(TackleAttempt attacker, second + 2)
            s'

    let processTackle
        (defender: Player)
        (s: MatchState)
        (q: PriorityQueue<ScheduledEvent, int>)
        (second: int)
        (homeId: ClubId)
        =
        let isHome = s.Possession = Home
        let defSide = if isHome then s.AwaySide else s.HomeSide
        let attSide = if isHome then s.HomeSide else s.AwaySide
        let clubId = if isHome then s.Away.Id else s.Home.Id

        let defenderIdx =
            defSide.Players
            |> Array.tryFindIndex (fun p -> p.Id = defender.Id)
            |> Option.defaultValue 0

        let condition = float defSide.Conditions[defenderIdx] / 100.0

        let defScore =
            float defender.Technical.Tackling * 0.5
            + float defender.Mental.Positioning * 0.3
            + float defender.Physical.Strength * 0.2

        let _, attacker, _ =
            attSide.Players
            |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
            |> Array.minBy (fun (_, _, (px, py)) ->
                let bx, by = s.BallPosition
                let dx, dy = px - bx, py - by
                sqrt (dx * dx + dy * dy))

        let attScore =
            float attacker.Technical.Dribbling * 0.5
            + float attacker.Physical.Agility * 0.3
            + float attacker.Physical.Balance * 0.2

        let duelScore = (defScore - attScore) / 100.0 * condition

        if logisticBernoulli duelScore 1.5 then
            let aggression = float defender.Mental.Aggression / 100.0
            let positioning = float defender.Mental.Positioning / 100.0

            let foulChance =
                betaSample (0.08 + aggression * 0.15 - positioning * 0.1 |> Math.Abs) 10.0

            if bernoulli foulChance then
                let s' =
                    { s with
                        Possession = if isHome then Away else Home
                        Momentum = Math.Clamp(s.Momentum - (if isHome then 0.3 else -0.3), -10.0, 10.0) }
                    |> addEvent
                        { Second = second
                          PlayerId = defender.Id
                          ClubId = clubId
                          Type = FoulCommitted }

                q.Enqueue(FreeKickAttempt attacker, second + 2)

                if bernoulli 0.15 then
                    q.Enqueue(CardCheck(defender, clubId, false), second)

                s'
            else
                let s' =
                    { s with
                        Momentum = Math.Clamp(s.Momentum + (if isHome then 0.8 else -0.8), -10.0, 10.0) }
                    |> addEvent
                        { Second = second
                          PlayerId = defender.Id
                          ClubId = clubId
                          Type = TackleSuccess }

                q.Enqueue(PassSequence defender, second + normalInt 5.0 2.0 3 10)
                s'
        else
            let s' =
                { s with
                    Momentum = Math.Clamp(s.Momentum - (if isHome then 0.5 else -0.5), -10.0, 10.0) }
                |> addEvent
                    { Second = second
                      PlayerId = defender.Id
                      ClubId = clubId
                      Type = TackleFail }

            q.Enqueue(DribbleAttempt attacker, second + 2)
            s'
