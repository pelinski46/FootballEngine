namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FSharp.Stats.Distributions
open MatchContext
open MatchStats

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int

    exception SimulationException of SimulationError

    let private raise' e = raise (SimulationException e)

    // ── Priority queue ───────────────────────────────────────────────────────

    type EventQueue = PriorityQueue<ScheduledEvent, int>

    let private enqueue second ev (q: EventQueue) = q.Enqueue(ev, second)

    let private enqueueAll items (q: EventQueue) =
        for second, ev in items do
            enqueue second ev q

    // ── Possession view ──────────────────────────────────────────────────────

    type PossessionView =
        { Attackers: Player[]
          AttPositions: (float * float)[]
          AttConditions: int[]
          AttIsHome: bool
          Defenders: Player[]
          DefPositions: (float * float)[]
          DefConditions: int[] }

    let private activeSideView (ts: TeamSide) =
        let active = activeIndices ts.Players ts.Sidelined

        active |> Array.map (fun i -> ts.Players[i]),
        active |> Array.map (fun i -> positionOf ts.Positions ts.Players[i]),
        active |> Array.map (fun i -> ts.Conditions[i])

    let private resolveView (s: MatchState) : PossessionView =
        let attIsHome = s.Possession = Home
        let att = side attIsHome s
        let def = side (not attIsHome) s
        let ap, apos, ac = activeSideView att
        let dp, dpos, dc = activeSideView def

        { Attackers = ap
          AttPositions = apos
          AttConditions = ac
          AttIsHome = attIsHome
          Defenders = dp
          DefPositions = dpos
          DefConditions = dc }

    // ── Fatigue ──────────────────────────────────────────────────────────────

    let private fatigueDelta (p: Player) (isPressing: bool) =
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15
        int (float (base' + workRate) * (if isPressing then 1.5 else 1.0))

    let private applyFatigue (s: MatchState) : MatchState =
        let ballX = fst s.BallPosition

        let drain (ts: TeamSide) (pressing: bool) =
            { ts with
                Conditions =
                    Array.map2 (fun cond p -> Math.Max(0, cond - fatigueDelta p pressing)) ts.Conditions ts.Players }

        s
        |> withSide true (drain (homeSide s) (ballX > 70.0))
        |> withSide false (drain (awaySide s) (ballX < 30.0))

    // ── Duel ─────────────────────────────────────────────────────────────────

    let private pickDuel (s: MatchState) =
        let v = resolveView s

        if v.Attackers.Length = 0 then
            raise' (IncompleteLineup("attackers", 0))

        if v.Defenders.Length = 0 then
            raise' (IncompleteLineup("defenders", 0))

        let attIdx = nearestIdx v.AttPositions s.BallPosition
        let defIdx = nearestIdx v.DefPositions s.BallPosition
        v.Attackers[attIdx], v.Defenders[defIdx], attIdx, defIdx

    let private movedBall oX oY tX tY scale nX nY =
        let nx =
            Math.Clamp(oX + (tX - oX) * scale + Continuous.Normal.Sample 0.0 nX, 0.0, 100.0)

        let ny =
            Math.Clamp(oY + (tY - oY) * scale + Continuous.Normal.Sample 0.0 nY, 0.0, 100.0)

        nx, ny

    let private resolveDuel
        (homeClubId: int)
        (att: Player, def: Player, attIdx: int, defIdx: int)
        (s: MatchState)
        : MatchState =

        let v = resolveView s

        let homeBonus =
            if att.ClubId = homeClubId || def.ClubId = homeClubId then
                7.0
            else
                -5.0

        let momentumBonus = if s.Possession = Home then s.Momentum else -s.Momentum
        let pressureBonus = (pressureMultiplier v.AttIsHome s - 1.0) * 5.0

        let duelDiff =
            attackEffort (phaseFromBallZone (fst s.BallPosition)) att v.AttConditions[attIdx]
            + homeBonus
            + momentumBonus
            + pressureBonus
            - defenseEffort def v.DefConditions[defIdx]

        let bX, bY = s.BallPosition
        let aX, aY = v.AttPositions[attIdx]
        let dX, dY = v.DefPositions[defIdx]

        if duelDiff > 2.0 then
            { s with
                BallPosition = movedBall bX bY aX aY 0.5 10.0 10.0
                Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
        elif duelDiff > -2.0 then
            { s with
                BallPosition = movedBall bX bY bX bY 0.0 3.0 3.0 }
        else
            { s with
                Possession = flipPossession s.Possession
                BallPosition = movedBall bX bY dX dY 0.5 2.0 2.0
                Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }

    // ── Shot ─────────────────────────────────────────────────────────────────

    let private tryShot (attacker: Player) (s: MatchState) : MatchState =
        let bX, bY = s.BallPosition

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s
        else

            let v = resolveView s
            let distPenalty = (if s.Possession = Home then 100.0 - bX else bX) * 0.15
            let composureScale = pressureMultiplier v.AttIsHome s

            let shotPower =
                match v.Attackers |> Array.tryFindIndex (fun p -> p.Position = ST) with
                | Some stIdx ->
                    let st = v.Attackers[stIdx]
                    let cond = v.AttConditions[stIdx]

                    effectiveStat st.Technical.Finishing cond st.Morale 3.5
                    + effectiveStat st.Mental.Composure cond st.Morale (3.0 * composureScale)
                    + effectiveStat st.Technical.LongShots cond st.Morale 2.0
                    + effectiveStat st.Physical.Pace cond st.Morale 1.5
                    - distPenalty
                | None ->
                    effectiveStat attacker.Technical.Finishing 50 attacker.Morale 3.5
                    + effectiveStat attacker.Mental.Composure 50 attacker.Morale (3.0 * composureScale)
                    - distPenalty

            let defPressure =
                v.Defenders
                |> Array.mapi (fun i p -> p, v.DefPositions[i], v.DefConditions[i])
                |> Array.filter (fun (p, _, _) -> playerRole p = Defender)
                |> Array.map (fun (p, pos, cond) -> p, distance (bX, bY) pos, cond)
                |> Array.sortBy (fun (_, d, _) -> d)
                |> Array.truncate 1
                |> Array.sumBy (fun (p, _, cond) -> effectiveStat p.Technical.Marking cond p.Morale 0.5)

            let savePower =
                match v.Defenders |> Array.tryFindIndex (fun p -> p.Position = GK) with
                | Some gkIdx ->
                    let gk = v.Defenders[gkIdx]
                    let cond = v.DefConditions[gkIdx]

                    effectiveStat gk.Goalkeeping.Reflexes cond gk.Morale 1.5
                    + effectiveStat gk.Goalkeeping.OneOnOne cond gk.Morale 2.0
                    + defPressure
                | None -> defPressure + Continuous.Normal.Sample 5.0 2.0

            if shotPower > savePower + Continuous.Normal.Sample 5.0 10.0 then
                { s with
                    HomeScore = if s.Possession = Home then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if s.Possession = Away then s.AwayScore + 1 else s.AwayScore
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession
                    Momentum =
                        if s.Possession = Home then
                            Math.Clamp(s.Momentum + 3.0, -10.0, 10.0)
                        else
                            Math.Clamp(s.Momentum - 3.0, -10.0, 10.0) }
                |> addEvent
                    { Second = s.Second
                      PlayerId = attacker.Id
                      ClubId = attacker.ClubId
                      Type = Goal }
            else
                let rebX =
                    if s.Possession = Home then
                        Continuous.Normal.Sample 75.0 5.0
                    else
                        Continuous.Normal.Sample 25.0 5.0

                { s with
                    BallPosition = rebX, Math.Clamp(Continuous.Normal.Sample bY 10.0, 0.0, 100.0)
                    Possession = flipPossession s.Possession }

    // ── Cards ─────────────────────────────────────────────────────────────────

    let private cardProbability (p: Player) =
        0.010 + float p.Mental.Aggression * 0.0001

    let private processCard (player: Player) (clubId: ClubId) (second: int) (s: MatchState) =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s
        let count = ts.Yellows |> Map.tryFind player.Id |> Option.defaultValue 0
        let yellows = Map.add player.Id (count + 1) ts.Yellows

        let ts' =
            if count >= 1 then
                { ts with
                    Yellows = yellows
                    Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined }
            else
                { ts with Yellows = yellows }

        let s' = withSide isHome ts' s

        if count >= 1 then
            s'
            |> addEvent
                { Second = second
                  PlayerId = player.Id
                  ClubId = clubId
                  Type = YellowCard }
            |> addEvent
                { Second = second
                  PlayerId = player.Id
                  ClubId = clubId
                  Type = RedCard }
        else
            s'
            |> addEvent
                { Second = second
                  PlayerId = player.Id
                  ClubId = clubId
                  Type = YellowCard }

    // ── Injuries ─────────────────────────────────────────────────────────────

    let private injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let private processInjury (player: Player) (clubId: ClubId) (second: int) (s: MatchState) =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s

        let ts' =
            { ts with
                Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined }

        withSide isHome ts' s
        |> addEvent
            { Second = second
              PlayerId = player.Id
              ClubId = clubId
              Type = Injury "match" }

    // ── Substitutions ────────────────────────────────────────────────────────

    let private maxSubs = 3

    let private processSubstitution (clubId: ClubId) (ctx: MatchContext) (second: int) (s: MatchState) =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s
        let club = if isHome then s.Home else s.Away

        if ts.SubsUsed >= maxSubs then
            s
        else

            let activeOutfield =
                activeIndices ts.Players ts.Sidelined
                |> Array.filter (fun i -> ts.Players[i].Position <> GK)
                |> Array.sortBy (fun i -> ts.Conditions[i])

            if activeOutfield.Length = 0 then
                s
            else

                let outIdx = activeOutfield[0]
                let playerOut = ts.Players[outIdx]
                let minCondition = ts.Conditions[outIdx]
                let threshold = if goalDiff isHome s < 0 then 70 else 60

                if minCondition >= threshold then
                    s
                else

                    let startingIds =
                        club.CurrentLineup
                        |> Option.map (fun lu -> lu.Slots |> List.choose _.PlayerId |> Set.ofList)
                        |> Option.defaultValue Set.empty

                    let onPitchIds = ts.Players |> Array.map _.Id |> Set.ofArray

                    let reserve =
                        club.Players
                        |> List.filter (fun p ->
                            not (Set.contains p.Id startingIds)
                            && not (Set.contains p.Id onPitchIds)
                            && not (Map.containsKey p.Id ts.Sidelined))
                        |> List.sortByDescending _.Condition
                        |> List.tryHead

                    match reserve with
                    | None -> s
                    | Some incoming ->
                        let outPos =
                            ts.Positions |> Map.tryFind playerOut.Id |> Option.defaultValue (50.0, 50.0)

                        let ts' =
                            { ts with
                                Players = Array.append ts.Players [| incoming |]
                                Conditions = Array.append ts.Conditions [| incoming.Condition |]
                                Sidelined = Map.add playerOut.Id SidelinedBySub ts.Sidelined
                                Positions = Map.add incoming.Id outPos ts.Positions
                                BasePositions = Map.add incoming.Id outPos ts.BasePositions
                                SubsUsed = ts.SubsUsed + 1 }

                        withSide isHome ts' s
                        |> addEvent
                            { Second = second
                              PlayerId = playerOut.Id
                              ClubId = clubId
                              Type = SubstitutionOut }
                        |> addEvent
                            { Second = second
                              PlayerId = incoming.Id
                              ClubId = clubId
                              Type = SubstitutionIn }

    // ── Tactical movement ────────────────────────────────────────────────────

    let private tacticalTarget (p: Player) baseX baseY ballX ballY isPossessing =
        let offPush, defPull, lateral =
            match p.Position with
            | GK -> 0.05, 0.02, 0.05
            | DC -> 0.15, 0.08, 0.20
            | DR
            | DL -> 0.20, 0.10, 0.35
            | WBR
            | WBL -> 0.30, 0.12, 0.40
            | DM -> 0.25, 0.15, 0.30
            | MC -> 0.30, 0.20, 0.30
            | MR
            | ML -> 0.30, 0.18, 0.40
            | AMR
            | AMC
            | AML -> 0.40, 0.25, 0.35
            | ST -> 0.45, 0.30, 0.20

        let push = if isPossessing then offPush else defPull
        let targetX = Math.Clamp(baseX + (ballX - baseX) * push, 2.0, 98.0)
        let targetY = Math.Clamp(baseY + (ballY - baseY) * lateral, 2.0, 98.0)
        targetX, targetY

    let private updatePositions (s: MatchState) : MatchState =
        let ballX, ballY = s.BallPosition

        let moveTeam (ts: TeamSide) (isPossessing: bool) =
            { ts with
                Positions =
                    ts.Players
                    |> Array.fold
                        (fun acc p ->
                            let curX, curY = acc |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

                            let baseX, baseY =
                                ts.BasePositions |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

                            let tx, ty = tacticalTarget p baseX baseY ballX ballY isPossessing
                            let tx = tx + (baseX - tx) * 0.25
                            let ty = ty + (baseY - ty) * 0.25
                            Map.add p.Id (curX + (tx - curX) * 0.15, curY + (ty - curY) * 0.15) acc)
                        ts.Positions }

        s
        |> withSide true (moveTeam (homeSide s) (s.Possession = Home))
        |> withSide false (moveTeam (awaySide s) (s.Possession = Away))

    // ── Dispatcher ───────────────────────────────────────────────────────────

    let private dispatch
        (homeClubId: int)
        (ctx: MatchContext)
        (queue: EventQueue)
        (second: int)
        (event: ScheduledEvent)
        (s: MatchState)
        : MatchState =

        let s = { s with Second = second }

        match event with
        | FatigueCheck ->
            enqueue (second + 180) FatigueCheck queue
            applyFatigue s

        | Duel ->
            let possessionBefore = s.Possession
            let att, def, attIdx, defIdx = pickDuel s
            let s' = resolveDuel homeClubId (att, def, attIdx, defIdx) s

            if s'.Possession = possessionBefore then
                enqueue second (ShotAttempt att) queue

            if Continuous.Uniform.Sample 0.0 1.0 < cardProbability def then
                enqueue second (CardCheck(def, def.ClubId, false)) queue

            if Continuous.Uniform.Sample 0.0 1.0 < injuryProbability att then
                enqueue second (InjuryCheck(att, att.ClubId)) queue

            enqueue (second + 30) Duel queue
            s' |> updatePositions

        | ShotAttempt attacker -> tryShot attacker s
        | CardCheck(player, clubId, _) -> processCard player clubId second s
        | InjuryCheck(player, clubId) -> processInjury player clubId second s
        | SubstitutionCheck clubId -> processSubstitution clubId ctx second s
        | MatchEnd -> s

    // ── Context builder ──────────────────────────────────────────────────────

    let private toEngineCoords slotX slotY isHome =
        if isHome then
            (1.0 - slotY) * 100.0, slotX * 100.0
        else
            slotY * 100.0, slotX * 100.0

    let private extractLineup (club: Club) (isHome: bool) =
        match club.CurrentLineup with
        | None -> raise' (MissingLineup club.Name)
        | Some lineup ->
            lineup.Slots
            |> List.choose (fun slot ->
                slot.PlayerId
                |> Option.bind (fun pid ->
                    club.Players
                    |> List.tryFind (fun p -> p.Id = pid)
                    |> Option.map (fun p ->
                        let ex, ey = toEngineCoords slot.X slot.Y isHome
                        p, ex, ey)))
            |> Array.ofList

    let buildContext (home: Club) (away: Club) : MatchContext * Player[] * Player[] =
        let homeData = extractLineup home true
        let awayData = extractLineup away false

        if homeData.Length <> 11 then
            raise' (IncompleteLineup(home.Name, homeData.Length))

        if awayData.Length <> 11 then
            raise' (IncompleteLineup(away.Name, awayData.Length))

        let ctx =
            { HomePositions = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
              AwayPositions = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray }

        ctx, homeData |> Array.map (fun (p, _, _) -> p), awayData |> Array.map (fun (p, _, _) -> p)

    // ── Match loop ───────────────────────────────────────────────────────────

    let private subMinutes = [| 60; 75; 85 |]

    let private buildInitialQueue (homeId: ClubId) (awayId: ClubId) : EventQueue =
        let q = EventQueue()
        enqueueAll [ 30, Duel; 180, FatigueCheck; 95 * 60, MatchEnd ] q

        for min in subMinutes do
            enqueue (min * 60) (SubstitutionCheck homeId) q
            enqueue (min * 60) (SubstitutionCheck awayId) q

        q

    let private buildInitState
        (home: Club)
        (homePlayers: Player[])
        (away: Club)
        (awayPlayers: Player[])
        (ctx: MatchContext)
        : MatchState =
        { Home = home
          Away = away
          Second = 0
          HomeScore = 0
          AwayScore = 0
          BallPosition = 50.0, 50.0
          Possession = Home
          Momentum = 0.0
          HomePlayers = homePlayers
          AwayPlayers = awayPlayers
          HomeConditions = homePlayers |> Array.map _.Condition
          AwayConditions = awayPlayers |> Array.map _.Condition
          HomeSidelined = Map.empty
          AwaySidelined = Map.empty
          HomeYellows = Map.empty
          AwayYellows = Map.empty
          HomeSubsUsed = 0
          AwaySubsUsed = 0
          EventsRev = []
          HomePositions = ctx.HomePositions
          AwayPositions = ctx.AwayPositions
          HomeBasePositions = ctx.HomePositions
          AwayBasePositions = ctx.AwayPositions }

    let private runEventLoop (homeClubId: int) (ctx: MatchContext) (initState: MatchState) =
        let queue = buildInitialQueue initState.Home.Id initState.Away.Id
        let mutable s = initState
        let snapshots = List<MatchState>()

        while queue.Count > 0 do
            let mutable ev = Unchecked.defaultof<_>
            let mutable pr = 0
            queue.TryDequeue(&ev, &pr) |> ignore

            match ev with
            | MatchEnd ->
                s <- { s with Second = pr }
                queue.Clear()
            | _ ->
                s <- dispatch homeClubId ctx queue pr ev s

                match ev with
                | Duel
                | ShotAttempt _ -> snapshots.Add(s)
                | _ -> ()

        s, snapshots.ToArray()

    let private runMatch (home: Club) (away: Club) =
        let ctx, homePlayers, awayPlayers = buildContext home away

        let final, snapshots =
            runEventLoop home.Id ctx (buildInitState home homePlayers away awayPlayers ctx)

        { Final = final; Snapshots = snapshots }

    let simulateMatch (home: Club) (away: Club) : int * int * MatchEvent list =
        let r = runMatch home away
        r.Final.HomeScore, r.Final.AwayScore, r.Final.EventsRev

    let trySimulateMatch (home: Club) (away: Club) : Result<int * int * MatchEvent list, SimulationError> =
        try
            Ok(simulateMatch home away)
        with SimulationException e ->
            Error e

    let trySimulateMatchFull (home: Club) (away: Club) =
        try
            Ok(runMatch home away)
        with SimulationException e ->
            Error e
