namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FootballEngine.DomainTypes
open FSharp.Stats.Distributions
open MatchContext
open MatchStats

module MatchSimulator =

    // ------------------------------------------------------------------ //
    //  Domain errors                                                       //
    // ------------------------------------------------------------------ //

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | MissingGoalkeeper of clubName: string
        | MissingPlayer of playerId: int

    exception SimulationException of SimulationError

    let private raise' e = raise (SimulationException e)

    // ------------------------------------------------------------------ //
    //  Priority queue helpers                                              //
    // ------------------------------------------------------------------ //

    type EventQueue = PriorityQueue<ScheduledEvent, int>

    let private enqueue (second: int) (ev: ScheduledEvent) (q: EventQueue) = q.Enqueue(ev, second)

    let private enqueueAll (items: (int * ScheduledEvent) list) (q: EventQueue) =
        for second, ev in items do
            enqueue second ev q

    let private findIdxOrRaise (error: SimulationError) (predicate: Player -> bool) (arr: Player[]) =
        match arr |> Array.tryFindIndex predicate with
        | Some i -> i
        | None -> raise' error


    type PossessionView =
        { Attackers: Player[]
          AttPositions: (float * float)[]
          AttConditions: int[]
          AttIsHome: bool
          Defenders: Player[]
          DefPositions: (float * float)[]
          DefConditions: int[] }

    let private resolveView (ctx: MatchContext) (s: MatchState) : PossessionView =
        let build
            (players: Player[])
            (positions: Map<PlayerId, float * float>)
            (conds: int[])
            (sidelined: Map<PlayerId, PlayerOut>)
            =
            let active = activeIndices players sidelined

            active |> Array.map (fun i -> players[i]),
            active |> Array.map (fun i -> positionOf positions players[i]),
            active |> Array.map (fun i -> conds[i])

        match s.Possession with
        | Home ->
            let ap, apos, ac =
                build s.HomePlayers ctx.HomePositions s.HomeConditions s.HomeSidelined

            let dp, dpos, dc =
                build s.AwayPlayers ctx.AwayPositions s.AwayConditions s.AwaySidelined

            { Attackers = ap
              AttPositions = apos
              AttConditions = ac
              AttIsHome = true
              Defenders = dp
              DefPositions = dpos
              DefConditions = dc }
        | Away ->
            let ap, apos, ac =
                build s.AwayPlayers ctx.AwayPositions s.AwayConditions s.AwaySidelined

            let dp, dpos, dc =
                build s.HomePlayers ctx.HomePositions s.HomeConditions s.HomeSidelined

            { Attackers = ap
              AttPositions = apos
              AttConditions = ac
              AttIsHome = false
              Defenders = dp
              DefPositions = dpos
              DefConditions = dc }

    // ------------------------------------------------------------------ //
    //  Fatigue (also reads from MatchState.HomePlayers)                   //
    // ------------------------------------------------------------------ //

    let private fatigueDelta (p: Player) (isPressing: bool) =
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15
        int (float (base' + workRate) * (if isPressing then 1.5 else 1.0))

    let private applyFatigue (s: MatchState) : MatchState =
        let ballX = fst s.BallPosition

        let applyDelta players conditions pressing =
            Array.map2 (fun cond p -> Math.Max(0, cond - fatigueDelta p pressing)) conditions players

        { s with
            HomeConditions = applyDelta s.HomePlayers s.HomeConditions (ballX > 70.0)
            AwayConditions = applyDelta s.AwayPlayers s.AwayConditions (ballX < 30.0) }

    // ------------------------------------------------------------------ //
    //  Duel resolution                                                     //
    // ------------------------------------------------------------------ //

    let private pickDuel (ctx: MatchContext) (s: MatchState) =
        let v = resolveView ctx s

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
        (ctx: MatchContext)
        (s: MatchState)
        : MatchState =

        let v = resolveView ctx s

        // Home advantage: asymmetric — local team wins duels more often
        // regardless of possession. Att is home → +3.0, Def is home → -2.0
        // (net: home team gains on average +2.5 per duel vs away)
        let homeBonus =
            match s.Possession with
            | Home when homeClubId = att.ClubId -> 7.0 // home attacks: boost
            | Away when homeClubId = def.ClubId -> -4.0 // home defends: boost (reduces duelDiff)
            | _ -> 0.0

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

    // ------------------------------------------------------------------ //
    //  Shot resolution                                                     //
    // ------------------------------------------------------------------ //

    let private tryShot (attacker: Player) (ctx: MatchContext) (s: MatchState) : MatchState =
        let bX, bY = s.BallPosition

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s
        else

            let v = resolveView ctx s



            let shooterIdx =
                findIdxOrRaise (MissingPlayer attacker.Id) (fun p -> p.Id = attacker.Id) v.Attackers


            let sCond = v.AttConditions[shooterIdx]


            let distPenalty = (if s.Possession = Home then 100.0 - bX else bX) * 0.15
            let composureScale = pressureMultiplier v.AttIsHome s

            let shotPower =
                effectiveStat attacker.Technical.Finishing sCond attacker.Morale 3.5
                + effectiveStat attacker.Mental.Composure sCond attacker.Morale (3.0 * composureScale)
                + effectiveStat attacker.Technical.LongShots sCond attacker.Morale 2.0
                + effectiveStat attacker.Physical.Pace sCond attacker.Morale 1.5
                - distPenalty

            let defPressure =
                v.Defenders
                |> Array.mapi (fun i p -> p, v.DefPositions[i], v.DefConditions[i])
                |> Array.filter (fun (p, _, _) -> isDefender p)
                |> Array.map (fun (p, pos, cond) -> p, distance (bX, bY) pos, cond)
                |> Array.sortBy (fun (_, d, _) -> d)
                |> Array.truncate 1
                |> Array.sumBy (fun (p, _, cond) -> effectiveStat p.Technical.Marking cond p.Morale 0.5)

            let savePower =
                match v.Defenders |> Array.tryFindIndex (fun p -> p.Position = GK) with
                | Some gkIdx ->
                    let gk = v.Defenders[gkIdx]
                    let gkCond = v.DefConditions[gkIdx]

                    effectiveStat gk.Goalkeeping.Reflexes gkCond gk.Morale 1.5
                    + effectiveStat gk.Goalkeeping.OneOnOne gkCond gk.Morale 2.0
                    + defPressure
                | None ->

                    defPressure + Continuous.Normal.Sample 5.0 2.0

            if shotPower > savePower + Continuous.Normal.Sample 0.0 2.0 then
                { s with
                    HomeScore = if s.Possession = Home then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if s.Possession = Away then s.AwayScore + 1 else s.AwayScore
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession
                    Momentum =
                        if s.Possession = Home then
                            s.Momentum + 3.0
                        else
                            s.Momentum - 3.0 }
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

    // ------------------------------------------------------------------ //
    //  Cards                                                               //
    // ------------------------------------------------------------------ //

    let private cardProbability (p: Player) =
        0.010 + float p.Mental.Aggression * 0.0001

    let private processCard (player: Player) (clubId: ClubId) (second: int) (s: MatchState) =
        let isHome = clubId = s.Home.Id
        let yellows = if isHome then s.HomeYellows else s.AwayYellows
        let count = yellows |> Map.tryFind player.Id |> Option.defaultValue 0

        if count >= 1 then
            let newYellows = Map.add player.Id (count + 1) yellows

            let newSidelined =
                Map.add player.Id SidelinedByRedCard (if isHome then s.HomeSidelined else s.AwaySidelined)

            { s with
                HomeYellows = if isHome then newYellows else s.HomeYellows
                AwayYellows = if isHome then s.AwayYellows else newYellows
                HomeSidelined = if isHome then newSidelined else s.HomeSidelined
                AwaySidelined = if isHome then s.AwaySidelined else newSidelined }
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
            let newYellows = Map.add player.Id (count + 1) yellows

            { s with
                HomeYellows = if isHome then newYellows else s.HomeYellows
                AwayYellows = if isHome then s.AwayYellows else newYellows }
            |> addEvent
                { Second = second
                  PlayerId = player.Id
                  ClubId = clubId
                  Type = YellowCard }

    // ------------------------------------------------------------------ //
    //  Injuries                                                            //
    // ------------------------------------------------------------------ //

    let private injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let private processInjury (player: Player) (clubId: ClubId) (second: int) (s: MatchState) =
        let isHome = clubId = s.Home.Id

        let newSidelined =
            Map.add player.Id SidelinedByInjury (if isHome then s.HomeSidelined else s.AwaySidelined)

        { s with
            HomeSidelined = if isHome then newSidelined else s.HomeSidelined
            AwaySidelined = if isHome then s.AwaySidelined else newSidelined }
        |> addEvent
            { Second = second
              PlayerId = player.Id
              ClubId = clubId
              Type = Injury "match" }

    // ------------------------------------------------------------------ //
    //  Substitutions                                                       //
    //                                                                      //
    //  The incoming player is APPENDED to s.HomePlayers / s.AwayPlayers   //
    //  so resolveView will see them in future duels. The outgoing player   //
    //  is added to sidelined so they are excluded. Conditions array is     //
    //  extended with the substitute's current condition.                   //
    // ------------------------------------------------------------------ //

    let private maxSubs = 3

    let private processSubstitution (clubId: ClubId) (second: int) (s: MatchState) : MatchState =
        let isHome = clubId = s.Home.Id
        let subsUsed = if isHome then s.HomeSubsUsed else s.AwaySubsUsed

        if subsUsed >= maxSubs then
            s
        else

            let club = if isHome then s.Home else s.Away
            let players = if isHome then s.HomePlayers else s.AwayPlayers
            let conditions = if isHome then s.HomeConditions else s.AwayConditions
            let sidelined = if isHome then s.HomeSidelined else s.AwaySidelined

            let activeOutfield =
                activeIndices players sidelined
                |> Array.filter (fun i -> players[i].Position <> GK)
                |> Array.sortBy (fun i -> conditions[i])

            if activeOutfield.Length = 0 then
                s
            else

                let outIdx = activeOutfield[0]
                let playerOut = players[outIdx]
                let minCondition = conditions[outIdx]

                let threshold = if goalDiff isHome s < 0 then 70 else 60

                if minCondition >= threshold then
                    s
                else

                    let startingIds =
                        match club.CurrentLineup with
                        | None -> Set.empty
                        | Some lu -> lu.Slots |> List.choose _.PlayerId |> Set.ofList

                    // Reserves: not in starting XI, not already on the pitch (in players array), not sidelined
                    let onPitchIds = players |> Array.map _.Id |> Set.ofArray

                    let reserve =
                        club.Players
                        |> List.filter (fun p ->
                            not (Set.contains p.Id startingIds)
                            && not (Set.contains p.Id onPitchIds)
                            && not (Map.containsKey p.Id sidelined))
                        |> List.sortByDescending _.Condition
                        |> List.tryHead

                    match reserve with
                    | None -> s
                    | Some incoming ->
                        let newSidelined = Map.add playerOut.Id SidelinedBySub sidelined
                        // Append incoming to players array and their condition to conditions array
                        let newPlayers = Array.append players [| incoming |]
                        let newConditions = Array.append conditions [| incoming.Condition |]

                        let s' =
                            { s with
                                HomePlayers = if isHome then newPlayers else s.HomePlayers
                                AwayPlayers = if isHome then s.AwayPlayers else newPlayers
                                HomeConditions = if isHome then newConditions else s.HomeConditions
                                AwayConditions = if isHome then s.AwayConditions else newConditions
                                HomeSidelined = if isHome then newSidelined else s.HomeSidelined
                                AwaySidelined = if isHome then s.AwaySidelined else newSidelined
                                HomeSubsUsed = if isHome then subsUsed + 1 else s.HomeSubsUsed
                                AwaySubsUsed = if isHome then s.AwaySubsUsed else subsUsed + 1 }

                        s'
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

    // ------------------------------------------------------------------ //
    //  Dispatcher                                                          //
    // ------------------------------------------------------------------ //

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
            applyFatigue s // ctx no longer needed here

        | Duel ->
            let possessionBefore = s.Possession
            let att, def, attIdx, defIdx = pickDuel ctx s
            let s' = resolveDuel homeClubId (att, def, attIdx, defIdx) ctx s

            if s'.Possession = possessionBefore then
                enqueue second (ShotAttempt att) queue

            if Continuous.Uniform.Sample 0.0 1.0 < cardProbability def then
                enqueue second (CardCheck(def, def.ClubId, false)) queue

            if Continuous.Uniform.Sample 0.0 1.0 < injuryProbability att then
                enqueue second (InjuryCheck(att, att.ClubId)) queue

            enqueue (second + 30) Duel queue
            s'

        | ShotAttempt attacker -> tryShot attacker ctx s

        | CardCheck(player, clubId, _) -> processCard player clubId second s

        | InjuryCheck(player, clubId) -> processInjury player clubId second s

        | SubstitutionCheck clubId -> processSubstitution clubId second s

        | MatchEnd -> s

    // ------------------------------------------------------------------ //
    //  Context builder                                                     //
    // ------------------------------------------------------------------ //

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

        let homePlayers = homeData |> Array.map (fun (p, _, _) -> p)
        let awayPlayers = awayData |> Array.map (fun (p, _, _) -> p)
        ctx, homePlayers, awayPlayers

    // ------------------------------------------------------------------ //
    //  Initial queue                                                       //
    // ------------------------------------------------------------------ //

    let private subMinutes = [| 60; 75; 85 |]

    let private buildInitialQueue (homeId: ClubId) (awayId: ClubId) : EventQueue =
        let q = EventQueue()
        enqueueAll [ 30, Duel; 180, FatigueCheck; 95 * 60, MatchEnd ] q

        for min in subMinutes do
            enqueue (min * 60) (SubstitutionCheck homeId) q
            enqueue (min * 60) (SubstitutionCheck awayId) q

        q

    // ------------------------------------------------------------------ //
    //  Event loop                                                          //
    // ------------------------------------------------------------------ //

    let private runEventLoop (homeClubId: int) (ctx: MatchContext) (initState: MatchState) =
        let queue = buildInitialQueue initState.Home.Id initState.Away.Id
        let mutable s = initState

        while queue.Count > 0 do
            let mutable ev = Unchecked.defaultof<_>
            let mutable pr = 0
            queue.TryDequeue(&ev, &pr) |> ignore

            match ev with
            | MatchEnd -> queue.Clear()
            | _ -> s <- dispatch homeClubId ctx queue pr ev s

        s

    // ------------------------------------------------------------------ //
    //  Public API                                                          //
    // ------------------------------------------------------------------ //

    let simulateMatch (home: Club) (away: Club) : int * int * MatchEvent list =
        let ctx, homePlayers, awayPlayers = buildContext home away

        let initState: MatchState =
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
              EventsRev = [] }

        let final = runEventLoop home.Id ctx initState
        final.HomeScore, final.AwayScore, final.EventsRev

    let trySimulateMatch (home: Club) (away: Club) : Result<int * int * MatchEvent list, SimulationError> =
        try
            Ok(simulateMatch home away)
        with SimulationException e ->
            Error e
