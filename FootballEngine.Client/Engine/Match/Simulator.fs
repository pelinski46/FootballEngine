namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.MatchPlayerMovement
open FootballEngine.MatchPlayerDecision
open FootballEngine.MatchPlayerAction
open FootballEngine.Stats
open MatchState

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private fatigue
        (p: Player)
        (pressing: bool)
        (tactics: TeamTactics)
        (instructions: TacticalInstructions option)
        =
        let config = tacticsConfig tactics instructions
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15

        int (
            float (base' + workRate)
            * (if pressing then 1.5 else 1.0)
            * config.PressingIntensity
        )

    let private applyFatigue (s: MatchState) : MatchState =
        let ballX = s.Ball.Position.X

        let drain (ts: TeamSide) (pressing: bool) (fatigueMultiplier: float) =
            { ts with
                Conditions =
                    Array.map2
                        (fun c p -> Math.Max(0, c - int (float (fatigue p pressing ts.Tactics ts.Instructions) * fatigueMultiplier)))
                        ts.Conditions
                        ts.Players }

        s
        |> withSide true (drain (homeSide s) (ballX > 70.0) (1.0 - BalanceConfig.HomeFatigueReduction))
        |> withSide false (drain (awaySide s) (ballX < 30.0) 1.0)

    type TickKind =
        | DuelTick
        | PhysicsTick
        | SubstitutionTick
        | MatchEndTick

    type Tick = { Second: int; Kind: TickKind }

    let private generateTicks (homeId: ClubId) (awayId: ClubId) : Tick list =
        let rec duelTicks acc t =
            if t >= 95 * 60 then List.rev acc
            else
                let interval = normalInt 25.0 5.0 15 35
                let t' = t + interval
                if t' < 95 * 60 then
                    let variation = normalInt 0.0 5.0 (-8) 8
                    duelTicks ({ Second = t' + variation; Kind = DuelTick } :: acc) t'
                else
                    List.rev acc

        let duel = duelTicks [] 0
        let physics = [ for sec in 30..30 .. (95 * 60) -> { Second = sec; Kind = PhysicsTick } ]
        let subs = [ for min in [ 60; 75; 85 ] -> { Second = min * 60; Kind = SubstitutionTick } ]
        let endTick = { Second = 95 * 60; Kind = MatchEndTick }

        (duel @ physics @ subs @ [endTick]) |> List.sortBy _.Second

    let private physicsDt = 0.5

    let private runPlayerChain
        (homeId: ClubId)
        (second: int)
        (s: MatchState)
        : MatchState * MatchEvent list * Player option * Player option =

        let rec loop state events intent depth lastAtt lastDef =
            if depth >= BalanceConfig.AvgChainLength || intent = PlayerIdle then
                state, List.rev events, lastAtt, lastDef
            else
                let lastAtt' =
                    match intent with
                    | ResolveDuel(att, _, _, _) -> Some att
                    | ExecuteShot att -> Some att
                    | ExecutePass att -> Some att
                    | ExecuteDribble att -> Some att
                    | ExecuteCross att -> Some att
                    | ExecuteLongBall att -> Some att
                    | ExecuteFreeKick att -> Some att
                    | _ -> lastAtt

                let lastDef' =
                    match intent with
                    | ResolveDuel(_, def, _, _) -> Some def
                    | ExecuteTackle def -> Some def
                    | _ -> lastDef

                let newState, newEvents, nextIntent = resolve homeId second intent state
                loop newState (newEvents @ events) nextIntent (depth + 1) lastAtt' lastDef'

        loop s [] (decide s) 0 None None

    let private runRefereeStep second att def s =
        MatchReferee.decide second att def s
        |> List.fold
            (fun (accState, accEvents) intent ->
                let s', evs = MatchReferee.resolve second intent accState
                s', evs @ accEvents)
            (s, [])
        |> fun (s, evs) -> s, List.rev evs

    let private runManagerStep second homeSquad awaySquad s =
        MatchManager.decide second homeSquad awaySquad s
        |> List.fold
            (fun (accState, accEvents) intent ->
                let s', evs = MatchManager.resolve second intent accState
                s', evs @ accEvents)
            (s, [])
        |> fun (s, evs) -> s, List.rev evs

    let private runTick
        (homeId: ClubId)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (tick: Tick)
        (s: MatchState)
        : MatchState * MatchEvent list =

        let s = { s with Second = tick.Second }

        match tick.Kind with
        | MatchEndTick -> s, []
        | PhysicsTick ->
            s
            |> updatePlayerVelocities physicsDt
            |> Pitch.updatePositions physicsDt
            |> MatchBall.updatePhysics physicsDt
            |> applyFatigue,
            []
        | SubstitutionTick -> runManagerStep tick.Second homeSquad awaySquad s
        | DuelTick ->
            let s1 = s |> updatePlayerVelocities physicsDt
            let s2, playerEvents, att, def = runPlayerChain homeId tick.Second s1
            let s3 = s2 |> MatchBall.updatePhysics physicsDt |> Pitch.updatePositions physicsDt
            let s4, refereeEvents = runRefereeStep tick.Second att def s3
            let s5, managerEvents = runManagerStep tick.Second homeSquad awaySquad s4
            s5 |> applyFatigue, playerEvents @ refereeEvents @ managerEvents

    let private runLoop
        (homeId: ClubId)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (init: MatchState)
        (saveSnapshots: bool)
        =

        let ticks = generateTicks init.Home.Id init.Away.Id

        let snapshots =
            if saveSnapshots then
                Some(System.Collections.Generic.List<MatchState>())
            else
                None

        let final, allEvents =
            ticks
            |> List.fold
                (fun (s, evs) tick ->
                    if tick.Kind = MatchEndTick then
                        s, evs
                    else
                        let s', tickEvents = runTick homeId homeSquad awaySquad tick s

                        match snapshots with
                        | Some snaps when tick.Kind = DuelTick -> snaps.Add(s')
                        | _ -> ()

                        s', tickEvents @ evs)
                (init, [])
            |> fun (s, evs) -> s, List.rev evs

        final, allEvents, snapshots |> Option.map _.ToArray()

    let private runLoopFast homeId homeSquad awaySquad init =
        let s, evs, _ = runLoop homeId homeSquad awaySquad init false
        s, evs

    let private runLoopFull homeId homeSquad awaySquad init =
        match runLoop homeId homeSquad awaySquad init true with
        | s, evs, Some snaps -> s, evs, snaps
        | s, evs, None -> s, evs, [||]

    open FsToolkit.ErrorHandling

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    let private toCoords slotX slotY isHome =
        if isHome then
            (1.0 - slotY) * 100.0, slotX * 100.0
        else
            slotY * 100.0, slotX * 100.0

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (isHome: bool)
        : Result<(Player * float * float)[] * TeamTactics * TacticalInstructions option, SimulationError> =
        match headCoach.Attributes.Coaching.Lineup with
        | None -> Error(MissingLineup club.Name)
        | Some lu ->
            let slots =
                lu.Slots
                |> List.choose (fun s ->
                    s.PlayerId
                    |> Option.bind (fun pid ->
                        players
                        |> Map.tryFind pid
                        |> Option.map (fun p ->
                            let x, y = toCoords s.X s.Y isHome
                            p, x, y)))
                |> Array.ofList

            Ok(slots, lu.Tactics, lu.Instructions)

    let private validateLineups (home: Club) homeData (away: Club) awayData : Result<unit, SimulationError> =
        if home.Id = away.Id then
            Error(SameClub home.Name)
        elif homeData |> Array.length <> 11 then
            Error(IncompleteLineup(home.Name, homeData |> Array.length))
        elif awayData |> Array.length <> 11 then
            Error(IncompleteLineup(away.Name, awayData |> Array.length))
        else
            Ok()

    let private buildContext (homeData: (Player * float * float)[]) (awayData: (Player * float * float)[]) =
        { HomePositions = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
          AwayPositions = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray }

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float * float>) : Spatial[] =
        players
        |> Array.map (fun p ->
            let x, y = posMap |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)
            defaultSpatial x y)

    let private initState
        home
        homeCoach
        hp
        hPosMap
        homeTactics
        homeInstructions
        away
        awayCoach
        ap
        aPosMap
        awayTactics
        awayInstructions
        isKnockout
        =

        let hPos = positionArrayOf hp hPosMap
        let aPos = positionArrayOf ap aPosMap
        let defaultInstructions = TacticalInstructions.defaultInstructions

        { Home = home
          Away = away
          HomeCoach = homeCoach
          AwayCoach = awayCoach
          Second = 0
          HomeScore = 0
          AwayScore = 0
          Ball = defaultBall
          Possession = Home
          Momentum = 0.0
          HomeSide =
            { Players = hp
              Conditions = hp |> Array.map _.Condition
              Positions = hPos
              BasePositions = hPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = homeTactics
              Instructions = homeInstructions |> Option.orElse (Some defaultInstructions) }
          AwaySide =
            { Players = ap
              Conditions = ap |> Array.map _.Condition
              Positions = aPos
              BasePositions = aPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0
              Tactics = awayTactics
              Instructions = awayInstructions |> Option.orElse (Some defaultInstructions) }
          PenaltyShootout = None
          IsExtraTime = false
          IsKnockoutMatch = isKnockout }

    let private simulatePenaltyShootout (s: MatchState) (home: Club) (away: Club) (players: Map<PlayerId, Player>) =
        result {
            let homePlayers =
                home.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let awayPlayers =
                away.PlayerIds
                |> List.choose players.TryFind
                |> List.sortByDescending _.CurrentSkill

            let rec simulateKicks homeKicks awayKicks kickNum =
                let homeKicker = homePlayers |> List.item ((kickNum - 1) % homePlayers.Length)
                let awayKicker = awayPlayers |> List.item ((kickNum - 1) % awayPlayers.Length)

                let homeS, _, _ =
                    resolve home.Id (95 * 60 + kickNum) (ExecutePenalty(homeKicker, true, kickNum)) s

                let awayS, _, _ =
                    resolve home.Id (95 * 60 + kickNum) (ExecutePenalty(awayKicker, false, kickNum)) homeS

                let homeScored = homeS.HomeScore > s.HomeScore
                let awayScored = awayS.AwayScore > homeS.AwayScore

                let newHomeKicks = (homeKicker.Id, homeScored, kickNum) :: homeKicks
                let newAwayKicks = (awayKicker.Id, awayScored, kickNum) :: awayKicks
                let homeGoals = newHomeKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)
                let awayGoals = newAwayKicks |> List.sumBy (fun (_, sc, _) -> if sc then 1 else 0)

                if kickNum > 5 && homeGoals <> awayGoals then
                    { HomeKicks = List.rev newHomeKicks
                      AwayKicks = List.rev newAwayKicks
                      CurrentKick = kickNum
                      IsComplete = true }
                elif kickNum > 5 then
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                elif kickNum <= 5 then
                    let remaining = 5 - kickNum

                    if homeGoals > awayGoals + remaining || awayGoals > homeGoals + remaining then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    elif kickNum = 5 then
                        { HomeKicks = List.rev newHomeKicks
                          AwayKicks = List.rev newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    else
                        simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                else
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)

            let shootout = simulateKicks [] [] 1

            let events =
                [ for (pid, scored, k) in shootout.HomeKicks ->
                      { Second = 95 * 60 + k
                        PlayerId = pid
                        ClubId = home.Id
                        Type = PenaltyAwarded scored }
                  for (pid, scored, k) in shootout.AwayKicks ->
                      { Second = 95 * 60 + k
                        PlayerId = pid
                        ClubId = away.Id
                        Type = PenaltyAwarded scored } ]

            return
                { Final =
                    { s with
                        PenaltyShootout = Some shootout }
                  Events = events
                  Snapshots = [||] }
        }

    let private setup
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (isKnockout: bool)
        : Result<MatchState * Player list * Player list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players true
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players false
            do! validateLineups home homeData away awayData

            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)

            let init =
                initState
                    home
                    homeCoach
                    hp
                    ctx.HomePositions
                    homeTactics
                    homeInstructions
                    away
                    awayCoach
                    ap
                    ctx.AwayPositions
                    awayTactics
                    awayInstructions
                    isKnockout

            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            return init, homeSquad, awaySquad
        }

    let trySimulateMatch
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<int * int * MatchEvent list * MatchState, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events = runLoopFast home.Id homeSquad awaySquad init
            return final.HomeScore, final.AwayScore, events, final
        }

    let trySimulateMatchFull
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff false
            let final, events, snapshots = runLoopFull home.Id homeSquad awaySquad init

            return
                { Final = final
                  Events = events
                  Snapshots = snapshots }
        }

    let trySimulateMatchKnockout
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay * bool, SimulationError> =
        result {
            let! init, homeSquad, awaySquad = setup home away players staff true
            let final, events, snapshots = runLoopFull home.Id homeSquad awaySquad init

            let replay =
                { Final = final
                  Events = events
                  Snapshots = snapshots }

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootout = simulatePenaltyShootout replay.Final home away players
                return shootout, true
            else
                return replay, false
        }
