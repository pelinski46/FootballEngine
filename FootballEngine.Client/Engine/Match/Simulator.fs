namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FSharp.Stats.Distributions
open FsToolkit.ErrorHandling
open MatchState
open MatchReferee
open MatchManager
open Pitch
open MatchPlayer
open Stats

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
        let ballX = fst s.BallPosition

        let drain (ts: TeamSide) (pressing: bool) =
            { ts with
                Conditions =
                    Array.map2
                        (fun c p -> Math.Max(0, c - fatigue p pressing ts.Tactics ts.Instructions))
                        ts.Conditions
                        ts.Players }

        s
        |> withSide true (drain (homeSide s) (ballX > 70.0))
        |> withSide false (drain (awaySide s) (ballX < 30.0))

    type private Queue = PriorityQueue<ScheduledEvent, int>

    let private clubIdOf (p: Player) (s: MatchState) =
        if s.HomeSide.Players |> Array.exists (fun x -> x.Id = p.Id) then
            s.Home.Id
        else
            s.Away.Id

    let private dispatch
        (homeId: ClubId)
        (players: Map<PlayerId, Player>)
        (q: Queue)
        (second: int)
        (ev: ScheduledEvent)
        (s: MatchState)
        =
        let s = { s with Second = second }

        match ev with
        | FatigueCheck ->
            q.Enqueue(FatigueCheck, second + 180)
            applyFatigue s

        | Duel ->
            match pickDuel s with
            | None -> s
            | Some(att, def, _, _ as duel) ->
                let possBefore = s.Possession
                let s' = resolveDuel homeId duel s

                if s'.Possession = possBefore then
                    q.Enqueue(ShotAttempt att, second)

                let foulChance = 0.02 + float def.Mental.Aggression * 0.001
                if Continuous.Uniform.Sample 0.0 1.0 < foulChance && s'.Possession <> possBefore then
                    q.Enqueue(FreeKickAttempt att, second + 2)

                if Continuous.Uniform.Sample 0.0 1.0 < cardProbability def then
                    q.Enqueue(CardCheck(def, clubIdOf def s, false), second)

                if Continuous.Uniform.Sample 0.0 1.0 < injuryProbability att then
                    q.Enqueue(InjuryCheck(att, clubIdOf att s), second)

                s' |> updatePositions

        | ShotAttempt att -> tryShot att s q second
        | CardCheck(p, clubId, _) -> processCard p clubId second s
        | InjuryCheck(p, clubId) -> processInjury p clubId second s

        | SubstitutionCheck clubId ->
            let squad = if clubId = homeId then s.HomeSide.Players |> Array.toList else s.AwaySide.Players |> Array.toList
            processSubstitution squad clubId second s

        | PenaltyKick(kicker, isHome, kickNum) ->
            processPenaltyKick kicker isHome kickNum s homeId players

        | FreeKickAttempt kicker ->
            processFreeKick kicker s homeId players

        | CornerTaken ->
            processCorner s homeId q second

        | MatchEnd -> s

    let private toCoords slotX slotY isHome =
        if isHome then
            (1.0 - slotY) * 100.0, slotX * 100.0
        else
            slotY * 100.0, slotX * 100.0

    let private resolveCoach (club: Club) (staff: Map<StaffId, Staff>) : Result<Staff, SimulationError> =
        staff
        |> Map.values
        |> Seq.tryFind (fun s -> s.Role = HeadCoach && s.Contract |> Option.map _.ClubId = Some club.Id)
        |> Option.map Ok
        |> Option.defaultValue (Error(MissingLineup club.Name))

    let private extractLineup
        (club: Club)
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (isHome: bool)
        : Result<(Player * float * float)[] * TeamTactics * TacticalInstructions option, SimulationError> =
        match headCoach.Attributes.Coaching.Lineup with
        | None -> Error(MissingLineup club.Name)
        | Some lu ->
            let tactics = lu.Tactics
            let instructions = lu.Instructions

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

            Ok(slots, tactics, instructions)

    let private validateLineups
        (home: Club)
        (homeData: 'a[])
        (away: Club)
        (awayData: 'a[])
        : Result<unit, SimulationError> =
        if home.Id = away.Id then
            Error(SameClub home.Name)
        elif homeData.Length <> 11 then
            Error(IncompleteLineup(home.Name, homeData.Length))
        elif awayData.Length <> 11 then
            Error(IncompleteLineup(away.Name, awayData.Length))
        else
            Ok()

    let private buildContext (homeData: (Player * float * float)[]) (awayData: (Player * float * float)[]) =
        { HomePositions = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
          AwayPositions = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray }

    let private positionArrayOf (players: Player[]) (posMap: Map<PlayerId, float * float>) : (float * float)[] =
        players
        |> Array.map (fun p -> posMap |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0))

    let private initState
        (home: Club)
        (homeCoach: Staff)
        (hp: Player[])
        (hPosMap: Map<PlayerId, float * float>)
        (homeTactics: TeamTactics)
        (homeInstructions: TacticalInstructions option)
        (away: Club)
        (awayCoach: Staff)
        (ap: Player[])
        (aPosMap: Map<PlayerId, float * float>)
        (awayTactics: TeamTactics)
        (awayInstructions: TacticalInstructions option)
        (isKnockout: bool)
        : MatchState =
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
          BallPosition = 50.0, 50.0
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
          EventsRev = []
          PenaltyShootout = None
          IsExtraTime = false
          IsKnockoutMatch = isKnockout }

    let private initQueue (homeId: ClubId) (awayId: ClubId) =
        let q = Queue()
        let mutable currentTime = 0

        while currentTime < 95 * 60 do
            let interval = normalInt 20.0 4.0 12 28
            currentTime <- currentTime + interval

            if currentTime < 95 * 60 then
                let variation = normalInt 0.0 5.0 (-8) 8
                q.Enqueue(Duel, currentTime + variation)

        q.Enqueue(FatigueCheck, 180)
        q.Enqueue(MatchEnd, 95 * 60)

        for min in [| 60; 75; 85 |] do
            q.Enqueue(SubstitutionCheck homeId, min * 60)
            q.Enqueue(SubstitutionCheck awayId, min * 60)

        q

    let private runLoopFast (homeId: ClubId) (players: Map<PlayerId, Player>) (init: MatchState) =
        let q = initQueue init.Home.Id init.Away.Id
        let mutable s = init

        while q.Count > 0 do
            let mutable ev = Unchecked.defaultof<_>
            let mutable pr = 0
            q.TryDequeue(&ev, &pr) |> ignore

            match ev with
            | MatchEnd ->
                s <- { s with Second = pr }
                q.Clear()
            | _ -> s <- dispatch homeId players q pr ev s

        s

    let private runLoop (homeId: ClubId) (players: Map<PlayerId, Player>) (init: MatchState) =
        let q = initQueue init.Home.Id init.Away.Id
        let mutable s = init
        let snapshots = List<MatchState>()

        while q.Count > 0 do
            let mutable ev = Unchecked.defaultof<_>
            let mutable pr = 0
            q.TryDequeue(&ev, &pr) |> ignore

            match ev with
            | MatchEnd ->
                s <- { s with Second = pr }
                q.Clear()
            | _ ->
                s <- dispatch homeId players q pr ev s

                match ev with
                | Duel
                | ShotAttempt _ -> snapshots.Add(s)
                | _ -> ()

        s, snapshots.ToArray()

    let private simulatePenaltyShootout (s: MatchState) (home: Club) (away: Club) (players: Map<PlayerId, Player>) (staff: Map<StaffId, Staff>) =
        result {
            let homePlayers = home.PlayerIds |> List.choose players.TryFind |> List.sortByDescending _.CurrentSkill
            let awayPlayers = away.PlayerIds |> List.choose players.TryFind |> List.sortByDescending _.CurrentSkill

            let takePenaltyKick (kicker: Player) (gk: Player) =
                let kickerSkill = float kicker.CurrentSkill
                let gkSkill = float gk.CurrentSkill
                let kickerMorale = float kicker.Morale
                let pressure = 0.85
                let baseChance = 0.75 + (kickerSkill - gkSkill) * 0.002 + kickerMorale * 0.001
                Continuous.Uniform.Sample 0.0 1.0 < baseChance * pressure

            let homeGk = awayPlayers |> List.tryFind (fun p -> p.Position = GK)
            let awayGk = homePlayers |> List.tryFind (fun p -> p.Position = GK)

            let rec simulateKicks (homeKicks: (PlayerId * bool) list) (awayKicks: (PlayerId * bool) list) (kickNum: int) =
                let homeKicker = homePlayers |> List.item ((kickNum - 1) % homePlayers.Length)
                let awayKicker = awayPlayers |> List.item ((kickNum - 1) % awayPlayers.Length)

                let homeScored = match homeGk with Some gk -> takePenaltyKick homeKicker gk | None -> false
                let awayScored = match awayGk with Some gk -> takePenaltyKick awayKicker gk | None -> false

                let newHomeKicks = (homeKicker.Id, homeScored) :: homeKicks
                let newAwayKicks = (awayKicker.Id, awayScored) :: awayKicks

                let homeGoals = newHomeKicks |> List.sumBy (fun (_, scored) -> if scored then 1 else 0)
                let awayGoals = newAwayKicks |> List.sumBy (fun (_, scored) -> if scored then 1 else 0)

                if kickNum > 5 && homeGoals <> awayGoals then
                    { HomeKicks = newHomeKicks
                      AwayKicks = newAwayKicks
                      CurrentKick = kickNum
                      IsComplete = true }
                elif kickNum > 5 && homeGoals = awayGoals then
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                elif kickNum <= 5 then
                    let remaining = 5 - kickNum
                    if homeGoals > awayGoals + remaining || awayGoals > homeGoals + remaining then
                        { HomeKicks = newHomeKicks
                          AwayKicks = newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    elif kickNum = 5 then
                        { HomeKicks = newHomeKicks
                          AwayKicks = newAwayKicks
                          CurrentKick = kickNum
                          IsComplete = true }
                    else
                        simulateKicks newHomeKicks newAwayKicks (kickNum + 1)
                else
                    simulateKicks newHomeKicks newAwayKicks (kickNum + 1)

            let shootout = simulateKicks [] [] 1

            let events =
                [ for (pid, scored) in shootout.HomeKicks ->
                    { Second = 95 * 60 + shootout.CurrentKick
                      PlayerId = pid
                      ClubId = home.Id
                      Type = PenaltyAwarded scored }
                  for (pid, scored) in shootout.AwayKicks ->
                    { Second = 95 * 60 + shootout.CurrentKick
                      PlayerId = pid
                      ClubId = away.Id
                      Type = PenaltyAwarded scored } ]

            let finalState = { s with PenaltyShootout = Some shootout; EventsRev = events @ s.EventsRev }
            return { Final = finalState; Snapshots = [|finalState|] }
        }

    let private runFast
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (isKnockout: bool)
        : Result<int * int * MatchEvent list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players true
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players false
            do! validateLineups home homeData away awayData
            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)
            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

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

            let final = runLoopFast home.Id players init
            return final.HomeScore, final.AwayScore, final.EventsRev
        }

    let private run
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (isKnockout: bool)
        : Result<MatchReplay, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData, homeTactics, homeInstructions = extractLineup home homeCoach players true
            let! awayData, awayTactics, awayInstructions = extractLineup away awayCoach players false
            do! validateLineups home homeData away awayData
            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)
            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

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

            let final, snapshots = runLoop home.Id players init
            return { Final = final; Snapshots = snapshots }
        }

    let trySimulateMatch
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<int * int * MatchEvent list, SimulationError> =
        runFast home away players staff false

    let trySimulateMatchFull
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay, SimulationError> =
        run home away players staff false

    let trySimulateMatchKnockout
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay * bool, SimulationError> =
        result {
            let! replay = run home away players staff true

            if replay.Final.HomeScore = replay.Final.AwayScore then
                let! shootoutResult = simulatePenaltyShootout replay.Final home away players staff
                return shootoutResult, true
            else
                return replay, false
        }
