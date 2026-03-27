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

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int
        | SameClub of clubName: string

    let private fatigue (p: Player) (pressing: bool) =
        let base' = (100 - p.Physical.Stamina) / 20
        let workRate = p.Mental.WorkRate / 15
        int (float (base' + workRate) * (if pressing then 1.5 else 1.0))

    let private applyFatigue (s: MatchState) : MatchState =
        let ballX = fst s.BallPosition

        let drain (ts: TeamSide) (pressing: bool) =
            { ts with
                Conditions = Array.map2 (fun c p -> Math.Max(0, c - fatigue p pressing)) ts.Conditions ts.Players }

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
        (homeSquad: Player list)
        (awaySquad: Player list)
        (homeId: ClubId)
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
            let att, def, _, _ as duel = pickDuel s
            let possBefore = s.Possession
            let s' = resolveDuel homeId duel s

            if s'.Possession = possBefore then
                q.Enqueue(ShotAttempt att, second)

            if Continuous.Uniform.Sample 0.0 1.0 < cardProbability def then
                q.Enqueue(CardCheck(def, clubIdOf def s, false), second)

            if Continuous.Uniform.Sample 0.0 1.0 < injuryProbability att then
                q.Enqueue(InjuryCheck(att, clubIdOf att s), second)

            q.Enqueue(Duel, second + 30)
            s' |> updatePositions

        | ShotAttempt att -> tryShot att s
        | CardCheck(p, clubId, _) -> processCard p clubId second s
        | InjuryCheck(p, clubId) -> processInjury p clubId second s

        | SubstitutionCheck clubId ->
            let squad = if clubId = homeId then homeSquad else awaySquad
            processSubstitution squad clubId second s

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
        (headCoach: Staff)
        (players: Map<PlayerId, Player>)
        (isHome: bool)
        : Result<(Player * float * float)[], SimulationError> =
        match headCoach.Attributes.Coaching.Lineup with
        | None -> Error(MissingLineup headCoach.Name)
        | Some lu ->
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
            |> Ok

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

    let private initState
        (home: Club)
        (homeCoach: Staff)
        (hp: Player[])
        (hPos: Map<PlayerId, float * float>)
        (away: Club)
        (awayCoach: Staff)
        (ap: Player[])
        (aPos: Map<PlayerId, float * float>)
        : MatchState =
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
              SubsUsed = 0 }
          AwaySide =
            { Players = ap
              Conditions = ap |> Array.map _.Condition
              Positions = aPos
              BasePositions = aPos
              Sidelined = Map.empty
              Yellows = Map.empty
              SubsUsed = 0 }
          EventsRev = [] }

    let private initQueue (homeId: ClubId) (awayId: ClubId) =
        let q = Queue()
        q.Enqueue(Duel, 30)
        q.Enqueue(FatigueCheck, 180)
        q.Enqueue(MatchEnd, 95 * 60)

        for min in [| 60; 75; 85 |] do
            q.Enqueue(SubstitutionCheck homeId, min * 60)
            q.Enqueue(SubstitutionCheck awayId, min * 60)

        q

    let private runLoopFast (homeSquad: Player list) (awaySquad: Player list) (homeId: ClubId) (init: MatchState) =
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
            | _ -> s <- dispatch homeSquad awaySquad homeId q pr ev s

        s

    let private runLoop (homeSquad: Player list) (awaySquad: Player list) (homeId: ClubId) (init: MatchState) =
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
                s <- dispatch homeSquad awaySquad homeId q pr ev s

                match ev with
                | Duel
                | ShotAttempt _ -> snapshots.Add(s)
                | _ -> ()

        s, snapshots.ToArray()

    let private runFast
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<int * int * MatchEvent list, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData = extractLineup homeCoach players true
            let! awayData = extractLineup awayCoach players false
            do! validateLineups home homeData away awayData
            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)
            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            let init =
                initState home homeCoach hp ctx.HomePositions away awayCoach ap ctx.AwayPositions

            let final = runLoopFast homeSquad awaySquad home.Id init
            return final.HomeScore, final.AwayScore, final.EventsRev
        }

    let private run
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay, SimulationError> =
        result {
            let! homeCoach = resolveCoach home staff
            let! awayCoach = resolveCoach away staff
            let! homeData = extractLineup homeCoach players true
            let! awayData = extractLineup awayCoach players false
            do! validateLineups home homeData away awayData
            let ctx = buildContext homeData awayData
            let hp = homeData |> Array.map (fun (p, _, _) -> p)
            let ap = awayData |> Array.map (fun (p, _, _) -> p)
            let homeSquad = home.PlayerIds |> List.choose players.TryFind
            let awaySquad = away.PlayerIds |> List.choose players.TryFind

            let init =
                initState home homeCoach hp ctx.HomePositions away awayCoach ap ctx.AwayPositions

            let final, snapshots = runLoop homeSquad awaySquad home.Id init
            return { Final = final; Snapshots = snapshots }
        }

    let trySimulateMatch
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<int * int * MatchEvent list, SimulationError> =
        runFast home away players staff

    let trySimulateMatchFull
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        : Result<MatchReplay, SimulationError> =
        run home away players staff
