namespace FootballEngine

open System
open System.Collections.Generic
open FootballEngine.Domain
open FSharp.Stats.Distributions
open MatchState
open MatchReferee
open MatchManager
open Pitch

module MatchSimulator =

    type SimulationError =
        | MissingLineup of clubName: string
        | IncompleteLineup of clubName: string * playerCount: int

    exception SimulationException of SimulationError

    let private raise' e = raise (SimulationException e)

    // ── Fatigue ───────────────────────────────────────────────────────────

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

    // ── Event dispatch ────────────────────────────────────────────────────

    type private Queue = PriorityQueue<ScheduledEvent, int>

    let private dispatch (homeId: ClubId) (q: Queue) (second: int) (ev: ScheduledEvent) (s: MatchState) =
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
                q.Enqueue(CardCheck(def, def.ClubId, false), second)

            if Continuous.Uniform.Sample 0.0 1.0 < injuryProbability att then
                q.Enqueue(InjuryCheck(att, att.ClubId), second)

            q.Enqueue(Duel, second + 30)
            s' |> updatePositions

        | ShotAttempt att -> tryShot att s
        | CardCheck(p, clubId, _) -> processCard p clubId second s
        | InjuryCheck(p, clubId) -> processInjury p clubId second s
        | SubstitutionCheck clubId -> processSubstitution clubId second s
        | MatchEnd -> s


    let private toCoords slotX slotY isHome =
        if isHome then
            (1.0 - slotY) * 100.0, slotX * 100.0
        else
            slotY * 100.0, slotX * 100.0

    let private extractLineup (club: Club) (isHome: bool) =
        match club.CurrentLineup with
        | None -> raise' (MissingLineup club.Name)
        | Some lu ->
            lu.Slots
            |> List.choose (fun s ->
                s.PlayerId
                |> Option.bind (fun pid ->
                    club.Players
                    |> List.tryFind (fun p -> p.Id = pid)
                    |> Option.map (fun p -> let x, y = toCoords s.X s.Y isHome in p, x, y)))
            |> Array.ofList

    let private initQueue (homeId: ClubId) (awayId: ClubId) =
        let q = Queue()
        q.Enqueue(Duel, 30)
        q.Enqueue(FatigueCheck, 180)
        q.Enqueue(MatchEnd, 95 * 60)

        for min in [| 60; 75; 85 |] do
            q.Enqueue(SubstitutionCheck homeId, min * 60)
            q.Enqueue(SubstitutionCheck awayId, min * 60)

        q

    let private initState (home: Club) (hp: Player[]) (away: Club) (ap: Player[]) (ctx: MatchContext) : MatchState =
        { Home = home
          Away = away
          Second = 0
          HomeScore = 0
          AwayScore = 0
          BallPosition = 50.0, 50.0
          Possession = Home
          Momentum = 0.0
          HomePlayers = hp
          AwayPlayers = ap
          HomeConditions = hp |> Array.map _.Condition
          AwayConditions = ap |> Array.map _.Condition
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

    let private runLoop (homeId: ClubId) (init: MatchState) =
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
                s <- dispatch homeId q pr ev s

                match ev with
                | Duel
                | ShotAttempt _ -> snapshots.Add(s)
                | _ -> ()

        s, snapshots.ToArray()

    let private run (home: Club) (away: Club) =
        let homeData = extractLineup home true
        let awayData = extractLineup away false

        if homeData.Length <> 11 then
            raise' (IncompleteLineup(home.Name, homeData.Length))

        if awayData.Length <> 11 then
            raise' (IncompleteLineup(away.Name, awayData.Length))

        let ctx =
            { HomePositions = homeData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray
              AwayPositions = awayData |> Array.map (fun (p, x, y) -> p.Id, (x, y)) |> Map.ofArray }

        let hp = homeData |> Array.map (fun (p, _, _) -> p)
        let ap = awayData |> Array.map (fun (p, _, _) -> p)
        let final, snapshots = runLoop home.Id (initState home hp away ap ctx)
        { Final = final; Snapshots = snapshots }

    let simulate (home: Club) (away: Club) : int * int * MatchEvent list =
        let r = run home away
        r.Final.HomeScore, r.Final.AwayScore, r.Final.EventsRev

    let trySimulateMatch (home: Club) (away: Club) : Result<int * int * MatchEvent list, SimulationError> =
        try
            Ok(simulate home away)
        with SimulationException e ->
            Error e

    let trySimulateMatchFull (home: Club) (away: Club) =
        try
            Ok(run home away)
        with SimulationException e ->
            Error e
