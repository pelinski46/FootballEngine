namespace FootballEngine

open FootballEngine.Domain
open MatchStateOps
open SchedulingTypes

module RefereeAgent =

    let private event second playerId clubId t =
        { Second = second
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    let cardProbability (playerClub: ClubSide) (p: Player) =
        let baseProb = 0.010 + float p.Mental.Aggression * 0.0004

        match playerClub with
        | HomeClub -> baseProb * (1.0 - BalanceConfig.HomeCardReduction)
        | AwayClub -> baseProb

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let private ballOutOfBounds (s: MatchState) : ClubSide option =
        let pos = s.Ball.Position
        let outX = pos.X < 2.0 || pos.X > 98.0
        let outY = pos.Y < 2.0 || pos.Y > 98.0

        if outX || outY then
            let lastTouchClub =
                s.Ball.LastTouchBy
                |> Option.bind (fun pid ->
                    if s.HomeSide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some s.Home.Id
                    elif s.AwaySide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some s.Away.Id
                    else
                        None)

            lastTouchClub
            |> Option.map (fun clubId -> ClubSide.flip (ClubSide.ofClubId clubId s))
        else
            None

    let decide (second: int) (att: Player option) (def: Player option) (s: MatchState) : RefereeAction list =
        let goalIntent =
            GoalDetector.detect s.Ball
            |> Option.bind (fun scoringClub ->
                let offsideActive =
                    s.PendingOffsideSnapshot
                    |> Option.map MatchSpatial.isOffsideFromSnapshot
                    |> Option.defaultValue false

                if offsideActive then
                    Some AnnulGoal
                else
                    let scorerId, isOwnGoal = GoalDetector.scorer scoringClub s.Ball s
                    Some(ConfirmGoal(scoringClub, scorerId, isOwnGoal)))
            |> Option.toList

        let throwInIntent = ballOutOfBounds s |> Option.map AwardThrowIn |> Option.toList

        let cardIntent =
            def
            |> Option.filter (fun d ->
                let defClub = ClubSide.ofClubId (clubIdOf d s) s
                Stats.bernoulli (cardProbability defClub d))
            |> Option.map (fun d -> IssueYellow(d, clubIdOf d s))
            |> Option.toList

        let injuryIntent =
            att
            |> Option.filter (fun a -> Stats.bernoulli (injuryProbability a))
            |> Option.map (fun a -> IssueInjury(a, clubIdOf a s))
            |> Option.toList

        goalIntent @ throwInIntent @ cardIntent @ injuryIntent

    let resolve (second: int) (action: RefereeAction) (s: MatchState) : MatchState * MatchEvent list =
        match action with
        | RefereeIdle -> s, []

        | ConfirmGoal(scoringClub, scorerId, isOwnGoal) ->
            let s', goalEvents = awardGoal scoringClub scorerId second s

            let events =
                if isOwnGoal then
                    goalEvents |> List.map (fun e -> { e with Type = OwnGoal })
                else
                    goalEvents

            s' |> clearOffsideSnapshot, events

        | AnnulGoal ->
            let resetX =
                match s.PendingOffsideSnapshot with
                | Some snap -> snap.BallXAtPass
                | None -> 50.0

            { s with
                Ball =
                    { s.Ball with
                        Position = defaultSpatial resetX 50.0
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot,
            []

        | AwardThrowIn team ->
            let throwX =
                match team with
                | HomeClub -> 5.0
                | AwayClub -> 95.0

            { s with
                AttackingClub = team
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = throwX
                                Y = 50.0
                                Z = 0.0
                                Vx = 0.0
                                Vy = 0.0
                                Vz = 0.0 }
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot,
            []

        | AwardCorner team ->
            let cornerX =
                match team with
                | HomeClub -> 99.0
                | AwayClub -> 1.0

            { s with
                AttackingClub = team
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = cornerX
                                Y = 50.0
                                Z = 0.0
                                Vx = 0.0
                                Vy = 0.0
                                Vz = 0.0 }
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot,
            []

        | IssueYellow(player, clubId) ->
            let ts = side clubId s
            let count = ts.Yellows |> Map.tryFind player.Id |> Option.defaultValue 0

            let ts', events =
                if count >= 1 then
                    { ts with
                        Yellows = Map.add player.Id (count + 1) ts.Yellows
                        Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined },
                    [ event second player.Id clubId YellowCard
                      event second player.Id clubId RedCard ]
                else
                    { ts with
                        Yellows = Map.add player.Id (count + 1) ts.Yellows },
                    [ event second player.Id clubId YellowCard ]

            withSide clubId ts' s, events

        | IssueRed(player, clubId) ->
            let ts = side clubId s

            withSide
                clubId
                { ts with
                    Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined }
                s,
            [ event second player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            let ts = side clubId s

            withSide
                clubId
                { ts with
                    Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined }
                s,
            [ event second player.Id clubId (MatchEventType.Injury "match") ]

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | HalfTimeTick ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }
        | FullTimeTick ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }
        | MatchEndTick ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }
        | ExtraTimeTick _ ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }
        | InjuryTick(playerId, _severity) ->
            let player =
                seq {
                    yield! state.HomeSide.Players
                    yield! state.AwaySide.Players
                }
                |> Seq.tryFind (fun p -> p.Id = playerId)

            let newState, events =
                match player with
                | Some p -> resolve tick.Second (IssueInjury(p, clubIdOf p state)) state
                | None -> state, []

            { State = newState
              Events = events
              Spawned =
                [ { Second = tick.Second + Stats.delayFrom BalanceConfig.injuryDelay
                    Priority = TickPriority.MatchControl
                    SequenceId = 0L
                    Kind = ResumePlayTick } ]
              Transition = Some(Stopped Injury) }
        | ResumePlayTick ->
            { State = state
              Events = []
              Spawned = []
              Transition = Some LivePlay }
        | _ ->
            { State = state
              Events = []
              Spawned = []
              Transition = None }

    let runRefereeStep second (att: Player option) (def: Player option) s =
        let actions = decide second att def s

        let s', evs =
            actions
            |> List.fold
                (fun (accState, accEvents) action ->
                    let s', evs = resolve second action accState
                    s', evs @ accEvents)
                (s, [])
            |> fun (s, evs) -> s, List.rev evs

        s', evs, actions
