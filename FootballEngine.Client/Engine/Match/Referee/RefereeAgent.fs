namespace FootballEngine

open FootballEngine.Domain
open MatchStateOps
open SchedulingTypes

module RefereeAgent =

    // Phase 0: Second -> SubTick
    let private event subTick playerId clubId t =
        { SubTick = subTick
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

    // Phase 2: out-of-bounds thresholds in metres (pitch is 105×68)
    let private ballOutOfBounds (s: MatchState) : ClubSide option =
        let pos = s.Ball.Position
        let outX = pos.X < 0.5 || pos.X > PhysicsContract.PitchLength - 0.5
        let outY = pos.Y < 0.5 || pos.Y > PhysicsContract.PitchWidth - 0.5

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

    let decide (subTick: int) (att: Player option) (def: Player option) (s: MatchState) : RefereeAction list =
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

    let resolve (subTick: int) (action: RefereeAction) (s: MatchState) : MatchState * MatchEvent list =
        match action with
        | RefereeIdle -> s, []

        | ConfirmGoal(scoringClub, scorerId, isOwnGoal) ->
            let s', goalEvents = awardGoal scoringClub scorerId subTick s
            let events =
                if isOwnGoal then goalEvents |> List.map (fun e -> { e with Type = OwnGoal })
                else goalEvents
            s' |> clearOffsideSnapshot, events

        | AnnulGoal ->
            let resetX =
                match s.PendingOffsideSnapshot with
                | Some snap -> snap.BallXAtPass
                | None -> PhysicsContract.HalfwayLineX

            { s with
                Ball =
                    { s.Ball with
                        Position = defaultSpatial resetX (PhysicsContract.PitchWidth / 2.0)
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot, []

        | AwardThrowIn team ->
            // Phase 2: throw-in positions in metres
            let throwX =
                match team with
                | HomeClub -> PhysicsContract.PenaltyAreaDepth
                | AwayClub -> PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth

            { s with
                AttackingClub = team
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = throwX; Y = PhysicsContract.PitchWidth / 2.0
                                Z = 0.0; Vx = 0.0; Vy = 0.0; Vz = 0.0 }
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot, []

        | AwardCorner team ->
            // Phase 2: corner positions in metres
            let cornerX =
                match team with
                | HomeClub -> PhysicsContract.PitchLength - 0.5
                | AwayClub -> 0.5

            { s with
                AttackingClub = team
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = cornerX; Y = PhysicsContract.PitchWidth / 2.0
                                Z = 0.0; Vx = 0.0; Vy = 0.0; Vz = 0.0 }
                        Spin = Spin.zero
                        LastTouchBy = None
                        IsInPlay = true } }
            |> clearOffsideSnapshot, []

        | IssueYellow(player, clubId) ->
            let ts = side clubId s
            let count = ts.Yellows |> Map.tryFind player.Id |> Option.defaultValue 0

            let ts', events =
                if count >= 1 then
                    { ts with
                        Yellows = Map.add player.Id (count + 1) ts.Yellows
                        Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined },
                    [ event subTick player.Id clubId YellowCard
                      event subTick player.Id clubId RedCard ]
                else
                    { ts with Yellows = Map.add player.Id (count + 1) ts.Yellows },
                    [ event subTick player.Id clubId YellowCard ]

            withSide clubId ts' s, events

        | IssueRed(player, clubId) ->
            let ts = side clubId s
            withSide clubId { ts with Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined } s,
            [ event subTick player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            let ts = side clubId s
            withSide clubId { ts with Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined } s,
            [ event subTick player.Id clubId (MatchEventType.Injury "match") ]

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | HalfTimeTick | FullTimeTick | MatchEndTick | ExtraTimeTick _ ->
            { State = state; Events = []; Spawned = []; Transition = None }

        | InjuryTick(playerId, _severity) ->
            let player =
                seq { yield! state.HomeSide.Players; yield! state.AwaySide.Players }
                |> Seq.tryFind (fun p -> p.Id = playerId)

            let newState, events =
                match player with
                | Some p -> resolve tick.SubTick (IssueInjury(p, clubIdOf p state)) state
                | None -> state, []

            { State = newState
              Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.injuryDelay
                    Priority = TickPriority.MatchControl
                    SequenceId = 0L
                    Kind = ResumePlayTick } ]
              Transition = Some(Stopped Injury) }

        | ResumePlayTick ->
            { State = state; Events = []; Spawned = []; Transition = Some LivePlay }

        | _ ->
            { State = state; Events = []; Spawned = []; Transition = None }

    let runRefereeStep subTick (att: Player option) (def: Player option) s =
        let actions = decide subTick att def s
        let s', evs =
            actions
            |> List.fold
                (fun (accState, accEvents) action ->
                    let s', evs = resolve subTick action accState
                    s', evs @ accEvents)
                (s, [])
            |> fun (s, evs) -> s, List.rev evs
        s', evs, actions
