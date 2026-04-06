namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open SchedulingTypes

module RefereeAgent =

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

    let private ballOutOfBounds (ctx: MatchContext) (state: SimState) : ClubSide option =
        let pos = state.Ball.Position
        let outX = pos.X < 0.5 || pos.X > PhysicsContract.PitchLength - 0.5
        let outY = pos.Y < 0.5 || pos.Y > PhysicsContract.PitchWidth - 0.5

        if outX || outY then
            let lastTouchClub =
                state.Ball.LastTouchBy
                |> Option.bind (fun pid ->
                    if
                        state.HomeSlots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = pid
                            | _ -> false)
                    then
                        Some ctx.Home.Id
                    elif
                        state.AwaySlots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = pid
                            | _ -> false)
                    then
                        Some ctx.Away.Id
                    else
                        None)

            lastTouchClub
            |> Option.map (fun clubId -> ClubSide.flip (if clubId = ctx.Home.Id then HomeClub else AwayClub))
        else
            None

    let decide
        (subTick: int)
        (att: Player option)
        (def: Player option)
        (ctx: MatchContext)
        (state: SimState)
        : RefereeAction list =
        let goalIntent =
            GoalDetector.detect state.Ball
            |> Option.bind (fun scoringClub ->
                let offsideActive =
                    state.PendingOffsideSnapshot
                    |> Option.map MatchSpatial.isOffsideFromSnapshot
                    |> Option.defaultValue false

                if offsideActive then
                    Some AnnulGoal
                else
                    let scorerId, isOwnGoal = GoalDetector.scorer scoringClub state.Ball ctx state
                    Some(ConfirmGoal(scoringClub, scorerId, isOwnGoal)))
            |> Option.toList

        let throwInIntent =
            ballOutOfBounds ctx state |> Option.map AwardThrowIn |> Option.toList

        let cardIntent =
            def
            |> Option.filter (fun d ->
                let defClub =
                    if
                        state.HomeSlots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = d.Id
                            | _ -> false)
                    then
                        HomeClub
                    else
                        AwayClub

                Stats.bernoulli (cardProbability defClub d))
            |> Option.map (fun d ->
                IssueYellow(
                    d,
                    if
                        state.HomeSlots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = d.Id
                            | _ -> false)
                    then
                        ctx.Home.Id
                    else
                        ctx.Away.Id
                ))
            |> Option.toList

        let injuryIntent =
            att
            |> Option.filter (fun a -> Stats.bernoulli (injuryProbability a))
            |> Option.map (fun a ->
                IssueInjury(
                    a,
                    if
                        state.HomeSlots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = a.Id
                            | _ -> false)
                    then
                        ctx.Home.Id
                    else
                        ctx.Away.Id
                ))
            |> Option.toList

        goalIntent @ throwInIntent @ cardIntent @ injuryIntent

    let resolve (subTick: int) (action: RefereeAction) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        match action with
        | RefereeIdle -> []

        | ConfirmGoal(scoringClub, scorerId, isOwnGoal) ->
            let goalEvents = awardGoal scoringClub scorerId subTick ctx state
            clearOffsideSnapshot state

            if isOwnGoal then
                goalEvents |> List.map (fun e -> { e with Type = OwnGoal })
            else
                goalEvents

        | AnnulGoal ->
            let resetX =
                match state.PendingOffsideSnapshot with
                | Some snap -> snap.BallXAtPass
                | None -> PhysicsContract.HalfwayLineX

            state.Ball <-
                { state.Ball with
                    Position = defaultSpatial resetX (PhysicsContract.PitchWidth / 2.0)
                    Spin = Spin.zero
                    LastTouchBy = None
                    IsInPlay = true }

            clearOffsideSnapshot state
            []

        | AwardThrowIn team ->
            let throwX =
                match team with
                | HomeClub -> PhysicsContract.PenaltyAreaDepth
                | AwayClub -> PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth

            state.AttackingClub <- team

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = throwX
                            Y = PhysicsContract.PitchWidth / 2.0
                            Z = 0.0
                            Vx = 0.0
                            Vy = 0.0
                            Vz = 0.0 }
                    Spin = Spin.zero
                    LastTouchBy = None
                    IsInPlay = true }

            clearOffsideSnapshot state
            []

        | AwardCorner team ->
            let cornerX =
                match team with
                | HomeClub -> PhysicsContract.PitchLength - 0.5
                | AwayClub -> 0.5

            state.AttackingClub <- team

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = cornerX
                            Y = PhysicsContract.PitchWidth / 2.0
                            Z = 0.0
                            Vx = 0.0
                            Vy = 0.0
                            Vz = 0.0 }
                    Spin = Spin.zero
                    LastTouchBy = None
                    IsInPlay = true }

            clearOffsideSnapshot state
            []

        | IssueYellow(player, clubId) ->
            let isHome = clubId = ctx.Home.Id

            let count =
                (if isHome then state.HomeYellows else state.AwayYellows)
                |> Map.tryFind player.Id
                |> Option.defaultValue 0

            if count >= 1 then
                if isHome then
                    state.HomeYellows <- Map.add player.Id (count + 1) state.HomeYellows
                    state.HomeSidelined <- Map.add player.Id SidelinedByRedCard state.HomeSidelined
                else
                    state.AwayYellows <- Map.add player.Id (count + 1) state.AwayYellows
                    state.AwaySidelined <- Map.add player.Id SidelinedByRedCard state.AwaySidelined

                [ event subTick player.Id clubId YellowCard
                  event subTick player.Id clubId RedCard ]
            else
                if isHome then
                    state.HomeYellows <- Map.add player.Id (count + 1) state.HomeYellows
                else
                    state.AwayYellows <- Map.add player.Id (count + 1) state.AwayYellows

                [ event subTick player.Id clubId YellowCard ]

        | IssueRed(player, clubId) ->
            let isHome = clubId = ctx.Home.Id

            if isHome then
                state.HomeSidelined <- Map.add player.Id SidelinedByRedCard state.HomeSidelined
            else
                state.AwaySidelined <- Map.add player.Id SidelinedByRedCard state.AwaySidelined

            [ event subTick player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            let isHome = clubId = ctx.Home.Id

            if isHome then
                state.HomeSidelined <- Map.add player.Id SidelinedByInjury state.HomeSidelined
            else
                state.AwaySidelined <- Map.add player.Id SidelinedByInjury state.AwaySidelined

            [ event subTick player.Id clubId (MatchEventType.Injury "match") ]

    let agent homeId homeSquad awaySquad tick ctx (state: SimState) : AgentOutput =
        match tick.Kind with
        | HalfTimeTick ->
            state.HomeAttackDir <- RightToLeft

            state.HomeSlots <-
                state.HomeSlots
                |> Array.map (function
                    | PlayerSlot.Active s -> PlayerSlot.Active { s with Pos = mirrorSpatial s.Pos }
                    | Sidelined(p, r) -> Sidelined(p, r))

            state.AwaySlots <-
                state.AwaySlots
                |> Array.map (function
                    | PlayerSlot.Active s -> PlayerSlot.Active { s with Pos = mirrorSpatial s.Pos }
                    | Sidelined(p, r) -> Sidelined(p, r))

            { Events = []
              Spawned =
                [ { SubTick = tick.SubTick + PhysicsContract.secondsToSubTicks 1.0
                    Priority = TickPriority.SetPiece
                    SequenceId = 0L
                    Kind = KickOffTick } ]
              Transition = Some(SetPiece KickOff) }
        | FullTimeTick
        | MatchEndTick
        | ExtraTimeTick _ ->
            { Events = []
              Spawned = []
              Transition = None }

        | InjuryTick(playerId, _severity) ->
            let player =
                seq {
                    for slot in state.HomeSlots do
                        match slot with
                        | PlayerSlot.Active s -> yield s.Player
                        | _ -> ()

                    for slot in state.AwaySlots do
                        match slot with
                        | PlayerSlot.Active s -> yield s.Player
                        | _ -> ()
                }
                |> Seq.tryFind (fun p -> p.Id = playerId)

            let events =
                match player with
                | Some p ->
                    resolve
                        tick.SubTick
                        (IssueInjury(
                            p,
                            if
                                state.HomeSlots
                                |> Array.exists (function
                                    | PlayerSlot.Active s -> s.Player.Id = p.Id
                                    | _ -> false)
                            then
                                ctx.Home.Id
                            else
                                ctx.Away.Id
                        ))
                        ctx
                        state
                | None -> []

            { Events = events
              Spawned =
                [ { SubTick = tick.SubTick + Stats.delayFrom BalanceConfig.injuryDelay
                    Priority = TickPriority.MatchControl
                    SequenceId = 0L
                    Kind = ResumePlayTick } ]
              Transition = Some(Stopped Injury) }

        | ResumePlayTick ->
            { Events = []
              Spawned = []
              Transition = Some LivePlay }

        | _ ->
            { Events = []
              Spawned = []
              Transition = None }

    let runRefereeStep subTick (att: Player option) (def: Player option) ctx state =
        let actions = decide subTick att def ctx state

        let evs =
            actions
            |> List.fold
                (fun accEvents action ->
                    let evs = resolve subTick action ctx state
                    evs @ accEvents)
                []
            |> List.rev

        evs, actions
