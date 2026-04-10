namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open SchedulingTypes
open Stats

module RefereeAgent =

    let cardProbability (playerClub: ClubSide) (p: Player) =
        let baseProb = 0.010 + float p.Mental.Aggression * 0.0004

        match playerClub with
        | HomeClub -> baseProb * (1.0 - BalanceConfig.HomeCardReduction)
        | AwayClub -> baseProb

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    type BallOutResult =
        | NoOut
        | ThrowIn of ClubSide
        | Corner of ClubSide

    let private ballOutOfBounds (ctx: MatchContext) (state: SimState) : BallOutResult =
        let pos = state.Ball.Position
        let outY = pos.Y < 0.5 || pos.Y > PhysicsContract.PitchWidth - 0.5
        let outX = pos.X < 0.5 || pos.X > PhysicsContract.PitchLength - 0.5

        let lastTouchClubSide () =
            state.Ball.LastTouchBy
            |> Option.bind (fun pid ->
                if
                    state.Home.Slots
                    |> Array.exists (function
                        | PlayerSlot.Active s -> s.Player.Id = pid
                        | _ -> false)
                then
                    Some HomeClub
                elif
                    state.Away.Slots
                    |> Array.exists (function
                        | PlayerSlot.Active s -> s.Player.Id = pid
                        | _ -> false)
                then
                    Some AwayClub
                else
                    None)

        if outY then
            match lastTouchClubSide () with
            | Some side -> ThrowIn (ClubSide.flip side)
            | None -> NoOut
        elif outX then
            let behindHome = pos.X < 0.5
            let behindAway = pos.X > PhysicsContract.PitchLength - 0.5
            let inGoalY = pos.Y >= PhysicsContract.PostNearY && pos.Y <= PhysicsContract.PostFarY

            if inGoalY then NoOut
            elif behindHome then
                match lastTouchClubSide () with
                | Some AwayClub -> Corner HomeClub
                | _ -> NoOut
            elif behindAway then
                match lastTouchClubSide () with
                | Some HomeClub -> Corner AwayClub
                | _ -> NoOut
            else
                NoOut
        else
            NoOut

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
            match ballOutOfBounds ctx state with
            | ThrowIn team -> [ AwardThrowIn team ]
            | Corner team -> [ AwardCorner team ]
            | NoOut -> []

        let injuryIntent =
            att
            |> Option.filter (fun a -> Stats.bernoulli (injuryProbability a))
            |> Option.map (fun a ->
                IssueInjury(
                    a,
                    if
                        state.Home.Slots
                        |> Array.exists (function
                            | PlayerSlot.Active s -> s.Player.Id = a.Id
                            | _ -> false)
                    then
                        ctx.Home.Id
                    else
                        ctx.Away.Id
                ))
            |> Option.toList

        goalIntent @ throwInIntent @ injuryIntent

    let decideCard (subTick: int) (fouler: Player) (ctx: MatchContext) (state: SimState) : RefereeAction list =
        let aggressionNorm = PhysicsContract.normaliseAttr fouler.Mental.Aggression
        let isHome =
            state.Home.Slots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = fouler.Id
                | _ -> false)

        let clubSide = if isHome then HomeClub else AwayClub
        let clubId = if isHome then ctx.Home.Id else ctx.Away.Id
        let yellows = SimStateOps.getYellows state (if isHome then HomeClub else AwayClub)
        let currentYellows = yellows |> Map.tryFind fouler.Id |> Option.defaultValue 0

        let isSidelined =
            SimStateOps.getSidelined state (if isHome then HomeClub else AwayClub)
            |> Map.containsKey fouler.Id

        if isSidelined then
            []
        elif currentYellows >= 1 then
            [ IssueRed(fouler, clubId) ]
        elif Stats.bernoulli (0.25 + aggressionNorm * 0.35) then
            [ IssueYellow(fouler, clubId) ]
        else
            []

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
                    IsInPlay = true
                    Phase = PossessionPhase.Contest state.AttackingClub }

            clearOffsideSnapshot state
            []

        | AwardThrowIn team ->
            let throwX =
                match team with
                | HomeClub -> PhysicsContract.PenaltyAreaDepth
                | AwayClub -> PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth

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
                    IsInPlay = true
                    Phase = PossessionPhase.SetPiece team }

            clearOffsideSnapshot state
            []

        | AwardCorner team ->
            let cornerX =
                match team with
                | HomeClub -> PhysicsContract.PitchLength - 0.5
                | AwayClub -> 0.5

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
                    IsInPlay = true
                    Phase = PossessionPhase.SetPiece team }

            clearOffsideSnapshot state
            [ { SubTick = subTick
                PlayerId = 0
                ClubId = if team = HomeClub then ctx.Home.Id else ctx.Away.Id
                Type = MatchEventType.Corner } ]

        | IssueYellow(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            let count =
                SimStateOps.getYellows state side
                |> Map.tryFind player.Id
                |> Option.defaultValue 0

            if count >= 1 then
                SimStateOps.setYellows state side (Map.add player.Id (count + 1) (SimStateOps.getYellows state side))
                SimStateOps.setSidelined state side (Map.add player.Id SidelinedByRedCard (SimStateOps.getSidelined state side))

                [ createEvent subTick player.Id clubId YellowCard
                  createEvent subTick player.Id clubId RedCard ]
            else
                SimStateOps.setYellows state side (Map.add player.Id (count + 1) (SimStateOps.getYellows state side))

                [ createEvent subTick player.Id clubId YellowCard ]

        | IssueRed(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            SimStateOps.setSidelined state side (Map.add player.Id SidelinedByRedCard (SimStateOps.getSidelined state side))

            [ createEvent subTick player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            SimStateOps.setSidelined state side (Map.add player.Id SidelinedByInjury (SimStateOps.getSidelined state side))

            [ createEvent subTick player.Id clubId (MatchEventType.Injury "match") ]

    let agent homeId homeSquad awaySquad tick ctx (state: SimState) : AgentOutput =
        match tick.Kind with
        | HalfTimeTick ->
            state.HomeAttackDir <- RightToLeft

            SimStateOps.setSlots state HomeClub
                (state.Home.Slots
                |> Array.map (function
                    | PlayerSlot.Active s -> PlayerSlot.Active { s with Pos = mirrorSpatial s.Pos }
                    | Sidelined(p, r) -> Sidelined(p, r)))

            SimStateOps.setSlots state AwayClub
                (state.Away.Slots
                |> Array.map (function
                    | PlayerSlot.Active s -> PlayerSlot.Active { s with Pos = mirrorSpatial s.Pos }
                    | Sidelined(p, r) -> Sidelined(p, r)))

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
                    for slot in state.Home.Slots do
                        match slot with
                        | PlayerSlot.Active s -> yield s.Player
                        | _ -> ()

                    for slot in state.Away.Slots do
                        match slot with
                        | PlayerSlot.Active s -> yield s.Player
                        | _ -> ()
                }
                |> Seq.tryFind (fun p -> p.Id = playerId)

            let events =
                match player with
                | Some p ->
                    let pSide =
                        if
                            state.Home.Slots
                            |> Array.exists (function
                                | PlayerSlot.Active s -> s.Player.Id = p.Id
                                | _ -> false)
                        then
                            ctx.Home.Id
                        else
                            ctx.Away.Id

                    resolve tick.SubTick (IssueInjury(p, pSide)) ctx state
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
