namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open SchedulingTypes
open Stats
open PhysicsContract

module RefereeAgent =

    let cardProbability (config: HomeAdvantageConfig) (playerClub: ClubSide) (p: Player) =
        let baseProb = 0.010 + float p.Mental.Aggression * 0.0004

        match playerClub with
        | HomeClub -> baseProb * (1.0 - config.CardReduction)
        | AwayClub -> baseProb

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    type BallOutResult =
        | NoOut
        | ThrowIn of ClubSide
        | Corner of ClubSide
        | GoalKick of ClubSide

    let private ballOutOfBounds (ctx: MatchContext) (state: SimState) : BallOutResult =
        let pos = state.Ball.Position
        let outY = pos.Y < 0.5<meter> || pos.Y > PhysicsContract.PitchWidth - 0.5<meter>
        let outX = pos.X < 0.5<meter> || pos.X > PhysicsContract.PitchLength - 0.5<meter>

        let lastTouchClubSide () =
            state.Ball.LastTouchBy
            |> Option.bind (fun pid ->
                if playerOnSide ctx state HomeClub pid then
                    Some HomeClub
                elif playerOnSide ctx state AwayClub pid then
                    Some AwayClub
                else
                    None)

        if outY then
            match lastTouchClubSide () with
            | Some side -> ThrowIn(ClubSide.flip side)
            | None -> NoOut
        elif outX then
            let behindHome = pos.X < 0.5<meter>
            let behindAway = pos.X > PhysicsContract.PitchLength - 0.5<meter>

            let inGoalY =
                pos.Y >= PhysicsContract.PostNearY && pos.Y <= PhysicsContract.PostFarY

            if inGoalY then
                NoOut
            elif behindHome then
                match lastTouchClubSide () with
                | Some AwayClub -> Corner HomeClub
                | _ -> GoalKick HomeClub
            elif behindAway then
                match lastTouchClubSide () with
                | Some HomeClub -> Corner AwayClub
                | _ -> GoalKick AwayClub
            else
                NoOut
        else
            NoOut

    let decide (att: Player option) (def: Player option) (ctx: MatchContext) (state: SimState) : RefereeAction list =
        let goalIntent =
            GoalDetector.detect state.Ball
            |> Option.bind (fun scoringClub ->
                let offsideActive =
                    state.PendingOffsideSnapshot
                    |> Option.map isOffsideFromSnapshot
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
            | GoalKick team -> [ AwardGoalKick team ]
            | NoOut -> []

        let injuryIntent =
            att
            |> Option.filter (fun a -> bernoulli (injuryProbability a))
            |> Option.map (fun a ->
                IssueInjury(
                    a,
                    if playerOnSide ctx state HomeClub a.Id then
                        ctx.Home.Id
                    else
                        ctx.Away.Id
                ))
            |> Option.toList

        let stuckBallIntent =
            match state.Ball.StationarySinceSubTick with
            | Some since when state.SubTick - since >= state.Config.Timing.StuckBallDelay -> [ DropBall state.AttackingSide ]
            | _ -> []

        goalIntent @ throwInIntent @ injuryIntent @ stuckBallIntent

    let decideCard (fouler: Player) (ctx: MatchContext) (state: SimState) : RefereeAction list =
        let aggressionNorm = PhysicsContract.normaliseAttr fouler.Mental.Aggression

        let isHome = playerOnSide ctx state HomeClub fouler.Id

        let clubId = if isHome then ctx.Home.Id else ctx.Away.Id
        let yellows = getYellows state (if isHome then HomeClub else AwayClub)
        let currentYellows = yellows |> Map.tryFind fouler.Id |> Option.defaultValue 0

        let isSidelined =
            getSidelined state (if isHome then HomeClub else AwayClub)
            |> Map.containsKey fouler.Id

        if isSidelined then
            []
        elif currentYellows >= 1 then
            [ IssueRed(fouler, clubId) ]
        elif bernoulli (0.25 + aggressionNorm * 0.35) then
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

                    Possession = Loose }

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
                            Z = 0.0<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    LastTouchBy = None

                    Possession = Possession.SetPiece(team, SetPieceKind.ThrowIn) }

            clearOffsideSnapshot state
            []

        | AwardCorner team ->
            let cornerX =
                match team with
                | HomeClub -> PhysicsContract.PitchLength - 0.5<meter>
                | AwayClub -> 0.5<meter>

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = cornerX
                            Y = PhysicsContract.PitchWidth / 2.0
                            Z = 0.0<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    LastTouchBy = None

                    Possession = Possession.SetPiece(team, SetPieceKind.Corner) }

            clearOffsideSnapshot state

            [ { SubTick = subTick
                PlayerId = 0
                ClubId = if team = HomeClub then ctx.Home.Id else ctx.Away.Id
                Type = MatchEventType.Corner } ]

        | AwardGoalKick team ->
            let gkX =
                match team with
                | HomeClub -> PhysicsContract.GoalAreaDepth
                | AwayClub -> PhysicsContract.PitchLength - PhysicsContract.GoalAreaDepth

            state.Ball <-
                { state.Ball with
                    Position = defaultSpatial gkX (PhysicsContract.PitchWidth / 2.0)
                    Spin = Spin.zero
                    LastTouchBy = None

                    Possession = Possession.SetPiece(team, SetPieceKind.GoalKick) }

            clearOffsideSnapshot state
            []

        | DropBall team ->
            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            Z = 0.0<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    Possession = Possession.Loose }

            clearOffsideSnapshot state
            []

        | IssueYellow(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            let count = getYellows state side |> Map.tryFind player.Id |> Option.defaultValue 0

            if count >= 1 then
                setYellows state side (Map.add player.Id (count + 1) (getYellows state side))
                setSidelined state side (Map.add player.Id SidelinedByRedCard (getSidelined state side))

                [ createEvent subTick player.Id clubId YellowCard
                  createEvent subTick player.Id clubId RedCard ]
            else
                setYellows state side (Map.add player.Id (count + 1) (getYellows state side))

                [ createEvent subTick player.Id clubId YellowCard ]

        | IssueRed(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            setSidelined state side (Map.add player.Id SidelinedByRedCard (getSidelined state side))

            [ createEvent subTick player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            setSidelined state side (Map.add player.Id SidelinedByInjury (getSidelined state side))

            [ createEvent subTick player.Id clubId (MatchEventType.Injury "match") ]

    let private defaultIntent : PlayerIntent =
        { Movement = MovementIntent.MaintainShape { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
          Action = None
          Context = NormalPlay
          Urgency = 0.0
          Confidence = 0.5 }

    let agent tick ctx (state: SimState) (clock: SimulationClock) : AgentResult =
        match tick.Kind with
        | HalfTimeTick ->
            state.HomeAttackDir <- RightToLeft

            let mirrorFrame (frame: TeamFrame) : unit =
                for i = 0 to frame.SlotCount - 1 do
                    match frame.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let x = float frame.PosX[i] * 1.0<meter>
                        let y = float frame.PosY[i] * 1.0<meter>
                        let mirrored = mirrorSpatial { X = x; Y = y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                        FrameMutate.setPos frame i mirrored.X mirrored.Y
                    | _ -> ()

            mirrorFrame state.Home.Frame
            mirrorFrame state.Away.Frame

            state.HomeBasePositions <- Array.map mirrorSpatial state.HomeBasePositions
            state.AwayBasePositions <- Array.map mirrorSpatial state.AwayBasePositions

            { Intent = defaultIntent
              NextTick = Some { SubTick = tick.SubTick + TickDelay.delayFrom clock state.Config.Timing.KickOffDelay; Priority = TickPriority.MatchControl; Kind = KickOffTick }
              Events = []
              Transition = Some(SetPiece SetPieceKind.KickOff) }

        | FullTimeTick
        | MatchEndTick ->
            { Intent = defaultIntent; NextTick = None; Events = []; Transition = None }

        | InjuryTick(playerId, _severity) ->
            let homeFrame = state.Home.Frame
            let homeRoster = getRoster ctx HomeClub
            let awayFrame = state.Away.Frame
            let awayRoster = getRoster ctx AwayClub

            let player =
                match tryFindPlayerByPidInFrame homeFrame homeRoster playerId with
                | Some p -> Some(p, HomeClub)
                | None ->
                    match tryFindPlayerByPidInFrame awayFrame awayRoster playerId with
                    | Some p -> Some(p, AwayClub)
                    | None -> None

            let events =
                match player with
                | Some (p, pSide) ->
                    let pClubId = if pSide = HomeClub then ctx.Home.Id else ctx.Away.Id
                    resolve tick.SubTick (IssueInjury(p, pClubId)) ctx state
                | None -> []

            { Intent = defaultIntent
              NextTick = Some { SubTick = tick.SubTick + TickDelay.delayFrom clock state.Config.Timing.InjuryDelay; Priority = TickPriority.MatchControl; Kind = ResumePlayTick }
              Events = events
              Transition = Some(Stopped Injury) }

        | ResumePlayTick ->
            { Intent = defaultIntent; NextTick = None; Events = []; Transition = Some LivePlay }

        | RefereeTick ->
            let refActions = decide None None ctx state

            let evs =
                refActions
                |> List.fold
                    (fun accEvents action ->
                        let evs = resolve tick.SubTick action ctx state
                        evs @ accEvents)
                    []
                |> List.rev

            let hasDropBall =
                refActions
                |> List.exists (function
                    | DropBall _ -> true
                    | _ -> false)

            let setpieceNextTick =
                refActions
                |> List.tryPick (function
                    | AwardThrowIn team -> Some { SubTick = tick.SubTick + 1; Priority = TickPriority.SetPiece; Kind = SetPieceTick(SetPieceKind.ThrowIn, team) }
                    | AwardCorner team -> Some { SubTick = tick.SubTick + 1; Priority = TickPriority.SetPiece; Kind = SetPieceTick(SetPieceKind.Corner, team) }
                    | AwardGoalKick team -> Some { SubTick = tick.SubTick + 1; Priority = TickPriority.SetPiece; Kind = SetPieceTick(SetPieceKind.GoalKick, team) }
                    | _ -> None)

            let nextTick =
                if hasDropBall then
                    None
                else
                    setpieceNextTick

            let transition =
                if hasDropBall then
                    Some LivePlay
                else
                    match setpieceNextTick with
                    | Some { Kind = SetPieceTick(SetPieceKind.ThrowIn, _) } -> Some(PlayState.SetPiece SetPieceKind.ThrowIn)
                    | Some { Kind = SetPieceTick(SetPieceKind.Corner, _) } -> Some(PlayState.SetPiece SetPieceKind.Corner)
                    | Some { Kind = SetPieceTick(SetPieceKind.GoalKick, _) } -> Some(PlayState.SetPiece SetPieceKind.GoalKick)
                    | _ -> None

            { Intent = defaultIntent; NextTick = nextTick; Events = evs; Transition = transition }

        | _ ->
            { Intent = defaultIntent; NextTick = None; Events = []; Transition = None }

    let runRefereeStep subTick (att: Player option) (def: Player option) ctx state =
        let actions = decide att def ctx state

        let evs =
            actions
            |> List.fold
                (fun accEvents action ->
                    let evs = resolve subTick action ctx state
                    evs @ accEvents)
                []
            |> List.rev

        evs, actions
