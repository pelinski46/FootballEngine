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

    let private defaultIntent : PlayerIntent =
        { Movement = MovementIntent.MaintainShape { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
          Action = None
          Context = NormalPlay
          Urgency = 0.0
          Confidence = 0.5 }

    let agent tick ctx (state: SimState) (clock: SimulationClock) : RefereeResult =
        match tick.Kind with
        | HalfTimeTick ->
            { NextTick = Some { SubTick = tick.SubTick + TickDelay.delayFrom clock state.Config.Timing.KickOffDelay; Priority = TickPriority.MatchControl; Kind = KickOffTick }
              Actions = []
              Transition = Some(SetPiece SetPieceKind.KickOff) }

        | FullTimeTick
        | MatchEndTick ->
            { NextTick = None; Actions = []; Transition = None }

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

            let actions =
                match player with
                | Some (p, pSide) ->
                    let pClubId = if pSide = HomeClub then ctx.Home.Id else ctx.Away.Id
                    [ IssueInjury(p, pClubId) ]
                | None -> []

            { NextTick = Some { SubTick = tick.SubTick + TickDelay.delayFrom clock state.Config.Timing.InjuryDelay; Priority = TickPriority.MatchControl; Kind = ResumePlayTick }
              Actions = actions
              Transition = Some(Stopped Injury) }

        | ResumePlayTick ->
            { NextTick = None; Actions = []; Transition = Some LivePlay }

        | RefereeTick ->
            let refActions = decide None None ctx state

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

            { NextTick = nextTick; Actions = refActions; Transition = transition }

        | _ ->
            { NextTick = None; Actions = []; Transition = None }

    let runRefereeStep subTick (att: Player option) (def: Player option) ctx state =
        let actions = decide att def ctx state

        let evs =
            actions
            |> List.fold
                (fun accEvents action ->
                    let evs = RefereeApplicator.apply subTick action ctx state
                    evs @ accEvents)
                []
            |> List.rev

        evs, actions
