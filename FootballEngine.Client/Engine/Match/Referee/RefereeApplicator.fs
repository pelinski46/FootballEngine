namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open PhysicsContract

module RefereeApplicator =

    let private awardGoal (scoringClub: ClubSide) (scorerId: PlayerId option) (subTick: int) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        state.StoppageTime.Add(subTick, StoppageReason.GoalDelay) |> ignore

        if scoringClub = HomeClub then
            state.HomeScore <- state.HomeScore + 1
            state.Momentum <- clampFloat (state.Momentum + 3.0) -10.0 10.0
        else
            state.AwayScore <- state.AwayScore + 1
            state.Momentum <- clampFloat (state.Momentum - 3.0) -10.0 10.0

        resetBallForKickOff (ClubSide.flip scoringClub) state

        let clubId = if scoringClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        match scorerId with
        | Some pid ->
            [ { SubTick = subTick
                PlayerId = pid
                ClubId = clubId
                Type = Goal
                Context = EventContext.empty } ]
        | None -> []

    let apply (subTick: int) (action: RefereeAction) (ctx: MatchContext) (state: SimState) : MatchEvent list =
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
                | None -> HalfwayLineX

            state.Ball <-
                { state.Ball with
                    Position = defaultSpatial resetX (PitchWidth / 2.0)
                    Spin = Spin.zero
                    LastTouchBy = None
                    Control = Free }

            clearOffsideSnapshot state
            []

        | AwardThrowIn team ->
            let throwX =
                match team with
                | HomeClub -> PenaltyAreaDepth
                | AwayClub -> PitchLength - PenaltyAreaDepth

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = throwX
                            Y = PitchWidth / 2.0
                            Z = 0.0<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    LastTouchBy = None
                    Control = Free }

            clearOffsideSnapshot state
            []

        | AwardCorner team ->
            let cornerX =
                match team with
                | HomeClub -> PitchLength - 0.5<meter>
                | AwayClub -> 0.5<meter>

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = cornerX
                            Y = PitchWidth / 2.0
                            Z = 0.0<meter>
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    LastTouchBy = None
                    Control = Free }

            clearOffsideSnapshot state

            [ { SubTick = subTick
                PlayerId = 0
                ClubId = if team = HomeClub then ctx.Home.Id else ctx.Away.Id
                Type = MatchEventType.Corner
                Context = EventContext.empty } ]

        | AwardGoalKick team ->
            let gkX =
                match team with
                | HomeClub -> GoalAreaDepth
                | AwayClub -> PitchLength - GoalAreaDepth

            state.Ball <-
                { state.Ball with
                    Position = defaultSpatial gkX (PitchWidth / 2.0)
                    Spin = Spin.zero
                    LastTouchBy = None
                    Control = Free }

            clearOffsideSnapshot state
            []

        | AwardIndirectFreeKick team ->
            let ballX = state.Ball.Position.X
            let ballY = state.Ball.Position.Y

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Spin = Spin.zero
                    LastTouchBy = None
                    Control = Free }

            clearOffsideSnapshot state

            let clubId = if team = HomeClub then ctx.Home.Id else ctx.Away.Id

            [ { SubTick = subTick
                PlayerId = 0
                ClubId = clubId
                Type = MatchEventType.IndirectFreeKickAwarded team
                Context = EventContext.empty } ]

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
                    Control = Free }

            clearOffsideSnapshot state
            []

        | IssueYellow(player, clubId) ->
            state.StoppageTime.Add(subTick, StoppageReason.CardDelay) |> ignore
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
            state.StoppageTime.Add(subTick, StoppageReason.CardDelay) |> ignore
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            setSidelined state side (Map.add player.Id SidelinedByRedCard (getSidelined state side))

            [ createEvent subTick player.Id clubId RedCard ]

        | IssueInjury(player, clubId) ->
            state.StoppageTime.Add(subTick, StoppageReason.InjuryDelay 1) |> ignore
            let isHome = clubId = ctx.Home.Id
            let side = if isHome then HomeClub else AwayClub

            setSidelined state side (Map.add player.Id SidelinedByInjury (getSidelined state side))

            [ createEvent subTick player.Id clubId (MatchEventType.Injury "match") ]
