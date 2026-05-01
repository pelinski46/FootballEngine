namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract
open SimStateOps

module VARApplicator =

    let applyOverturn (subTick: int) (incident: VARReviewableIncident) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        match incident with
        | GoalCheck(scoringClub, scorerId, isOwnGoal, goalSubTick) ->
            if scoringClub = HomeClub then
                state.HomeScore <- max 0 (state.HomeScore - 1)
            else
                state.AwayScore <- max 0 (state.AwayScore - 1)
            state.Ball <-
                { state.Ball with
                    Position = kickOffSpatial
                    Spin = Spin.zero
                    Possession = SetPiece(ClubSide.flip scoringClub, SetPieceKind.KickOff)
                    LastTouchBy = None
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
            let clubId = if scoringClub = HomeClub then ctx.Home.Id else ctx.Away.Id
            let pid = scorerId |> Option.defaultValue 0
            [ { SubTick = subTick; PlayerId = pid; ClubId = clubId; Type = MatchEventType.Goal; Context = EventContext.empty } ]

        | PenaltyCheck(awardedTo, fx, fy) ->
            state.Ball <-
                { state.Ball with
                    Position = { state.Ball.Position with Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    Spin = Spin.zero
                    LastTouchBy = None
                    Possession = Loose
                    PendingOffsideSnapshot = None
                    StationarySinceSubTick = None
                    GKHoldSinceSubTick = None
                    PlayerHoldSinceSubTick = None
                    Trajectory = None }
            []

        | RedCardCheck(playerId, _) ->
            let side = clubSideOf ctx state playerId |> Option.defaultValue HomeClub
            setSidelined state side (Map.add playerId SidelinedByRedCard (getSidelined state side))
            let clubId = if side = HomeClub then ctx.Home.Id else ctx.Away.Id
            [ { SubTick = subTick; PlayerId = playerId; ClubId = clubId; Type = MatchEventType.RedCard; Context = EventContext.empty } ]

        | OffsideCheck(_, _) ->
            clearOffsideSnapshot state
            state.Ball <- { state.Ball with Possession = Loose; PendingOffsideSnapshot = None }
            []

    let applyCheckComplete (subTick: int) (incident: VARReviewableIncident) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        []
