namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps
open SchedulingTypes

module ActionResolver =

    let private bothSides = [| HomeClub; AwayClub |]

    let private findController
        (ctx: MatchContext)
        (state: SimState)
        : Player option * PlayerRoster option * ClubSide option =
        match state.Ball.Possession with
        | Owned(_, pid) ->
            let mutable result: Player option * PlayerRoster option * ClubSide option =
                None, None, None

            for clubSide in bothSides do
                let frame = getFrame state clubSide
                let roster = getRoster ctx clubSide

                match SimStateOps.findIdxByPid pid frame roster with
                | ValueSome idx -> result <- Some roster.Players[idx], Some roster, Some clubSide
                | ValueNone -> ()

            result
        | _ ->
            let bx = state.Ball.Position.X
            let by = state.Ball.Position.Y
            let mutable bestDist = System.Double.PositiveInfinity * 1.0<meter^2>
            let mutable bestPlayer: Player option = None
            let mutable bestRoster: PlayerRoster option = None
            let mutable bestClub: ClubSide option = None

            for clubSide in bothSides do
                let frame = getFrame state clubSide
                let roster = getRoster ctx clubSide

                for i = 0 to frame.SlotCount - 1 do
                    match frame.Occupancy[i] with
                    | OccupancyKind.Active _ ->
                        let px = float frame.PosX[i] * 1.0<meter>
                        let py = float frame.PosY[i] * 1.0<meter>
                        let dx = px - bx
                        let dy = py - by
                        let distSq = dx * dx + dy * dy

                        if distSq < bestDist then
                            bestDist <- distSq
                            bestPlayer <- Some roster.Players[i]
                            bestRoster <- Some roster
                            bestClub <- Some clubSide
                    | _ -> ()

            bestPlayer, bestRoster, bestClub

    let private decideAction
        (controller: Player)
        (profile: BehavioralProfile)
        (meIdx: int)
        (clubSide: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : OnBallIntent option =
        let team = SimStateOps.buildTeamPerspective clubSide ctx state
        let cachedIntent = SimStateOps.getTeamIntent state clubSide

        let teamIntent =
            match cachedIntent with
            | Some ti -> ti
            | None -> TeamIntentModule.empty clubSide

        let influence =
            if clubSide = HomeClub then state.HomeInfluenceFrame
            else state.AwayInfluenceFrame

        let actx =
            AgentContext.build
                controller
                profile
                meIdx
                team
                teamIntent
                ValueNone
                0
                state
                clock
                ctx
                state.Config.Decision
                state.Config.BuildUp
                None
                ValueNone
                influence

        let scores = PlayerScorer.computeAll actx state.MatchMemory
        PlayerDecision.decide actx scores

    let private resolveIntent
        (subTick: int)
        (intent: OnBallIntent)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : ActionResult * RefereeAction list =
        match intent with
        | OnBallIntent.Shoot ->
            ShotAction.resolve subTick ctx state clock
            |> fun events -> ActionResult.ofEvents events, []

        | OnBallIntent.Pass targetPid ->
            match SimStateOps.findActivePlayer ctx state targetPid with
            | Some target -> ActionResult.ofEvents (PassAction.resolve subTick ctx state clock target), []
            | None -> ActionResult.empty, []

        | OnBallIntent.Dribble ->
            let events, actions = DuelAction.resolve subTick ctx state clock
            ActionResult.ofEvents events, actions

        | OnBallIntent.Cross ->
            let result = CrossAction.resolve subTick ctx state clock
            result, []

        | OnBallIntent.LongBall _ -> ActionResult.ofEvents (PassAction.resolveLong subTick ctx state clock), []

        | OnBallIntent.PassIntoSpace targetCell ->
            ActionResult.ofEvents (PassAction.resolveIntoSpace subTick ctx state clock targetCell), []

        | OnBallIntent.Tackle oppPid ->
            match SimStateOps.findActivePlayer ctx state oppPid with
            | Some opponent ->
                let events, actions = DuelAction.resolveTackle subTick ctx state opponent
                ActionResult.ofEvents events, actions
            | None -> ActionResult.empty, []

    let run (subTick: int) (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : ActionResult =
        let controller, rosterOpt, controllerClubSide = findController ctx state

        let actionResult, actionRefereeActions =
            match controller, rosterOpt, controllerClubSide with
            | Some ctrl, Some roster, Some clubSide ->
                if ctrl.Position = GK then
                    match state.Ball.Possession with
                    | Owned(side, gkId) when gkId = ctrl.Id ->
                        let events = GKAction.resolve subTick ctx state clock
                        ActionResult.ofEvents events, []
                    | _ ->
                        let meIdx = roster.Players |> Array.findIndex (fun p -> p.Id = ctrl.Id)
                        let profile = roster.Profiles[meIdx]
                        match decideAction ctrl profile meIdx clubSide ctx state clock with
                        | Some action -> resolveIntent subTick action ctx state clock
                        | None -> ActionResult.empty, []
                else
                    let meIdx = roster.Players |> Array.findIndex (fun p -> p.Id = ctrl.Id)
                    let profile = roster.Profiles[meIdx]

                    match decideAction ctrl profile meIdx clubSide ctx state clock with
                    | Some action -> resolveIntent subTick action ctx state clock
                    | None ->
                        match state.Ball.Possession with
                        | Owned _ ->
                            let holdTimeout =
                                match state.Ball.PlayerHoldSinceSubTick with
                                | Some since -> subTick - since >= 12
                                | None -> true
                            if holdTimeout then
                                SimStateOps.loosePossession state
                                ActionResult.empty, []
                            else
                                ActionResult.empty, []
                        | _ -> ActionResult.empty, []
            | _ -> ActionResult.empty, []

        let defSide =
            match controllerClubSide with
            | Some cs -> ClubSide.flip cs
            | None -> ClubSide.flip state.AttackingSide

        let defFrame = getFrame state defSide
        let defRoster = getRoster ctx defSide

        let bx, by = state.Ball.Position.X, state.Ball.Position.Y

        let defOpt =
            match SimStateOps.nearestActiveSlotInFrame defFrame bx by with
            | ValueSome idx -> Some defRoster.Players[idx]
            | ValueNone -> None

        let playerHasBall =
            match controller with
            | Some ctrl ->
                match state.Ball.Possession with
                | Owned(_, pid) -> pid = ctrl.Id
                | _ -> false
            | None -> false

        let attOpt = if playerHasBall then controller else None
        let refActions = RefereeAgent.decide attOpt defOpt ctx state

        let allRefereeActions = actionRefereeActions @ refActions

        let refereeEvents =
            allRefereeActions
            |> List.collect (fun a -> RefereeApplicator.apply subTick a ctx state)

        let combined =
            { Events = actionResult.Events @ refereeEvents
              PendingGoal = actionResult.PendingGoal }

        match combined.PendingGoal with
        | Some pg ->
            let scorerId, isOwnGoal = GoalDetector.scorer pg.ScoringClub state.Ball ctx state
            let confirmAction = ConfirmGoal(pg.ScoringClub, scorerId, isOwnGoal)
            let goalEvents = RefereeApplicator.apply subTick confirmAction ctx state

            { combined with
                Events = combined.Events @ goalEvents }
        | None -> combined
