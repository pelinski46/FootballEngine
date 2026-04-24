namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps
open SchedulingTypes

module ActionResolver =

    let private bothSides = [| HomeClub; AwayClub |]

    let private findController (ctx: MatchContext) (state: SimState) : Player option * PlayerRoster option * ClubSide option =
        match state.Ball.Possession with
        | Owned(_, pid) ->
            let mutable result: Player option * PlayerRoster option * ClubSide option = None, None, None
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
        let actx =
            AgentContext.build
                controller profile meIdx team teamIntent
                None 0 state clock state.Config.Decision state.Config.BuildUp
                None
                None

        let scores = PlayerScorer.computeAll actx
        PlayerDecision.decide actx scores

    let private resolveIntent
        (subTick: int)
        (intent: OnBallIntent)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : MatchEvent list =
        match intent with
        | OnBallIntent.Shoot ->
            ShotAction.resolve subTick ctx state clock

        | OnBallIntent.Pass targetPid ->
            match SimStateOps.findActivePlayer ctx state targetPid with
            | Some target -> PassAction.resolve subTick ctx state target
            | None -> []

        | OnBallIntent.Dribble ->
            DuelAction.resolve subTick ctx state clock

        | OnBallIntent.Cross ->
            CrossAction.resolve subTick ctx state

        | OnBallIntent.LongBall _ ->
            PassAction.resolveLong subTick ctx state

        | OnBallIntent.Tackle oppPid ->
            match SimStateOps.findActivePlayer ctx state oppPid with
            | Some opponent -> DuelAction.resolveTackle subTick ctx state opponent
            | None -> []

    let run
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        (clock: SimulationClock)
        : MatchEvent list =
        let controller, rosterOpt, controllerClubSide = findController ctx state

        let playerEvents =
            match controller, rosterOpt, controllerClubSide with
            | Some ctrl, Some roster, Some clubSide ->
                let meIdx = roster.Players |> Array.findIndex (fun p -> p.Id = ctrl.Id)
                let profile = roster.Profiles[meIdx]
                match decideAction ctrl profile meIdx clubSide ctx state clock with
                | Some action -> resolveIntent subTick action ctx state clock
                | None ->
                    match state.Ball.Possession with
                    | Owned _ ->
                        SimStateOps.loosePossession state
                        []
                    | _ -> []
            | _ -> []

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
        let refEvents, _ = RefereeAgent.runRefereeStep subTick attOpt defOpt ctx state

        EventPipeline.run (playerEvents @ refEvents) ctx state subTick
