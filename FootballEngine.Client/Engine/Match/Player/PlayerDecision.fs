namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

module PlayerDecision =

    let private canCross (profile: BehavioralProfile) (zone: PitchZone) =
        abs (profile.LateralTendency - 0.5) > 0.3 && zone = AttackingZone

    let decide (ctx: AgentContext) (scores: ActionScores) : OnBallIntent option =
        let shootBlocked =
            scores.Shoot < 0.15<decisionScore> || ctx.Zone = DefensiveZone

        let crossBlocked =
            not (canCross ctx.Profile ctx.Zone)

        let passBlocked =
            match ctx.BestPassTargetIdx with
            | ValueNone -> true
            | ValueSome _ -> scores.Pass < 0.2<decisionScore>

        let dribbleBlocked =
            ctx.Zone = DefensiveZone && ctx.Profile.Directness < 0.2

        let passMod = if ctx.Phase = BuildUp then 1.0 + ctx.Profile.CreativityWeight * 0.15 else 1.0

        let mutable bestScore = -1.0<decisionScore>
        let mutable bestIntent: OnBallIntent option = None

        if not shootBlocked then
            let s = scores.Shoot * (1.0 + ctx.Urgency * 0.5)
            if s > bestScore then bestScore <- s; bestIntent <- Some OnBallIntent.Shoot

        if not passBlocked then
            let s = scores.Pass * passMod
            let targetPid = match ctx.BestPassTargetIdx with ValueSome idx -> ctx.Team.OwnRoster.Players[idx].Id | ValueNone -> failwith "unreachable"
            if s > bestScore then bestScore <- s; bestIntent <- Some (OnBallIntent.Pass targetPid)

        if not dribbleBlocked then
            let s = scores.Dribble
            if s > bestScore then bestScore <- s; bestIntent <- Some OnBallIntent.Dribble

        if not crossBlocked then
            let s = scores.Cross * (1.0 + ctx.Urgency * 0.1)
            if s > bestScore then bestScore <- s; bestIntent <- Some OnBallIntent.Cross

        match ctx.BestPassTargetIdx with
        | ValueSome idx ->
            let s = scores.LongBall * (1.0 + ctx.Urgency * 0.15)
            let targetPid = ctx.Team.OwnRoster.Players[idx].Id
            if s > bestScore then bestScore <- s; bestIntent <- Some (OnBallIntent.LongBall targetPid)
        | ValueNone -> ()

        bestIntent
