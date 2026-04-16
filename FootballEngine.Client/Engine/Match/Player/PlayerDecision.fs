namespace FootballEngine

open FootballEngine.Domain

type DecisionOutcome =
    | PlayerActing of PlayerAction
    | BallContested

module PlayerDecision =

    let private canCross (profile: BehavioralProfile) =
        abs (profile.LateralTendency - 0.5) > 0.3

    let decide (ctx: AgentContext) (scores: ActionScores) : DecisionOutcome =
        let shootBlocked =
            scores.Shoot < 0.15 || ctx.Zone = DefensiveZone

        let crossBlocked =
            not (canCross ctx.Profile)

        let passBlocked =
            ctx.BestPassTarget.IsNone

        let dribbleBlocked =
            ctx.Zone = DefensiveZone && ctx.Profile.Directness < 0.2
            || (ctx.Phase = BuildUp && ctx.Profile.CreativityWeight < 0.2)

        let passMod = if ctx.Phase = BuildUp then 1.0 + ctx.Profile.CreativityWeight * 0.15 else 1.0

        let candidates =
            [ if not shootBlocked then scores.Shoot * (1.0 + ctx.Urgency * 0.2), PlayerAction.Shoot
              if not passBlocked then scores.Pass * passMod, PlayerAction.Pass (ctx.BestPassTarget |> Option.map fst |> Option.defaultValue ctx.Me)
              if not dribbleBlocked then scores.Dribble * 0.8, PlayerAction.Dribble
              if not crossBlocked then scores.Cross * (1.0 + ctx.Urgency * 0.1), PlayerAction.Cross
              scores.LongBall * (1.0 + ctx.Urgency * 0.15), PlayerAction.LongBall ]

        match candidates |> List.sortByDescending fst |> List.tryHead with
        | Some(_, action) -> PlayerActing action
        | None -> BallContested
