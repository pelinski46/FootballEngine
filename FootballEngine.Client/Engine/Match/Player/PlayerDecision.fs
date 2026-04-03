namespace FootballEngine

open FootballEngine.Domain

module PlayerDecision =

    let private isFlankPosition =
        function
        | DR | DL | WBR | WBL | MR | ML | AMR | AML -> true
        | _ -> false

    let decide (ctx: AgentContext) (scores: ActionScores) : PlayerAction =
        let shootBlocked =
            scores.Shoot < 0.15 || ctx.Zone = DefensiveZone

        let crossBlocked =
            not (isFlankPosition ctx.Me.Position)

        let passBlocked =
            ctx.BestPassTarget.IsNone

        let dribbleBlocked =
            ctx.Zone = DefensiveZone && (ctx.Me.Position = DC || ctx.Me.Position = GK)

        let candidates =
            [ if not shootBlocked then scores.Shoot * (1.0 + ctx.Urgency * 0.2), PlayerAction.Shoot
              if not passBlocked then scores.Pass, PlayerAction.Pass (ctx.BestPassTarget |> Option.map fst |> Option.defaultValue ctx.Me)
              if not dribbleBlocked then scores.Dribble, PlayerAction.Dribble
              if not crossBlocked then scores.Cross * (1.0 + ctx.Urgency * 0.1), PlayerAction.Cross
              scores.LongBall * (1.0 + ctx.Urgency * 0.15), PlayerAction.LongBall ]

        match candidates |> List.sortByDescending fst |> List.tryHead with
        | Some(_, action) -> action
        | None -> Idle
