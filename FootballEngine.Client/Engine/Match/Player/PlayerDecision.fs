namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes
open PlayerPersonality

module PlayerDecision =

    let private canCross (profile: BehavioralProfile) (zone: PitchZone) =
        abs (profile.LateralTendency - 0.5) > 0.3 && zone = AttackingZone

    let decide (ctx: AgentContext) (scores: ActionScores) : OnBallIntent option =
        let t = ctx.Tactics

        let shootBlocked = scores.Shoot < 0.55<decisionScore> || ctx.Zone = DefensiveZone

        let crossBlocked = not (canCross ctx.Profile ctx.Zone)

        let passThreshold = 0.20<decisionScore> - t.Directness * 0.08<decisionScore>

        let passBlocked =
            match ctx.BestPassTargetIdx with
            | ValueNone -> true
            | ValueSome _ -> scores.Pass < passThreshold

        let dribbleBlocked =
            ctx.Zone = DefensiveZone && ctx.Profile.Directness < 0.2
            || t.Tempo > 0.7 && ctx.Profile.Directness < 0.4

        let passMod =
            if ctx.Phase = BuildUp then
                1.0 + ctx.Profile.CreativityWeight * 0.15
            else
                1.0

        // Check for through ball / space pass opportunity
        let spacePassCandidate =
            match SpaceScorer.findThroughBallTarget ctx with
            | ValueNone -> None
            | ValueSome spaceTarget ->
                let spaceScore = SpaceScorer.spacePassScore ctx spaceTarget.Cell
                // Only consider if space pass score is competitive with regular pass
                if float spaceScore > 0.25 then
                    Some(float spaceScore, OnBallIntent.PassIntoSpace spaceTarget.Cell)
                else
                    None

        let personality = derive ctx.Team.OwnRoster.Players[0]

        let candidates: (float * OnBallIntent) list =
            [ if not shootBlocked then
                  let flairBonus = personality.Flair * 0.2
                  let s = float scores.Shoot * (1.0 + ctx.Urgency * 0.5 + flairBonus)
                  yield s, OnBallIntent.Shoot

              if not passBlocked then
                  let teamworkBonus = personality.Teamwork * 0.15
                  let s = float scores.Pass * passMod * (1.0 + teamworkBonus)

                  match ctx.BestPassTargetIdx with
                  | ValueSome idx ->
                      let targetPid = ctx.Team.OwnRoster.Players[idx].Id
                      yield s, OnBallIntent.Pass targetPid
                  | ValueNone -> ()

              // Space pass candidate (through balls)
              match spacePassCandidate with
              | Some(s, intent) -> yield s, intent
              | None -> ()

              if not dribbleBlocked then
                  let consistencyMod = 1.0 - personality.Consistency * 0.1
                  let s = float scores.Dribble * consistencyMod
                  yield s, OnBallIntent.Dribble

              if not crossBlocked then
                  let s = float scores.Cross * (1.0 + ctx.Urgency * 0.1)
                  yield s, OnBallIntent.Cross

              match ctx.BestPassTargetIdx with
              | ValueSome idx ->
                  let longBallThreshold = 0.15<decisionScore> - t.Directness * 0.06<decisionScore>
                  let s = float scores.LongBall * (1.0 + ctx.Urgency * 0.15)

                  if s > float longBallThreshold then
                      let targetPid = ctx.Team.OwnRoster.Players[idx].Id
                      yield s, OnBallIntent.LongBall targetPid
              | ValueNone -> () ]

        let effectiveTemp =
            ctx.Decision.DecisionTemperature
            * (2.0 - ctx.MentalState.ComposureLevel)
            * (1.0 + ctx.Urgency * 0.5)
            * (2.0 - ctx.MentalState.FocusLevel)

        ActionMath.softmaxSample effectiveTemp candidates
