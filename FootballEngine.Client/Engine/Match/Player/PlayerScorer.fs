namespace FootballEngine

open System
open FootballEngine.Domain

[<Struct>]
type ActionScores =
    { Shoot: float
      Pass: float
      Dribble: float
      Cross: float
      LongBall: float }

module PlayerScorer =

    let private normStat (v: int) = float v / 20.0
    let private normCond (v: int) = float v / 100.0

    let private shootScore (ctx: AgentContext) =
        let me = ctx.Me
        let d = ctx.Decision
        let finishing = normStat me.Technical.Finishing * d.ShootFinishingWeight
        let longShots = normStat me.Technical.LongShots * d.ShootLongShotsWeight
        let composure = normStat me.Mental.Composure * d.ShootComposureWeight
        let distNorm = (1.0 - PhysicsContract.clampFloat (float ctx.DistToGoal / d.ShootDistNormDivisor) 0.0 1.0) * d.ShootDistNormWeight
        let posBonus = ctx.Profile.Directness * d.ShootPosDirectnessWeight + ctx.Profile.AttackingDepth * d.ShootPosDepthWeight + if me.Position = ST then d.ShootSTBonus else 0.0
        let distPenalty = PhysicsContract.clampFloat (float ctx.DistToGoal / d.ShootDistPenaltyDivisor) 0.0 d.ShootDistPenaltyMax

        (finishing + longShots + composure + distNorm + posBonus - distPenalty)
        * normCond ctx.MyCondition

    let private passScore (ctx: AgentContext) =
        let me = ctx.Me
        let d = ctx.Decision
        let passing = normStat me.Technical.Passing * d.PassPassingWeight
        let vision = normStat me.Mental.Vision * d.PassVisionWeight
        let targetBonus = if ctx.BestPassTarget.IsSome then d.PassTargetBonus else 0.0

        let phaseMod =
            match ctx.Phase with
            | BuildUp ->
                let posBonus =
                    match me.Position with
                    | GK -> ctx.BuildUp.GKDistributionBonus
                    | DC
                    | DM -> ctx.BuildUp.DCPassingBonus
                    | _ -> 0.0

                ctx.BuildUp.PassSuccessBonus + posBonus
            | Midfield -> 0.0
            | Attack -> d.PassAttackPhasePenalty

        let creativityMod = ctx.Profile.CreativityWeight * d.CreativityWeight + (1.0 - ctx.Profile.Directness) * d.DirectnessWeight

        (passing + vision + targetBonus + phaseMod + creativityMod) * normCond ctx.MyCondition

    let private dribbleScore (ctx: AgentContext) =
        let me = ctx.Me
        let d = ctx.Decision
        let dribbling = normStat me.Technical.Dribbling * ctx.Dribble.TechnicalWeight
        let agility = normStat me.Physical.Agility * ctx.Dribble.AgilityWeight
        let balance = normStat me.Physical.Balance * ctx.Dribble.BalanceWeight

        let zoneBonus =
            match ctx.Zone with
            | AttackingZone -> d.DribbleZoneBonusAttacking
            | MidfieldZone -> d.DribbleZoneBonusMidfield
            | DefensiveZone -> 0.0

        let phaseMod =
            match ctx.Phase with
            | BuildUp -> -ctx.BuildUp.DribblePenalty
            | Midfield -> 0.0
            | Attack -> d.DribbleAttackPhaseBonus

        (dribbling + agility + balance + zoneBonus + phaseMod)
        * normCond ctx.MyCondition

    let private crossScore (ctx: AgentContext) =
        let me = ctx.Me
        let d = ctx.Decision
        let crossing = normStat me.Technical.Crossing * d.CrossCrossingWeight
        let posBonus = abs (ctx.Profile.LateralTendency - 0.5) * d.CrossLateralTendencyWeight
        let zoneBonus = if ctx.Zone = AttackingZone then d.CrossZoneBonus else 0.0
        (crossing + posBonus + zoneBonus) * normCond ctx.MyCondition

    let private longBallScore (ctx: AgentContext) =
        let me = ctx.Me
        let d = ctx.Decision
        let passing = normStat me.Technical.Passing * d.LongBallPassingWeight
        let vision = normStat me.Mental.Vision * d.LongBallVisionWeight

        let pressureMod =
            match ctx.NearestOpponent with
            | Some(_, oppPos) ->
                let dx = ctx.MyPos.X - oppPos.X
                let dy = ctx.MyPos.Y - oppPos.Y
                let dist = sqrt (dx * dx + dy * dy)
                PhysicsContract.clampFloat (float dist / d.LongBallPressDistBase) d.LongBallPressMin d.LongBallPressMax
            | None -> d.LongBallPressNoOpponent

        let phaseMod =
            match ctx.Phase with
            | BuildUp -> -ctx.BuildUp.LongBallPenalty
            | Midfield -> 0.0
            | Attack -> d.LongBallAttackPhaseBonus

        (passing + vision) * pressureMod * normCond ctx.MyCondition + phaseMod

    let computeAll (ctx: AgentContext) : ActionScores =
        { Shoot = shootScore ctx
          Pass = passScore ctx
          Dribble = dribbleScore ctx
          Cross = crossScore ctx
          LongBall = longBallScore ctx }
