namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.MatchMemory

[<Struct>]
type ActionScores =
    { Shoot    : float<decisionScore>
      Pass     : float<decisionScore>
      Dribble  : float<decisionScore>
      Cross    : float<decisionScore>
      LongBall : float<decisionScore> }

module PlayerScorer =

    let private normStat (v: int) = float v / 20.0
    let private condFactor (c: int) = 0.90 + 0.10 * (float c / 100.0)

    let private shootScore (ctx: AgentContext) (matchMemory: MatchMemory) : float<decisionScore> =
        let me = ctx.Me
        let d = ctx.Decision
        let finishing = normStat me.Technical.Finishing * d.ShootFinishingWeight
        let longShots = normStat me.Technical.LongShots * d.ShootLongShotsWeight
        let composure = normStat me.Mental.Composure * d.ShootComposureWeight
        let distNorm = (1.0 - PhysicsContract.clampFloat (float ctx.DistToGoal / d.ShootDistNormDivisor) 0.0 1.0) * d.ShootDistNormWeight
        let posBonus = ctx.Profile.Directness * d.ShootPosDirectnessWeight + ctx.Profile.AttackingDepth * d.ShootPosDepthWeight + if me.Position = ST then d.ShootSTBonus else 0.0
        let distPenalty = PhysicsContract.clampFloat (float ctx.DistToGoal / d.ShootDistPenaltyDivisor) 0.0 d.ShootDistPenaltyMax

        let streakMod = MatchMemory.successStreakModifier ctx.Team.ClubSide ctx.MeIdx matchMemory

        let scoreRaw = finishing + longShots + composure + distNorm + posBonus - distPenalty + streakMod
        let maxPossible = d.ShootFinishingWeight + d.ShootLongShotsWeight + d.ShootComposureWeight + d.ShootDistNormWeight + d.ShootPosDirectnessWeight + d.ShootPosDepthWeight + d.ShootSTBonus
        ((scoreRaw / maxPossible) * condFactor ctx.MyCondition) |> LanguagePrimitives.FloatWithMeasure<decisionScore>

    let private passScore (ctx: AgentContext) (matchMemory: MatchMemory) : float<decisionScore> =
        let me = ctx.Me
        let d = ctx.Decision
        let passing = normStat me.Technical.Passing * d.PassPassingWeight
        let vision = normStat me.Mental.Vision * d.PassVisionWeight
        let targetBonus = if ctx.BestPassTargetIdx.IsSome then d.PassTargetBonus else 0.0

        let phaseBonusMax = max (ctx.BuildUp.PassSuccessBonus + ctx.BuildUp.GKDistributionBonus) 0.0
        let creativityBonusMax = d.CreativityWeight + d.DirectnessWeight

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

        let passMemMod = MatchMemory.passFailureModifier ctx.Team.ClubSide ctx.MeIdx matchMemory

        let scoreRaw = passing + vision + targetBonus + phaseMod + creativityMod + passMemMod
        let maxPossible = d.PassPassingWeight + d.PassVisionWeight + d.PassTargetBonus + phaseBonusMax + creativityBonusMax
        ((scoreRaw / maxPossible) * condFactor ctx.MyCondition) |> LanguagePrimitives.FloatWithMeasure<decisionScore>

    let private dribbleScore (ctx: AgentContext) (matchMemory: MatchMemory) : float<decisionScore> =
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

        let zoneBonusMax = max d.DribbleZoneBonusAttacking d.DribbleZoneBonusMidfield
        let phaseBonusMax = d.DribbleAttackPhaseBonus
        let maxPossible = ctx.Dribble.TechnicalWeight + ctx.Dribble.AgilityWeight + ctx.Dribble.BalanceWeight + zoneBonusMax + phaseBonusMax

        let memMod =
            match ctx.NearestOpponentIdx with
            | ValueSome oppIdx ->
                let oppId = ctx.Team.OppRoster.Players[oppIdx].Id
                MatchMemory.duelHistoryModifier ctx.Team.ClubSide ctx.MeIdx matchMemory
            | ValueNone -> 0.0

        let streakMod = MatchMemory.successStreakModifier ctx.Team.ClubSide ctx.MeIdx matchMemory

        let scoreRaw = dribbling + agility + balance + zoneBonus + phaseMod + memMod + streakMod
        ((scoreRaw / maxPossible) * condFactor ctx.MyCondition) |> LanguagePrimitives.FloatWithMeasure<decisionScore>

    let private crossScore (ctx: AgentContext) (matchMemory: MatchMemory) : float<decisionScore> =
        let me = ctx.Me
        let d = ctx.Decision
        let crossing = normStat me.Technical.Crossing * d.CrossCrossingWeight
        let posBonus = abs (ctx.Profile.LateralTendency - 0.5) * d.CrossLateralTendencyWeight
        let zoneBonus = if ctx.Zone = AttackingZone then d.CrossZoneBonus else 0.0
        let scoreRaw = crossing + posBonus + zoneBonus
        let maxPossible = d.CrossCrossingWeight + d.CrossLateralTendencyWeight + d.CrossZoneBonus
        ((scoreRaw / maxPossible) * condFactor ctx.MyCondition) |> LanguagePrimitives.FloatWithMeasure<decisionScore>

    let private longBallScore (ctx: AgentContext) (matchMemory: MatchMemory) : float<decisionScore> =
        let me = ctx.Me
        let d = ctx.Decision
        let passing = normStat me.Technical.Passing * d.LongBallPassingWeight
        let vision = normStat me.Mental.Vision * d.LongBallVisionWeight

        let pressureMod =
            match ctx.NearestOpponentIdx with
            | ValueSome oppIdx ->
                let oppX = float ctx.Team.OppFrame.PosX[oppIdx] * 1.0<meter>
                let oppY = float ctx.Team.OppFrame.PosY[oppIdx] * 1.0<meter>
                let dx = ctx.MyPos.X - oppX
                let dy = ctx.MyPos.Y - oppY
                let dist = sqrt (dx * dx + dy * dy)
                PhysicsContract.clampFloat (float dist / d.LongBallPressDistBase) d.LongBallPressMin d.LongBallPressMax
            | ValueNone -> d.LongBallPressNoOpponent

        let phaseMod =
            match ctx.Phase with
            | BuildUp -> -ctx.BuildUp.LongBallPenalty
            | Midfield -> 0.0
            | Attack -> d.LongBallAttackPhaseBonus

        let phaseBonusMax = d.LongBallAttackPhaseBonus
        let maxPossible = d.LongBallPassingWeight + d.LongBallVisionWeight + phaseBonusMax

        let scoreRaw = (passing + vision) * pressureMod + phaseMod
        ((scoreRaw / maxPossible) * condFactor ctx.MyCondition) |> LanguagePrimitives.FloatWithMeasure<decisionScore>

    let computeAll (ctx: AgentContext) (matchMemory: MatchMemory) : ActionScores =
        { Shoot = shootScore ctx matchMemory
          Pass = passScore ctx matchMemory
          Dribble = dribbleScore ctx matchMemory
          Cross = crossScore ctx matchMemory
          LongBall = longBallScore ctx matchMemory }
