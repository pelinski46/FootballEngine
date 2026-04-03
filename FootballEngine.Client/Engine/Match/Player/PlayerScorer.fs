namespace FootballEngine

open System
open FootballEngine.Domain
open MatchCalc

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
        let finishing = normStat me.Technical.Finishing * 0.35
        let longShots = normStat me.Technical.LongShots * 0.15
        let composure = normStat me.Mental.Composure * 0.20
        let distNorm = (1.0 - Math.Clamp(ctx.DistToGoal / 30.0, 0.0, 1.0)) * 0.20
        let posBonus =
            match me.Position with
            | ST | AMC -> 0.10
            | AML | AMR | MC -> 0.07
            | _ -> 0.02
        (finishing + longShots + composure + distNorm + posBonus) * normCond ctx.MyCondition

    let private passScore (ctx: AgentContext) =
        let me = ctx.Me
        let passing = normStat me.Technical.Passing * 0.40
        let vision = normStat me.Mental.Vision * 0.30
        let targetBonus = if ctx.BestPassTarget.IsSome then 0.30 else 0.0
        (passing + vision + targetBonus) * normCond ctx.MyCondition

    let private dribbleScore (ctx: AgentContext) =
        let me = ctx.Me
        let dribbling = normStat me.Technical.Dribbling * 0.50
        let agility = normStat me.Physical.Agility * 0.30
        let balance = normStat me.Physical.Balance * 0.20
        let zoneBonus =
            match ctx.Zone with
            | AttackingZone -> 0.1
            | MidfieldZone -> 0.05
            | DefensiveZone -> 0.0
        (dribbling + agility + balance + zoneBonus) * normCond ctx.MyCondition

    let private crossScore (ctx: AgentContext) =
        let me = ctx.Me
        let crossing = normStat me.Technical.Crossing * 0.60
        let posBonus =
            match me.Position with
            | DR | DL | WBR | WBL | MR | ML | AMR | AML -> 0.40
            | _ -> 0.10
        let zoneBonus = if ctx.Zone = AttackingZone then 0.15 else 0.0
        (crossing + posBonus + zoneBonus) * normCond ctx.MyCondition

    let private longBallScore (ctx: AgentContext) =
        let me = ctx.Me
        let passing = normStat me.Technical.Passing * 0.30
        let vision = normStat me.Mental.Vision * 0.20
        let pressureMod =
            match ctx.NearestOpponent with
            | Some(_, oppPos) ->
                let dx = ctx.MyPos.X - oppPos.X
                let dy = ctx.MyPos.Y - oppPos.Y
                let dist = sqrt (dx * dx + dy * dy)
                Math.Clamp(dist / 10.0, 0.3, 1.0)
            | None -> 0.7
        (passing + vision) * pressureMod * normCond ctx.MyCondition

    let computeAll (ctx: AgentContext) : ActionScores =
        { Shoot = shootScore ctx
          Pass = passScore ctx
          Dribble = dribbleScore ctx
          Cross = crossScore ctx
          LongBall = longBallScore ctx }
