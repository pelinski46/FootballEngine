namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

[<Struct>]
type MovementScores =
    { MaintainShape: float
      MarkMan: float
      PressBall: float
      CoverSpace: float
      SupportAttack: float
      RecoverBall: float }

module MovementScorer =

    let private normStat (v: int) = float v / 20.0
    let private normCond (v: int) = float v / 100.0

    let private hasBall (ballState: BallPhysicsState) (pid: PlayerId) =
        match ballState.Possession with
        | Owned(_, p) -> p = pid
        | _ -> false

    let maintainShapeScore (ctx: AgentContext) =
        let baseScore = 0.3
        let positionBonus =
            match ctx.Me.Position with
            | DC | DM | MC -> 0.15
            | DL | DR | WBL | WBR -> 0.08
            | GK -> 0.3
            | _ -> 0.03
        if ctx.TeamHasBall then
            (baseScore + positionBonus) * normCond ctx.MyCondition
        else
            (baseScore + positionBonus - ctx.Profile.PressingIntensity * 0.15) * normCond ctx.MyCondition

    let markManScore (ctx: AgentContext) =
        if ctx.TeamHasBall then 0.0
        else
            match ctx.NearestOpponentIdx with
            | None -> 0.0
            | Some oppIdx ->
                let oppX = float ctx.Team.OppFrame.PosX[oppIdx] * 1.0<meter>
                let oppY = float ctx.Team.OppFrame.PosY[oppIdx] * 1.0<meter>
                let oppPos = { X = oppX; Y = oppY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                let dist = ctx.MyPos.DistTo2D oppPos
                if dist > 20.0<meter> then 0.0
                else
                    let baseScore = 1.0 - (float dist / 20.0)
                    let positioningWeight = normStat ctx.Me.Mental.Positioning
                    let defensiveBonus =
                        match ctx.Me.Position with
                        | DC | DM | DL | DR | WBL | WBR -> 0.35
                        | MC -> 0.2
                        | _ -> 0.05
                    (baseScore * positioningWeight + defensiveBonus) * normCond ctx.MyCondition

    let pressBallScore (ctx: AgentContext) =
        if ctx.TeamHasBall then 0.0
        else
            let dist = ctx.MyPos.DistTo2D ctx.BallState.Position
            if dist > 30.0<meter> then 0.0
            else
                let baseScore = 1.2 - (float dist / 30.0)
                let pressWeight = 0.3 + ctx.Profile.PressingIntensity * 0.7
                let workRate = normStat ctx.Me.Mental.WorkRate
                let positionMod =
                    match ctx.Me.Position with
                    | MC | AML | AMR | AMC -> 0.15
                    | ST -> 0.1
                    | _ -> 0.0
                let staminaPenalty = float (100 - ctx.MyCondition) / 100.0 * 0.3
                (baseScore * pressWeight * workRate + positionMod - staminaPenalty) |> max 0.0

    let coverSpaceScore (ctx: AgentContext) =
        if ctx.TeamHasBall then
            match ctx.Me.Position with
            | DC | DM -> 0.15 * normStat ctx.Me.Mental.Positioning
            | _ -> 0.05
        else
            match ctx.Me.Position with
            | DC | DM -> 0.35 * normStat ctx.Me.Mental.Positioning
            | DL | DR | WBL | WBR -> 0.2 * normStat ctx.Me.Mental.Positioning
            | _ -> 0.1

    let supportAttackScore (ctx: AgentContext) =
        if not ctx.TeamHasBall then 0.0
        elif hasBall ctx.BallState ctx.Me.Id then 0.0
        else
            let baseScore = 0.5
            let offTheBall = normStat ctx.Me.Mental.WorkRate
            let depthBonus = ctx.Profile.AttackingDepth * 0.4
            let positionBonus =
                match ctx.Me.Position with
                | ST | AML | AMR | AMC -> 0.4
                | ML | MR | MC -> 0.3
                | DL | DR | WBL | WBR -> 0.2
                | DC | DM -> 0.1
                | GK -> 0.0
            (baseScore + offTheBall * 0.4 + depthBonus + positionBonus) * normCond ctx.MyCondition

    let recoverBallScore (ctx: AgentContext) =
        if ctx.TeamHasBall then 0.0
        else
            match ctx.BallState.Possession with
            | Loose | Contest _ ->
                let dist = ctx.MyPos.DistTo2D ctx.BallState.Position
                if dist > 20.0<meter> then 0.0
                else
                    let baseScore = 1.5 - (float dist / 20.0)
                    let positionMod =
                        match ctx.Me.Position with
                        | MC | DM | AML | AMR | AMC -> 0.2
                        | ST -> 0.15
                        | _ -> 0.0
                    baseScore + positionMod
            | Owned(oppositeSide, _) ->
                if oppositeSide = ctx.Team.ClubSide then 0.0
                else
                    let dist = ctx.MyPos.DistTo2D ctx.BallState.Position
                    if dist > 15.0<meter> then 0.0
                    else 0.8 - (float dist / 15.0) * 0.4
            | _ -> 0.0

    let applyEmergentModifiers (emergent: EmergentState) (scores: MovementScores) : MovementScores =
        { scores with
            PressBall = scores.PressBall * emergent.PressingIntensity
            MarkMan = scores.MarkMan * emergent.CompactnessLevel
            CoverSpace = scores.CoverSpace * emergent.CompactnessLevel
            SupportAttack = scores.SupportAttack * emergent.RiskAppetite }

    let computeAll (ctx: AgentContext) (emergent: EmergentState) : MovementScores =
        let raw =
            { MaintainShape = maintainShapeScore ctx
              MarkMan = markManScore ctx
              PressBall = pressBallScore ctx
              CoverSpace = coverSpaceScore ctx
              SupportAttack = supportAttackScore ctx
              RecoverBall = recoverBallScore ctx }
        applyEmergentModifiers emergent raw

    let private scoreForIntent (scores: MovementScores) (intent: MovementIntent) : float =
        match intent with
        | MaintainShape _ -> scores.MaintainShape
        | MarkMan _ -> scores.MarkMan
        | PressBall _ -> scores.PressBall
        | CoverSpace _ -> scores.CoverSpace
        | SupportAttack _ -> scores.SupportAttack
        | RecoverBall _ -> scores.RecoverBall
        | ExecuteRun _ -> scores.SupportAttack * 0.8

    let pickIntent (currentSubTick: int) (scores: MovementScores) (ctx: AgentContext) : MovementIntent =
        let ti = ctx.TeamIntent
        let supportTarget =
            if ctx.MeIdx < ti.SupportPositions.Length then
                let sp = ti.SupportPositions[ctx.MeIdx]
                if sp.X <> 0.0<meter> || sp.Y <> 0.0<meter> then sp
                elif ctx.BestPassTargetPos.IsSome then ctx.BestPassTargetPos.Value
                else
                    let forwardGoalX = if ctx.Team.AttackDir = LeftToRight then min (ctx.MyPos.X + 10.0<meter>) 100.0<meter> else max (ctx.MyPos.X - 10.0<meter>) 5.0<meter>
                    { X = forwardGoalX; Y = ctx.MyPos.Y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
            else
                match ctx.BestPassTargetPos with
                | Some sp -> sp
                | None ->
                    let forwardGoalX = if ctx.Team.AttackDir = LeftToRight then min (ctx.MyPos.X + 10.0<meter>) 100.0<meter> else max (ctx.MyPos.X - 10.0<meter>) 5.0<meter>
                    { X = forwardGoalX; Y = ctx.MyPos.Y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

        let markTarget =
            match ctx.NearestOpponentIdx with
            | Some oppIdx ->
                let oppX = float ctx.Team.OppFrame.PosX[oppIdx] * 1.0<meter>
                let oppY = float ctx.Team.OppFrame.PosY[oppIdx] * 1.0<meter>
                let oppPos = { X = oppX; Y = oppY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                let oppPlayerId = ctx.Team.OppRoster.Players[oppIdx].Id
                MarkMan (oppPlayerId, oppPos)
            | None -> MaintainShape ctx.MyPos

        let mutable bestScore = -1.0
        let mutable bestIntent = MaintainShape ctx.MyPos

        if scores.MaintainShape > 0.05 && scores.MaintainShape > bestScore then
            bestScore <- scores.MaintainShape; bestIntent <- MaintainShape supportTarget

        if scores.MarkMan > 0.05 && scores.MarkMan > bestScore then
            bestScore <- scores.MarkMan; bestIntent <- markTarget

        if scores.PressBall > 0.05 && scores.PressBall > bestScore then
            bestScore <- scores.PressBall; bestIntent <- PressBall ctx.BallState.Position

        if scores.CoverSpace > 0.05 && scores.CoverSpace > bestScore then
            bestScore <- scores.CoverSpace; bestIntent <- CoverSpace supportTarget

        if scores.SupportAttack > 0.05 && scores.SupportAttack > bestScore then
            bestScore <- scores.SupportAttack; bestIntent <- SupportAttack supportTarget

        if scores.RecoverBall > 0.05 && scores.RecoverBall > bestScore then
            bestScore <- scores.RecoverBall; bestIntent <- RecoverBall ctx.BallState.Position

        if hasBall ctx.BallState ctx.Me.Id && scores.SupportAttack > 0.3 then
            let forwardGoalX = if ctx.Team.AttackDir = LeftToRight then min (ctx.MyPos.X + 15.0<meter>) 100.0<meter> else max (ctx.MyPos.X - 15.0<meter>) 5.0<meter>
            let runTarget = { X = forwardGoalX; Y = ctx.MyPos.Y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
            let runScore = 1.5 + scores.SupportAttack
            if runScore > bestScore then
                let assignment = RunAssignment.create RunType.DeepRun ctx.MyPos.X ctx.MyPos.Y runTarget.X runTarget.Y ctx.Me.Id currentSubTick 120
                bestScore <- runScore
                bestIntent <- ExecuteRun assignment

        match ti.TargetRunner with
        | Some runnerId when runnerId = ctx.Me.Id ->
            match ti.RunType, ti.RunTarget with
            | Some runType, Some runTarget ->
                let runScore = 2.0 + scores.SupportAttack
                if runScore > bestScore then
                    let assignment =
                        RunAssignment.create
                            runType ctx.MyPos.X ctx.MyPos.Y runTarget.X runTarget.Y ctx.Me.Id
                            currentSubTick 120
                    bestScore <- runScore
                    bestIntent <- ExecuteRun assignment
            | _ -> ()
        | _ -> ()

        if bestScore < 0.15 then
            if not ctx.TeamHasBall then
                let distToBall = ctx.MyPos.DistTo2D ctx.BallState.Position
                if distToBall < 35.0<meter> then
                    bestIntent <- RecoverBall ctx.BallState.Position
                else
                    bestIntent <- SupportAttack supportTarget
            else
                bestIntent <- SupportAttack supportTarget

        if currentSubTick < ctx.IntentLockExpiry then
            match ctx.PreviousIntent with
            | Some prev -> prev
            | None -> bestIntent
        else
            match ctx.PreviousIntent with
            | None -> bestIntent
            | Some prev ->
                let prevScore = scoreForIntent scores prev
                if bestScore > prevScore + 0.12 then
                    bestIntent
                else
                    prev
