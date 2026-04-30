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
      RecoverBall: float
      MoveToSetPiecePos: float }

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
            | DC
            | DM
            | MC -> 0.15
            | DL
            | DR
            | WBL
            | WBR -> 0.08
            | GK -> 0.3
            | _ -> 0.03

        if ctx.TeamHasBall then
            (baseScore + positionBonus) * normCond ctx.MyCondition
        else
            (baseScore + positionBonus - ctx.Profile.PressingIntensity * 0.15)
            * normCond ctx.MyCondition

    let markManScore (ctx: AgentContext) =
        if ctx.TeamHasBall then
            0.0
        else
            let bcIdx = int ctx.BallCarrierOppIdx
            let hasBallCarrierInfo = bcIdx >= 0

            let (oppIdx, oppX, oppY) =
                if hasBallCarrierInfo then
                    let ox = float ctx.Team.OppFrame.PosX[bcIdx] * 1.0<meter>
                    let oy = float ctx.Team.OppFrame.PosY[bcIdx] * 1.0<meter>
                    bcIdx, ox, oy
                else
                    match ctx.NearestOpponentIdx with
                    | ValueNone -> -1, 0.0<meter>, 0.0<meter>
                    | ValueSome idx ->
                        let ox = float ctx.Team.OppFrame.PosX[idx] * 1.0<meter>
                        let oy = float ctx.Team.OppFrame.PosY[idx] * 1.0<meter>
                        idx, ox, oy

            if oppIdx < 0 then 0.0
            else
                let oppPos =
                    { X = oppX
                      Y = oppY
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> }

                let dist = ctx.MyPos.DistTo2D oppPos
                let markRadius = if hasBallCarrierInfo then 25.0<meter> else 20.0<meter>

                if dist > markRadius then
                    0.0
                else
                    let baseScore = 1.0 - (float dist / float markRadius)
                    let positioningWeight = normStat ctx.Me.Mental.Positioning

                    let defensiveBonus =
                        match ctx.Me.Position with
                        | DC
                        | DM
                        | DL
                        | DR
                        | WBL
                        | WBR -> 0.35
                        | MC -> 0.2
                        | _ -> 0.05

                    let shapeFactor = ctx.Tactics.DefensiveShape
                    let markBonus = shapeFactor * 0.3

                    let ballCarrierBonus = if hasBallCarrierInfo then 0.5 else 0.0

                    (baseScore * positioningWeight + defensiveBonus + markBonus + ballCarrierBonus) * normCond ctx.MyCondition

    let pressBallScore (ctx: AgentContext) =
        if ctx.TeamHasBall then
            0.0
        else
            let dist = ctx.MyPos.DistTo2D ctx.BallState.Position

            if dist > 45.0<meter> then
                0.0
            else
                let zoneScore =
                    if dist < 12.0<meter> then
                        1.5 - float dist / 12.0
                    elif dist < 25.0<meter> then
                        1.2 - (float dist - 12.0) / 13.0
                    elif dist < 45.0<meter> then
                        0.6 - (float dist - 25.0) / 20.0
                    else
                        0.0

                let pressFactor = ctx.Tactics.PressingIntensity * 0.5 + ctx.Profile.PressingIntensity * 0.5
                let pressWeight = 0.4 + pressFactor * 0.6
                let workRate = normStat ctx.Me.Mental.WorkRate

                let positionMod =
                    match ctx.Me.Position with
                    | MC
                    | AML
                    | AMR
                    | AMC -> 0.15
                    | ST -> 0.1
                    | _ -> 0.0

                let staminaPenalty = float (100 - ctx.MyCondition) / 100.0 * 0.2

                let transitionBonus =
                    if ctx.CurrentSubTick < ctx.TransitionPressExpiry then 1.5 else 1.0

                let shapeDist =
                    if ctx.MeIdx < ctx.TeamIntent.SupportPositions.Length then
                        let sp = ctx.TeamIntent.SupportPositions[ctx.MeIdx]
                        if sp.X <> 0.0<meter> || sp.Y <> 0.0<meter> then
                            ctx.MyPos.DistTo2D sp
                        else
                            0.0<meter>
                    else
                        0.0<meter>

                let positionalPenalty =
                    if shapeDist > 15.0<meter> then
                        1.0 - (float shapeDist - 15.0) / 30.0
                    else
                        1.0

                (zoneScore * pressWeight * workRate * transitionBonus * positionalPenalty + positionMod - staminaPenalty) |> max 0.0

    let coverSpaceScore (ctx: AgentContext) =
        let zonalFactor = (1.0 - ctx.Tactics.DefensiveShape) * 0.3

        let baseScore =
            if ctx.TeamHasBall then
                match ctx.Me.Position with
                | DC
                | DM -> (0.15 * normStat ctx.Me.Mental.Positioning + zonalFactor)
                | _ -> 0.05
            else
                match ctx.Me.Position with
                | DC
                | DM -> (0.35 * normStat ctx.Me.Mental.Positioning + zonalFactor)
                | DL
                | DR
                | WBL
                | WBR -> (0.2 * normStat ctx.Me.Mental.Positioning + zonalFactor)
                | _ -> 0.1 + zonalFactor

        let influenceBonus =
            let cell = InfluenceTypes.posToCell ctx.Team.OwnFrame.PosX[ctx.MeIdx] ctx.Team.OwnFrame.PosY[ctx.MeIdx]
            let contested = ctx.Influence.ContestedGrid[cell]
            let gapRead = normStat ctx.Me.Mental.Positioning
            if contested < 0.0f then
                let deficit = min 1.0f (-contested)
                float deficit * 0.15 * gapRead
            else
                0.0

        baseScore + influenceBonus

    let supportAttackScore (ctx: AgentContext) =
        if not ctx.TeamHasBall then
            0.0
        elif hasBall ctx.BallState ctx.Me.Id then
            0.0
        else
            let baseScore = 0.5
            let offTheBall = normStat ctx.Me.Mental.WorkRate
            let depthBonus = ctx.Profile.AttackingDepth * 0.4

            let widthBonus = ctx.Tactics.Width * ctx.Profile.LateralTendency * 0.25

            let buildUpSideBonus =
                match ctx.TeamIntent.BuildUpSide with
                | BuildUpSide.LeftFlank ->
                    if ctx.Me.Position = AMR || ctx.Me.Position = MR || ctx.Me.Position = WBR then 0.15 else 0.0
                | BuildUpSide.RightFlank ->
                    if ctx.Me.Position = AML || ctx.Me.Position = ML || ctx.Me.Position = WBL then 0.15 else 0.0
                | _ -> 0.0

            let positionBonus =
                match ctx.Me.Position with
                | ST
                | AML
                | AMR
                | AMC -> 0.4
                | ML
                | MR
                | MC -> 0.3
                | DL
                | DR
                | WBL
                | WBR -> 0.2
                | DC
                | DM -> 0.1
                | GK -> 0.0

            let spaceBonus =
                let mutable bestScore = 0.0
                for cell = 0 to InfluenceTypes.GridSize - 1 do
                    let s = InfluenceTypes.scoreCellRaw
                                cell ctx.Influence
                                (float32 ctx.MyPos.X) (float32 ctx.MyPos.Y)
                                ctx.Team.AttackDir
                    if s > bestScore then bestScore <- s
                if bestScore > 0.4 then
                    let positioning = normStat ctx.Me.Mental.Positioning
                    let qualityFactor = min 1.0 ((bestScore - 0.4) / 0.6)
                    qualityFactor * 0.20 * positioning
                else
                    0.0

            (baseScore + offTheBall * 0.4 + depthBonus + positionBonus + widthBonus + buildUpSideBonus + spaceBonus)
            * normCond ctx.MyCondition

    let recoverBallScore (ctx: AgentContext) =
        if ctx.TeamHasBall then
            0.0
        else
            match ctx.BallState.Possession with
            | Loose
            | Contest _ ->
                let dist = ctx.MyPos.DistTo2D ctx.BallState.Position

                if dist > 20.0<meter> then
                    0.0
                else
                    let baseScore = 1.5 - (float dist / 20.0)

                    let positionMod =
                        match ctx.Me.Position with
                        | MC
                        | DM
                        | AML
                        | AMR
                        | AMC -> 0.2
                        | ST -> 0.15
                        | _ -> 0.0

                    let shapeDist =
                        if ctx.MeIdx < ctx.TeamIntent.SupportPositions.Length then
                            let sp = ctx.TeamIntent.SupportPositions[ctx.MeIdx]
                            if sp.X <> 0.0<meter> || sp.Y <> 0.0<meter> then
                                ctx.MyPos.DistTo2D sp
                            else
                                0.0<meter>
                        else
                            0.0<meter>

                    let positionalPenalty =
                        if shapeDist > 12.0<meter> then
                            1.0 - (float shapeDist - 12.0) / 25.0
                        else
                            1.0

                    (baseScore + positionMod) * positionalPenalty
            | Owned(oppositeSide, _) ->
                if oppositeSide = ctx.Team.ClubSide then
                    0.0
                else
                    let dist = ctx.MyPos.DistTo2D ctx.BallState.Position

                    if dist > 15.0<meter> then
                        0.0
                    else
                        0.8 - (float dist / 15.0) * 0.4
            | _ -> 0.0

    let applyEmergentModifiers (emergent: EmergentState) (scores: MovementScores) : MovementScores =
        { scores with
            PressBall = scores.PressBall * emergent.PressingIntensity
            MarkMan = scores.MarkMan * emergent.CompactnessLevel
            CoverSpace = scores.CoverSpace * emergent.CompactnessLevel
            SupportAttack = scores.SupportAttack * emergent.RiskAppetite }

    let interceptPassScore (ctx: AgentContext) : float =
        if ctx.TeamHasBall then 0.0
        else
            match ctx.TeamIntent.DefensiveAssignments with
            | assignments when assignments.Length > ctx.MeIdx ->
                match assignments[ctx.MeIdx] with
                | DefensiveRole.Marker ->
                    let myX = ctx.MyPos.X
                    let myY = ctx.MyPos.Y
                    let bcIdx = int ctx.BallCarrierOppIdx
                    if bcIdx >= 0 then
                        let bcX = float ctx.Team.OppFrame.PosX[bcIdx] * 1.0<meter>
                        let bcY = float ctx.Team.OppFrame.PosY[bcIdx] * 1.0<meter>
                        let dist = ctx.MyPos.DistTo2D { X = bcX; Y = bcY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                        if dist > 25.0<meter> then 0.0
                        else
                            let mutable bestInterceptScore = 0.0
                            for j = 0 to ctx.Team.OppFrame.SlotCount - 1 do
                                match ctx.Team.OppFrame.Occupancy[j] with
                                | OccupancyKind.Active _ when j <> bcIdx ->
                                    let ox = float ctx.Team.OppFrame.PosX[j] * 1.0<meter>
                                    let oy = float ctx.Team.OppFrame.PosY[j] * 1.0<meter>
                                    let lx1, ly1 = float32 bcX, float32 bcY
                                    let lx2, ly2 = float32 ox, float32 oy
                                    let laneDistSq = MatchSpatial.pointToLineDistSq (float32 myX) (float32 myY) lx1 ly1 lx2 ly2
                                    if laneDistSq < 9.0f then
                                        let bonus = 1.0 - float laneDistSq / 9.0
                                        bestInterceptScore <- max bestInterceptScore bonus * 0.5
                                | _ -> ()
                            bestInterceptScore
                    else 0.0
                | _ -> 0.0
            | _ -> 0.0

    let private applyRoleModifiers (ctx: AgentContext) (scores: MovementScores) : MovementScores =
        match ctx.TeamIntent.DefensiveAssignments with
        | assignments when not ctx.TeamHasBall && assignments.Length > ctx.MeIdx ->
            match assignments[ctx.MeIdx] with
            | DefensiveRole.FirstDefender ->
                { scores with
                    PressBall = scores.PressBall * 2.0
                    MarkMan = scores.MarkMan * 0.3
                    CoverSpace = scores.CoverSpace * 0.3 }
            | DefensiveRole.Cover ->
                { scores with
                    CoverSpace = scores.CoverSpace * 2.0
                    MarkMan = scores.MarkMan * 0.5
                    PressBall = scores.PressBall * 0.3 }
            | DefensiveRole.Marker ->
                { scores with
                    MarkMan = scores.MarkMan * 2.0
                    PressBall = scores.PressBall * 0.3
                    CoverSpace = scores.CoverSpace * 0.5 }
        | _ -> scores

    let computeAll (ctx: AgentContext) (emergent: EmergentState) : MovementScores =
        let raw =
            { MaintainShape = maintainShapeScore ctx
              MarkMan = markManScore ctx + interceptPassScore ctx
              PressBall = pressBallScore ctx
              CoverSpace = coverSpaceScore ctx
              SupportAttack = supportAttackScore ctx
              RecoverBall = recoverBallScore ctx
              MoveToSetPiecePos = maintainShapeScore ctx }

        raw |> applyEmergentModifiers emergent |> applyRoleModifiers ctx

    let private scoreForIntent (scores: MovementScores) (intent: MovementIntent) : float =
        match intent with
        | MaintainShape _ -> scores.MaintainShape
        | MarkMan _ -> scores.MarkMan
        | PressBall _ -> scores.PressBall
        | CoverSpace _ -> scores.CoverSpace
        | SupportAttack _ -> scores.SupportAttack
        | RecoverBall _ -> scores.RecoverBall
        | ExecuteRun _ -> scores.SupportAttack * 0.8
        | MoveToSetPiecePos _ -> scores.MoveToSetPiecePos

    let private isInPenaltyArea (x: float<meter>) (y: float<meter>) (dir: AttackDir) : bool =
        let penX = PhysicsContract.PitchLength - PhysicsContract.PenaltyAreaDepth
        match dir with
        | LeftToRight -> x > penX
        | RightToLeft -> x < PhysicsContract.PenaltyAreaDepth

    let private pickIntentNormal (currentSubTick: int) (scores: MovementScores) (ctx: AgentContext) (ti: TeamIntent) : MovementIntent =

        let shapeTarget =
            if ctx.MeIdx < ti.SupportPositions.Length then
                let sp = ti.SupportPositions[ctx.MeIdx]
                if sp.X <> 0.0<meter> || sp.Y <> 0.0<meter> then sp else ctx.MyPos
            else
                ctx.MyPos

        let supportTarget =
            if ctx.MeIdx < ti.SupportPositions.Length then
                let sp = ti.SupportPositions[ctx.MeIdx]

                if sp.X <> 0.0<meter> || sp.Y <> 0.0<meter> then
                    sp
                else
                    match ctx.BestPassTargetPos with
                    | ValueSome pos -> pos
                    | ValueNone ->
                        let forwardGoalX =
                            if ctx.Team.AttackDir = LeftToRight then
                                min (ctx.MyPos.X + 10.0<meter>) 100.0<meter>
                            else
                                max (ctx.MyPos.X - 10.0<meter>) 5.0<meter>

                        { X = forwardGoalX
                          Y = ctx.MyPos.Y
                          Z = 0.0<meter>
                          Vx = 0.0<meter / second>
                          Vy = 0.0<meter / second>
                          Vz = 0.0<meter / second> }
            else
                let forwardGoalX =
                    if ctx.Team.AttackDir = LeftToRight then
                        min (ctx.MyPos.X + 10.0<meter>) 100.0<meter>
                    else
                        max (ctx.MyPos.X - 10.0<meter>) 5.0<meter>

                { X = forwardGoalX
                  Y = ctx.MyPos.Y
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

        let markTarget =
            match ctx.NearestOpponentIdx with
            | ValueSome oppIdx ->
                let oppX = float ctx.Team.OppFrame.PosX[oppIdx] * 1.0<meter>
                let oppY = float ctx.Team.OppFrame.PosY[oppIdx] * 1.0<meter>

                let oppPos =
                    { X = oppX
                      Y = oppY
                      Z = 0.0<meter>
                      Vx = 0.0<meter / second>
                      Vy = 0.0<meter / second>
                      Vz = 0.0<meter / second> }

                let oppPlayerId = ctx.Team.OppRoster.Players[oppIdx].Id
                MarkMan(oppPlayerId, oppPos)
            | ValueNone -> MaintainShape shapeTarget

        let mutable bestScore = -1.0
        let mutable bestIntent = MaintainShape shapeTarget

        if scores.MaintainShape > 0.05 && scores.MaintainShape > bestScore then
            bestScore <- scores.MaintainShape
            bestIntent <- MaintainShape shapeTarget

        if scores.MarkMan > 0.05 && scores.MarkMan > bestScore then
            bestScore <- scores.MarkMan
            bestIntent <- markTarget

        if scores.PressBall > 0.05 && scores.PressBall > bestScore then
            bestScore <- scores.PressBall
            bestIntent <- PressBall ctx.BallState.Position

        if scores.CoverSpace > 0.05 && scores.CoverSpace > bestScore then
            bestScore <- scores.CoverSpace
            bestIntent <- CoverSpace shapeTarget

        if scores.SupportAttack > 0.05 && scores.SupportAttack > bestScore then
            bestScore <- scores.SupportAttack
            bestIntent <- SupportAttack supportTarget

        if scores.RecoverBall > 0.05 && scores.RecoverBall > bestScore then
            bestScore <- scores.RecoverBall
            bestIntent <- RecoverBall ctx.BallState.Position

        if hasBall ctx.BallState ctx.Me.Id && scores.SupportAttack > 0.3 then
            let forwardGoalX =
                if ctx.Team.AttackDir = LeftToRight then
                    min (ctx.MyPos.X + 15.0<meter>) 100.0<meter>
                else
                    max (ctx.MyPos.X - 15.0<meter>) 5.0<meter>

            let runTarget =
                { X = forwardGoalX
                  Y = ctx.MyPos.Y
                  Z = 0.0<meter>
                  Vx = 0.0<meter / second>
                  Vy = 0.0<meter / second>
                  Vz = 0.0<meter / second> }

            let runScore = 1.5 + scores.SupportAttack

            if runScore > bestScore then
                let assignment =
                    RunAssignment.create
                        RunType.DeepRun
                        ctx.MyPos.X
                        ctx.MyPos.Y
                        runTarget.X
                        runTarget.Y
                        ctx.Me.Id
                        currentSubTick
                        120

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
                            runType
                            ctx.MyPos.X
                            ctx.MyPos.Y
                            runTarget.X
                            runTarget.Y
                            ctx.Me.Id
                            currentSubTick
                            120

                    bestScore <- runScore
                    bestIntent <- ExecuteRun assignment
            | _ -> ()
        | _ -> ()

        if bestScore < 0.15 then
            if not ctx.TeamHasBall then
                let distToBall = ctx.MyPos.DistTo2D ctx.BallState.Position

                if distToBall < 12.0<meter> then
                    bestIntent <- PressBall ctx.BallState.Position
                elif distToBall < 25.0<meter> then
                    bestIntent <- CoverSpace shapeTarget
                elif distToBall < 45.0<meter> then
                    bestIntent <- MaintainShape shapeTarget
                else
                    bestIntent <- MaintainShape shapeTarget
            else
                bestIntent <- SupportAttack supportTarget

        if currentSubTick < ctx.IntentLockExpiry then
            match ctx.PreviousIntent with
            | ValueSome prev -> prev
            | ValueNone -> bestIntent
        else
            match ctx.PreviousIntent with
            | ValueNone -> bestIntent
            | ValueSome prev ->
                let prevScore = scoreForIntent scores prev
                if bestScore > prevScore + 0.12 then bestIntent else prev

    let pickIntent (currentSubTick: int) (scores: MovementScores) (ctx: AgentContext) : MovementIntent =
        let ti = ctx.TeamIntent

        match ctx.BallState.Possession with
        | Possession.InFlight ->
            match ctx.BallState.Trajectory with
            | Some traj ->
                let landingPos = { X = traj.TargetX; Y = traj.TargetY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                match traj.ActionKind with
                | BallActionKind.Pass(_, targetId, _) | BallActionKind.LongBall(_, targetId, _) ->
                    if ctx.Me.Id = targetId then
                        SupportAttack landingPos
                    elif ctx.Me.Position = GK && isInPenaltyArea landingPos.X landingPos.Y ctx.Team.AttackDir then
                        MaintainShape ctx.MyPos
                    else
                        let midX = (ctx.MyPos.X + landingPos.X) / 2.0
                        let midY = (ctx.MyPos.Y + landingPos.Y) / 2.0
                        RecoverBall { X = midX; Y = midY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | BallActionKind.Cross(_, targetId, _) ->
                    if ctx.Me.Id = targetId then
                        SupportAttack landingPos
                    elif ctx.Me.Position = GK then
                        MaintainShape ctx.MyPos
                    else
                        RecoverBall landingPos
                | BallActionKind.Shot _ ->
                    if ctx.Me.Position = GK then
                        MaintainShape ctx.MyPos
                    else
                        let midX = (ctx.MyPos.X + landingPos.X) / 2.0
                        let midY = (ctx.MyPos.Y + landingPos.Y) / 2.0
                        RecoverBall { X = midX; Y = midY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                | BallActionKind.Clearance _ | BallActionKind.Deflection _ | BallActionKind.FreeBall ->
                    RecoverBall landingPos
            | None ->
                pickIntentNormal currentSubTick scores ctx ti
        | _ ->
            pickIntentNormal currentSubTick scores ctx ti
