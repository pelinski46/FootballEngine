namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine.InfluenceTypes

// ============================================================
// Space Scorer — evaluates off-ball space targets using influence maps
// Enables pass-into-space and intelligent runs
// ============================================================

module SpaceScorer =

    [<Struct>]
    type SpaceTarget =
        { Cell: int
          Score: float
          Position: Spatial }

    // Minimum teammate convergence required for a space pass
    [<Literal>]
    let private MinTeammateConvergence = 0.15

    // Forward bias: how much to favor cells closer to opponent goal
    [<Literal>]
    let private ForwardBiasWeight = 0.25

    let private normStat (v: int) = float v / 20.0

    let private pitchLengthM = float PitchLength
    let private cellWidthM = float CellWidth
    let private cellHeightM = float CellHeight

    let private cellCenterPos (cell: int) : Spatial =
        let cx, cy = cellToCenter cell
        { X = float cx * 1.0<meter>
          Y = float cy * 1.0<meter>
          Z = 0.0<meter>
          Vx = 0.0<meter/second>
          Vy = 0.0<meter/second>
          Vz = 0.0<meter/second> }

    /// Score a single cell from the perspective of the attacking team.
    /// High score = good space to pass into or run to.
    let scoreCell
        (cell: int)
        (influence: InfluenceFrame)
        (myPos: Spatial)
        (attackDir: AttackDir)
        : float =

        scoreCellRaw cell influence (float32 myPos.X) (float32 myPos.Y) attackDir

    /// Compute how many teammates are moving toward a given cell.
    /// Returns a convergence score 0.0-1.0.
    let private computeTeammateConvergence
        (ctx: AgentContext)
        (targetCell: int)
        : float =

        let targetPos = cellCenterPos targetCell
        let mutable totalConvergence = 0.0
        let mutable count = 0

        for i = 0 to ctx.Team.OwnFrame.SlotCount - 1 do
            if i <> ctx.MeIdx then
                match ctx.Team.OwnFrame.Physics.Occupancy[i] with
                | OccupancyKind.Active _ ->
                    let px = float ctx.Team.OwnFrame.Physics.PosX[i] * 1.0<meter>
                    let py = float ctx.Team.OwnFrame.Physics.PosY[i] * 1.0<meter>
                    let vx = float ctx.Team.OwnFrame.Physics.VelX[i] * 1.0<meter/second>
                    let vy = float ctx.Team.OwnFrame.Physics.VelY[i] * 1.0<meter/second>

                    let playerPos = { X = px; Y = py; Z = 0.0<meter>; Vx = vx; Vy = vy; Vz = 0.0<meter/second> }
                    let dist = playerPos.DistTo2D targetPos

                    // Convergence: closer players with velocity toward target count more
                    let proximityScore = max 0.0 (1.0 - float dist / 40.0)

                    // Velocity alignment: is the player moving toward the target?
                    let velAlign =
                        let toTargetX = float (targetPos.X - playerPos.X)
                        let toTargetY = float (targetPos.Y - playerPos.Y)
                        let toTargetDist = sqrt (toTargetX * toTargetX + toTargetY * toTargetY)
                        if toTargetDist < 0.01 then 0.0
                        else
                            let dot = vx * toTargetX / toTargetDist + vy * toTargetY / toTargetDist
                            max 0.0 (float dot / 8.0) // normalize by max sprint speed

                    let convergence = proximityScore * 0.4 + velAlign * 0.6
                    totalConvergence <- totalConvergence + convergence
                    count <- count + 1
                | _ -> ()

        if count = 0 then 0.0
        else totalConvergence / float count

    /// Find the best space target for a through ball — cells behind the defensive line
    /// with high attacker influence and low defender coverage.
    let findThroughBallTarget (ctx: AgentContext) : SpaceTarget voption =
        let influence = ctx.Influence
        let attackDir = ctx.Team.AttackDir

        // Find the defensive line position (deepest defender x)
        let mutable deepestDefX =
            match attackDir with
            | LeftToRight -> 0.0f
            | RightToLeft -> float32 PitchLength

        for i = 0 to ctx.Team.OppFrame.SlotCount - 1 do
            match ctx.Team.OppFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let dx = ctx.Team.OppFrame.Physics.PosX[i]
                let isDeeper =
                    match attackDir with
                    | LeftToRight -> dx > deepestDefX
                    | RightToLeft -> dx < deepestDefX
                if isDeeper then deepestDefX <- dx
            | _ -> ()

        // Push the through ball line slightly behind the defensive line
        let throughBallLine =
            match attackDir with
            | LeftToRight -> deepestDefX + 5.0f
            | RightToLeft -> deepestDefX - 5.0f

        let mutable bestTarget: SpaceTarget voption = ValueNone
        let mutable bestScore = 0.0

        for cell = 0 to GridSize - 1 do
            let cx, _ = cellToCenter cell
            let isBehindLine =
                match attackDir with
                | LeftToRight -> cx > throughBallLine
                | RightToLeft -> cx < throughBallLine

            if isBehindLine then
                let score = scoreCell cell influence ctx.MyPos attackDir

                // Bonus for being in the "through ball zone"
                let depthBonus =
                    let depth =
                        match attackDir with
                        | LeftToRight -> float cx / pitchLengthM
                        | RightToLeft -> 1.0 - float cx / pitchLengthM
                    if depth > 0.7 then 0.2 else 0.0

                let adjustedScore = score + depthBonus

                if adjustedScore > bestScore then
                    bestScore <- adjustedScore
                    bestTarget <- ValueSome { Cell = cell; Score = adjustedScore; Position = cellCenterPos cell }

        bestTarget

    /// Find the best general space target (not necessarily behind defensive line).
    /// Used for general space passes and off-ball runs.
    let findBestSpaceTarget (ctx: AgentContext) : SpaceTarget voption =
        let influence = ctx.Influence
        let attackDir = ctx.Team.AttackDir

        let mutable bestTarget: SpaceTarget voption = ValueNone
        let mutable bestScore = 0.0

        for cell = 0 to GridSize - 1 do
            let score = scoreCell cell influence ctx.MyPos attackDir

            if score > bestScore then
                bestScore <- score
                bestTarget <- ValueSome { Cell = cell; Score = score; Position = cellCenterPos cell }

        bestTarget

    /// Score a potential space pass to a given cell.
    /// Combines space quality with teammate convergence.
    let spacePassScore (ctx: AgentContext) (targetCell: int) : float<decisionScore> =
        let influence = ctx.Influence
        let attackDir = ctx.Team.AttackDir

        let spaceScore = scoreCell targetCell influence ctx.MyPos attackDir
        let convergence = computeTeammateConvergence ctx targetCell

        // Space pass requires at least some teammate convergence
        let convergenceFactor =
            if convergence < MinTeammateConvergence then 0.3
            elif convergence < 0.4 then 0.6
            else 0.9

        let combined = spaceScore * convergenceFactor

        combined |> LanguagePrimitives.FloatWithMeasure<decisionScore>
