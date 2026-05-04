namespace FootballEngine.Types

open FootballEngine.Domain
open FootballEngine.Types.PhysicsContract


// ============================================================
// Influence Map types — spatial discretization for AI awareness
// ============================================================

module InfluenceTypes =

    // Grid resolution: 10x7 cells (~10.5m x ~9.7m each on a 105x68 pitch)
    [<Literal>]
    let GridCols = 10
    [<Literal>]
    let GridRows = 7
    [<Literal>]
    let GridSize = GridCols * GridRows

    let CellWidth = float32 PitchLength / float32 GridCols
    let CellHeight = float32 PitchWidth / float32 GridRows

    /// Influence value for a single cell: positive = team control, negative = opponent control
    type InfluenceValue = float32

    /// Flat array representing the grid, row-major order
    type InfluenceGrid = InfluenceValue[]

    /// Pre-computed influence data for both teams, updated at cognitive tick frequency
    type InfluenceFrame =
        { /// Home team's influence on each cell
          HomeGrid: InfluenceGrid
          /// Away team's influence on each cell
          AwayGrid: InfluenceGrid
          /// Combined: HomeGrid - AwayGrid (positive = home advantage, negative = away advantage)
          ContestedGrid: InfluenceGrid
          /// For each cell, which team controls it (0 = contested, 1 = home, 2 = away)
          ControlGrid: byte[]
          /// Attacker's perspective: how safe is a pass to each cell? (0.0 = blocked, 1.0 = clear)
          AttackerPassSafety: float32[]
          /// Defender's perspective: how well covered is each cell? (0.0 = open, 1.0 = covered)
          DefenderCoverage: float32[] }

    let emptyInfluenceFrame () =
        { HomeGrid = Array.zeroCreate GridSize
          AwayGrid = Array.zeroCreate GridSize
          ContestedGrid = Array.zeroCreate GridSize
          ControlGrid = Array.zeroCreate GridSize
          AttackerPassSafety = Array.zeroCreate GridSize
          DefenderCoverage = Array.zeroCreate GridSize }

    /// Convert a pitch position (meters) to a grid cell index
    let posToCell (x: float32) (y: float32) : int =
        let col = max 0 (min (GridCols - 1) (int (x / CellWidth)))
        let row = max 0 (min (GridRows - 1) (int (y / CellHeight)))
        row * GridCols + col

    /// Convert a grid cell index to the center position (meters)
    let cellToCenter (cell: int) : float32 * float32 =
        let row = cell / GridCols
        let col = cell % GridCols
        let cx = (float32 col + 0.5f) * CellWidth
        let cy = (float32 row + 0.5f) * CellHeight
        cx, cy

    /// Get neighboring cell indices (up to 8, excluding out-of-bounds)
    let neighbors (cell: int) : int[] =
        let row = cell / GridCols
        let col = cell % GridCols
        let mutable count = 0
        let result = Array.zeroCreate<int> 8
        for dr = -1 to 1 do
            for dc = -1 to 1 do
                if dr <> 0 || dc <> 0 then
                    let nr = row + dr
                    let nc = col + dc
                    if nr >= 0 && nr < GridRows && nc >= 0 && nc < GridCols then
                        result[count] <- nr * GridCols + nc
                        count <- count + 1
        result.[..count-1]

    /// Gaussian kernel weight based on distance from cell center to player position
    let gaussianWeight (cellCenterX: float32) (cellCenterY: float32) (playerX: float32) (playerY: float32) (sigma: float32) : float32 =
        let dx = cellCenterX - playerX
        let dy = cellCenterY - playerY
        let distSq = dx * dx + dy * dy
        let sigmaSq = sigma * sigma
        // exp(-d² / 2σ²)
        System.MathF.Exp(-distSq / (2.0f * sigmaSq))

    /// Score a single cell for attacking space quality.
    /// Pure function — no AgentContext needed.
    let scoreCellRaw
        (cell: int)
        (influence: InfluenceFrame)
        (myX: float32) (myY: float32)
        (attackDir: AttackDir)
        : float =

        let attSafety = influence.AttackerPassSafety[cell]
        let defCoverage = influence.DefenderCoverage[cell]
        let contested = influence.ContestedGrid[cell]

        let safetyScore = float attSafety
        let opennessScore = 1.0 - float defCoverage
        let contestedBonus = max 0.0 (float contested) * 0.5

        let cx, _ = cellToCenter cell
        let forwardness =
            match attackDir with
            | LeftToRight -> float cx / float (GridCols * int CellWidth)
            | RightToLeft -> 1.0 - float cx / float (GridCols * int CellWidth)

        let forwardBonus = forwardness * 0.25

        let dx = float (cx - myX)
        let _, cy = cellToCenter cell
        let dy = float (cy - myY)
        let dist = sqrt (dx * dx + dy * dy)
        let distScore =
            if dist < 5.0 then 0.2
            elif dist < 35.0 then 0.6
            elif dist < 50.0 then 0.3
            else 0.0

        safetyScore * 0.30 + opennessScore * 0.25 + contestedBonus * 0.15
        + forwardBonus * 0.15 + distScore * 0.15
