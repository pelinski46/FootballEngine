namespace FootballEngine

open PhysicsContract

// ⚠️ WRITE ACCESS: Solo MovementEngine, BallAgent, ManagerAgent
// Si estás leyendo esto desde otro módulo, NO deberías estar acá.
module FrameMutate =

    let setPos (frame: TeamFrame) (idx: int) (x: float<meter>) (y: float<meter>) : unit =
        frame.PosX[idx] <- float32 x
        frame.PosY[idx] <- float32 y

    let setVel (frame: TeamFrame) (idx: int) (vx: float<meter/second>) (vy: float<meter/second>) : unit =
        frame.VelX[idx] <- float32 vx
        frame.VelY[idx] <- float32 vy

    let setCondition (frame: TeamFrame) (idx: int) (cond: int) : unit =
        frame.Condition[idx] <- byte cond

    let setIntent (frame: TeamFrame) (idx: int) (kind: IntentKind) (tx: float32) (ty: float32) (tpid: int) : unit =
        frame.IntentKind[idx] <- kind
        frame.IntentTargetX[idx] <- tx
        frame.IntentTargetY[idx] <- ty
        frame.IntentTargetPid[idx] <- tpid

    let setOccupancy (frame: TeamFrame) (idx: int) (occ: OccupancyKind) : unit =
        frame.Occupancy[idx] <- occ

    let setCachedTarget (frame: TeamFrame) (idx: int) (x: float<meter>) (y: float<meter>) : unit =
        frame.CachedTargetX[idx] <- float32 x
        frame.CachedTargetY[idx] <- float32 y

    let setCachedExecution (frame: TeamFrame) (idx: int) (exec: float) : unit =
        frame.CachedExecution[idx] <- float32 exec

    let setMental (frame: TeamFrame) (idx: int) (composure: float) (confidence: float) (aggression: float) : unit =
        frame.ComposureLevel[idx] <- float32 composure
        frame.ConfidenceLevel[idx] <- float32 confidence
        frame.AggressionLevel[idx] <- float32 aggression
