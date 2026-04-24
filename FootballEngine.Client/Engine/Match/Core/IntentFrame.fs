namespace FootballEngine

open PhysicsContract

module IntentFrame =

    let fromMovementIntent (intent: MovementIntent) : IntentKind * float32 * float32 * int =
        match intent with
        | MaintainShape sp -> (IntentKind.MaintainShape, float32 sp.X, float32 sp.Y, 0)
        | MarkMan (pid, pos) -> (IntentKind.MarkMan, float32 pos.X, float32 pos.Y, pid)
        | PressBall pos -> (IntentKind.PressBall, float32 pos.X, float32 pos.Y, 0)
        | ExecuteRun run ->
            let t = RunAssignment.progress 0 run
            let x, y = RunAssignment.evaluateTrajectory t run.Trajectory
            (IntentKind.ExecuteRun, float32 x, float32 y, run.PlayerId)
        | CoverSpace sp -> (IntentKind.CoverSpace, float32 sp.X, float32 sp.Y, 0)
        | SupportAttack sp -> (IntentKind.SupportAttack, float32 sp.X, float32 sp.Y, 0)
        | RecoverBall pos -> (IntentKind.RecoverBall, float32 pos.X, float32 pos.Y, 0)

    let toMovementIntent (kind: IntentKind) (tx: float32) (ty: float32) (tpid: int) (fallback: Spatial) : MovementIntent option =
        let sp = {
            X = float tx * 1.0<meter>
            Y = float ty * 1.0<meter>
            Z = 0.0<meter>
            Vx = 0.0<meter/second>
            Vy = 0.0<meter/second>
            Vz = 0.0<meter/second>
        }
        match kind with
        | IntentKind.MaintainShape -> Some (MaintainShape sp)
        | IntentKind.MarkMan -> Some (MarkMan (tpid, sp))
        | IntentKind.PressBall -> Some (PressBall sp)
        | IntentKind.ExecuteRun -> None
        | IntentKind.CoverSpace -> Some (CoverSpace sp)
        | IntentKind.SupportAttack -> Some (SupportAttack sp)
        | IntentKind.RecoverBall -> Some (RecoverBall sp)
        | IntentKind.Idle
        | _ -> Some (MaintainShape fallback)
