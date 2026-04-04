namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open MatchStateOps

module ShapeEngine =

    let ShapeInterval = 15

    let computeShapeTargets
        (basePositions: Spatial[])
        (dir: AttackDir)
        (phase: MatchPhase)
        (ballX: float)
        (tacticsCfg: TacticsConfig)
        : (float * float)[] =

        let forwardDir = AttackDir.forwardX dir

        let pressMod = tacticsCfg.PressingIntensity - 1.0

        let phaseShift =
            match phase with
            | BuildUp -> (-8.0 - pressMod * 4.0) * forwardDir
            | Midfield -> pressMod * 2.0 * forwardDir
            | Attack -> (5.0 + pressMod * 3.0) * forwardDir

        basePositions
        |> Array.map (fun basePos ->
            let baseX = basePos.X
            let baseY = basePos.Y

            let tacticalPush = tacticsCfg.ForwardPush * forwardDir
            let defensiveDrop = tacticsCfg.DefensiveDrop * forwardDir

            let ballPullX = (ballX - 50.0) * 0.15 * forwardDir

            let compactShift =
                (baseY - 50.0) * (tacticsCfg.PressureDistance * 0.01)

            let x = baseX + phaseShift + tacticalPush + defensiveDrop + ballPullX
            let y = baseY + compactShift

            let clampedX = System.Math.Clamp(x, 2.0, 98.0)
            let clampedY = System.Math.Clamp(y, 2.0, 98.0)

            (clampedX, clampedY))
