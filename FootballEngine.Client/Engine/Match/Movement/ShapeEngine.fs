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

        let n = basePositions.Length
        let result = Array.zeroCreate<(float * float)> n

        let tacticalPush = tacticsCfg.ForwardPush * forwardDir
        let defensiveDrop = tacticsCfg.DefensiveDrop * forwardDir
        let ballPullBase = (ballX - 50.0) * 0.15 * forwardDir
        let pressureCoeff = tacticsCfg.PressureDistance * 0.01

        for i = 0 to n - 1 do
            let basePos = basePositions[i]
            let ballPullX = ballPullBase

            let compactShift =
                (basePos.Y - 50.0) * pressureCoeff

            let x = basePos.X + phaseShift + tacticalPush + defensiveDrop + ballPullX
            let y = basePos.Y + compactShift

            result[i] <-
                (System.Math.Clamp(x, 2.0, 98.0),
                 System.Math.Clamp(y, 2.0, 98.0))

        result
