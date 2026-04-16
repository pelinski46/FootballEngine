namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

module ShapeEngine =

    let ShapeInterval = 15

    let computeShapeTargets
        (basePositions: Spatial[])
        (dir: AttackDir)
        (phase: MatchPhase)
        (ballX: float<meter>)
        (tacticsCfg: TacticsConfig)
        : (float<meter> * float<meter>)[] =

        let forwardDir = forwardX dir

        let pressMod = tacticsCfg.PressingIntensity - 1.0

        let phaseShift =
            match phase with
            | BuildUp -> (-8.0<meter> - pressMod * 4.0<meter>) * forwardDir
            | Midfield -> pressMod * 2.0<meter> * forwardDir
            | Attack -> (5.0<meter> + pressMod * 3.0<meter>) * forwardDir

        let n = basePositions.Length
        let result = Array.zeroCreate<(float<meter> * float<meter>)> n

        // tacticsCfg.ForwardPush/DefensiveDrop are numeric offsets in metres (dimensionless values), convert to metre units
        let tacticalPush = tacticsCfg.ForwardPush * 1.0<meter> * forwardDir
        let defensiveDrop = tacticsCfg.DefensiveDrop * 1.0<meter> * forwardDir
        let ballPullBase = (ballX - HalfwayLineX) * 0.15 * forwardDir
        let pressureCoeff = tacticsCfg.PressureDistance * 0.01

        for i = 0 to n - 1 do
            let basePos = basePositions[i]
            let ballPullX = ballPullBase

            let compactShift =
                (basePos.Y - 34.0<meter>) * pressureCoeff

            let x = basePos.X + phaseShift + tacticalPush + defensiveDrop + ballPullX
            let y = basePos.Y + compactShift

            result[i] <-
                (PhysicsContract.clamp x 2.0<meter> 98.0<meter>,
                 PhysicsContract.clamp y 2.0<meter> 98.0<meter>)

        result
