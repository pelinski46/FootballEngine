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


        let tacticalPush = tacticsCfg.ForwardPush * 1.0<meter> * forwardDir
        let defensiveDrop = tacticsCfg.DefensiveDrop * 1.0<meter> * forwardDir
        let ballPullBase = (ballX - HalfwayLineX) * 0.15 * forwardDir
        let pressureCoeff = tacticsCfg.PressureDistance * 0.01

        let damping = 0.2 

        for i = 0 to n - 1 do
            let basePos = basePositions[i]
            let targetPullX = PhysicsContract.clamp (ballPullBase * (float i / float (n - 1))) -8.0<meter> 8.0<meter>
            let ballPullX = targetPullX * damping

            let compactShift =
                (basePos.Y - 34.0<meter>) * pressureCoeff

            let x = basePos.X + phaseShift + tacticalPush + defensiveDrop + ballPullX
            let y = basePos.Y + compactShift

            let finalX = PhysicsContract.clamp x 2.0<meter> 98.0<meter>
            let finalY = PhysicsContract.clamp y 2.0<meter> 98.0<meter>
            


            result[i] <- (finalX, finalY)

        result
