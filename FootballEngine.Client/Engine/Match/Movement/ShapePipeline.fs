namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

module ShapePipeline =

    let calculate
        (basePositions: Spatial[])
        (dir: AttackDir)
        (phase: MatchPhase)
        (ballX: float<meter>)
        (tacticsCfg: TacticsConfig)
        (currentSubTick: int)
        : Directive[] =


        let shapeTargets =
            ShapeEngine.computeShapeTargets basePositions dir phase ballX tacticsCfg

        let expiry = currentSubTick + 600
        let result = System.Collections.Generic.List<Directive>()

        for i = 0 to shapeTargets.Length - 1 do
            let (targetX, targetY) = shapeTargets[i]


            result.Add(
                Directive.create Shape targetX targetY 0.4 0.5 expiry "shape-pipeline" Directive.strategicPriority
            )

        result.ToArray()
