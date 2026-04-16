namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract


[<Measure>] type intention
[<Measure>] type execution
[<Measure>] type outcome

[<Struct>]
type IntentionInput = { Vision: int; Composure: int; Condition: int; Pressure: float }

[<Struct>]
type ExecutionInput = { TechnicalA: int; TechnicalB: int; PhysicalA: int; Condition: int }

module ActionMath =
    let private norm = PhysicsContract.normaliseAttr
    let private cond = PhysicsContract.normaliseCondition

    let calcIntention (i: IntentionInput) : float<intention> =
        // Use vision and composure to estimate tactical intention; decisions field removed to match domain
        let base' = (norm i.Vision * 0.65) + (norm i.Composure * 0.35)
        let penalty = i.Pressure * (1.0 - norm i.Composure) * 0.30
        (base' * cond i.Condition - penalty) |> clampFloat 0.05 1.0 |> LanguagePrimitives.FloatWithMeasure<intention>

    let calcExecution (e: ExecutionInput) : float<execution> =
        let base' = (norm e.TechnicalA * 0.55) + (norm e.TechnicalB * 0.25) + (norm e.PhysicalA * 0.20)
        (base' * sqrt (cond e.Condition)) |> clampFloat 0.05 1.0 |> LanguagePrimitives.FloatWithMeasure<execution>

    let combineScores (i: float<intention>) (e: float<execution>) (noise: float) : float<outcome> =
        let product = float i * float e
        Stats.normalSample product (noise * (1.0 - product)) |> clampFloat 0.0 1.0 |> LanguagePrimitives.FloatWithMeasure<outcome>

    /// Evaluate a weighted feature list with a condition modifier.
    let evalWeighted (features: (float * float) list) (condition: int) (minVal: float) : float =
        let base' = features |> List.sumBy (fun (value, weight) -> value * weight)
        let result = base' * cond condition
        clampFloat minVal 1.0 result
