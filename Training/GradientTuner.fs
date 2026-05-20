namespace Training

open FootballEngine
open FootballEngine.Domain
open FootballEngine.ML
open FootballEngine.Simulation

module GradientTuner =

    type ParamPerturbation = {
        Name: string
        Getter: EngineWeights -> float
        Setter: EngineWeights -> float -> EngineWeights
        Min: float
        Max: float
    }

    let private clamp value min max =
        if value < min then min elif value > max then max else value

    let private setDuelSteepness (w: EngineWeights) (v: float) =
        { w with Outcomes = { w.Outcomes with Duel = { w.Outcomes.Duel with DuelSteepness = v } } }

    let private setShotOnTargetBase (w: EngineWeights) (v: float) =
        { w with Outcomes = { w.Outcomes with Shot = { w.Outcomes.Shot with OnTargetBase = v } } }

    let private setShotDistDecay (w: EngineWeights) (v: float) =
        { w with Outcomes = { w.Outcomes with Shot = { w.Outcomes.Shot with OnTargetDistDecayRate = v } } }

    let private setHomeDuelBonus (w: EngineWeights) (v: float) =
        { w with HomeAdvantage = { w.HomeAdvantage with DuelAttackBonus = v } }

    let private setHomePassBonus (w: EngineWeights) (v: float) =
        { w with HomeAdvantage = { w.HomeAdvantage with PassAccuracyBonus = v } }

    let private setSoftmaxTemp (w: EngineWeights) (v: float) =
        { w with Individual = { w.Individual with SoftmaxTemperature = v } }

    let private setDirectnessBlend (w: EngineWeights) (v: float) =
        { w with Individual = { w.Individual with DirectnessBlendTactic = v } }

    let private setFinishWeight (w: EngineWeights) (v: float) =
        { w with Individual = { w.Individual with Shoot = { w.Individual.Shoot with FinishingWeight = v } } }

    let private setPassWeight (w: EngineWeights) (v: float) =
        { w with Individual = { w.Individual with Pass = { w.Individual.Pass with PassingWeight = v } } }

    let private setDribbleWeight (w: EngineWeights) (v: float) =
        { w with Individual = { w.Individual with Dribble = { w.Individual.Dribble with DribblingWeight = v } } }

    let private setTackleSteepness (w: EngineWeights) (v: float) =
        { w with Outcomes = { w.Outcomes with Tackle = { w.Outcomes.Tackle with TackleSteepness = v } } }

    let private setFoulBeta (w: EngineWeights) (v: float) =
        { w with Outcomes = { w.Outcomes with Tackle = { w.Outcomes.Tackle with FoulShapeBeta = v } } }

    let private setCardBase (w: EngineWeights) (v: float) =
        { w with Referee = { w.Referee with CardBaseProb = v } }

    let private setInjuryBase (w: EngineWeights) (v: float) =
        { w with Referee = { w.Referee with InjuryBaseProb = v } }

    let private tunableParams: ParamPerturbation list = [
        { Name = "DuelSteepness"; Getter = (fun w -> w.Outcomes.Duel.DuelSteepness); Setter = setDuelSteepness; Min = 0.8; Max = 2.0 }
        { Name = "ShotOnTargetBase"; Getter = (fun w -> w.Outcomes.Shot.OnTargetBase); Setter = setShotOnTargetBase; Min = 0.30; Max = 0.55 }
        { Name = "ShotDistDecay"; Getter = (fun w -> w.Outcomes.Shot.OnTargetDistDecayRate); Setter = setShotDistDecay; Min = 5.0; Max = 30.0 }
        { Name = "HomeDuelBonus"; Getter = (fun w -> w.HomeAdvantage.DuelAttackBonus); Setter = setHomeDuelBonus; Min = 0.0; Max = 10.0 }
        { Name = "HomePassBonus"; Getter = (fun w -> w.HomeAdvantage.PassAccuracyBonus); Setter = setHomePassBonus; Min = 0.0; Max = 0.2 }
        { Name = "SoftmaxTemp"; Getter = (fun w -> w.Individual.SoftmaxTemperature); Setter = setSoftmaxTemp; Min = 0.01; Max = 1.0 }
        { Name = "DirectnessBlend"; Getter = (fun w -> w.Individual.DirectnessBlendTactic); Setter = setDirectnessBlend; Min = 0.0; Max = 1.0 }
        { Name = "FinishWeight"; Getter = (fun w -> w.Individual.Shoot.FinishingWeight); Setter = setFinishWeight; Min = 0.0; Max = 1.0 }
        { Name = "PassWeight"; Getter = (fun w -> w.Individual.Pass.PassingWeight); Setter = setPassWeight; Min = 0.0; Max = 1.0 }
        { Name = "DribbleWeight"; Getter = (fun w -> w.Individual.Dribble.DribblingWeight); Setter = setDribbleWeight; Min = 0.0; Max = 1.0 }
        { Name = "TackleSteepness"; Getter = (fun w -> w.Outcomes.Tackle.TackleSteepness); Setter = setTackleSteepness; Min = 0.5; Max = 3.0 }
        { Name = "FoulBeta"; Getter = (fun w -> w.Outcomes.Tackle.FoulShapeBeta); Setter = setFoulBeta; Min = 1.0; Max = 20.0 }
        { Name = "CardBase"; Getter = (fun w -> w.Referee.CardBaseProb); Setter = setCardBase; Min = 0.0; Max = 0.05 }
        { Name = "InjuryBase"; Getter = (fun w -> w.Referee.InjuryBaseProb); Setter = setInjuryBase; Min = 0.0; Max = 0.005 }
    ]

    let tune
        (epsilon: float)
        (learningRate: float)
        (currentWeights: EngineWeights)
        (errorFn: EngineWeights -> float)
        : EngineWeights =
        let baseError = errorFn currentWeights

        let adjusted =
            tunableParams
            |> List.fold (fun w param ->
                let current = param.Getter w
                let perturbed = current + epsilon
                let clampedPerturbed = clamp perturbed param.Min param.Max
                let wPlus = param.Setter w clampedPerturbed
                let errorPlus = errorFn wPlus

                let gradient = (errorPlus - baseError) / epsilon

                let newValue = current - learningRate * gradient
                let clampedNew = clamp newValue param.Min param.Max
                param.Setter w clampedNew) currentWeights

        adjusted
