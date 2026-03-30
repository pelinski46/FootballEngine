namespace FootballEngine

open System
open FSharp.Stats
open FSharp.Stats.Distributions

module Stats =
    let clamp (lo: int) hi v = Math.Clamp(v, lo, hi)

    let rollProbability () : float = Continuous.Uniform.Sample 0.0 1.0

    let uniformSample (lo: float) (hi: float) : float = Continuous.Uniform.Sample lo hi

    let normalSample (mean: float) (stdDev: float) : float = Continuous.Normal.Sample mean stdDev

    let normalInt (mean: float) (stdDev: float) (lo: int) (hi: int) =
        Continuous.Normal.Sample mean stdDev
        |> Math.Round
        |> int
        |> fun n -> Math.Clamp(n, lo, hi)

    let normalFloat (mean: float) (stdDev: float) (lo: float) (hi: float) =
        Continuous.Normal.Sample mean stdDev |> fun n -> Math.Clamp(n, lo, hi)

    let normalFloatUnbounded (mean: float) (stdDev: float) = Continuous.Normal.Sample mean stdDev

    let betaInt (alpha: float) (beta: float) (lo: int) (hi: int) =
        Continuous.Beta.Sample alpha beta
        |> fun s -> lo + int (Math.Round(s * float (hi - lo)))
        |> fun n -> Math.Clamp(n, lo, hi)

    let bernoulli (p: float) : bool =
        (Discrete.Bernoulli.Init p).Sample() = 1

    let pickWeighted (weights: (float * 'a) list) : 'a =
        let total = weights |> List.sumBy fst
        let roll = Continuous.Uniform.Sample 0.0 total

        weights
        |> List.fold
            (fun (acc, chosen) (w, v) ->
                match chosen with
                | Some _ -> acc, chosen
                | None ->
                    let acc' = acc + w
                    if roll <= acc' then acc', Some v else acc', None)
            (0.0, None)
        |> snd
        |> Option.defaultWith (fun () -> weights |> List.last |> snd)

    let setSeed (seed: int) =
        Random.SetSampleGenerator(Random.RandThreadSafe(seed))

    let clearSeed () =
        Random.SetSampleGenerator(Random.RandThreadSafe())

    let exponentialSample (mean: float) : float =
        Continuous.Exponential.Sample(1.0 / mean)

    let betaSample (mean: float) (concentration: float) : float =
        let alpha = Math.Max(0.01, mean * concentration)
        let beta' = Math.Max(0.01, (1.0 - mean) * concentration)
        Continuous.Beta.Sample alpha beta'

    let logistic (x: float) : float = 1.0 / (1.0 + exp (-x))

    let logisticBernoulli (score: float) (steepness: float) : bool =
        bernoulli (logistic (score * steepness))

    let poissonSample (lambda: float) : int = Discrete.Poisson.Sample lambda
