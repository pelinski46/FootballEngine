namespace FootballEngine

open System
open FSharp.Stats
open FSharp.Stats.Distributions

// Stats es el motor de imperfección humana y variación física del sistema de agentes.
// Cada función modela por qué los jugadores no rinden siempre al 100% de sus atributos.
module Stats =

    // ── Primitivas base ────────────────────────────────────────────────────────

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
        let clampedP = if Double.IsNaN p then 0.0 else Math.Clamp(p, 0.0, 1.0)
        (Discrete.Bernoulli.Init clampedP).Sample() = 1

    let logistic (x: float) : float = 1.0 / (1.0 + exp (-x))

    let logisticBernoulli (score: float) (steepness: float) : bool =
        bernoulli (logistic (score * steepness))

    let betaSample (mean: float) (concentration: float) : float =
        let alpha = Math.Max(0.01, mean * concentration)
        let beta' = Math.Max(0.01, (1.0 - mean) * concentration)
        Continuous.Beta.Sample alpha beta'

    let poissonSample (lambda: float) : int = Discrete.Poisson.Sample lambda

    let exponentialSample (mean: float) : float =
        Continuous.Exponential.Sample(1.0 / mean)

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

    // ── Imperfección humana ────────────────────────────────────────────────────
    // Modela por qué un jugador de 80 de pase no siempre completa el pase.
    // La varianza aumenta con la fatiga, la presión y la situación táctica.

    /// Rendimiento efectivo de un atributo dado condición física y estado mental.
    /// La concentración sube cuando hay frescos, baja cuando están agotados.
    let humanPerformance (stat: int) (condition: int) (morale: int) : float =
        let base' = float stat * (float condition / 100.0) * (0.8 + float morale / 500.0)
        // Un jugador fresco varía menos; uno agotado es impredecible
        let variability = 1.0 + (1.0 - float condition / 100.0) * 0.4
        normalSample base' (variability * 0.6)

    /// Variación física pura — el cuerpo no responde igual cada vez.
    /// Modela el "buen día / mal día" independiente de los atributos.
    let physicalVariation (condition: int) : float =
        let base' = float condition / 100.0
        // Beta da asimetría natural: es más fácil rendir al 60% que al 100%
        betaSample base' (5.0 + base' * 10.0)

    /// Imprecisión bajo presión — cuanta más urgencia táctica, más varianza.
    /// Un jugador con mucha composure resiste mejor la presión.
    let pressureNoise (composure: int) (urgencyMultiplier: float) : float =
        let resistance = float composure / 100.0
        let noise = (urgencyMultiplier - 1.0) * (1.0 - resistance) * 15.0
        normalSample 0.0 (Math.Max(0.5, noise))

    /// Consistencia por posición — un DC comete menos errores graves que un extremo.
    /// Modela la naturaleza de cada rol: defensores más conservadores, atacantes más arriesgados.
    let positionalConsistency (position: FootballEngine.Domain.Position) : float =
        match position with
        | FootballEngine.Domain.GK -> betaSample 0.85 12.0
        | FootballEngine.Domain.DC
        | FootballEngine.Domain.DM -> betaSample 0.80 10.0
        | FootballEngine.Domain.DL
        | FootballEngine.Domain.DR
        | FootballEngine.Domain.WBL
        | FootballEngine.Domain.WBR -> betaSample 0.75 8.0
        | FootballEngine.Domain.MC -> betaSample 0.72 8.0
        | FootballEngine.Domain.ML
        | FootballEngine.Domain.MR -> betaSample 0.68 7.0
        | FootballEngine.Domain.AML
        | FootballEngine.Domain.AMR
        | FootballEngine.Domain.AMC -> betaSample 0.65 6.0
        | FootballEngine.Domain.ST -> betaSample 0.60 5.0

    /// Variación de energía durante el partido — los jugadores se fatigan de forma no lineal.
    /// El esfuerzo en pressing consume más que el juego posicional.
    let fatigueVariation (stamina: int) (isPressing: bool) : float =
        let base' = float stamina / 100.0
        let pressingPenalty = if isPressing then betaSample 0.3 5.0 else 0.0
        normalSample (base' - pressingPenalty) 0.05 |> fun v -> Math.Clamp(v, 0.0, 1.0)

    /// Momento psicológico — el momentum del partido afecta la toma de decisiones.
    /// Un equipo que va ganando juega con más calma; uno que pierde se apresura.
    let momentumEffect (momentum: float) (isAttacking: bool) : float =
        let m = if isAttacking then momentum else -momentum
        // Momentum positivo reduce varianza (más calma), negativo la aumenta
        normalSample (m * 0.5) (3.0 + Math.Abs(m) * 0.5)
