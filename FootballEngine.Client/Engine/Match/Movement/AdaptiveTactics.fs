namespace FootballEngine.Movement

// AdaptiveTactics.fs — lógica de táctica adaptativa.
// Usa AdaptiveState canónico de FootballEngine (MatchStateTypes.fs).
// AdaptiveState.Records es PatternRecord[] (array, no Map).

open FootballEngine
open FootballEngine.PhysicsContract

module AdaptiveTactics =

    let recordAttempt (pattern: AttackPattern) (result: PatternResult) (state: AdaptiveState) : AdaptiveState =
        { Records =
            state.Records
            |> Array.map (fun r ->
                if r.Pattern <> pattern then
                    r
                else
                    match result with
                    | SuccessfulXG xg ->
                        { r with
                            Attempts = r.Attempts + 1
                            Successes = r.Successes + 1
                            TotalXG = r.TotalXG + xg }
                    | LostPossession -> { r with Attempts = r.Attempts + 1 }
                    | StillInProgress -> r) }

    let toTacticalDirectives (state: AdaptiveState) (clock: SimulationClock) : Directive list =

        let weakness =
            state.Records
            |> Array.filter (fun r -> r.Attempts >= 3)
            |> Array.sortBy (fun r ->
                if r.Attempts = 0 then
                    1.0
                else
                    1.0 - (float r.Successes / float r.Attempts))
            |> Array.tryHead
            |> Option.map (fun r -> r.Pattern)

        match weakness with
        | None -> []
        | Some pattern ->
            let expiry = clock.AdaptiveRate + 30

            match pattern with
            | LeftFlank -> [ Directive.create Flank 15.0<meter> 20.0<meter> 0.6 0.5 expiry "adaptive" Directive.tacticalPriority ]
            | RightFlank -> [ Directive.create Flank 15.0<meter> 80.0<meter> 0.6 0.5 expiry "adaptive" Directive.tacticalPriority ]
            | Central -> [ Directive.create Support 50.0<meter> 50.0<meter> 0.5 0.4 expiry "adaptive" Directive.tacticalPriority ]
            | AttackPattern.LongBall -> [ Directive.create Run 60.0<meter> 50.0<meter> 0.5 0.6 expiry "adaptive" Directive.tacticalPriority ]
            | ShortPass -> [ Directive.create Support 40.0<meter> 50.0<meter> 0.4 0.3 expiry "adaptive" Directive.tacticalPriority ]
