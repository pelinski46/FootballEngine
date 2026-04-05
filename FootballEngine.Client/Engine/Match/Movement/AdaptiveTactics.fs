namespace FootballEngine.Movement

// AdaptiveTactics.fs — lógica de táctica adaptativa.
// Usa AdaptiveState canónico de FootballEngine (MatchStateTypes.fs).
// AdaptiveState.Records es PatternRecord[] (array, no Map).

open FootballEngine
open FootballEngine.Domain

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

    let toTacticalDirectives (state: AdaptiveState) (dir: AttackDir) : Directive list =

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
            let expiry = PhysicsContract.AdaptiveIntervalSubTicks + 30

            match pattern with
            | LeftFlank -> [ Directive.create Flank 15.0 20.0 0.6 0.5 expiry "adaptive" ]
            | RightFlank -> [ Directive.create Flank 15.0 80.0 0.6 0.5 expiry "adaptive" ]
            | Central -> [ Directive.create Support 50.0 50.0 0.5 0.4 expiry "adaptive" ]
            | AttackPattern.LongBall -> [ Directive.create Run 60.0 50.0 0.5 0.6 expiry "adaptive" ]
            | ShortPass -> [ Directive.create Support 40.0 50.0 0.4 0.3 expiry "adaptive" ]
