namespace FootballEngine.Movement


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


