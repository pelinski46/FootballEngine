namespace FootballEngine.Movement

open FootballEngine.Domain

module AdaptiveTactics =
    let AdaptiveCheckInterval = 60

    type AdaptiveState =
        { Records: Map<AttackPattern, PatternRecord>
          DetectedWeakness: AttackPattern option
          Confidence: float }


    let initial =
        { Records = Map.empty
          DetectedWeakness = None
          Confidence = 0.5 }

    let recordAttempt pattern result state =
        let existing =
            state.Records
            |> Map.tryFind pattern
            |> Option.defaultValue
                { Pattern = pattern
                  Attempts = 0
                  Successes = 0
                  TotalXG = 0.0 }

        let updated =
            match result with
            | SuccessfulXG xg ->
                { existing with
                    Attempts = existing.Attempts + 1
                    Successes = existing.Successes + 1
                    TotalXG = existing.TotalXG + xg }
            | LostPossession ->
                { existing with
                    Attempts = existing.Attempts + 1 }
            | StillInProgress -> existing

        let newState =
            { state with
                Records = state.Records |> Map.add pattern updated }

        let weakness =
            newState.Records
            |> Map.toList
            |> List.filter (fun (_, r) -> r.Attempts >= 3)
            |> List.sortBy (fun (_, r) ->
                if r.Attempts = 0 then
                    1.0
                else
                    1.0 - (float r.Successes / float r.Attempts))
            |> List.tryHead
            |> Option.map fst

        { newState with
            DetectedWeakness = weakness
            Confidence = min 1.0 (state.Confidence + 0.05) }

    let toTacticalDirectives (state: AdaptiveState) (dir: AttackDir) =
        match state.DetectedWeakness with
        | None -> []
        | Some pattern ->
            match pattern with
            | LeftFlank -> [ Directive.create Flank 15.0 20.0 0.6 0.5 (AdaptiveCheckInterval + 30) "adaptive" ]
            | RightFlank -> [ Directive.create Flank 15.0 80.0 0.6 0.5 (AdaptiveCheckInterval + 30) "adaptive" ]
            | Central -> [ Directive.create Support 50.0 50.0 0.5 0.4 (AdaptiveCheckInterval + 30) "adaptive" ]
            | AttackPattern.LongBall ->
                [ Directive.create Run 60.0 50.0 0.5 0.6 (AdaptiveCheckInterval + 30) "adaptive" ]
            | ShortPass -> [ Directive.create Support 40.0 50.0 0.4 0.3 (AdaptiveCheckInterval + 30) "adaptive" ]
