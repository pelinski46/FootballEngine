namespace FootballEngine.Movement

open FootballEngine.Domain

module EventWindow =

    open FootballEngine

    let recentEvents windowSeconds (reversedEvents: MatchEvent list) =
        match reversedEvents with
        | [] -> []
        | first :: _ ->
            let cutoff = first.Second - windowSeconds
            reversedEvents |> List.takeWhile (fun (e: MatchEvent) -> e.Second >= cutoff)

    let shortPassSuccessRate (events: MatchEvent list) =
        let passes =
            events
            |> List.filter (fun (e: MatchEvent) ->
                match e.Type with
                | MatchEventType.PassCompleted _
                | MatchEventType.PassIncomplete _ -> true
                | _ -> false)

        if List.isEmpty passes then
            0.5
        else
            let successes =
                passes
                |> List.filter (fun (e: MatchEvent) ->
                    match e.Type with
                    | MatchEventType.PassCompleted _ -> true
                    | _ -> false)
                |> List.length

            float successes / float passes.Length

    let pressSuccessRate (events: MatchEvent list) =
        let pressEvents =
            events
            |> List.filter (fun (e: MatchEvent) ->
                match e.Type with
                | MatchEventType.TackleSuccess
                | MatchEventType.TackleFail
                | MatchEventType.DribbleSuccess
                | MatchEventType.DribbleFail -> true
                | _ -> false)

        if List.isEmpty pressEvents then
            0.5
        else
            let successes =
                pressEvents
                |> List.filter (fun (e: MatchEvent) ->
                    match e.Type with
                    | MatchEventType.TackleSuccess
                    | MatchEventType.DribbleFail -> true
                    | _ -> false)
                |> List.length

            float successes / float pressEvents.Length

    let flankSuccessRate dir (events: MatchEvent list) =
        let flankEvents =
            events
            |> List.filter (fun (e: MatchEvent) ->
                match e.Type with
                | MatchEventType.CrossAttempt _ -> true
                | _ -> false)

        if List.isEmpty flankEvents then
            0.5
        else
            let successes =
                flankEvents
                |> List.filter (fun (e: MatchEvent) ->
                    match e.Type with
                    | MatchEventType.CrossAttempt true -> true
                    | _ -> false)
                |> List.length

            float successes / float flankEvents.Length

    let avgCondition (teamSide: TeamSide) =
        if Array.isEmpty teamSide.Conditions then
            50.0
        else
            teamSide.Conditions |> Array.averageBy float

    let consecutivePossessionLosses (events: MatchEvent list) =
        let rec loop (evts: MatchEvent list) count =
            match evts with
            | [] -> count
            | e :: rest ->
                match e.Type with
                | PassIncomplete _
                | DribbleFail
                | FoulCommitted -> loop rest (count + 1)
                | _ -> count

        loop events 0

    let patternResults pattern (events: MatchEvent list) =
        let relevant =
            events
            |> List.filter (fun (e: MatchEvent) ->
                match e.Type with
                | MatchEventType.PassCompleted _
                | MatchEventType.PassIncomplete _
                | MatchEventType.LongBall _
                | MatchEventType.ShotBlocked
                | MatchEventType.ShotOffTarget
                | MatchEventType.Save
                | MatchEventType.Goal -> true
                | _ -> false)

        let isPatternMatch (evType: MatchEventType) =
            match pattern with
            | AttackPattern.LeftFlank
            | AttackPattern.RightFlank ->
                match evType with
                | MatchEventType.CrossAttempt _ -> true
                | _ -> false
            | AttackPattern.Central ->
                match evType with
                | MatchEventType.PassCompleted _
                | MatchEventType.PassIncomplete _ -> true
                | _ -> false
            | AttackPattern.LongBall ->
                match evType with
                | MatchEventType.LongBall _ -> true
                | _ -> false
            | AttackPattern.ShortPass ->
                match evType with
                | MatchEventType.PassCompleted _
                | MatchEventType.PassIncomplete _ -> true
                | _ -> false

        let attempts =
            relevant |> List.filter (fun e -> isPatternMatch e.Type) |> List.length

        let successes =
            relevant
            |> List.filter (fun e ->
                isPatternMatch e.Type
                && match e.Type with
                   | MatchEventType.PassCompleted _
                   | MatchEventType.CrossAttempt true
                   | MatchEventType.LongBall true
                   | MatchEventType.Goal -> true
                   | _ -> false)
            |> List.length

        let totalXG =
            relevant
            |> List.sumBy (fun (e: MatchEvent) ->
                match e.Type with
                | MatchEventType.Goal -> 0.3
                | MatchEventType.ShotOffTarget
                | MatchEventType.ShotBlocked
                | MatchEventType.Save -> 0.1
                | _ -> 0.0)

        { Pattern = pattern
          Attempts = max attempts 1
          Successes = successes
          TotalXG = totalXG }
