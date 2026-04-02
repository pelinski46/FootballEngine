namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchStateOps
open FootballEngine.Stats


module MatchManager =

    type ManagerIntent =
        | MakeSubstitution of clubId: ClubId * squadPlayers: Player list
        | HoldLine
        | ManagerIdle

    let private maxSubs = 3

    type Situation =
        | Winning
        | Drawing
        | Losing

    let situation (clubId: ClubId) (s: MatchState) =
        match goalDiff clubId s with
        | d when d > 0 -> Winning
        | d when d < 0 -> Losing
        | _ -> Drawing

    let urgency (clubId: ClubId) (s: MatchState) : float =
        let late = s.Second > 60 * 60
        let ts = side clubId s
        let tacticsCfg = tacticsConfig ts.Tactics ts.Instructions

        match situation clubId s, late with
        | Losing, true -> 1.35 * tacticsCfg.UrgencyMultiplier
        | Losing, false -> 1.15 * tacticsCfg.UrgencyMultiplier
        | Winning, _ -> 0.85 * tacticsCfg.UrgencyMultiplier
        | Drawing, true -> 1.10 * tacticsCfg.UrgencyMultiplier
        | Drawing, false -> 1.00 * tacticsCfg.UrgencyMultiplier

    let private conditionThreshold =
        function
        | Losing -> 75
        | Drawing -> 65
        | Winning -> 55

    let private preferredPositions =
        function
        | Losing -> [ ST; AML; AMR; AMC ]
        | Drawing -> [ MC; AMC; ST ]
        | Winning -> [ DC; DM; MC ]

    let private pickOutgoing (ts: TeamSide) (threshold: int) : int option =
        activeIndices ts.Players ts.Sidelined
        |> Array.filter (fun i -> ts.Players[i].Position <> GK && ts.Conditions[i] < threshold)
        |> Array.sortBy (fun i -> ts.Conditions[i])
        |> Array.tryHead

    let private pickIncoming
        (squadPlayers: Player list)
        (manager: Staff)
        (ts: TeamSide)
        (preferred: Position list)
        : Player option =
        let startingIds =
            manager.Attributes.Coaching.Lineup
            |> Option.map (fun lu -> lu.Slots |> List.choose _.PlayerId |> Set.ofList)
            |> Option.defaultValue Set.empty

        let onPitchIds = ts.Players |> Array.map _.Id |> Set.ofArray

        let bench =
            squadPlayers
            |> List.filter (fun p ->
                not (Set.contains p.Id startingIds)
                && not (Set.contains p.Id onPitchIds)
                && not (Map.containsKey p.Id ts.Sidelined)
                && p.Status = Available)

        bench
        |> List.filter (fun p -> List.contains p.Position preferred)
        |> List.sortByDescending _.CurrentSkill
        |> List.tryHead
        |> Option.orElseWith (fun () -> bench |> List.sortByDescending _.CurrentSkill |> List.tryHead)

    let decide (second: int) (homeSquad: Player list) (awaySquad: Player list) (s: MatchState) : ManagerIntent list =
        let isSubMinute = second = 60 * 60 || second = 75 * 60 || second = 85 * 60

        if not isSubMinute then
            [ ManagerIdle ]
        else
            [ let homeCoachRating = float s.HomeCoach.CurrentSkill / 100.0
              let awayCoachRating = float s.AwayCoach.CurrentSkill / 100.0

              if s.HomeSide.SubsUsed < maxSubs && bernoulli (0.7 + homeCoachRating * 0.2) then
                  yield MakeSubstitution(s.Home.Id, homeSquad)

              if s.AwaySide.SubsUsed < maxSubs && bernoulli (0.7 + awayCoachRating * 0.2) then
                  yield MakeSubstitution(s.Away.Id, awaySquad) ]

    let resolve (second: int) (intent: ManagerIntent) (s: MatchState) : MatchState * MatchEvent list =
        match intent with
        | ManagerIdle
        | HoldLine -> s, []
        | MakeSubstitution(clubId, squadPlayers) ->
            let ts = side clubId s
            let coach = if clubId = s.Home.Id then s.HomeCoach else s.AwayCoach

            if ts.SubsUsed >= maxSubs then
                s, []
            else
                let sit = situation clubId s

                match pickOutgoing ts (conditionThreshold sit) with
                | None -> s, []
                | Some outIdx ->
                    let playerOut = ts.Players[outIdx]

                    match pickIncoming squadPlayers coach ts (preferredPositions sit) with
                    | None -> s, []
                    | Some incoming ->
                        let inheritedPos = ts.Positions[outIdx]

                        let ts' =
                            { ts with
                                Players = Array.append ts.Players [| incoming |]
                                Conditions = Array.append ts.Conditions [| incoming.Condition |]
                                Positions = Array.append ts.Positions [| inheritedPos |]
                                BasePositions = Array.append ts.BasePositions [| inheritedPos |]
                                Sidelined = Map.add playerOut.Id SidelinedBySub ts.Sidelined
                                SubsUsed = ts.SubsUsed + 1 }

                        withSide clubId ts' s,
                        [ { Second = second
                            PlayerId = playerOut.Id
                            ClubId = clubId
                            Type = SubstitutionOut }
                          { Second = second
                            PlayerId = incoming.Id
                            ClubId = clubId
                            Type = SubstitutionIn } ]
