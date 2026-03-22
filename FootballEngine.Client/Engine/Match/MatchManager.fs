namespace FootballEngine

open FootballEngine.Domain
open MatchState

module MatchManager =

    let private maxSubs = 3

    type Situation =
        | Winning
        | Drawing
        | Losing

    let situation (isHome: bool) (s: MatchState) =
        match goalDiff isHome s with
        | d when d > 0 -> Winning
        | d when d < 0 -> Losing
        | _ -> Drawing

    let urgency (isHome: bool) (s: MatchState) : float =
        let late = s.Second > 60 * 60

        match situation isHome s, late with
        | Losing, true -> 1.35
        | Losing, false -> 1.15
        | Winning, _ -> 0.85
        | Drawing, true -> 1.10
        | Drawing, false -> 1.00

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
        (club: Club)
        (ts: TeamSide)
        (preferred: Position list)
        : Player option =
        let startingIds =
            club.CurrentLineup
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

    let processSubstitution (squadPlayers: Player list) (clubId: ClubId) (second: int) (s: MatchState) : MatchState =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s
        let club = if isHome then s.Home else s.Away

        if ts.SubsUsed >= maxSubs then
            s
        else

            let sit = situation isHome s

            match pickOutgoing ts (conditionThreshold sit) with
            | None -> s
            | Some outIdx ->
                let playerOut = ts.Players[outIdx]

                match pickIncoming squadPlayers club ts (preferredPositions sit) with
                | None -> s
                | Some incoming ->
                    let pos =
                        ts.Positions |> Map.tryFind playerOut.Id |> Option.defaultValue (50.0, 50.0)

                    let ts' =
                        { ts with
                            Players = Array.append ts.Players [| incoming |]
                            Conditions = Array.append ts.Conditions [| incoming.Condition |]
                            Sidelined = Map.add playerOut.Id SidelinedBySub ts.Sidelined
                            Positions = Map.add incoming.Id pos ts.Positions
                            BasePositions = Map.add incoming.Id pos ts.BasePositions
                            SubsUsed = ts.SubsUsed + 1 }

                    withSide isHome ts' s
                    |> addEvent
                        { Second = second
                          PlayerId = playerOut.Id
                          ClubId = clubId
                          Type = SubstitutionOut }
                    |> addEvent
                        { Second = second
                          PlayerId = incoming.Id
                          ClubId = clubId
                          Type = SubstitutionIn }
