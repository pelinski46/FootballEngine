namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open MatchStateOps
open SchedulingTypes

module ManagerAgent =

    // Phase 0: Second -> SubTick
    let private event subTick playerId clubId t =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t }

    type Situation =
        | Winning
        | Drawing
        | Losing

    let private situation (clubId: ClubId) (s: MatchState) =
        match goalDiff clubId s with
        | d when d > 0 -> Winning
        | d when d < 0 -> Losing
        | _ -> Drawing

    let urgency (clubId: ClubId) (s: MatchState) : float =
        let late = PhysicsContract.subTicksToSeconds s.SubTick > 60.0 * 60.0
        let ts = side clubId s
        let tacticsCfg = tacticsConfig ts.Tactics ts.Instructions
        match situation clubId s, late with
        | Losing, true  -> 1.35 * tacticsCfg.UrgencyMultiplier
        | Losing, false -> 1.15 * tacticsCfg.UrgencyMultiplier
        | Winning, _    -> 0.85 * tacticsCfg.UrgencyMultiplier
        | Drawing, true -> 1.10 * tacticsCfg.UrgencyMultiplier
        | Drawing, false -> 1.00 * tacticsCfg.UrgencyMultiplier

    let private maxSubs = 3

    let private conditionThreshold =
        function
        | Losing  -> 75
        | Drawing -> 65
        | Winning -> 55

    let private preferredPositions =
        function
        | Losing  -> [ ST; AML; AMR; AMC ]
        | Drawing -> [ MC; AMC; ST ]
        | Winning -> [ DC; DM; MC ]

    let private reactiveTactics (sit: Situation) (current: TeamTactics) : TeamTactics =
        match sit, current with
        | Losing, _  -> TeamTactics.Attacking
        | Drawing, TeamTactics.Defensive -> TeamTactics.Balanced
        | Drawing, TeamTactics.Balanced  -> TeamTactics.Attacking
        | Drawing, _ -> TeamTactics.Balanced
        | Winning, TeamTactics.Attacking -> TeamTactics.Balanced
        | Winning, TeamTactics.Counter   -> TeamTactics.Balanced
        | Winning, _ -> TeamTactics.Defensive

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

    let private trySub
        (clubId: ClubId)
        (s: MatchState)
        (squad: Player list)
        (coach: Staff)
        (sit: Situation)
        : ManagerAction =
        let ts = side clubId s
        if ts.SubsUsed >= maxSubs then
            AdjustTactics(clubId, reactiveTactics sit ts.Tactics)
        else
            match pickOutgoing ts (conditionThreshold sit) with
            | Some outIdx ->
                match pickIncoming squad coach ts (preferredPositions sit) with
                | Some incoming -> MakeSubstitution(clubId, outIdx, incoming)
                | None -> AdjustTactics(clubId, reactiveTactics sit ts.Tactics)
            | None -> AdjustTactics(clubId, reactiveTactics sit ts.Tactics)

    let private handleFatigueAlert (clubId: ClubId) (s: MatchState) (squad: Player list) (coach: Staff) : ManagerAction =
        trySub clubId s squad coach (situation clubId s)

    let private handleMomentumSwing (disadvantagedClub: ClubSide) (s: MatchState) (squad: Player list) (coach: Staff) : ManagerAction =
        let clubId = ClubSide.toClubId disadvantagedClub s
        trySub clubId s squad coach (situation clubId s)

    let private handleRedCard (playerId: PlayerId) (s: MatchState) : ManagerAction list =
        let clubId =
            if s.HomeSide.Players |> Array.exists (fun p -> p.Id = playerId) then s.Home.Id
            else s.Away.Id
        let ts = side clubId s
        let compactTactics =
            match ts.Tactics with
            | TeamTactics.Attacking | TeamTactics.Pressing -> TeamTactics.Balanced
            | TeamTactics.Counter -> TeamTactics.Defensive
            | t -> t
        [ AdjustTactics(clubId, compactTactics) ]

    let private handleSubstitutionWindow (homeSquad: Player list) (awaySquad: Player list) (s: MatchState) : ManagerAction list =
        let homeCoachRating = float s.HomeCoach.CurrentSkill / 100.0
        let awayCoachRating = float s.AwayCoach.CurrentSkill / 100.0

        [ if s.HomeSide.SubsUsed < maxSubs && bernoulli (0.7 + homeCoachRating * 0.2) then
              match pickOutgoing s.HomeSide (conditionThreshold (situation s.Home.Id s)) with
              | Some outIdx ->
                  match pickIncoming homeSquad s.HomeCoach s.HomeSide (preferredPositions (situation s.Home.Id s)) with
                  | Some incoming -> yield MakeSubstitution(s.Home.Id, outIdx, incoming)
                  | None -> yield ManagerIdle
              | None -> yield ManagerIdle

          if s.AwaySide.SubsUsed < maxSubs && bernoulli (0.7 + awayCoachRating * 0.2) then
              match pickOutgoing s.AwaySide (conditionThreshold (situation s.Away.Id s)) with
              | Some outIdx ->
                  match pickIncoming awaySquad s.AwayCoach s.AwaySide (preferredPositions (situation s.Away.Id s)) with
                  | Some incoming -> yield MakeSubstitution(s.Away.Id, outIdx, incoming)
                  | None -> yield ManagerIdle
              | None -> yield ManagerIdle ]

    let decide
        (subTick: int)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (s: MatchState)
        (trigger: ReactionTrigger option)
        : ManagerAction list =
        // Phase 0: use subTick, convert to seconds for minute checks
        let elapsedSec = int (PhysicsContract.subTicksToSeconds subTick)
        let isSubMinute = elapsedSec = 60 * 60 || elapsedSec = 75 * 60 || elapsedSec = 85 * 60

        match trigger with
        | Some(FatigueAlert(clubId, _playerId, _condition)) ->
            let squad = if clubId = s.Home.Id then homeSquad else awaySquad
            let coach = if clubId = s.Home.Id then s.HomeCoach else s.AwayCoach
            [ handleFatigueAlert clubId s squad coach ]

        | Some(MomentumSwing disadvantagedClub) ->
            let squad = if disadvantagedClub = HomeClub then homeSquad else awaySquad
            let coach = if disadvantagedClub = HomeClub then s.HomeCoach else s.AwayCoach
            [ handleMomentumSwing disadvantagedClub s squad coach ]

        | Some(RedCardTrigger playerId) -> handleRedCard playerId s

        | Some(GoalScored | GoalConceded | InjuryTrigger _) ->
            if isSubMinute then handleSubstitutionWindow homeSquad awaySquad s
            else [ ManagerIdle ]

        | None ->
            if isSubMinute then handleSubstitutionWindow homeSquad awaySquad s
            else [ ManagerIdle ]

    // Phase 4: substitution — incoming player inherits position and cognitive state
    let resolve (subTick: int) (action: ManagerAction) (s: MatchState) : MatchState * MatchEvent list =
        match action with
        | ManagerIdle -> s, []

        | MakeSubstitution(clubId, outIdx, incoming) ->
            let ts = side clubId s
            if ts.SubsUsed >= maxSubs then s, []
            else
                let playerOut = ts.Players[outIdx]
                let inheritedPos = ts.Positions[outIdx]

                let ts' =
                    { ts with
                        Players      = Array.append ts.Players      [| incoming |]
                        Conditions   = Array.append ts.Conditions   [| incoming.Condition |]
                        Positions    = Array.append ts.Positions    [| inheritedPos |]
                        BasePositions = Array.append ts.BasePositions [| inheritedPos |]
                        Sidelined    = Map.add playerOut.Id SidelinedBySub ts.Sidelined
                        SubsUsed     = ts.SubsUsed + 1 }

                // Phase 4: extend cognitive arrays for new player
                let isHome = clubId = s.Home.Id
                let s1 = withSide clubId ts' s

                let s2 =
                    if isHome then
                        { s1 with
                            HomeMentalStates = Array.append s1.HomeMentalStates [| MentalState.initial incoming |]
                            HomeDirectives   = Array.append s1.HomeDirectives   [| Array.empty |] }
                    else
                        { s1 with
                            AwayMentalStates = Array.append s1.AwayMentalStates [| MentalState.initial incoming |]
                            AwayDirectives   = Array.append s1.AwayDirectives   [| Array.empty |] }

                s2,
                [ event subTick playerOut.Id clubId SubstitutionOut
                  event subTick incoming.Id clubId SubstitutionIn ]

        | AdjustTactics(clubId, newTactics) ->
            let ts = side clubId s
            withSide clubId { ts with Tactics = newTactics } s, []

    let agent homeId homeSquad awaySquad tick state : AgentOutput =
        match tick.Kind with
        | SubstitutionTick _clubId ->
            let actions = decide tick.SubTick homeSquad awaySquad state None
            let newState, events =
                actions
                |> List.fold
                    (fun (accState, accEvents) action ->
                        let s', evs = resolve tick.SubTick action accState
                        s', evs @ accEvents)
                    (state, [])
                |> fun (s, evs) -> s, List.rev evs
            { State = newState; Events = events; Spawned = []; Transition = None }

        | ManagerReactionTick trigger ->
            let actions = decide tick.SubTick homeSquad awaySquad state (Some trigger)
            let newState, events =
                actions
                |> List.fold
                    (fun (accState, accEvents) action ->
                        let s', evs = resolve tick.SubTick action accState
                        s', evs @ accEvents)
                    (state, [])
                |> fun (s, evs) -> s, List.rev evs
            { State = newState; Events = events; Spawned = []; Transition = Some LivePlay }

        | _ ->
            { State = state; Events = []; Spawned = []; Transition = None }
