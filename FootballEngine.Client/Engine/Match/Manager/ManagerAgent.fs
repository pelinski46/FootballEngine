namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.Stats
open SimStateOps
open SchedulingTypes

module ManagerAgent =

    type Situation =
        | Winning
        | Drawing
        | Losing

    let private situation (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        match goalDiff clubId ctx state with
        | d when d > 0 -> Winning
        | d when d < 0 -> Losing
        | _ -> Drawing

    let private maxSubs = 3

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

    let private reactiveTactics (sit: Situation) (current: TeamTactics) : TeamTactics =
        match sit, current with
        | Losing, _ -> TeamTactics.Attacking
        | Drawing, TeamTactics.Defensive -> TeamTactics.Balanced
        | Drawing, TeamTactics.Balanced -> TeamTactics.Attacking
        | Drawing, _ -> TeamTactics.Balanced
        | Winning, TeamTactics.Attacking -> TeamTactics.Balanced
        | Winning, TeamTactics.Counter -> TeamTactics.Balanced
        | Winning, _ -> TeamTactics.Defensive

    let private pickOutgoing
        (players: Player[])
        (positions: Spatial[])
        (conditions: int[])
        (sidelined: Map<PlayerId, PlayerOut>)
        (subsUsed: int)
        (threshold: int)
        : int option =
        players
        |> Array.mapi (fun i p -> i, p)
        |> Array.filter (fun (i, p) -> not (Map.containsKey p.Id sidelined))
        |> Array.filter (fun (i, p) -> p.Position <> GK && conditions[i] < threshold)
        |> Array.sortBy (fun (i, _) -> conditions[i])
        |> Array.tryHead
        |> Option.map fst

    let private pickIncoming
        (squadPlayers: Player list)
        (manager: Staff)
        (players: Player[])
        (sidelined: Map<PlayerId, PlayerOut>)
        (preferred: Position list)
        : Player option =
        let startingIds =
            manager.Attributes.Coaching.Lineup
            |> Option.map (fun lu -> lu.Slots |> List.choose _.PlayerId |> Set.ofList)
            |> Option.defaultValue Set.empty

        let onPitchIds = players |> Array.map _.Id |> Set.ofArray

        let bench =
            squadPlayers
            |> List.filter (fun p ->
                not (Set.contains p.Id startingIds)
                && not (Set.contains p.Id onPitchIds)
                && not (Map.containsKey p.Id sidelined)
                && p.Status = Available)

        bench
        |> List.filter (fun p -> List.contains p.Position preferred)
        |> List.sortByDescending _.CurrentSkill
        |> List.tryHead
        |> Option.orElseWith (fun () -> bench |> List.sortByDescending _.CurrentSkill |> List.tryHead)

    let private trySub
        (clubId: ClubId)
        (ctx: MatchContext)
        (state: SimState)
        (squad: Player list)
        (coach: Staff)
        (sit: Situation)
        : ManagerAction =
        let subsUsed, sidelined, slots =
            if clubId = ctx.Home.Id then
                state.HomeSubsUsed, state.HomeSidelined, state.HomeSlots
            else
                state.AwaySubsUsed, state.AwaySidelined, state.AwaySlots

        if subsUsed >= maxSubs then
            AdjustTactics(
                clubId,
                reactiveTactics
                    sit
                    (if clubId = ctx.Home.Id then
                         state.HomeTactics
                     else
                         state.AwayTactics)
            )
        else
            let outgoingIdx =
                slots
                |> Array.mapi (fun i slot ->
                    match slot with
                    | PlayerSlot.Active s ->
                        if s.Player.Position <> GK && s.Condition < conditionThreshold sit then
                            Some(i, s.Condition)
                        else
                            None
                    | Sidelined _ -> None)
                |> Array.choose id
                |> Array.sortBy snd
                |> Array.tryHead
                |> Option.map fst

            match outgoingIdx with
            | Some outIdx ->
                match pickIncoming squad coach (activePlayers slots) sidelined (preferredPositions sit) with
                | Some incoming -> MakeSubstitution(clubId, outIdx, incoming)
                | None ->
                    AdjustTactics(
                        clubId,
                        reactiveTactics
                            sit
                            (if clubId = ctx.Home.Id then
                                 state.HomeTactics
                             else
                                 state.AwayTactics)
                    )
            | None ->
                AdjustTactics(
                    clubId,
                    reactiveTactics
                        sit
                        (if clubId = ctx.Home.Id then
                             state.HomeTactics
                         else
                             state.AwayTactics)
                )

    let private handleFatigueAlert
        (clubId: ClubId)
        (ctx: MatchContext)
        (state: SimState)
        (squad: Player list)
        (coach: Staff)
        : ManagerAction =
        trySub clubId ctx state squad coach (situation clubId ctx state)

    let private handleMomentumSwing
        (disadvantagedClub: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (squad: Player list)
        (coach: Staff)
        : ManagerAction =
        let clubId =
            if disadvantagedClub = HomeClub then
                ctx.Home.Id
            else
                ctx.Away.Id

        trySub clubId ctx state squad coach (situation clubId ctx state)

    let private handleRedCard (playerId: PlayerId) (ctx: MatchContext) (state: SimState) : ManagerAction list =
        let clubId =
            if
                state.HomeSlots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = playerId
                    | _ -> false)
            then
                ctx.Home.Id
            else
                ctx.Away.Id

        let currentTactics =
            if clubId = ctx.Home.Id then
                state.HomeTactics
            else
                state.AwayTactics

        let compactTactics =
            match currentTactics with
            | TeamTactics.Attacking
            | TeamTactics.Pressing -> TeamTactics.Balanced
            | TeamTactics.Counter -> TeamTactics.Defensive
            | t -> t

        [ AdjustTactics(clubId, compactTactics) ]

    let private handleSubstitutionWindow
        (homeSquad: Player list)
        (awaySquad: Player list)
        (ctx: MatchContext)
        (state: SimState)
        : ManagerAction list =
        let homeCoachRating = float ctx.HomeCoach.CurrentSkill / 100.0
        let awayCoachRating = float ctx.AwayCoach.CurrentSkill / 100.0

        [ if state.HomeSubsUsed < maxSubs && bernoulli (0.7 + homeCoachRating * 0.2) then
              let homeSlots = state.HomeSlots

              let outgoingIdx =
                  homeSlots
                  |> Array.mapi (fun i slot ->
                      match slot with
                      | PlayerSlot.Active s ->
                          if
                              s.Player.Position <> GK
                              && s.Condition < conditionThreshold (situation ctx.Home.Id ctx state)
                          then
                              Some(i, s.Condition)
                          else
                              None
                      | Sidelined _ -> None)
                  |> Array.choose id
                  |> Array.sortBy snd
                  |> Array.tryHead
                  |> Option.map fst

              match outgoingIdx with
              | Some outIdx ->
                  match
                      pickIncoming
                          homeSquad
                          ctx.HomeCoach
                          (activePlayers homeSlots)
                          state.HomeSidelined
                          (preferredPositions (situation ctx.Home.Id ctx state))
                  with
                  | Some incoming -> yield MakeSubstitution(ctx.Home.Id, outIdx, incoming)
                  | None -> yield ManagerIdle
              | None -> yield ManagerIdle

          if state.AwaySubsUsed < maxSubs && bernoulli (0.7 + awayCoachRating * 0.2) then
              let awaySlots = state.AwaySlots

              let outgoingIdx =
                  awaySlots
                  |> Array.mapi (fun i slot ->
                      match slot with
                      | PlayerSlot.Active s ->
                          if
                              s.Player.Position <> GK
                              && s.Condition < conditionThreshold (situation ctx.Away.Id ctx state)
                          then
                              Some(i, s.Condition)
                          else
                              None
                      | Sidelined _ -> None)
                  |> Array.choose id
                  |> Array.sortBy snd
                  |> Array.tryHead
                  |> Option.map fst

              match outgoingIdx with
              | Some outIdx ->
                  match
                      pickIncoming
                          awaySquad
                          ctx.AwayCoach
                          (activePlayers awaySlots)
                          state.AwaySidelined
                          (preferredPositions (situation ctx.Away.Id ctx state))
                  with
                  | Some incoming -> yield MakeSubstitution(ctx.Away.Id, outIdx, incoming)
                  | None -> yield ManagerIdle
              | None -> yield ManagerIdle ]

    let decide
        (subTick: int)
        (homeSquad: Player list)
        (awaySquad: Player list)
        (ctx: MatchContext)
        (state: SimState)
        (trigger: ReactionTrigger option)
        : ManagerAction list =
        let elapsedSec = int (PhysicsContract.subTicksToSeconds subTick)

        let isSubMinute =
            elapsedSec = 60 * 60 || elapsedSec = 75 * 60 || elapsedSec = 85 * 60

        match trigger with
        | Some(FatigueAlert(clubId, _playerId, _condition)) ->
            let squad = if clubId = ctx.Home.Id then homeSquad else awaySquad

            let coach =
                if clubId = ctx.Home.Id then
                    ctx.HomeCoach
                else
                    ctx.AwayCoach

            [ handleFatigueAlert clubId ctx state squad coach ]

        | Some(MomentumSwing disadvantagedClub) ->
            let squad =
                if disadvantagedClub = HomeClub then
                    homeSquad
                else
                    awaySquad

            let coach =
                if disadvantagedClub = HomeClub then
                    ctx.HomeCoach
                else
                    ctx.AwayCoach

            [ handleMomentumSwing disadvantagedClub ctx state squad coach ]

        | Some(RedCardTrigger playerId) -> handleRedCard playerId ctx state

        | Some(GoalScored | GoalConceded | InjuryTrigger _) ->
            if isSubMinute then
                handleSubstitutionWindow homeSquad awaySquad ctx state
            else
                [ ManagerIdle ]

        | None ->
            if isSubMinute then
                handleSubstitutionWindow homeSquad awaySquad ctx state
            else
                [ ManagerIdle ]

    let resolve (subTick: int) (action: ManagerAction) (ctx: MatchContext) (state: SimState) : MatchEvent list =
        match action with
        | ManagerIdle -> []

        | MakeSubstitution(clubId, outIdx, incoming) ->
            let isHome = clubId = ctx.Home.Id
            let subsUsed = if isHome then state.HomeSubsUsed else state.AwaySubsUsed

            if subsUsed >= maxSubs then
                []
            else
                let slots = if isHome then state.HomeSlots else state.AwaySlots

                match slots[outIdx] with
                | Sidelined _ -> []
                | PlayerSlot.Active s ->
                    let playerOut = s.Player
                    let inheritedPos = s.Pos

                    let newSlot =
                        PlayerSlot.Active
                            { Player = incoming
                              Pos = inheritedPos
                              Condition = incoming.Condition
                              Mental = MentalState.initial incoming
                              Directives = Array.empty }

                    slots[outIdx] <- newSlot

                    if isHome then
                        state.HomeSubsUsed <- state.HomeSubsUsed + 1
                        state.HomeSidelined <- Map.add playerOut.Id SidelinedBySub state.HomeSidelined
                    else
                        state.AwaySubsUsed <- state.AwaySubsUsed + 1
                        state.AwaySidelined <- Map.add playerOut.Id SidelinedBySub state.AwaySidelined

                    [ createEvent subTick playerOut.Id clubId SubstitutionOut
                      createEvent subTick incoming.Id clubId SubstitutionIn ]

        | AdjustTactics(clubId, newTactics) ->
            if clubId = ctx.Home.Id then
                state.HomeTactics <- newTactics
            else
                state.AwayTactics <- newTactics

            []

    let agent homeId homeSquad awaySquad tick ctx state : AgentOutput =
        match tick.Kind with
        | SubstitutionTick _clubId ->
            let actions = decide tick.SubTick homeSquad awaySquad ctx state None

            let events =
                actions
                |> List.fold
                    (fun accEvents action ->
                        let evs = resolve tick.SubTick action ctx state
                        evs @ accEvents)
                    []
                |> List.rev

            { Events = events
              Spawned = []
              Transition = None }

        | ManagerReactionTick trigger ->
            let actions = decide tick.SubTick homeSquad awaySquad ctx state (Some trigger)

            let events =
                actions
                |> List.fold
                    (fun accEvents action ->
                        let evs = resolve tick.SubTick action ctx state
                        evs @ accEvents)
                    []
                |> List.rev

            { Events = events
              Spawned = []
              Transition = Some LivePlay }

        | _ ->
            { Events = []
              Spawned = []
              Transition = None }
