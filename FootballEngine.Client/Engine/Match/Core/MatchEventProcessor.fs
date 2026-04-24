namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes
open FootballEngine.PhysicsContract

module MatchEventProcessor =

    let private hasTerminatingEvent (events: MatchEvent list) =
        events
        |> List.exists (fun e ->
            match e.Type with
            | MatchEventType.Goal
            | MatchEventType.FoulCommitted
            | MatchEventType.ShotOffTarget
            | MatchEventType.ShotBlocked
            | MatchEventType.Save -> true
            | _ -> false)

    let private transitionFromEvents (events: MatchEvent list) =
        let hasGoal =
            events
            |> List.exists (fun e -> e.Type = MatchEventType.Goal || e.Type = MatchEventType.OwnGoal)

        if hasGoal then
            Some(Stopped Goal)
        else
            events
            |> List.tryPick (fun e ->
                match e.Type with
                | MatchEventType.FoulCommitted -> Some(Stopped Foul)
                | MatchEventType.Corner -> Some(SetPiece SetPieceKind.Corner)
                | _ -> None)

    let private defaultPlayerIntent : PlayerIntent =
        { Movement = MovementIntent.RecoverBall { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
          Action = None
          Context = NormalPlay
          Urgency = 0.5
          Confidence = 0.5 }

    let processEventsAndSpawnTicks
        (subTick: int)
        (_depth: int)
        (allEvents: MatchEvent list)
        (playerHasBall: bool)
        (_attId: PlayerId option)
        (prevAttackingClub: ClubSide)
        (ctx: MatchContext)
        (state: SimState)
        (_clock: SimulationClock)
        : AgentResult =

        let tc = ctx.Config.Timing

        let possessionChanged = state.AttackingSide <> prevAttackingClub

        let hasTerminating = hasTerminatingEvent allEvents
        let chainBreaks =
            allEvents
            |> List.exists (fun e ->
                match e.Type with
                | MatchEventType.PassDeflected _
                | MatchEventType.PassMisplaced _ -> true
                | _ -> false)
            || possessionChanged
            || not playerHasBall

        if hasTerminating || chainBreaks then
            let transition = transitionFromEvents allEvents

            { Intent = defaultPlayerIntent
              NextTick = None
              Events = allEvents
              Transition = transition }
        else
            { Intent = defaultPlayerIntent
              NextTick = None
              Events = allEvents
              Transition = None }
