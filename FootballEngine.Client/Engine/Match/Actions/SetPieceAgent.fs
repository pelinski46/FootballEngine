namespace FootballEngine

open BalanceConfig.TickDelay
open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open SchedulingTypes
open SimulationClock
open FootballEngine.PhysicsContract

module SetPieceAgent =

    let agent tick ctx state (clock: SimulationClock) : AgentOutput =
        match tick.Kind with
        | FreeKickTick(_kickerId, _position, _chainDepth) ->
            let events = SetPlayAction.resolveFreeKick tick.SubTick ctx state

            { Events = events
              Spawned =
                [ { SubTick = tick.SubTick + delayFrom BalanceConfig.freeKickDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | CornerTick(_club, _chainDepth) ->
            let events = SetPlayAction.resolveCorner tick.SubTick ctx state

            { Events = events
              Spawned =
                [ { SubTick = tick.SubTick + delayFrom BalanceConfig.cornerDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | ThrowInTick(team, _chainDepth) ->
            let events = SetPlayAction.resolveThrowIn tick.SubTick ctx state team

            { Events = events
              Spawned =
                [ { SubTick = tick.SubTick + delayFrom BalanceConfig.throwInDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | PenaltyTick(kicker, isHome) ->
            let kickerPlayer =
                let slots = getSlots state (if isHome then HomeClub else AwayClub)

                slots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Id = kicker -> Some s.Player
                    | _ -> None)
                |> Option.defaultWith (fun () ->
                    state.Home.Slots
                    |> Array.pick (function
                        | PlayerSlot.Active s -> Some s.Player
                        | _ -> None))

            let kickClub = if isHome then HomeClub else AwayClub

            let events = SetPlayAction.resolvePenalty ctx state kickerPlayer kickClub 1 clock

            { Events = events
              Spawned =
                [ { SubTick = tick.SubTick + delayFrom BalanceConfig.foulDelay
                    Priority = TickPriority.Duel
                    SequenceId = 0L
                    Kind = DuelTick 0 } ]
              Transition = Some LivePlay }

        | GoalKickTick
        | KickOffTick ->
            let centerX = PhysicsContract.HalfwayLineX
            let centerY = PhysicsContract.PitchWidth / 2.0

            let isHomeKickOff = state.HomeAttackDir = LeftToRight
            let kickingClub = if isHomeKickOff then HomeClub else AwayClub
            let kickingSlots = if isHomeKickOff then state.Home.Slots else state.Away.Slots
            let kickingClubId = if isHomeKickOff then ctx.Home.Id else ctx.Away.Id

            state.Ball <-
                { state.Ball with
                    Position =
                        { state.Ball.Position with
                            X = centerX
                            Y = centerY
                            Vx = 0.0<meter / second>
                            Vy = 0.0<meter / second>
                            Vz = 0.0<meter / second> }
                    Possession = Possession.SetPiece(kickingClub, SetPieceKind.KickOff) }

            let kickerOpt =
                kickingSlots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Position = ST -> Some s
                    | _ -> None)
                |> Option.orElse (
                    kickingSlots
                    |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Position = AMC -> Some s
                        | _ -> None)
                )
                |> Option.orElse (
                    kickingSlots
                    |> Array.tryPick (function
                        | PlayerSlot.Active s when s.Player.Position <> GK -> Some s
                        | _ -> None)
                )

            let partnerOpt =
                kickingSlots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when
                        (s.Player.Position = AMC || s.Player.Position = MC || s.Player.Position = ST)
                        && (kickerOpt
                            |> Option.map (fun k -> k.Player.Id <> s.Player.Id)
                            |> Option.defaultValue true)
                        ->
                        Some s
                    | _ -> None)

            match kickerOpt with
            | None ->
                { Events = []
                  Spawned = []
                  Transition = Some LivePlay }
            | Some kicker ->
                let targetX, targetY =
                    match partnerOpt with
                    | Some partner -> partner.Pos.X, partner.Pos.Y
                    | None -> centerX - 3.0<meter>, centerY + 2.0<meter>

                let partnerId = 
                    partnerOpt |> Option.map (fun p -> p.Player.Id) |> Option.defaultValue kicker.Player.Id

                // CAMBIO: La pelota sale "InFlight", no "Owned"
                state.Ball <-
                    { state.Ball with
                        LastTouchBy = Some kicker.Player.Id
                        Possession = InFlight(kickingClub, partnerId) }

                let dx = targetX - centerX
                let dy = targetY - centerY
                let dist = sqrt (dx * dx + dy * dy)
                let speed = BalanceConfig.PassSpeed

                if dist > 0.1<meter> then
                    withBallVelocity (dx / dist * speed) (dy / dist * speed) 0.0<meter / second> state

                { Events =
                    [ { SubTick = tick.SubTick
                        PlayerId = kicker.Player.Id
                        ClubId = kickingClubId
                        Type = MatchEventType.KickOff } ]
                  Spawned = [] // Ya no necesitamos spawnear un DuelTick manual, el BallAgent lo hará al detectar la recepción
                  Transition = Some LivePlay }

        | _ ->
            { Events = []
              Spawned = []
              Transition = None }
