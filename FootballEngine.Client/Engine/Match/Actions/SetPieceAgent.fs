namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.MatchSpatial
open SimStateOps
open SchedulingTypes
open FootballEngine.PhysicsContract

module SetPieceAgent =

    let agent tick ctx state (clock: SimulationClock) : AgentOutput =
        match tick.Kind with
        | FreeKickTick(_kickerId, _position, _chainDepth) ->
            let events = SetPlayAction.resolveFreeKick tick.SubTick ctx state

            { Events = events
              Continuation = Defer(BalanceConfig.freeKickDelay, DuelTick 0)
              Transition = Some LivePlay
              SideEffects = [] }

        | CornerTick(_club, _chainDepth) ->
            let events = SetPlayAction.resolveCorner tick.SubTick ctx state

            { Events = events
              Continuation = Defer(BalanceConfig.cornerDelay, DuelTick 0)
              Transition = Some LivePlay
              SideEffects = [] }

        | ThrowInTick(team, _chainDepth) ->
            let events = SetPlayAction.resolveThrowIn tick.SubTick ctx state team

            { Events = events
              Continuation = Defer(BalanceConfig.throwInDelay, DuelTick 0)
              Transition = Some LivePlay
              SideEffects = [] }

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
              Continuation = Defer(BalanceConfig.foulDelay, DuelTick 0)
              Transition = Some LivePlay
              SideEffects = [] }

        | GoalKickTick ->
            let isHome =
                match state.Ball.Possession with
                | Possession.SetPiece(side, _) -> side = HomeClub
                | _ -> state.AttackingSide = HomeClub

            let kickingClub = if isHome then HomeClub else AwayClub
            let kickingClubId = if isHome then ctx.Home.Id else ctx.Away.Id
            let slots = if isHome then state.Home.Slots else state.Away.Slots

            let gkX =
                if isHome then
                    PhysicsContract.GoalAreaDepth
                else
                    PhysicsContract.PitchLength - PhysicsContract.GoalAreaDepth

            let centerY = PhysicsContract.PitchWidth / 2.0

            state.Ball <-
                { state.Ball with
                    Position = { defaultSpatial gkX centerY with Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    Possession = Possession.SetPiece(kickingClub, SetPieceKind.GoalKick) }

            let gkOpt =
                slots
                |> Array.tryPick (function
                    | PlayerSlot.Active s when s.Player.Position = GK -> Some s
                    | _ -> None)

            match gkOpt with
            | Some gk ->
                // Basic implementation: GK kicks to nearest teammate
                let targetX, targetY =
                    let sideSlots = if isHome then state.Home.Slots else state.Away.Slots

                    match MatchSpatial.nearestActiveSlotExcluding sideSlots gk.Player.Id gk.Pos.X gk.Pos.Y with
                    | ValueSome s -> s.Pos.X, s.Pos.Y
                    | ValueNone -> (if isHome then 30.0<meter> else 75.0<meter>), centerY

                state.Ball <-
                    { state.Ball with
                        LastTouchBy = Some gk.Player.Id
                        Possession = InFlight(kickingClub, gk.Player.Id) } 

                let dx = targetX - gk.Pos.X
                let dy = targetY - gk.Pos.Y
                let dist = sqrt (dx * dx + dy * dy)
                let speed = BalanceConfig.PassSpeed

                if dist > 0.1<meter> then
                    withBallVelocity (dx / dist * speed) (dy / dist * speed) 0.0<meter / second> state

                { Events = [ createEvent tick.SubTick gk.Player.Id kickingClubId MatchEventType.GoalKick ]
                  Continuation = EndChain
                  Transition = Some LivePlay
                  SideEffects = [] }
            | None ->
                { Events = []
                  Continuation = EndChain
                  Transition = Some LivePlay
                  SideEffects = [] }

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
                        { X = centerX
                          Y = centerY
                          Z = 0.0<meter>
                          Vx = 0.0<meter/second>
                          Vy = 0.0<meter/second>
                          Vz = 0.0<meter/second> }
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
                  Continuation = Defer(BalanceConfig.duelNextDelay, DuelTick 0)
                  Transition = Some LivePlay
                  SideEffects = [] }
            | Some kicker ->
                let targetX, targetY =
                    match partnerOpt with
                    | Some partner -> partner.Pos.X, partner.Pos.Y
                    | None -> centerX - 3.0<meter>, centerY + 2.0<meter>

                let partnerId =
                    partnerOpt
                    |> Option.map (fun p -> p.Player.Id)
                    |> Option.defaultValue kicker.Player.Id

                // La pelota se mantiene en SetPiece tras el KickOffTick
                state.Ball <-
                    { state.Ball with
                        LastTouchBy = Some kicker.Player.Id }

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
                  Continuation = EndChain
                  Transition = Some LivePlay
                  SideEffects = [] }

        | _ ->
            { Events = []
              Continuation = EndChain // No continuation defined for this tick kind
              Transition = None
              SideEffects = [] }
