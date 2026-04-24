namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

module Interception =
    let estimateTimeToBall (config: PhysicsConfig) (player: Player) (pPos: Spatial) (ballPos: Spatial) : float =
        let captureRadius =
            config.ContactRadius
            + (float player.Technical.BallControl / 20.0) * 0.20<meter>

        let dist = float (ballPos.DistTo2D pPos)

        if dist <= float captureRadius then
            0.0
        else
            let maxSpeed = float (playerMaxSpeed player.Physical.Pace player.Condition)
            if maxSpeed <= 1e-6 then System.Double.PositiveInfinity
            else (dist - float captureRadius) / maxSpeed

    let chooseBestInterceptorSoA
        (config: PhysicsConfig)
        (ballPos: Spatial)
        (homeFrame: TeamFrame)
        (awayFrame: TeamFrame)
        (homeRoster: PlayerRoster)
        (awayRoster: PlayerRoster)
        : Player option * Spatial option * bool =
        let mutable touchingCount = 0
        let mutable touchingPlayer1 = Unchecked.defaultof<Player>
        let mutable touchingPos1 = { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let mutable touchingPlayer2 = Unchecked.defaultof<Player>
        let mutable touchingPos2 = { X = 0.0<meter>; Y = 0.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

        for i = 0 to homeFrame.SlotCount - 1 do
            match homeFrame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = homeRoster.Players[i]
                let captureRadius =
                    config.ContactRadius
                    + (float player.Technical.BallControl / 20.0) * 0.20<meter>
                let px = float homeFrame.PosX[i] * 1.0<meter>
                let py = float homeFrame.PosY[i] * 1.0<meter>
                let dx = ballPos.X - px
                let dy = ballPos.Y - py
                let dist = sqrt (dx * dx + dy * dy)
                if dist <= captureRadius then
                    if touchingCount = 0 then
                        touchingPlayer1 <- player; touchingPos1 <- { X = px; Y = py; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    elif touchingCount = 1 then
                        touchingPlayer2 <- player; touchingPos2 <- { X = px; Y = py; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    touchingCount <- touchingCount + 1
            | _ -> ()

        for i = 0 to awayFrame.SlotCount - 1 do
            match awayFrame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                let player = awayRoster.Players[i]
                let captureRadius =
                    config.ContactRadius
                    + (float player.Technical.BallControl / 20.0) * 0.20<meter>
                let px = float awayFrame.PosX[i] * 1.0<meter>
                let py = float awayFrame.PosY[i] * 1.0<meter>
                let dx = ballPos.X - px
                let dy = ballPos.Y - py
                let dist = sqrt (dx * dx + dy * dy)
                if dist <= captureRadius then
                    if touchingCount = 0 then
                        touchingPlayer1 <- player; touchingPos1 <- { X = px; Y = py; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    elif touchingCount = 1 then
                        touchingPlayer2 <- player; touchingPos2 <- { X = px; Y = py; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    touchingCount <- touchingCount + 1
            | _ -> ()

        match touchingCount with
        | 0 -> None, None, false
        | 1 -> Some touchingPlayer1, Some touchingPos1, false
        | _ -> None, None, true
