namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open FootballEngine

module Interception =
    let estimateTimeToBall (player: Player) (pPos: Spatial) (ballPos: Spatial) : float =
        let captureRadius =
            BalanceConfig.BallContactRadius
            + (float player.Technical.BallControl / 20.0) * 0.20<meter>

        let dist = float (ballPos.DistTo2D pPos)

        if dist <= float captureRadius then
            0.0
        else
            let maxSpeed = float (playerMaxSpeed player.Physical.Pace player.Condition)
            if maxSpeed <= 1e-6 then System.Double.PositiveInfinity
            else (dist - float captureRadius) / maxSpeed

    let chooseBestInterceptor (ballPos: Spatial) (homeSlots: PlayerSlot[]) (awaySlots: PlayerSlot[]) : Player option * Spatial option * bool =
        let mutable touching =[]

        let consider (player: Player) (pPos: Spatial) =
            let captureRadius =
                BalanceConfig.BallContactRadius
                + (float player.Technical.BallControl / 20.0) * 0.20<meter>

            let dist = float (ballPos.DistTo2D pPos)

            if dist <= float captureRadius then
                touching <- (player, pPos) :: touching

        for i = 0 to homeSlots.Length - 1 do
            match homeSlots[i] with
            | PlayerSlot.Active s -> consider s.Player s.Pos
            | _ -> ()

        for i = 0 to awaySlots.Length - 1 do
            match awaySlots[i] with
            | PlayerSlot.Active s -> consider s.Player s.Pos
            | _ -> ()

        match touching with
        |[] -> None, None, false
        | [(p, pos)] -> Some p, Some pos, false
        | _ -> None, None, true
