namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

module MatchSpatial =

    let spatialXY (sp: Spatial) = sp.X, sp.Y


    let withBallVelocity (vx: float<meter/second>) (vy: float<meter/second>) (vz: float<meter/second>) (state: SimState) =
        state.Ball <- { state.Ball with Position = { state.Ball.Position with Vx = vx; Vy = vy; Vz = vz } }

    let ballTowards
        (originX: float<meter>)
        (originY: float<meter>)
        (targetX: float<meter>)
        (targetY: float<meter>)
        (speed: float<meter/second>)
        (vz: float<meter/second>)
        (state: SimState)
        =
        let origin = { X = originX; Y = originY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let target = { X = targetX; Y = targetY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let dist = origin.DistTo2D target

        if dist < 0.01<meter> then
            withBallVelocity 0.0<meter/second> 0.0<meter/second> vz state
        else
            let dx = targetX - originX
            let dy = targetY - originY
            withBallVelocity (dx / dist * speed) (dy / dist * speed) vz state

    let nearestActiveSlot (slots: PlayerSlot[]) (ballX: float<meter>) (ballY: float<meter>) : ActiveSlot voption =
        let target = { X = ballX; Y = ballY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let mutable bestSlot = ValueNone
        let mutable bestDistSq = PhysicsContract.MaxDistanceSq

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let dSq = s.Pos.DistSqTo2D target
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestSlot <- ValueSome s
            | _ -> ()

        bestSlot

    let nearestActiveSlotExcluding
        (slots: PlayerSlot[])
        (excludeId: PlayerId)
        (x: float<meter>)
        (y: float<meter>)
        : ActiveSlot voption =
        let target = { X = x; Y = y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
        let mutable bestSlot = ValueNone
        let mutable bestDistSq = PhysicsContract.MaxDistanceSq

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s when s.Player.Id <> excludeId ->
                let dSq = s.Pos.DistSqTo2D target
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestSlot <- ValueSome s
            | _ -> ()

        bestSlot

    let findBestPassTarget (attacker: Player) (state: SimState) (dir: AttackDir) =
        let isHome =
            state.Home.Slots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = attacker.Id
                | _ -> false)

        let attSlots = if isHome then state.Home.Slots else state.Away.Slots
        let defSlots = if isHome then state.Away.Slots else state.Home.Slots

        let mutable ballPos = None

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = attacker.Id -> ballPos <- Some s.Pos
            | _ -> ()

        match ballPos with
        | None -> None
        | Some bPos ->
            let isForward (sp: Spatial) =
                match dir with
                | LeftToRight -> sp.X > bPos.X
                | RightToLeft -> sp.X < bPos.X

            let passLaneClear (targetSp: Spatial) =
                let mutable defendersNearLine = 0.0

                for i = 0 to defSlots.Length - 1 do
                    match defSlots[i] with
                    | PlayerSlot.Active s when s.Player.Position <> GK ->
                        let dSp = s.Pos
                        let dx = dSp.X - bPos.X
                        let dy = dSp.Y - bPos.Y
                        let tdx = targetSp.X - bPos.X
                        let tdy = targetSp.Y - bPos.Y
                        let lenSq = tdx * tdx + tdy * tdy

                        if lenSq >= 0.01<meterSquared> then
                            let t = (dx * tdx + dy * tdy) / lenSq

                            if t >= 0.0 && t <= 1.0 then
                                let projX = bPos.X + t * tdx
                                let projY = bPos.Y + t * tdy
                                let dxProj = dSp.X - projX
                                let dyProj = dSp.Y - projY
                                let distSq = dxProj * dxProj + dyProj * dyProj
                                let dist: float<meter> = sqrt distSq

                                if dist < 3.0<meter> then
                                    defendersNearLine <- defendersNearLine + 1.0
                    | _ -> ()

                defendersNearLine = 0.0

            let visionWeight = float attacker.Mental.Vision / 100.0
            let mutable bestPlayer: Player option = None
            let mutable bestSp: Spatial option = None
            let mutable bestScore = System.Double.MinValue

            for i = 0 to attSlots.Length - 1 do
                match attSlots[i] with
                | PlayerSlot.Active s when s.Player.Id <> attacker.Id ->
                    let sp = s.Pos
                    let dist = sp.DistTo2D bPos
                    let forwardBonus = if isForward sp then 0.15 else 0.0
                    let laneBonus = if passLaneClear sp then 0.2 else 0.0

                    let score =
                        (1.0 / (1.0 + (dist / 1.0<meter>) * 0.1)) + forwardBonus + laneBonus + visionWeight * 0.1

                    if score > bestScore then
                        bestScore <- score
                        bestPlayer <- Some s.Player
                        bestSp <- Some sp
                | _ -> ()

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some(p, p.Id, sp.XY)
            | _ -> None

    let isOffside (player: Player) (playerX: float<meter>) (state: SimState) (dir: AttackDir) =
        if player.Position = GK then
            false
        else
            let isAttHome =
                state.Home.Slots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = player.Id
                    | _ -> false)

            let dir =
                if isAttHome then state.HomeAttackDir
                else
                    match state.HomeAttackDir with
                    | LeftToRight -> RightToLeft
                    | RightToLeft -> LeftToRight

            let defSlots = if isAttHome then state.Away.Slots else state.Home.Slots

            let mutable firstX = if dir = LeftToRight then -1.0<meter> else 106.0<meter>
            let mutable secondX = if dir = LeftToRight then -1.0<meter> else 106.0<meter>
            let mutable found = 0

            for i = 0 to defSlots.Length - 1 do
                match defSlots[i] with
                | PlayerSlot.Active s ->
                    let x = s.Pos.X
                    found <- found + 1

                    if dir = LeftToRight then
                        if x > firstX then
                            secondX <- firstX
                            firstX <- x
                        elif x > secondX then
                            secondX <- x
                    else if x < firstX then
                        secondX <- firstX
                        firstX <- x
                    elif x < secondX then
                        secondX <- x
                | _ -> ()

            let ballX = state.Ball.Position.X
            let offsideLine =
                if found < 2 then
                    match dir with
                    | LeftToRight -> 106.0<meter>
                    | RightToLeft -> -1.0<meter>
                else
                    match dir with
                    | LeftToRight -> max secondX ballX
                    | RightToLeft -> min secondX ballX

            let inOwnHalf =
                match dir with
                | LeftToRight -> playerX <= 50.0<meter>
                | RightToLeft -> playerX >= 50.0<meter>

            not inOwnHalf && (match dir with
                              | LeftToRight -> playerX > offsideLine + 0.1<meter>
                              | RightToLeft -> playerX < offsideLine - 0.1<meter>)

    let private secondLastDefenderXSlots (slots: PlayerSlot[]) (dir: AttackDir) : float<meter> =
        let mutable firstX = if dir = LeftToRight then -1.0<meter> else 106.0<meter>
        let mutable secondX = if dir = LeftToRight then -1.0<meter> else 106.0<meter>
        let mutable found = 0

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let x = s.Pos.X
                found <- found + 1

                if dir = LeftToRight then
                    if x > firstX then
                        secondX <- firstX
                        firstX <- x
                    elif x > secondX then
                        secondX <- x
                else if x < firstX then
                    secondX <- firstX
                    firstX <- x
                elif x < secondX then
                    secondX <- x
            | _ -> ()

        if found < 2 then 0.0<meter> else secondX

    let snapshotAtPass
        (passer: Player)
        (receiver: Player)
        (state: SimState)
        (dir: AttackDir)
        : OffsideSnapshot =
        let isAttHome =
            state.Home.Slots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = passer.Id
                | _ -> false)

        let defSlots = if isAttHome then state.Away.Slots else state.Home.Slots
        let attSlots = if isAttHome then state.Home.Slots else state.Away.Slots

        let mutable receiverX = 50.0<meter>

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = receiver.Id -> receiverX <- s.Pos.X
            | _ -> ()

        { PasserId = passer.Id
          ReceiverId = receiver.Id
          ReceiverXAtPass = receiverX
          SecondLastDefenderX = secondLastDefenderXSlots defSlots dir
          BallXAtPass = state.Ball.Position.X
          Dir = dir }

    let isOffsideFromSnapshot (snapshot: OffsideSnapshot) : bool =
        match snapshot.Dir with
        | LeftToRight ->
            snapshot.ReceiverXAtPass > snapshot.SecondLastDefenderX
            && snapshot.ReceiverXAtPass > snapshot.BallXAtPass
        | RightToLeft ->
            snapshot.ReceiverXAtPass < snapshot.SecondLastDefenderX
            && snapshot.ReceiverXAtPass < snapshot.BallXAtPass

    let mirrorSpatial (s: Spatial) =
        { s with
            X = PhysicsContract.PitchLength - s.X
            Vx = -s.Vx }
