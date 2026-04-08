namespace FootballEngine

open FootballEngine.Domain

module MatchSpatial =

    type PlayerInfo =
        { Player: Player
          PlayerId: PlayerId
          Pos: Spatial
          Condition: int }

    let spatialXY (sp: Spatial) = sp.X, sp.Y


    let teamRoster (players: Player[]) (positions: Spatial[]) (conditions: int[]) : (Player * Spatial * int)[] =
        let len = players.Length
        let arr = Array.zeroCreate len

        for i = 0 to len - 1 do
            arr[i] <- (players[i], positions[i], conditions[i])

        arr

    let outfieldRoster (players: Player[]) (positions: Spatial[]) (conditions: int[]) : (Player * Spatial * int)[] =
        let mutable count = 0

        for i = 0 to players.Length - 1 do
            if players[i].Position <> GK then
                count <- count + 1

        let arr = Array.zeroCreate count
        let mutable j = 0

        for i = 0 to players.Length - 1 do
            if players[i].Position <> GK then
                arr[j] <- (players[i], positions[i], conditions[i])
                j <- j + 1

        arr

    let nearestOutfield (players: Player[]) (positions: Spatial[]) (x: float) (y: float) : (Player * Spatial) option =
        let mutable bestPlayer: Player option = None
        let mutable bestSp: Spatial option = None
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to players.Length - 1 do
            if players[i].Position <> GK then
                let sp = positions[i]
                let dx = sp.X - x
                let dy = sp.Y - y
                let distSq = dx * dx + dy * dy

                if distSq < bestDistSq then
                    bestDistSq <- distSq
                    bestPlayer <- Some players[i]
                    bestSp <- Some sp

        match bestPlayer, bestSp with
        | Some p, Some sp -> Some(p, sp)
        | _ -> None

    let withBallVelocity (vx: float) (vy: float) (vz: float) (state: SimState) =
        let pos = state.Ball.Position

        state.Ball <-
            { state.Ball with
                Position = { pos with Vx = vx; Vy = vy; Vz = vz } }

    let ballTowards (targetX: float) (targetY: float) (speed: float) (vz: float) (state: SimState) =
        let bX = state.Ball.Position.X
        let bY = state.Ball.Position.Y
        let dx = targetX - bX
        let dy = targetY - bY
        let dist = sqrt (dx * dx + dy * dy)

        if dist < 0.01 then
            withBallVelocity 0.0 0.0 vz state
        else
            withBallVelocity (dx / dist * speed) (dy / dist * speed) vz state

    let nearestIdxToBall (slots: PlayerSlot[]) (ballX: float) (ballY: float) : int =
        let mutable bestIdx = 0
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let dx = s.Pos.X - ballX
                let dy = s.Pos.Y - ballY
                let dSq = dx * dx + dy * dy

                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestIdx <- i
            | _ -> ()

        bestIdx

    let tryNearestToBall (slots: PlayerSlot[]) (ballX: float) (ballY: float) : ActiveSlot option =
        let idx = nearestIdxToBall slots ballX ballY

        match slots[idx] with
        | PlayerSlot.Active s -> Some s
        | _ -> None

    let findNearestOpponentToPos (x: float) (y: float) (ctx: MatchContext) (state: SimState) : (Player * Spatial) option =
        let mutable bestPlayer: Player option = None
        let mutable bestSp: Spatial option = None
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to state.HomeSlots.Length - 1 do
            match state.HomeSlots[i] with
            | PlayerSlot.Active s when s.Player.Position <> GK ->
                let dx = s.Pos.X - x
                let dy = s.Pos.Y - y
                let dSq = dx * dx + dy * dy
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestPlayer <- Some s.Player
                    bestSp <- Some s.Pos
            | _ -> ()

        for i = 0 to state.AwaySlots.Length - 1 do
            match state.AwaySlots[i] with
            | PlayerSlot.Active s when s.Player.Position <> GK ->
                let dx = s.Pos.X - x
                let dy = s.Pos.Y - y
                let dSq = dx * dx + dy * dy
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestPlayer <- Some s.Player
                    bestSp <- Some s.Pos
            | _ -> ()

        match bestPlayer, bestSp with
        | Some p, Some sp -> Some(p, sp)
        | _ -> None

    let findNearestTeammateToPos (excludePlayerId: PlayerId) (targetX: float) (targetY: float) (state: SimState) (attSide: ClubSide) : (Player * Spatial) option =
        let slots = if attSide = HomeClub then state.HomeSlots else state.AwaySlots
        let mutable bestPlayer: Player option = None
        let mutable bestSp: Spatial option = None
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s when s.Player.Id <> excludePlayerId && s.Player.Position <> GK ->
                let dx = s.Pos.X - targetX
                let dy = s.Pos.Y - targetY
                let dSq = dx * dx + dy * dy
                if dSq < bestDistSq then
                    bestDistSq <- dSq
                    bestPlayer <- Some s.Player
                    bestSp <- Some s.Pos
            | _ -> ()

        match bestPlayer, bestSp with
        | Some p, Some sp -> Some(p, sp)
        | _ -> None

    let getPlayerInfo
        (players: Player[])
        (positions: Spatial[])
        (conditions: int[])
        (pid: PlayerId)
        : PlayerInfo option =
        match players |> Array.tryFindIndex (fun p -> p.Id = pid) with
        | Some i ->
            Some
                { Player = players[i]
                  PlayerId = pid
                  Pos = positions[i]
                  Condition = conditions[i] }
        | None -> None

    let findNearestTeammate (attacker: Player) (ctx: MatchContext) (state: SimState) (_dir: AttackDir) =
        let isHome =
            state.HomeSlots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = attacker.Id
                | _ -> false)

        let slots = if isHome then state.HomeSlots else state.AwaySlots

        let mutable attackerX = 0.0
        let mutable attackerY = 0.0
        let mutable attackerFound = false

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s when s.Player.Id = attacker.Id ->
                attackerX <- s.Pos.X
                attackerY <- s.Pos.Y
                attackerFound <- true
            | _ -> ()

        if not attackerFound then
            None
        else
            let mutable bestPlayer: Player option = None
            let mutable bestSp: Spatial option = None
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to slots.Length - 1 do
                match slots[i] with
                | PlayerSlot.Active s when s.Player.Id <> attacker.Id && s.Player.Position <> GK ->
                    let sp = s.Pos
                    let dx = sp.X - attackerX
                    let dy = sp.Y - attackerY
                    let distSq = dx * dx + dy * dy

                    if distSq < bestDistSq then
                        bestDistSq <- distSq
                        bestPlayer <- Some s.Player
                        bestSp <- Some sp
                | _ -> ()

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some(p, p.Id, spatialXY sp)
            | _ -> None

    let findNearestOpponent (attacker: Player) (ctx: MatchContext) (state: SimState) (_dir: AttackDir) =
        let isHome =
            state.HomeSlots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = attacker.Id
                | _ -> false)

        let attSlots = if isHome then state.HomeSlots else state.AwaySlots
        let defSlots = if isHome then state.AwaySlots else state.HomeSlots

        let mutable found = false
        let mutable ax = 0.0
        let mutable ay = 0.0

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = attacker.Id ->
                ax <- s.Pos.X
                ay <- s.Pos.Y
                found <- true
            | _ -> ()

        if not found then
            None
        else
            let mutable bestPlayer: Player option = None
            let mutable bestSp: Spatial option = None
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to defSlots.Length - 1 do
                match defSlots[i] with
                | PlayerSlot.Active s when s.Player.Position <> GK ->
                    let sp = s.Pos
                    let dx = sp.X - ax
                    let dy = sp.Y - ay
                    let distSq = dx * dx + dy * dy

                    if distSq < bestDistSq then
                        bestDistSq <- distSq
                        bestPlayer <- Some s.Player
                        bestSp <- Some sp
                | _ -> ()

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some(p, p.Id, spatialXY sp)
            | _ -> None

    let findBestPassTarget (attacker: Player) (ctx: MatchContext) (state: SimState) (dir: AttackDir) =
        let isHome =
            state.HomeSlots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = attacker.Id
                | _ -> false)

        let attSlots = if isHome then state.HomeSlots else state.AwaySlots
        let defSlots = if isHome then state.AwaySlots else state.HomeSlots

        let mutable found = false
        let mutable ballX = 0.0
        let mutable ballY = 0.0

        for i = 0 to attSlots.Length - 1 do
            match attSlots[i] with
            | PlayerSlot.Active s when s.Player.Id = attacker.Id ->
                ballX <- s.Pos.X
                ballY <- s.Pos.Y
                found <- true
            | _ -> ()

        if not found then
            None
        else
            let isForward (sp: Spatial) =
                match dir with
                | LeftToRight -> sp.X > ballX
                | RightToLeft -> sp.X < ballX

            let passLaneClear (targetSp: Spatial) =
                let mutable defendersNearLine = 0.0

                for i = 0 to defSlots.Length - 1 do
                    match defSlots[i] with
                    | PlayerSlot.Active s when s.Player.Position <> GK ->
                        let dSp = s.Pos
                        let dx = dSp.X - ballX
                        let dy = dSp.Y - ballY
                        let tdx = targetSp.X - ballX
                        let tdy = targetSp.Y - ballY
                        let lenSq = tdx * tdx + tdy * tdy

                        if lenSq >= 0.01 then
                            let t = (dx * tdx + dy * tdy) / lenSq

                            if t >= 0.0 && t <= 1.0 then
                                let projX = ballX + t * tdx
                                let projY = ballY + t * tdy
                                let dist = sqrt ((dSp.X - projX) ** 2.0 + (dSp.Y - projY) ** 2.0)

                                if dist < 3.0 then
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
                    let dist = sqrt ((sp.X - ballX) ** 2.0 + (sp.Y - ballY) ** 2.0)
                    let forwardBonus = if isForward sp then 0.15 else 0.0
                    let laneBonus = if passLaneClear sp then 0.2 else 0.0

                    let score =
                        (1.0 / (1.0 + dist * 0.1)) + forwardBonus + laneBonus + visionWeight * 0.1

                    if score > bestScore then
                        bestScore <- score
                        bestPlayer <- Some s.Player
                        bestSp <- Some sp
                | _ -> ()

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some(p, p.Id, spatialXY sp)
            | _ -> None

    let isOffside (player: Player) (playerX: float) (ctx: MatchContext) (state: SimState) (dir: AttackDir) =
        if player.Position = GK then
            false
        else
            let isAttHome =
                state.HomeSlots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = player.Id
                    | _ -> false)

            let defSlots = if isAttHome then state.AwaySlots else state.HomeSlots

            let mutable firstX = if dir = LeftToRight then -1.0 else 106.0
            let mutable secondX = if dir = LeftToRight then -1.0 else 106.0
            let mutable found = 0

            for i = 0 to defSlots.Length - 1 do
                match defSlots[i] with
                | PlayerSlot.Active s when s.Player.Position <> GK ->
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

            if found < 2 then
                false
            else
                let secondLastX = secondX
                let ballX = state.Ball.Position.X

                let inOwnHalf =
                    match dir with
                    | LeftToRight -> playerX < 50.0
                    | RightToLeft -> playerX > 50.0

                not inOwnHalf
                && match dir with
                   | LeftToRight -> playerX > secondLastX && playerX > ballX
                   | RightToLeft -> playerX < secondLastX && playerX < ballX

    let private secondLastDefenderXSlots (slots: PlayerSlot[]) (dir: AttackDir) : float =
        let mutable firstX = if dir = LeftToRight then -1.0 else 106.0
        let mutable secondX = if dir = LeftToRight then -1.0 else 106.0
        let mutable found = 0

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s when s.Player.Position <> GK ->
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

        if found < 2 then 0.0 else secondX

    let snapshotAtPass
        (passer: Player)
        (receiver: Player)
        (ctx: MatchContext)
        (state: SimState)
        (dir: AttackDir)
        : OffsideSnapshot =
        let isAttHome =
            state.HomeSlots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = passer.Id
                | _ -> false)

        let defSlots = if isAttHome then state.AwaySlots else state.HomeSlots
        let attSlots = if isAttHome then state.HomeSlots else state.AwaySlots

        let mutable receiverX = 50.0

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
