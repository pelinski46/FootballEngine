namespace FootballEngine

open FootballEngine.Domain
open MatchStateOps

module MatchSpatial =

    type PlayerInfo =
        { Player: Player
          PlayerId: PlayerId
          Pos: Spatial
          Condition: int }

    let spatialXY (sp: Spatial) = sp.X, sp.Y

    let ballXY (s: MatchState) = s.Ball.Position.X, s.Ball.Position.Y

    let teamRoster (ts: TeamSide) : (Player * Spatial * int)[] =
        let len = ts.Players.Length
        let arr = Array.zeroCreate len

        for i = 0 to len - 1 do
            arr[i] <- (ts.Players[i], ts.Positions[i], ts.Conditions[i])

        arr

    let outfieldRoster (ts: TeamSide) : (Player * Spatial * int)[] =
        let mutable count = 0

        for i = 0 to ts.Players.Length - 1 do
            if ts.Players[i].Position <> GK then
                count <- count + 1

        let arr = Array.zeroCreate count
        let mutable j = 0

        for i = 0 to ts.Players.Length - 1 do
            if ts.Players[i].Position <> GK then
                arr[j] <- (ts.Players[i], ts.Positions[i], ts.Conditions[i])
                j <- j + 1

        arr

    let nearestOutfield (ts: TeamSide) (x: float) (y: float) : (Player * Spatial) option =
        let mutable bestPlayer: Player option = None
        let mutable bestSp: Spatial option = None
        let mutable bestDistSq = System.Double.MaxValue

        for i = 0 to ts.Players.Length - 1 do
            if ts.Players[i].Position <> GK then
                let sp = ts.Positions[i]
                let dx = sp.X - x
                let dy = sp.Y - y
                let distSq = dx * dx + dy * dy

                if distSq < bestDistSq then
                    bestDistSq <- distSq
                    bestPlayer <- Some ts.Players[i]
                    bestSp <- Some sp

        match bestPlayer, bestSp with
        | Some p, Some sp -> Some (p, sp)
        | _ -> None

    let withBallVelocity (vx: float) (vy: float) (vz: float) (s: MatchState) =
        let pos = s.Ball.Position

        { s with
            Ball =
                { s.Ball with
                    Position = { pos with Vx = vx; Vy = vy; Vz = vz } } }

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

    let findNearestTeammate (attacker: Player) (s: MatchState) (_dir: AttackDir) =
        let attSide = side (ClubSide.toClubId s.AttackingClub s) s

        // Pass 1: find attacker's position
        let mutable attackerX = 0.0
        let mutable attackerY = 0.0
        let mutable attackerFound = false
        for i = 0 to attSide.Players.Length - 1 do
            if attSide.Players[i].Id = attacker.Id then
                attackerX <- attSide.Positions[i].X
                attackerY <- attSide.Positions[i].Y
                attackerFound <- true

        if not attackerFound then None
        else
            // Pass 2: find nearest teammate (skip attacker + GK)
            let mutable bestPlayer: Player option = None
            let mutable bestSp: Spatial option = None
            let mutable bestDistSq = System.Double.MaxValue

            for i = 0 to attSide.Players.Length - 1 do
                let p = attSide.Players[i]
                if p.Id <> attacker.Id && p.Position <> GK then
                    let sp = attSide.Positions[i]
                    let dx = sp.X - attackerX
                    let dy = sp.Y - attackerY
                    let distSq = dx * dx + dy * dy
                    if distSq < bestDistSq then
                        bestDistSq <- distSq
                        bestPlayer <- Some p
                        bestSp <- Some sp

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some(p, p.Id, spatialXY sp)
            | _ -> None

    let findNearestOpponent (attacker: Player) (s: MatchState) (_dir: AttackDir) =
        let attSide = side (ClubSide.toClubId s.AttackingClub s) s
        let defSide = side (ClubSide.toClubId (ClubSide.flip s.AttackingClub) s) s

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> None
        | Some info ->
            let ax, ay = spatialXY info.Pos

            nearestOutfield defSide ax ay
            |> Option.map (fun (p, sp) -> p, p.Id, spatialXY sp)

    let findBestPassTarget (attacker: Player) (s: MatchState) (dir: AttackDir) =
        let attSide = side (ClubSide.toClubId s.AttackingClub s) s
        let defSide = side (ClubSide.toClubId (ClubSide.flip s.AttackingClub) s) s

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> None
        | Some info ->
            let ballX, ballY = spatialXY info.Pos

            let isForward (sp: Spatial) =
                match dir with
                | LeftToRight -> sp.X > ballX
                | RightToLeft -> sp.X < ballX

            let passLaneClear (targetSp: Spatial) =
                let mutable defendersNearLine = 0.0

                for i = 0 to defSide.Players.Length - 1 do
                    if defSide.Players[i].Position <> GK then
                        let dSp = defSide.Positions[i]
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

                defendersNearLine = 0.0

            let visionWeight = float attacker.Mental.Vision / 100.0
            let mutable bestPlayer: Player option = None
            let mutable bestSp: Spatial option = None
            let mutable bestScore = System.Double.MinValue

            for i = 0 to attSide.Players.Length - 1 do
                let p = attSide.Players[i]

                if p.Id <> attacker.Id then
                    let sp = attSide.Positions[i]
                    let dist = sqrt ((sp.X - ballX) ** 2.0 + (sp.Y - ballY) ** 2.0)
                    let forwardBonus = if isForward sp then 0.15 else 0.0
                    let laneBonus = if passLaneClear sp then 0.2 else 0.0

                    let score =
                        (1.0 / (1.0 + dist * 0.1)) + forwardBonus + laneBonus + visionWeight * 0.1

                    if score > bestScore then
                        bestScore <- score
                        bestPlayer <- Some p
                        bestSp <- Some sp

            match bestPlayer, bestSp with
            | Some p, Some sp -> Some (p, p.Id, spatialXY sp)
            | _ -> None

    let isOffside (player: Player) (playerX: float) (s: MatchState) (dir: AttackDir) =
        if player.Position = GK then
            false
        else
            let defSide = side (ClubSide.toClubId (ClubSide.flip s.AttackingClub) s) s

            // Find two deepest defenders in a single pass — no allocations
            let mutable firstX  = if dir = LeftToRight then -1.0 else 106.0
            let mutable secondX = if dir = LeftToRight then -1.0 else 106.0
            let mutable found   = 0

            for i = 0 to defSide.Players.Length - 1 do
                if defSide.Players[i].Position <> GK then
                    let x = defSide.Positions[i].X
                    found <- found + 1
                    if dir = LeftToRight then
                        if x > firstX then
                            secondX <- firstX
                            firstX  <- x
                        elif x > secondX then
                            secondX <- x
                    else
                        if x < firstX then
                            secondX <- firstX
                            firstX  <- x
                        elif x < secondX then
                            secondX <- x

            if found < 2 then false
            else
                let secondLastX = secondX
                let ballX = s.Ball.Position.X

                let inOwnHalf =
                    match dir with
                    | LeftToRight -> playerX < 50.0
                    | RightToLeft -> playerX > 50.0

                not inOwnHalf
                && match dir with
                   | LeftToRight -> playerX > secondLastX && playerX > ballX
                   | RightToLeft -> playerX < secondLastX && playerX < ballX

    let private secondLastDefenderX (defSide: TeamSide) (dir: AttackDir) : float =
        let mutable firstX  = if dir = LeftToRight then -1.0 else 106.0
        let mutable secondX = if dir = LeftToRight then -1.0 else 106.0
        let mutable found   = 0

        for i = 0 to defSide.Players.Length - 1 do
            if defSide.Players[i].Position <> GK then
                let x = defSide.Positions[i].X
                found <- found + 1
                if dir = LeftToRight then
                    if x > firstX then
                        secondX <- firstX
                        firstX  <- x
                    elif x > secondX then
                        secondX <- x
                else
                    if x < firstX then
                        secondX <- firstX
                        firstX  <- x
                    elif x < secondX then
                        secondX <- x

        if found < 2 then 0.0 else secondX

    let snapshotAtPass (passer: Player) (receiver: Player) (s: MatchState) (dir: AttackDir) : OffsideSnapshot =
        let defSide = ClubSide.teamSide (ClubSide.flip s.AttackingClub) s
        let receiverIdx = defSide.Players |> Array.tryFindIndex (fun p -> p.Id = receiver.Id)
        let receiverX =
            match receiverIdx with
            | Some idx -> defSide.Positions[idx].X
            | None ->
                let attSide = ClubSide.teamSide s.AttackingClub s
                match attSide.Players |> Array.tryFindIndex (fun p -> p.Id = receiver.Id) with
                | Some idx -> attSide.Positions[idx].X
                | None -> 50.0

        { PasserId = passer.Id
          ReceiverId = receiver.Id
          ReceiverXAtPass = receiverX
          SecondLastDefenderX = secondLastDefenderX defSide dir
          BallXAtPass = s.Ball.Position.X
          Dir = dir }

    let isOffsideFromSnapshot (snapshot: OffsideSnapshot) : bool =
        match snapshot.Dir with
        | LeftToRight ->
            snapshot.ReceiverXAtPass > snapshot.SecondLastDefenderX
            && snapshot.ReceiverXAtPass > snapshot.BallXAtPass
        | RightToLeft ->
            snapshot.ReceiverXAtPass < snapshot.SecondLastDefenderX
            && snapshot.ReceiverXAtPass < snapshot.BallXAtPass
