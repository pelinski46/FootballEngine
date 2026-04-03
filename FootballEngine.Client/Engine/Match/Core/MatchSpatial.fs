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
        Array.zip3 ts.Players ts.Positions ts.Conditions

    let outfieldRoster (ts: TeamSide) : (Player * Spatial * int)[] =
        teamRoster ts |> Array.filter (fun (p, _, _) -> p.Position <> GK)

    let nearestOutfield (ts: TeamSide) (x: float) (y: float) : (Player * Spatial) option =
        outfieldRoster ts
        |> Array.map (fun (p, sp, _) -> p, sp)
        |> Array.sortBy (fun (_, sp) ->
            let dx, dy = sp.X - x, sp.Y - y
            dx * dx + dy * dy)
        |> Array.tryHead

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

        match getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id with
        | None -> None
        | Some info ->
            let tx, ty = spatialXY info.Pos

            outfieldRoster attSide
            |> Array.filter (fun (p, _, _) -> p.Id <> attacker.Id)
            |> Array.map (fun (p, sp, _) -> p, sp, sp.X - tx, sp.Y - ty)
            |> Array.sortBy (fun (_, _, dx, dy) -> dx * dx + dy * dy)
            |> Array.tryHead
            |> Option.map (fun (p, sp, _, _) -> p, p.Id, spatialXY sp)

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
                let defendersNearLine =
                    outfieldRoster defSide
                    |> Array.sumBy (fun (_, dSp, _) ->
                        let dx = dSp.X - ballX
                        let dy = dSp.Y - ballY
                        let tdx = targetSp.X - ballX
                        let tdy = targetSp.Y - ballY
                        let lenSq = tdx * tdx + tdy * tdy

                        if lenSq < 0.01 then
                            0.0
                        else
                            let t = (dx * tdx + dy * tdy) / lenSq

                            if t < 0.0 || t > 1.0 then
                                0.0
                            else
                                let projX = ballX + t * tdx
                                let projY = ballY + t * tdy
                                let dist = sqrt ((dSp.X - projX) ** 2.0 + (dSp.Y - projY) ** 2.0)
                                if dist < 3.0 then 1.0 else 0.0)

                defendersNearLine = 0.0

            teamRoster attSide
            |> Array.filter (fun (p, _, _) -> p.Id <> attacker.Id)
            |> Array.map (fun (p, sp, _) ->
                let dist = sqrt ((sp.X - ballX) ** 2.0 + (sp.Y - ballY) ** 2.0)
                let forwardBonus = if isForward sp then 0.15 else 0.0
                let laneBonus = if passLaneClear sp then 0.2 else 0.0
                let visionWeight = float attacker.Mental.Vision / 100.0

                let score =
                    (1.0 / (1.0 + dist * 0.1)) + forwardBonus + laneBonus + visionWeight * 0.1

                p, sp, score)
            |> Array.sortByDescending (fun (_, _, score) -> score)
            |> Array.tryHead
            |> Option.map (fun (p, sp, _) -> p, p.Id, spatialXY sp)

    let isOffside (player: Player) (playerX: float) (s: MatchState) (dir: AttackDir) =
        if player.Position = GK then
            false
        else
            let defSide = side (ClubSide.toClubId (ClubSide.flip s.AttackingClub) s) s

            let defenders = outfieldRoster defSide |> Array.map (fun (_, sp, _) -> sp)

            if defenders.Length < 2 then
                false
            else
                let lastTwo =
                    defenders
                    |> Array.sortBy (fun sp ->
                        match dir with
                        | LeftToRight -> -sp.X
                        | RightToLeft -> sp.X)
                    |> Array.take 2

                let secondLastX = lastTwo |> Array.last |> (fun sp -> sp.X)
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
        let defenders =
            defSide.Players
            |> Array.mapi (fun i p -> i, p)
            |> Array.filter (fun (_, p) -> p.Position <> GK)

        if defenders.Length < 2 then 0.0
        else
            defenders
            |> Array.sortBy (fun (i, _) ->
                match dir with
                | LeftToRight -> -defSide.Positions[i].X
                | RightToLeft -> defSide.Positions[i].X)
            |> Array.take 2
            |> Array.last
            |> fun (i, _) -> defSide.Positions[i].X

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
