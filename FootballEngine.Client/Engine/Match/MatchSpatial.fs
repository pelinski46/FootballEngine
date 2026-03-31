namespace FootballEngine

open FootballEngine.Domain
open MatchState

module MatchSpatial =

    type PlayerWithIdx =
        { Player: Player
          Idx: int
          Pos: Spatial
          Condition: int }

    let spatialXY (sp: Spatial) = sp.X, sp.Y

    let ballXY (s: MatchState) = s.Ball.Position.X, s.Ball.Position.Y

    let withBallVelocity (vx: float) (vy: float) (vz: float) (s: MatchState) =
        let pos = s.Ball.Position

        { s with
            Ball =
                { s.Ball with
                    Position = { pos with Vx = vx; Vy = vy; Vz = vz } } }

    let getPlayerInfo (players: Player[]) (positions: Spatial[]) (conditions: int[]) (pid: PlayerId) =
        match players |> Array.tryFindIndex (fun p -> p.Id = pid) with
        | Some i ->
            { Player = players[i]
              Idx = i
              Pos = positions[i]
              Condition = conditions[i] }
        | None ->
            { Player = players[0]
              Idx = 0
              Pos = positions[0]
              Condition = conditions[0] }

    let findNearestTeammate (attacker: Player) (s: MatchState) (isHome: bool) =
        let attSide = side isHome s

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let tx, ty = spatialXY info.Pos

        attSide.Players
        |> Array.mapi (fun i p -> i, p, attSide.Positions[i])
        |> Array.filter (fun (i, _, _) -> i <> info.Idx)
        |> Array.minBy (fun (_, _, sp) -> let dx, dy = sp.X - tx, sp.Y - ty in sqrt (dx * dx + dy * dy))
        |> fun (i, p, sp) -> Some(p, i, spatialXY sp)

    let findNearestOpponent (attacker: Player) (s: MatchState) (isHome: bool) =
        let attSide = side isHome s
        let defSide = side (not isHome) s

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let ax, ay = spatialXY info.Pos

        if defSide.Players.Length = 0 then
            (attacker, 0, spatialXY attSide.Positions[0])
        else
            defSide.Players
            |> Array.mapi (fun i p -> i, p, defSide.Positions[i])
            |> Array.filter (fun (_, p, _) -> p.Position <> GK)
            |> Array.minBy (fun (_, _, sp) -> let dx, dy = sp.X - ax, sp.Y - ay in sqrt (dx * dx + dy * dy))
            |> fun (i, p, sp) -> (p, i, spatialXY sp)

    let findBestPassTarget (attacker: Player) (s: MatchState) (isHome: bool) =
        let attSide = side isHome s
        let defSide = side (not isHome) s

        let info =
            getPlayerInfo attSide.Players attSide.Positions attSide.Conditions attacker.Id

        let ballX, ballY = spatialXY info.Pos

        let isForward (sp: Spatial) =
            if isHome then sp.X > ballX else sp.X < ballX

        let passLaneClear (targetSp: Spatial) =
            let defendersNearLine =
                defSide.Players
                |> Array.mapi (fun di dp -> di, dp)
                |> Array.filter (fun (_, p) -> p.Position <> GK)
                |> Array.sumBy (fun (di, _) ->
                    let dSp = defSide.Positions[di]
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

        attSide.Players
        |> Array.mapi (fun i p ->
            let sp = attSide.Positions[i]
            let dist = sqrt ((sp.X - ballX) ** 2.0 + (sp.Y - ballY) ** 2.0)
            let forwardBonus = if isForward sp then 0.15 else 0.0
            let laneBonus = if passLaneClear sp then 0.2 else 0.0
            let visionWeight = float attacker.Mental.Vision / 100.0

            let score =
                (1.0 / (1.0 + dist * 0.1)) + forwardBonus + laneBonus + visionWeight * 0.1

            i, p, sp, score)
        |> Array.filter (fun (i, _, _, _) -> i <> info.Idx)
        |> Array.sortByDescending (fun (_, _, _, score) -> score)
        |> Array.tryHead
        |> Option.map (fun (i, p, sp, _) -> p, i, spatialXY sp)
