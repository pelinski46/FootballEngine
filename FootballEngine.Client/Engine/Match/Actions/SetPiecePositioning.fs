namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

module SetPiecePositioning =

    // Compute set-piece target positions for players on clubSide.
    // For corners we assign realistic roles: taker, near-post, far-post, box (2), edge (1-2), stay-back (1-2).
    let computePositions (ctx: MatchContext) (state: SimState) (clubSide: ClubSide) : Spatial[] =
        let frame = SimStateOps.getFrame state clubSide
        let roster = SimStateOps.getRoster ctx clubSide
        let slots = frame.SlotCount

        // Helper to create spatial from frame arrays
        let fromFrame i =
            { X = float frame.PosX[i] * 1.0<meter>
              Y = float frame.PosY[i] * 1.0<meter>
              Z = 0.0<meter>
              Vx = 0.0<meter/second>
              Vy = 0.0<meter/second>
              Vz = 0.0<meter/second> }

        // default: current positions
        let positions = Array.init slots (fun i -> fromFrame i)

        match state.Ball.Possession with
        | SetPiece(side, SetPieceKind.Corner) when side = clubSide ->
            let ballPos = state.Ball.Position
            // nearest active slot is the natural taker
            match SimStateOps.nearestActiveSlotInFrame frame ballPos.X ballPos.Y with
            | ValueNone -> positions
            | ValueSome takerIdx ->
                // mark taker at ball
                positions[takerIdx] <- { ballPos with Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

                // build candidate list of (slot, player, aerialThreat)
                let candidates =
                    [ for i = 0 to slots - 1 do
                        match SimStateOps.tryGetPlayerFromFrame frame roster i with
                        | Some p when i <> takerIdx -> yield (i, p, (Player.profile p).AerialThreat)
                        | _ -> () ]
                    |> List.sortByDescending (fun (_, _, at) -> at)

                // attack direction helper
                let dir = SimStateOps.attackDirFor clubSide state
                let insideOffset (d: float<meter>) = if dir = LeftToRight then -d else d
                let goalX = if dir = LeftToRight then PhysicsContract.GoalLineHome else PhysicsContract.GoalLineAway
                let centerY = PhysicsContract.PitchWidth / 2.0

                // role placements (best-effort)
                let mutable idx = 0

                // near-post
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset 1.0<meter>; Y = PhysicsContract.PostNearY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1

                // far-post
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset 1.0<meter>; Y = PhysicsContract.PostFarY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1

                // two box positions (~6m from goal)
                let boxDist = 6.0<meter>
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset boxDist; Y = centerY - 1.5<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset boxDist; Y = centerY + 1.5<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1

                // penalty spot (~11m)
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset PhysicsContract.PenaltySpotDistance; Y = centerY; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1

                // edge of area (~18m) one or two
                let edgeDist = 18.0<meter>
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset edgeDist; Y = centerY - 6.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1
                if candidates.Length > idx then
                    let sIdx, _, _ = candidates[idx]
                    positions[sIdx] <- { X = goalX + insideOffset edgeDist; Y = centerY + 6.0<meter>; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }
                    idx <- idx + 1

                // remaining players: pick 1-2 defensive/stay-back by position
                let stayBackCount = 2
                let defenders =
                    [ for i = 0 to slots - 1 do
                        match SimStateOps.tryGetPlayerFromFrame frame roster i with
                        | Some p when i <> takerIdx ->
                            match p.Position with
                            | DC | DM | GK -> yield (i, p)
                            | _ -> ()
                        | _ -> () ]
                let sb = defenders |> List.truncate stayBackCount
                for (sIdx, _) in sb do
                    // keep near own half a bit
                    let safeDist = PhysicsContract.PenaltyAreaDepth + 5.0<meter>
                    let safeX = goalX - (PhysicsContract.forwardX dir) * safeDist
                    positions[sIdx] <- { X = safeX; Y = positions[sIdx].Y; Z = 0.0<meter>; Vx = 0.0<meter/second>; Vy = 0.0<meter/second>; Vz = 0.0<meter/second> }

                positions
        | _ -> positions

    let assignPositions (ctx: MatchContext) (positions: Spatial[]) (clubSide: ClubSide) : unit =
        // No-op for now; higher level code will read positions and set intents.
        ()
