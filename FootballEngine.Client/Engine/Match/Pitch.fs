namespace FootballEngine

open System
open FootballEngine.Domain
open MatchState
open PitchMath
open PlayerMovement

module Pitch =

    let private activePlayers (ts: TeamSide) =
        let idx = activeIndices ts.Players ts.Sidelined

        idx |> Array.map (fun i -> ts.Players[i]),
        idx |> Array.map (fun i -> ts.Positions[i]),
        idx |> Array.map (fun i -> ts.Conditions[i])

    type PitchView =
        { Att: Player[]
          APos: (float * float)[]
          ACond: int[]
          AttIsHome: bool
          Def: Player[]
          DPos: (float * float)[]
          DCond: int[] }

    let buildView (s: MatchState) =
        let attIsHome = s.Possession = Home
        let at, ap, ac = activePlayers (side attIsHome s)
        let dt, dp, dc = activePlayers (side (not attIsHome) s)

        { Att = at
          APos = ap
          ACond = ac
          AttIsHome = attIsHome
          Def = dt
          DPos = dp
          DCond = dc }

    let pickDuel (s: MatchState) =
        let v = buildView s

        if v.Att.Length = 0 || v.Def.Length = 0 then
            None
        else
            let ai = nearestIdx v.APos s.BallPosition
            let di = nearestIdx v.DPos s.BallPosition
            Some(v.Att[ai], v.Def[di], ai, di)

    let private snapBallToPossessor (s: MatchState) =
        let attSide = if s.Possession = Home then s.HomeSide else s.AwaySide

        if attSide.Players.Length = 0 then
            s
        else
            let nearestIdx =
                attSide.Positions
                |> Array.mapi (fun i pos -> i, distance pos s.BallPosition)
                |> Array.minBy snd
                |> fst

            let px, py = attSide.Positions[nearestIdx]
            { s with BallPosition = px, py }

    let updatePositions (s: MatchState) : MatchState =
        let ballX, ballY = s.BallPosition

        let move (ts: TeamSide) (isPossessing: bool) =
            let newPositions =
                ts.Players
                |> Array.mapi (fun i p ->
                    let curX, curY = ts.Positions[i]
                    let baseX, baseY = ts.BasePositions[i]
                    let cond = ts.Conditions[i]
                    let tx, ty = decideTarget p baseX baseY ballX ballY isPossessing
                    let speed = moveSpeed p cond

                    let sx, sy =
                        separationForce i (curX, curY) ts.Positions (float p.Physical.Agility / 100.0)

                    Math.Clamp(curX + (tx - curX) * speed + sx, 0.0, 100.0),
                    Math.Clamp(curY + (ty - curY) * speed + sy, 0.0, 100.0))

            { ts with Positions = newPositions }

        s
        |> withSide true (move (homeSide s) (s.Possession = Home))
        |> withSide false (move (awaySide s) (s.Possession = Away))
        |> snapBallToPossessor
