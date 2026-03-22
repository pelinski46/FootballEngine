namespace FootballEngine

open System
open FootballEngine.Domain
open FSharp.Stats.Distributions
open MatchState
open MatchStats

module Pitch =

    let private activePlayers (ts: TeamSide) =
        let idx = activeIndices ts.Players ts.Sidelined

        idx |> Array.map (fun i -> ts.Players[i]),
        idx |> Array.map (fun i -> positionOf ts.Positions ts.Players[i]),
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

    let jitter oX oY tX tY scale nx ny =
        Math.Clamp(oX + (tX - oX) * scale + Continuous.Normal.Sample 0.0 nx, 0.0, 100.0),
        Math.Clamp(oY + (tY - oY) * scale + Continuous.Normal.Sample 0.0 ny, 0.0, 100.0)

    let pickDuel (s: MatchState) =
        let v = buildView s
        let ai = nearestIdx v.APos s.BallPosition
        let di = nearestIdx v.DPos s.BallPosition
        v.Att[ai], v.Def[di], ai, di

    let private tacticalTarget (p: Player) baseX baseY ballX ballY isPossessing =
        let offPush, defPull, lateral =
            match p.Position with
            | GK -> 0.05, 0.02, 0.05
            | DC -> 0.15, 0.08, 0.20
            | DR
            | DL -> 0.20, 0.10, 0.35
            | WBR
            | WBL -> 0.30, 0.12, 0.40
            | DM -> 0.25, 0.15, 0.30
            | MC -> 0.30, 0.20, 0.30
            | MR
            | ML -> 0.30, 0.18, 0.40
            | AMR
            | AMC
            | AML -> 0.40, 0.25, 0.35
            | ST -> 0.45, 0.30, 0.20

        let push = if isPossessing then offPush else defPull
        Math.Clamp(baseX + (ballX - baseX) * push, 2.0, 98.0), Math.Clamp(baseY + (ballY - baseY) * lateral, 2.0, 98.0)

    let updatePositions (s: MatchState) : MatchState =
        let ballX, ballY = s.BallPosition

        let move (ts: TeamSide) (isPossessing: bool) =
            { ts with
                Positions =
                    ts.Players
                    |> Array.fold
                        (fun acc p ->
                            let curX, curY = acc |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

                            let baseX, baseY =
                                ts.BasePositions |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

                            let tx, ty = tacticalTarget p baseX baseY ballX ballY isPossessing

                            Map.add
                                p.Id
                                (curX + ((tx + (baseX - tx) * 0.25) - curX) * 0.15,
                                 curY + ((ty + (baseY - ty) * 0.25) - curY) * 0.15)
                                acc)
                        ts.Positions }

        s
        |> withSide true (move (homeSide s) (s.Possession = Home))
        |> withSide false (move (awaySide s) (s.Possession = Away))
