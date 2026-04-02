namespace FootballEngine

open System
open FootballEngine.Domain
open MatchStateOps
open PitchMath


module Pitch =

    let private activePlayers (ts: TeamSide) =
        let idx = activeIndices ts.Players ts.Sidelined

        idx |> Array.map (fun i -> ts.Players[i]),
        idx |> Array.map (fun i -> ts.Positions[i]),
        idx |> Array.map (fun i -> ts.Conditions[i])

    type PitchView =
        { Att: Player[]
          APos: Spatial[]
          ACond: int[]
          AttIsHome: bool
          Def: Player[]
          DPos: Spatial[]
          DCond: int[] }

    let buildView (s: MatchState) =
        let at, ap, ac = activePlayers (ClubSide.teamSide s.AttackingClub s)
        let dt, dp, dc = activePlayers (ClubSide.teamSide (ClubSide.flip s.AttackingClub) s)

        { Att = at
          APos = ap
          ACond = ac
          AttIsHome = s.AttackingClub = HomeClub
          Def = dt
          DPos = dp
          DCond = dc }

    let pickDuel (s: MatchState) =
        let v = buildView s
        let ballPt = s.Ball.Position.X, s.Ball.Position.Y

        if v.Att.Length = 0 || v.Def.Length = 0 then
            None
        else
            let aPoints = v.APos |> Array.map (fun sp -> sp.X, sp.Y)
            let dPoints = v.DPos |> Array.map (fun sp -> sp.X, sp.Y)
            let ai = nearestIdx aPoints ballPt
            let di = nearestIdx dPoints ballPt
            Some(v.Att[ai], v.Def[di])

    let private controlBallByProximity (s: MatchState) =
        let attSide = ClubSide.teamSide s.AttackingClub s

        if attSide.Players.Length = 0 then
            s
        else
            let ballPt = s.Ball.Position.X, s.Ball.Position.Y

            let nearestIdx, nearestDist =
                attSide.Positions
                |> Array.mapi (fun i sp -> i, distance (sp.X, sp.Y) ballPt)
                |> Array.minBy snd

            if nearestDist < BalanceConfig.BallControlDistanceThreshold then
                let sp = attSide.Positions[nearestIdx]
                let bp = s.Ball.Position

                let newX = bp.X + (sp.X - bp.X) * BalanceConfig.BallControlDampingX
                let newY = bp.Y + (sp.Y - bp.Y) * BalanceConfig.BallControlDampingY
                let newVx = bp.Vx * BalanceConfig.BallControlVx
                let newVy = bp.Vy * BalanceConfig.BallControlVy

                { s with
                    Ball =
                        { s.Ball with
                            Position =
                                { bp with
                                    X = newX
                                    Y = newY
                                    Vx = newVx
                                    Vy = newVy } } }
            else
                s

    let updatePositions (dt: float) (s: MatchState) : MatchState =
        let move (ts: TeamSide) =
            let newPositions =
                ts.Players
                |> Array.mapi (fun i _ ->
                    let sp = ts.Positions[i]

                    { sp with
                        X = Math.Clamp(sp.X + sp.Vx * dt, 0.0, 100.0)
                        Y = Math.Clamp(sp.Y + sp.Vy * dt, 0.0, 100.0) })

            { ts with Positions = newPositions }

        s
        |> withSide s.Home.Id (move (homeSide s))
        |> withSide s.Away.Id (move (awaySide s))
        |> controlBallByProximity
