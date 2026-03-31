namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.MatchCalc
open FootballEngine.Stats
open MatchState
open PitchMath

module MatchPlayerMovement =

    let moveSpeed (p: Player) (condition: int) =
        let pace = float p.Physical.Pace
        let accel = float p.Physical.Acceleration
        let cond = float condition / 100.0
        Math.Clamp((pace * 0.6 + accel * 0.4) / 100.0 * cond * 0.45, 0.05, 0.45)

    let decideTarget
        (p: Player)
        (baseX: float)
        (baseY: float)
        (ballX: float)
        (ballY: float)
        (isPossessing: bool)
        (tactics: TacticsConfig)
        =
        let positioning = float p.Mental.Positioning / 100.0
        let vision = float p.Mental.Vision / 100.0
        let workRate = float p.Mental.WorkRate / 100.0
        let agility = float p.Physical.Agility / 100.0

        let baseOff, baseDef, baseLat = MovementConstants.positionCoefficients p.Position
        let modifier = MovementConstants.positionModifiers p.Position
        let attrs = (positioning, workRate, vision)
        let offPush, defPull, lateral = modifier (baseOff, baseDef, baseLat) attrs

        let push = if isPossessing then offPush else defPull

        let shapeX = baseX + (ballX - baseX) * push
        let shapeY = baseY + (ballY - baseY) * lateral

        let forwardBias =
            if isPossessing then
                match p.Position with
                | ST
                | AML
                | AMR
                | AMC -> 6.0
                | MC
                | ML
                | MR -> 3.0
                | _ -> 0.0
            else
                0.0

        let compactX =
            shapeX + tactics.ForwardPush * (if isPossessing then 0.5 else 0.3) + forwardBias

        let compactY = shapeY + tactics.PressureDistance * 0.2

        let tx = Math.Clamp(compactX, 2.0, 98.0)
        let ty = Math.Clamp(compactY, 2.0, 98.0)
        let jitterScale = 0.3 + agility * 0.5

        Math.Clamp(tx + normalSample 0.0 jitterScale, 2.0, 98.0),
        Math.Clamp(ty + normalSample 0.0 jitterScale, 2.0, 98.0)

    let separationForce (myIdx: int) (myPos: float * float) (teamPositions: Spatial[]) (agility: float) =
        let minDist = 4.0
        let mx, my = myPos

        teamPositions
        |> Array.mapi (fun i sp -> i, sp)
        |> Array.filter (fun (i, _) -> i <> myIdx)
        |> Array.fold
            (fun (fx, fy) (_, sp) ->
                let dx, dy = mx - sp.X, my - sp.Y
                let dist = sqrt (dx * dx + dy * dy)

                if dist < minDist && dist > 0.001 then
                    let strength = (minDist - dist) / minDist * (0.1 + agility * 0.15)
                    fx + dx / dist * strength, fy + dy / dist * strength
                else
                    fx, fy)
            (0.0, 0.0)

    let updatePlayerVelocities (dt: float) (s: MatchState) : MatchState =
        let ballX = s.Ball.Position.X
        let ballY = s.Ball.Position.Y

        let updateSide (ts: TeamSide) (isPossessing: bool) =
            let tacticsCfg = tacticsConfig ts.Tactics ts.Instructions

            let newPositions =
                ts.Players
                |> Array.mapi (fun i p ->
                    let sp = ts.Positions[i]
                    let baseSp = ts.BasePositions[i]
                    let cond = ts.Conditions[i]
                    let tx, ty = decideTarget p baseSp.X baseSp.Y ballX ballY isPossessing tacticsCfg
                    let speed = moveSpeed p cond

                    let sepX, sepY =
                        separationForce i (sp.X, sp.Y) ts.Positions (float p.Physical.Agility / 100.0)

                    let targetVx = (tx - sp.X) * speed + sepX
                    let targetVy = (ty - sp.Y) * speed + sepY

                    { sp with Vx = targetVx; Vy = targetVy })

            { ts with Positions = newPositions }

        s
        |> withSide true (updateSide (homeSide s) (s.Possession = Home))
        |> withSide false (updateSide (awaySide s) (s.Possession = Away))
