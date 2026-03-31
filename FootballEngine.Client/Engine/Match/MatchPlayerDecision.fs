namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats
open MatchState

module MatchPlayerDecision =

    type PlayerIntent =
        | ResolveDuel of attacker: Player * defender: Player * ai: int * di: int
        | ExecuteShot of attacker: Player
        | ExecutePass of attacker: Player
        | ExecuteDribble of attacker: Player
        | ExecuteCross of attacker: Player
        | ExecuteLongBall of attacker: Player
        | ExecuteTackle of defender: Player
        | ExecuteFreeKick of kicker: Player
        | ExecuteCorner
        | ExecuteThrowIn of throwTeam: Possession
        | ExecutePenalty of kicker: Player * isHome: bool * kickNum: int
        | PlayerIdle

    let private isOffside (player: Player) (playerX: float) (s: MatchState) (isHome: bool) =
        if player.Position = GK then
            false
        else
            let defSide = side (not isHome) s

            let defenders =
                defSide.Players
                |> Array.mapi (fun i p -> i, p)
                |> Array.filter (fun (_, p) -> p.Position <> GK)
                |> Array.map (fun (i, _) -> defSide.Positions[i])

            if defenders.Length < 2 then
                false
            else
                let lastTwo =
                    defenders
                    |> Array.sortBy (fun sp -> if isHome then -sp.X else sp.X)
                    |> Array.take 2

                let secondLastX = lastTwo |> Array.last |> (fun sp -> sp.X)
                let ballX = s.Ball.Position.X
                let inOwnHalf = if isHome then playerX < 50.0 else playerX > 50.0

                not inOwnHalf
                && ((isHome && playerX > secondLastX && playerX > ballX)
                    || (not isHome && playerX < secondLastX && playerX < ballX))

    let private computeShotQuality (ballX: float) (shooter: Player) (attIsHome: bool) =
        let distToGoal = if attIsHome then 100.0 - ballX else ballX
        let distNorm = Math.Clamp(distToGoal / 30.0, 0.0, 1.0)

        let positionBonus =
            match shooter.Position with
            | ST
            | AMC -> 0.3
            | AML
            | AMR
            | MC -> 0.2
            | _ -> 0.0

        (1.0 - distNorm) * 0.7 + positionBonus * 0.3

    let decide (s: MatchState) : PlayerIntent =
        match Pitch.pickDuel s with
        | None -> PlayerIdle
        | Some(att, def, ai, di) -> ResolveDuel(att, def, ai, di)

    let decideNextAttack (attacker: Player) (s: MatchState) : PlayerIntent =
        let shotT, passT, dribbleT, crossT =
            BalanceCalibrator.cumulative (BalanceCalibrator.computeThresholds ())

        let bX = s.Ball.Position.X

        let shotMultiplier =
            if (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0) then
                2.5
            elif (s.Possession = Home && bX >= 60.0) || (s.Possession = Away && bX <= 40.0) then
                1.5
            else
                0.3

        let effectiveShotT = Math.Min(shotT * shotMultiplier, passT - 0.01)

        let r = rollProbability ()

        if r < effectiveShotT then ExecuteShot attacker
        elif r < passT then ExecutePass attacker
        elif r < dribbleT then ExecuteDribble attacker
        elif r < crossT then ExecuteCross attacker
        else ExecuteLongBall attacker

    let isOffsideCheck (player: Player) (playerX: float) (s: MatchState) (isHome: bool) =
        isOffside player playerX s isHome

    let shotQuality (ballX: float) (shooter: Player) (attIsHome: bool) =
        computeShotQuality ballX shooter attIsHome
