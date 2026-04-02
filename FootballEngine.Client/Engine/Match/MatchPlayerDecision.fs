namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Stats

module MatchPlayerDecision =

    type PlayerIntent =
        | ResolveDuel of attacker: Player * defender: Player
        | ExecuteShot of attacker: Player
        | ExecutePass of attacker: Player
        | ExecuteDribble of attacker: Player
        | ExecuteCross of attacker: Player
        | ExecuteLongBall of attacker: Player
        | ExecuteTackle of defender: Player
        | ExecuteFreeKick of kicker: Player
        | ExecuteCorner
        | ExecuteThrowIn of throwClub: ClubSide
        | ExecutePenalty of kicker: Player * kickerClub: ClubSide * kickNum: int
        | PlayerIdle

    let private computeShotQuality (ballX: float) (shooter: Player) (dir: AttackDir) =
        let distToGoal = AttackDir.distToGoal ballX dir

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
        | Some(att, def) -> ResolveDuel(att, def)

    let decideNextAttack (attacker: Player) (s: MatchState) : PlayerIntent =
        let shotT, passT, dribbleT, crossT =
            BalanceCalibrator.cumulative (BalanceCalibrator.computeThresholds ())

        let bX = s.Ball.Position.X
        let dir = AttackDir.ofClubSide s.AttackingClub
        let zone = PitchZone.ofBallX bX dir

        let shotMultiplier =
            match zone with
            | AttackingZone -> 2.5
            | MidfieldZone -> 1.5
            | DefensiveZone -> 0.3

        let effectiveShotT = Math.Min(shotT * shotMultiplier, passT - 0.01)

        let r = rollProbability ()

        if r < effectiveShotT then ExecuteShot attacker
        elif r < passT then ExecutePass attacker
        elif r < dribbleT then ExecuteDribble attacker
        elif r < crossT then ExecuteCross attacker
        else ExecuteLongBall attacker

    let shotQuality (ballX: float) (shooter: Player) (dir: AttackDir) = computeShotQuality ballX shooter dir
