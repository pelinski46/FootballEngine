namespace FootballEngine

open System
open FootballEngine.Domain
open FSharp.Stats.Distributions
open MatchState
open MatchStats
open Pitch

module MatchPlayer =

    let private attackingClubId (s: MatchState) =
        if s.Possession = Home then s.Home.Id else s.Away.Id

    let resolveDuel (homeId: ClubId) (att: Player, def: Player, ai: int, di: int) (s: MatchState) : MatchState =
        let v = buildView s

        let homeBonus = if s.Home.Id = homeId then 7.0 else -5.0

        let momentum = if s.Possession = Home then s.Momentum else -s.Momentum
        let pressure = (pressureMultiplier v.AttIsHome s - 1.0) * 5.0
        let u = MatchManager.urgency v.AttIsHome s

        let diff =
            attackEffort (phaseFromBallZone (fst s.BallPosition)) att v.ACond[ai] * u
            + homeBonus
            + momentum
            + pressure
            - defenseEffort def v.DCond[di]

        let bX, bY = s.BallPosition
        let aX, aY = v.APos[ai]
        let dX, dY = v.DPos[di]

        if diff > 2.0 then
            { s with
                BallPosition = jitter bX bY aX aY 0.5 10.0 10.0
                Momentum = Math.Clamp(s.Momentum + 0.5, -10.0, 10.0) }
        elif diff > -2.0 then
            { s with
                BallPosition = jitter bX bY bX bY 0.0 3.0 3.0 }
        else
            { s with
                Possession = flipPossession s.Possession
                BallPosition = jitter bX bY dX dY 0.5 2.0 2.0
                Momentum = Math.Clamp(s.Momentum - 1.0, -10.0, 10.0) }

    let tryShot (attacker: Player) (s: MatchState) : MatchState =
        let bX, bY = s.BallPosition

        let inChance =
            (s.Possession = Home && bX >= 70.0) || (s.Possession = Away && bX <= 30.0)

        if not inChance then
            s
        else

            let v = buildView s
            let dist = (if s.Possession = Home then 100.0 - bX else bX) * 0.15
            let composure = pressureMultiplier v.AttIsHome s
            let u = MatchManager.urgency v.AttIsHome s

            let shotPower =
                match v.Att |> Array.tryFindIndex (fun p -> p.Position = ST) with
                | Some i ->
                    let st = v.Att[i]
                    let cond = v.ACond[i]

                    effectiveStat st.Technical.Finishing cond st.Morale 3.5
                    + effectiveStat st.Mental.Composure cond st.Morale (3.0 * composure * u)
                    + effectiveStat st.Technical.LongShots cond st.Morale 2.0
                    + effectiveStat st.Physical.Pace cond st.Morale 1.5
                    - dist
                | None ->
                    effectiveStat attacker.Technical.Finishing 50 attacker.Morale 3.5
                    + effectiveStat attacker.Mental.Composure 50 attacker.Morale (3.0 * composure * u)
                    - dist

            let nearDefenders =
                v.Def
                |> Array.mapi (fun i p -> p, v.DPos[i], v.DCond[i])
                |> Array.filter (fun (p, _, _) -> playerRole p = Defender)
                |> Array.sortBy (fun (_, pos, _) -> distance (bX, bY) pos)
                |> Array.truncate 2
                |> Array.sumBy (fun (p, _, cond) -> effectiveStat p.Technical.Marking cond p.Morale 0.5)

            let savePower =
                match v.Def |> Array.tryFindIndex (fun p -> p.Position = GK) with
                | Some i ->
                    let gk = v.Def[i]

                    effectiveStat gk.Goalkeeping.Reflexes v.DCond[i] gk.Morale 1.5
                    + effectiveStat gk.Goalkeeping.OneOnOne v.DCond[i] gk.Morale 2.0
                    + nearDefenders
                | None -> nearDefenders + Continuous.Normal.Sample 5.0 2.0

            if shotPower > savePower + Continuous.Normal.Sample 2.0 10.0 then
                { s with
                    HomeScore = if s.Possession = Home then s.HomeScore + 1 else s.HomeScore
                    AwayScore = if s.Possession = Away then s.AwayScore + 1 else s.AwayScore
                    BallPosition = 50.0, 50.0
                    Possession = flipPossession s.Possession
                    Momentum = Math.Clamp(s.Momentum + (if s.Possession = Home then 3.0 else -3.0), -10.0, 10.0) }
                |> addEvent
                    { Second = s.Second
                      PlayerId = attacker.Id
                      ClubId = attackingClubId s
                      Type = Goal }
            else
                let rebX =
                    if s.Possession = Home then
                        Continuous.Normal.Sample 75.0 5.0
                    else
                        Continuous.Normal.Sample 25.0 5.0

                { s with
                    BallPosition = rebX, Math.Clamp(Continuous.Normal.Sample bY 10.0, 0.0, 100.0)
                    Possession = flipPossession s.Possession }
