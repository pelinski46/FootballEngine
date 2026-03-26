namespace FootballEngine

open FootballEngine.Domain

module MatchState =

    let flipPossession =
        function
        | Home -> Away
        | Away -> Home

    let phaseFromBallZone (x: float) =
        if x < 30.0 || x > 70.0 then BuildUp
        elif x >= 40.0 && x <= 60.0 then Midfield
        else Attack

    let activeIndices (players: Player[]) (sidelined: Map<PlayerId, PlayerOut>) =
        players
        |> Array.mapi (fun i p -> i, p)
        |> Array.filter (fun (_, p) -> not (Map.containsKey p.Id sidelined))
        |> Array.map fst

    let positionOf (positions: Map<PlayerId, float * float>) (p: Player) =
        positions |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

    let goalDiff (isHome: bool) (s: MatchState) =
        if isHome then
            s.HomeScore - s.AwayScore
        else
            s.AwayScore - s.HomeScore

    let pressureMultiplier (isHome: bool) (s: MatchState) =
        1.1 - float (max -2 (min 2 (goalDiff isHome s))) * 0.25

    let addEvent (ev: MatchEvent) (s: MatchState) =
        { s with EventsRev = ev :: s.EventsRev }

    let homeSide (s: MatchState) = s.HomeSide
    let awaySide (s: MatchState) = s.AwaySide

    let side (isHome: bool) (s: MatchState) =
        if isHome then s.HomeSide else s.AwaySide

    let withSide (isHome: bool) (ts: TeamSide) (s: MatchState) =
        if isHome then
            { s with HomeSide = ts }
        else
            { s with AwaySide = ts }
