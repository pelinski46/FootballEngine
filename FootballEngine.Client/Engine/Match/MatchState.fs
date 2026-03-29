namespace FootballEngine

open FootballEngine.Domain
open TacticalInstructions

module MatchState =

    type TacticsConfig =
        { PressureDistance: float
          UrgencyMultiplier: float
          ForwardPush: float
          DefensiveDrop: float
          PressingIntensity: float
        }

    let private baseTacticsConfig =
        function
        | Balanced ->
            { PressureDistance = 0.0
              UrgencyMultiplier = 1.0
              ForwardPush = 0.0
              DefensiveDrop = 0.0
              PressingIntensity = 1.0 }
        | Attacking ->
            { PressureDistance = 8.0
              UrgencyMultiplier = 1.15
              ForwardPush = 10.0
              DefensiveDrop = -5.0
              PressingIntensity = 1.2 }
        | Defensive ->
            { PressureDistance = -10.0
              UrgencyMultiplier = 0.9
              ForwardPush = -5.0
              DefensiveDrop = 8.0
              PressingIntensity = 0.7 }
        | Pressing ->
            { PressureDistance = 12.0
              UrgencyMultiplier = 1.1
              ForwardPush = 8.0
              DefensiveDrop = -3.0
              PressingIntensity = 1.5 }
        | Counter ->
            { PressureDistance = -6.0
              UrgencyMultiplier = 1.2
              ForwardPush = -8.0
              DefensiveDrop = 6.0
              PressingIntensity = 0.8 }

    let tacticsConfig (teamTactics: TeamTactics) (instructions: TacticalInstructions option) =
        let baseCfg = baseTacticsConfig teamTactics
        let instr = instructions |> Option.defaultValue TacticalInstructions.defaultInstructions

        let mentalityMod = float (instr.Mentality - 2) * 0.08
        let defensiveLineMod = float (instr.DefensiveLine - 2) * 3.0
        let pressingMod = float (instr.PressingIntensity - 2) * 0.15
        
        { PressureDistance = baseCfg.PressureDistance + defensiveLineMod
          UrgencyMultiplier = baseCfg.UrgencyMultiplier * (1.0 + mentalityMod)
          ForwardPush = baseCfg.ForwardPush + mentalityMod * 5.0 + defensiveLineMod * 0.5
          DefensiveDrop = baseCfg.DefensiveDrop - mentalityMod * 5.0 - defensiveLineMod * 0.5
          PressingIntensity = baseCfg.PressingIntensity * (1.0 + pressingMod) }

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
