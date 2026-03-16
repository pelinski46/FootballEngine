namespace FootballEngine

open System
open FootballEngine.Domain

open FSharp.Stats.Distributions


module MatchStats =

    let nextNormalInt (mean: float) (stdDev: float) (lo: int) (hi: int) =
        let sample = Continuous.Normal.Sample mean stdDev
        Math.Clamp(int (Math.Round(sample)), lo, hi)

    let inline effectiveStat (stat: int) (condition: int) (morale: int) (sigma: float) =
        let baseValue =
            float stat * (float condition / 100.0) * (0.8 + (float morale / 500.0))

        Continuous.Normal.Sample baseValue sigma

    let attackEffort (phase: MatchPhase) (att: Player) (cond: int) =
        match phase with
        | BuildUp ->
            effectiveStat att.Technical.Passing cond att.Morale 2.0
            + effectiveStat att.Mental.Vision cond att.Morale 1.5
            + effectiveStat att.Mental.Composure cond att.Morale 1.0
        | Midfield ->
            effectiveStat att.Technical.BallControl cond att.Morale 2.0
            + effectiveStat att.Mental.Vision cond att.Morale 2.0
            + effectiveStat att.Technical.Dribbling cond att.Morale 1.5
        | Attack ->
            effectiveStat att.Technical.Dribbling cond att.Morale 2.5
            + effectiveStat att.Physical.Pace cond att.Morale 2.0
            + effectiveStat att.Physical.Agility cond att.Morale 1.5

    let defenseEffort (def: Player) (cond: int) =
        effectiveStat def.Technical.Tackling cond def.Morale 2.0
        + effectiveStat def.Mental.Positioning cond def.Morale 2.0
        + effectiveStat def.Physical.Strength cond def.Morale 1.0
        + effectiveStat def.Mental.Concentration cond def.Morale 1.0

    type PlayerRole =
        | Defender
        | Midfielder
        | Attacker
        | Goalkeeper

    let playerRole (p: Player) =
        match p.Position with
        | GK -> Goalkeeper
        | DC
        | DL
        | DR
        | DM -> Defender
        | MC
        | AML
        | AMR
        | AMC -> Midfielder
        | ST -> Attacker
        | _ -> Midfielder

    let inline distance (x1, y1) (x2, y2) =
        sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)

    let inline nearestIdx (positions: (float * float)[]) (point: float * float) =
        positions
        |> Array.mapi (fun i pos -> i, distance point pos)
        |> Array.minBy snd
        |> fst

    // ── TeamSide lens ────────────────────────────────────────────────────────
    // Eliminates the `if isHome then x else y` pattern throughout MatchSimulator.
    // All reads and writes go through this record rather than branching on isHome.

    type TeamSide =
        { Players: Player[]
          Conditions: int[]
          Positions: Map<PlayerId, float * float>
          BasePositions: Map<PlayerId, float * float>
          Sidelined: Map<PlayerId, PlayerOut>
          Yellows: Map<PlayerId, int>
          SubsUsed: int }

    let homeSide (s: MatchState) : TeamSide =
        { Players = s.HomePlayers
          Conditions = s.HomeConditions
          Positions = s.HomePositions
          BasePositions = s.HomeBasePositions
          Sidelined = s.HomeSidelined
          Yellows = s.HomeYellows
          SubsUsed = s.HomeSubsUsed }

    let awaySide (s: MatchState) : TeamSide =
        { Players = s.AwayPlayers
          Conditions = s.AwayConditions
          Positions = s.AwayPositions
          BasePositions = s.AwayBasePositions
          Sidelined = s.AwaySidelined
          Yellows = s.AwayYellows
          SubsUsed = s.AwaySubsUsed }

    let side (isHome: bool) (s: MatchState) : TeamSide =
        if isHome then homeSide s else awaySide s

    let withSide (isHome: bool) (ts: TeamSide) (s: MatchState) : MatchState =
        if isHome then
            { s with
                HomePlayers = ts.Players
                HomeConditions = ts.Conditions
                HomePositions = ts.Positions
                HomeBasePositions = ts.BasePositions
                HomeSidelined = ts.Sidelined
                HomeYellows = ts.Yellows
                HomeSubsUsed = ts.SubsUsed }
        else
            { s with
                AwayPlayers = ts.Players
                AwayConditions = ts.Conditions
                AwayPositions = ts.Positions
                AwayBasePositions = ts.BasePositions
                AwaySidelined = ts.Sidelined
                AwayYellows = ts.Yellows
                AwaySubsUsed = ts.SubsUsed }
