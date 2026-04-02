namespace FootballEngine

open System
open FootballEngine.Domain
open TacticalInstructions

module ClubSide =
    let ofClubId (clubId: ClubId) (s: MatchState) : ClubSide =
        if clubId = s.Home.Id then HomeClub else AwayClub

    let toClubId (side: ClubSide) (s: MatchState) : ClubId =
        match side with
        | HomeClub -> s.Home.Id
        | AwayClub -> s.Away.Id

    let teamSide (side: ClubSide) (s: MatchState) : TeamSide =
        match side with
        | HomeClub -> s.HomeSide
        | AwayClub -> s.AwaySide

module MatchStateOps =

    type TacticsConfig =
        { PressureDistance: float
          UrgencyMultiplier: float
          ForwardPush: float
          DefensiveDrop: float
          PressingIntensity: float }

    let private baseTacticsConfig =
        function
        | TeamTactics.Balanced ->
            { PressureDistance = 0.0
              UrgencyMultiplier = 1.0
              ForwardPush = 0.0
              DefensiveDrop = 0.0
              PressingIntensity = 1.0 }
        | TeamTactics.Attacking ->
            { PressureDistance = 8.0
              UrgencyMultiplier = 1.15
              ForwardPush = 10.0
              DefensiveDrop = -5.0
              PressingIntensity = 1.2 }
        | TeamTactics.Defensive ->
            { PressureDistance = -10.0
              UrgencyMultiplier = 0.9
              ForwardPush = -5.0
              DefensiveDrop = 8.0
              PressingIntensity = 0.7 }
        | TeamTactics.Pressing ->
            { PressureDistance = 12.0
              UrgencyMultiplier = 1.1
              ForwardPush = 8.0
              DefensiveDrop = -3.0
              PressingIntensity = 1.5 }
        | TeamTactics.Counter ->
            { PressureDistance = -6.0
              UrgencyMultiplier = 1.2
              ForwardPush = -8.0
              DefensiveDrop = 6.0
              PressingIntensity = 0.8 }

    let tacticsConfig (teamTactics: TeamTactics) (instructions: TacticalInstructions option) =
        let baseCfg = baseTacticsConfig teamTactics
        let instr = instructions |> Option.defaultValue defaultInstructions
        let mentalityMod = float (instr.Mentality - 2) * 0.08
        let defensiveLineMod = float (instr.DefensiveLine - 2) * 3.0
        let pressingMod = float (instr.PressingIntensity - 2) * 0.15

        { PressureDistance = baseCfg.PressureDistance + defensiveLineMod
          UrgencyMultiplier = baseCfg.UrgencyMultiplier * (1.0 + mentalityMod)
          ForwardPush = baseCfg.ForwardPush + mentalityMod * 5.0 + defensiveLineMod * 0.5
          DefensiveDrop = baseCfg.DefensiveDrop - mentalityMod * 5.0 - defensiveLineMod * 0.5
          PressingIntensity = baseCfg.PressingIntensity * (1.0 + pressingMod) }

    let phaseFromBallZone (dir: AttackDir) (x: float) =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> 100.0 - x

        if effectiveX < 30.0 then BuildUp
        elif effectiveX <= 70.0 then Midfield
        else Attack

    let adjustMomentum (dir: AttackDir) (delta: float) (s: MatchState) : MatchState =
        let signedDelta = AttackDir.momentumDelta dir delta
        { s with Momentum = Math.Clamp(s.Momentum + signedDelta, -10.0, 10.0) }

    let activeIndices (players: Player[]) (sidelined: Map<PlayerId, PlayerOut>) =
        players
        |> Array.mapi (fun i p -> i, p)
        |> Array.filter (fun (_, p) -> not (Map.containsKey p.Id sidelined))
        |> Array.map fst

    let goalDiff (clubId: ClubId) (s: MatchState) =
        if clubId = s.Home.Id then
            s.HomeScore - s.AwayScore
        else
            s.AwayScore - s.HomeScore

    let pressureMultiplier (clubId: ClubId) (s: MatchState) =
        1.1 - float (max -2 (min 2 (goalDiff clubId s))) * 0.25

    let homeSide (s: MatchState) = s.HomeSide
    let awaySide (s: MatchState) = s.AwaySide

    let side (clubId: ClubId) (s: MatchState) =
        if clubId = s.Home.Id then s.HomeSide else s.AwaySide

    let withSide (clubId: ClubId) (ts: TeamSide) (s: MatchState) =
        if clubId = s.Home.Id then
            { s with HomeSide = ts }
        else
            { s with AwaySide = ts }

    let defaultSpatial x y =
        { X = x
          Y = y
          Z = 0.0
          Vx = 0.0
          Vy = 0.0
          Vz = 0.0 }

    let defaultBall =
        { Position = defaultSpatial 50.0 50.0
          LastTouchBy = None }

    let clubIdOf (p: Player) (s: MatchState) =
        if s.HomeSide.Players |> Array.exists (fun x -> x.Id = p.Id) then
            s.Home.Id
        else
            s.Away.Id

    let findPlayerIdx (players: Player[]) (pid: PlayerId) =
        players |> Array.tryFindIndex (fun p -> p.Id = pid) |> Option.defaultValue 0

    let resetBallToCenter (s: MatchState) =
        { s with
            Ball =
                { s.Ball with
                    Position = defaultSpatial 50.0 50.0
                    LastTouchBy = None } }

    let awardGoal (scoringClub: ClubSide) (scorerId: PlayerId option) (second: int) (s: MatchState) =
        let isHome = scoringClub = HomeClub
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let s' =
            { s with
                HomeScore = if isHome then s.HomeScore + 1 else s.HomeScore
                AwayScore = if isHome then s.AwayScore else s.AwayScore + 1
                Momentum = Math.Clamp(s.Momentum + (if isHome then 3.0 else -3.0), -10.0, 10.0) }
            |> resetBallToCenter
            |> fun s'' -> { s'' with AttackingClub = ClubSide.flip scoringClub }

        let events =
            match scorerId with
            | Some pid ->
                [ { Second = second
                    PlayerId = pid
                    ClubId = clubId
                    Type = Goal } ]
            | None -> []

        s', events
