namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Domain.TacticalInstructions


type DirectiveModifiers =
    { Shape: float
      Run: float
      MarkMan: float
      MarkZone: float
      Press: float
      Cover: float
      Support: float
      Flank: float
      Compact: float
      Spread: float
      ThirdMan: float }

module DirectiveModifiers =
    let neutral =
        { Shape = 1.0
          Run = 1.0
          MarkMan = 1.0
          MarkZone = 1.0
          Press = 1.0
          Cover = 1.0
          Support = 1.0
          Flank = 1.0
          Compact = 1.0
          Spread = 1.0
          ThirdMan = 1.0 }

type DirectiveKind =
    | Shape
    | Run
    | MarkMan
    | MarkZone
    | Press
    | Cover
    | Support
    | Flank
    | Compact
    | Spread
    | ThirdMan

[<Struct>]
type Directive =
    { Kind: DirectiveKind
      TargetX: float
      TargetY: float
      Weight: float
      Urgency: float
      ExpirySubTick: int
      Source: string }

module Directive =
    let expired currentSubTick (d: Directive) = currentSubTick > d.ExpirySubTick

    let create kind targetX targetY weight urgency expiry source =
        { Kind = kind
          TargetX = targetX
          TargetY = targetY
          Weight = weight
          Urgency = urgency
          ExpirySubTick = expiry
          Source = source }

    let composeDirectives currentSubTick (directives: Directive[]) (modifiers: DirectiveModifiers) =
        let mutable tw = 0.0
        let mutable sx = 0.0
        let mutable sy = 0.0
        let mutable hasActive = false

        let applyModifier kind weight =
            match kind with
            | Shape -> weight * modifiers.Shape
            | Run -> weight * modifiers.Run
            | MarkMan -> weight * modifiers.MarkMan
            | MarkZone -> weight * modifiers.MarkZone
            | Press -> weight * modifiers.Press
            | Cover -> weight * modifiers.Cover
            | Support -> weight * modifiers.Support
            | Flank -> weight * modifiers.Flank
            | Compact -> weight * modifiers.Compact
            | Spread -> weight * modifiers.Spread
            | ThirdMan -> weight * modifiers.ThirdMan

        for i = 0 to directives.Length - 1 do
            let d = directives[i]

            if not (expired currentSubTick d) && d.Weight > 0.0 then
                hasActive <- true
                let w = applyModifier d.Kind d.Weight
                tw <- tw + w
                sx <- sx + d.TargetX * w
                sy <- sy + d.TargetY * w

        if not hasActive then (52.5, 34.0)
        elif tw = 0.0 then (52.5, 34.0)
        else (sx / tw, sy / tw)

type RunType =
    | DeepRun
    | OverlapRun
    | UnderlapRun
    | DiagonalRun
    | CheckToBall
    | DriftWide
    | ThirdManRun
    | FalseNineDrop
    | WingBackSurge

type RunTrigger =
    | TeammateHasBall
    | TeammateStartedDribble
    | SpaceDetected
    | TacticalInstruction
    | SetPieceRoutine
    | CounterAttack

type RunTrajectory =
    | Linear of float * float * float * float
    | Waypoints of (float * float)[]

type RunAssignment =
    { PlayerId: PlayerId
      RunType: RunType
      Trigger: RunTrigger
      Trajectory: RunTrajectory
      StartSubTick: int
      DurationSubTicks: int
      Intensity: float
      Priority: int }

module RunAssignment =
    let isActive currentSubTick (r: RunAssignment) =
        currentSubTick >= r.StartSubTick
        && currentSubTick < r.StartSubTick + r.DurationSubTicks

    let progress currentSubTick (r: RunAssignment) =
        if not (isActive currentSubTick r) then
            0.0
        else
            min 1.0 (float (currentSubTick - r.StartSubTick) / float r.DurationSubTicks)

    let evaluateTrajectory t trajectory =
        match trajectory with
        | Linear(sx, sy, ex, ey) -> (sx + (ex - sx) * t, sy + (ey - sy) * t)
        | Waypoints pts ->
            if Array.isEmpty pts then
                (52.5, 34.0)
            elif pts.Length = 1 then
                pts[0]
            else
                let st = t * float (pts.Length - 1)
                let idx = int st
                let frac = st - float idx
                let i = min idx (pts.Length - 2)
                let ax, ay = pts[i]
                let bx, by = pts[i + 1]
                (ax + (bx - ax) * frac, ay + (by - ay) * frac)

[<Struct>]
type MentalState =
    { ComposureLevel: float
      ConfidenceLevel: float
      FocusLevel: float
      AggressionLevel: float
      RiskTolerance: float }

module MentalState =
    let initial (player: Player) =
        { ComposureLevel = PhysicsContract.normaliseAttr player.Mental.Composure
          ConfidenceLevel = PhysicsContract.normaliseCondition player.Morale
          FocusLevel = PhysicsContract.normaliseAttr player.Mental.Concentration
          AggressionLevel = PhysicsContract.normaliseAttr player.Mental.Aggression
          RiskTolerance = 0.5 }

type ChemistryGraph =
    { Familiarity: float[,]
      Leadership: float[]
      PlayerCount: int }

module ChemistryGraph =
    let init playerCount =
        { Familiarity = Array2D.create playerCount playerCount 0.5
          Leadership = Array.zeroCreate playerCount
          PlayerCount = playerCount }

type AttackPattern =
    | LeftFlank
    | RightFlank
    | Central
    | LongBall
    | ShortPass

type PatternResult =
    | SuccessfulXG of float
    | LostPossession
    | StillInProgress

type PatternRecord =
    { Pattern: AttackPattern
      Attempts: int
      Successes: int
      TotalXG: float }

type AdaptiveState = { Records: PatternRecord[] }

module AdaptiveTactics =
    let initial =
        { Records =
            [| { Pattern = LeftFlank
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = RightFlank
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = Central
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = LongBall
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 }
               { Pattern = ShortPass
                 Attempts = 0
                 Successes = 0
                 TotalXG = 0.0 } |] }

[<Struct>]
type EmergentState =
    { CompactnessLevel: float
      PressingIntensity: float
      WingPlayPreference: float
      TempoLevel: float
      RiskAppetite: float }

module EmergentState =
    let initial =
        { CompactnessLevel = 0.5
          PressingIntensity = 0.5
          WingPlayPreference = 0.5
          TempoLevel = 0.5
          RiskAppetite = 0.5 }

// =============================================================================
// Spatial types
// =============================================================================

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

/// All spatial values are in metres. Field is 105m × 68m.
[<Struct>]
type Spatial =
    { X: float // metres, 0 = away goal line, 105 = home goal line
      Y: float // metres, 0..68
      Z: float // metres above ground
      Vx: float // m/s
      Vy: float // m/s
      Vz: float } // m/s

[<Struct>]
type Spin = { Top: float; Side: float }

module Spin =
    let zero = { Top = 0.0; Side = 0.0 }

type OffsideSnapshot =
    { PasserId: PlayerId
      ReceiverId: PlayerId
      ReceiverXAtPass: float // metres
      SecondLastDefenderX: float // metres
      BallXAtPass: float // metres
      Dir: AttackDir }

type TeamSide =
    { Players: Player[]
      Conditions: int[]
      Positions: Spatial[]
      BasePositions: Spatial[]
      Sidelined: Map<PlayerId, PlayerOut>
      Yellows: Map<PlayerId, int>
      SubsUsed: int
      Tactics: TeamTactics
      Instructions: TacticalInstructions option }

type PenaltyShootout =
    { HomeKicks: (PlayerId * bool * int) list
      AwayKicks: (PlayerId * bool * int) list
      CurrentKick: int
      IsComplete: bool }

type BallPhysicsState =
    { Position: Spatial
      Spin: Spin
      ControlledBy: PlayerId option // explicit possession — set by PhysicsTick only
      LastTouchBy: PlayerId option // last player to make contact
      IsInPlay: bool }

// =============================================================================
// MatchState — SSOT includes cognitive / movement state
// =============================================================================

type MatchState =
    { Home: Club
      Away: Club
      HomeCoach: Staff
      AwayCoach: Staff
      SubTick: int // current simulation time
      HomeScore: int
      AwayScore: int
      Ball: BallPhysicsState
      AttackingClub: ClubSide
      Momentum: float
      HomeSide: TeamSide
      AwaySide: TeamSide
      PenaltyShootout: PenaltyShootout option
      IsExtraTime: bool
      IsKnockoutMatch: bool
      PendingOffsideSnapshot: OffsideSnapshot option

      // Cognitive / Movement state — Home team
      HomeMentalStates: MentalState[]
      HomeDirectives: Directive[][]
      HomeActiveRuns: RunAssignment list
      HomeChemistry: ChemistryGraph
      HomeEmergentState: EmergentState
      HomeAdaptiveState: AdaptiveState
      HomeLastCognitiveSubTick: int
      HomeLastShapeSubTick: int
      HomeLastMarkingSubTick: int
      HomeLastAdaptiveSubTick: int

      // Cognitive / Movement state — Away team
      AwayMentalStates: MentalState[]
      AwayDirectives: Directive[][]
      AwayActiveRuns: RunAssignment list
      AwayChemistry: ChemistryGraph
      AwayEmergentState: EmergentState
      AwayAdaptiveState: AdaptiveState
      AwayLastCognitiveSubTick: int
      AwayLastShapeSubTick: int
      AwayLastMarkingSubTick: int
      AwayLastAdaptiveSubTick: int }

/// Convenience projection — real seconds elapsed in the match.
module MatchState =
    let elapsedSeconds (s: MatchState) : float =
        PhysicsContract.subTicksToSeconds s.SubTick

type MatchContext =
    { HomePositions: Map<PlayerId, float * float>
      AwayPositions: Map<PlayerId, float * float> }

type MatchReplay =
    { Final: MatchState
      Events: MatchEvent list
      Snapshots: MatchState[] }

// =============================================================================
// MatchStateOps — all construction helpers use PhysicsContract coordinates
// =============================================================================

module MatchStateOps =

    // -------------------------------------------------------------------------
    // Tactics configuration
    // -------------------------------------------------------------------------

    [<Struct>]
    type TacticsConfig =
        { PressureDistance: float // metres
          UrgencyMultiplier: float
          ForwardPush: float // metres
          DefensiveDrop: float // metres
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

    // -------------------------------------------------------------------------
    // Pitch zone — thresholds in metres
    // -------------------------------------------------------------------------

    let phaseFromBallZone (dir: AttackDir) (x: float) =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> PhysicsContract.PitchLength - x

        let third = PhysicsContract.PitchLength / 3.0 // 35m

        if effectiveX < third then BuildUp
        elif effectiveX < third * 2.0 then Midfield
        else Attack

    // -------------------------------------------------------------------------
    // Momentum helpers
    // -------------------------------------------------------------------------

    let adjustMomentum (dir: AttackDir) (delta: float) (s: MatchState) : MatchState =
        { s with
            Momentum = Math.Clamp(s.Momentum + AttackDir.momentumDelta dir delta, -10.0, 10.0) }

    // -------------------------------------------------------------------------
    // Side helpers
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    // Spatial construction — all in metres
    // -------------------------------------------------------------------------

    let defaultSpatial (x: float) (y: float) : Spatial =
        { X = x
          Y = y
          Z = 0.0
          Vx = 0.0
          Vy = 0.0
          Vz = 0.0 }

    /// Kick-off position: centre of the pitch.
    let kickOffSpatial =
        defaultSpatial PhysicsContract.HalfwayLineX (PhysicsContract.PitchWidth / 2.0)

    let defaultBall =
        { Position = kickOffSpatial
          Spin = Spin.zero
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true }

    // -------------------------------------------------------------------------
    // Ball helpers
    // -------------------------------------------------------------------------

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
                    Position = kickOffSpatial
                    Spin = Spin.zero
                    ControlledBy = None
                    LastTouchBy = None
                    IsInPlay = true } }

    let clearOffsideSnapshot (s: MatchState) : MatchState =
        { s with PendingOffsideSnapshot = None }

    let flipPossessionAndClearOffside (clubSide: ClubSide) (s: MatchState) : MatchState =
        { s with
            AttackingClub = ClubSide.flip clubSide
            Ball = { s.Ball with ControlledBy = None }
            PendingOffsideSnapshot = None }

    // -------------------------------------------------------------------------
    // Goal
    // -------------------------------------------------------------------------

    let awardGoal (scoringClub: ClubSide) (scorerId: PlayerId option) (subTick: int) (s: MatchState) =
        let isHome = scoringClub = HomeClub
        let clubId = if isHome then s.Home.Id else s.Away.Id

        let s' =
            { s with
                HomeScore = if isHome then s.HomeScore + 1 else s.HomeScore
                AwayScore = if isHome then s.AwayScore else s.AwayScore + 1
                Momentum = Math.Clamp(s.Momentum + (if isHome then 3.0 else -3.0), -10.0, 10.0) }
            |> resetBallToCenter
            |> fun s'' ->
                { s'' with
                    AttackingClub = ClubSide.flip scoringClub }

        let events =
            match scorerId with
            | Some pid ->
                [ { SubTick = subTick
                    PlayerId = pid
                    ClubId = clubId
                    Type = Goal } ]
            | None -> []

        s', events

    // -------------------------------------------------------------------------
    // Urgency — uses elapsed real seconds derived from SubTick
    // -------------------------------------------------------------------------

    let matchUrgency (clubId: ClubId) (s: MatchState) : float =
        let elapsedSeconds = PhysicsContract.subTicksToSeconds s.SubTick
        let late = elapsedSeconds > 60.0 * 60.0 // last 30 min
        let ts = side clubId s
        let tacticsCfg = tacticsConfig ts.Tactics ts.Instructions

        match goalDiff clubId s, late with
        | d, true when d < 0 -> 1.35 * tacticsCfg.UrgencyMultiplier
        | d, false when d < 0 -> 1.15 * tacticsCfg.UrgencyMultiplier
        | d, _ when d > 0 -> 0.85 * tacticsCfg.UrgencyMultiplier
        | _, true -> 1.10 * tacticsCfg.UrgencyMultiplier
        | _ -> 1.00 * tacticsCfg.UrgencyMultiplier
