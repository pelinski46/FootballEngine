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

type PlayerOut =
    | SidelinedByRedCard
    | SidelinedByInjury
    | SidelinedBySub

[<Struct>]
type Spatial =
    { X: float
      Y: float
      Z: float
      Vx: float
      Vy: float
      Vz: float }

[<Struct>]
type ActiveSlot =
    { Player: Player
      Pos: Spatial
      Condition: int
      Mental: MentalState
      Directives: Directive[] }

type PlayerSlot =
    | Active of ActiveSlot
    | Sidelined of Player * PlayerOut



[<Struct>]
type Spin = { Top: float; Side: float }

module Spin =
    let zero = { Top = 0.0; Side = 0.0 }

[<Struct>]
type PossessionPhase =
    | InPossession of ClubSide
    | Transition of ClubSide
    | Contest of ClubSide
    | InFlight of ClubSide
    | SetPiece of ClubSide

type OffsideSnapshot =
    { PasserId: PlayerId
      ReceiverId: PlayerId
      ReceiverXAtPass: float
      SecondLastDefenderX: float
      BallXAtPass: float
      Dir: AttackDir }

type BallPhysicsState =
    { Position: Spatial
      Spin: Spin
      ControlledBy: PlayerId option
      LastTouchBy: PlayerId option
      IsInPlay: bool
      Phase: PossessionPhase
      PendingOffsideSnapshot: OffsideSnapshot option }

type PenaltyShootout =
    { HomeKicks: (PlayerId * bool * int) list
      AwayKicks: (PlayerId * bool * int) list
      CurrentKick: int
      IsComplete: bool }

type MatchContext =
    { Home: Club
      Away: Club
      HomeCoach: Staff
      AwayCoach: Staff
      HomePlayers: Player[]
      AwayPlayers: Player[]
      HomeBasePositions: Spatial[]
      AwayBasePositions: Spatial[]
      HomeChemistry: ChemistryGraph
      AwayChemistry: ChemistryGraph
      IsKnockoutMatch: bool }

type TeamSimState =
    { Slots: PlayerSlot[]
      Sidelined: Map<PlayerId, PlayerOut>
      Yellows: Map<PlayerId, int>
      SubsUsed: int
      Tactics: TeamTactics
      Instructions: TacticalInstructions option
      ActiveRuns: RunAssignment list
      EmergentState: EmergentState
      AdaptiveState: AdaptiveState
      LastCognitiveSubTick: int
      LastShapeSubTick: int
      LastMarkingSubTick: int
      LastAdaptiveSubTick: int }

module TeamSimState =
    let empty =
        { Slots = Array.empty<PlayerSlot>
          Sidelined = Map.empty<PlayerId, PlayerOut>
          Yellows = Map.empty<PlayerId, int>
          SubsUsed = 0
          Tactics = TeamTactics.Balanced
          Instructions = None
          ActiveRuns = []
          EmergentState = EmergentState.initial
          AdaptiveState = AdaptiveTactics.initial
          LastCognitiveSubTick = 0
          LastShapeSubTick = 0
          LastMarkingSubTick = 0
          LastAdaptiveSubTick = 0 }

type SimState() =
    member val SubTick = 0 with get, set
    member val HomeScore = 0 with get, set
    member val AwayScore = 0 with get, set

    member val Ball =
        { Position =
            { X = 52.5
              Y = 34.0
              Z = 0.0
              Vx = 0.0
              Vy = 0.0
              Vz = 0.0 }
          Spin = { Top = 0.0; Side = 0.0 }
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true
          Phase = PossessionPhase.SetPiece HomeClub
          PendingOffsideSnapshot = None } with get, set

    member this.AttackingClub =
        match this.Ball.Phase with
        | PossessionPhase.InPossession s | PossessionPhase.Transition s | PossessionPhase.Contest s | PossessionPhase.InFlight s | PossessionPhase.SetPiece s -> s
    member val HomeAttackDir = LeftToRight with get, set
    member val Momentum = 0.0 with get, set

    member this.PendingOffsideSnapshot = this.Ball.PendingOffsideSnapshot

    member val HomeBasePositions = Array.empty<Spatial> with get, set
    member val AwayBasePositions = Array.empty<Spatial> with get, set
    member val Home = TeamSimState.empty with get, set
    member val Away = TeamSimState.empty with get, set

type SimSnapshot =
    { SubTick: int
      HomePositions: Spatial[]
      AwayPositions: Spatial[]
      BallX: float
      BallY: float
      BallVx: float
      BallVy: float
      BallControlledBy: PlayerId option
      HomeScore: int
      AwayScore: int
      HomeConditions: int[]
      AwayConditions: int[]
      HomeSidelined: Map<PlayerId, PlayerOut>
      AwaySidelined: Map<PlayerId, PlayerOut>
      AttackingClub: ClubSide
      HomeAttackDir: AttackDir
      Momentum: float
      Phase: PossessionPhase }

type MatchReplay =
    { Context: MatchContext
      Final: SimState
      Events: MatchEvent list
      Snapshots: SimSnapshot[] }

module SimStateOps =

    [<Struct>]
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

    let defaultSpatial (x: float) (y: float) : Spatial =
        { X = x
          Y = y
          Z = 0.0
          Vx = 0.0
          Vy = 0.0
          Vz = 0.0 }

    let kickOffSpatial =
        defaultSpatial PhysicsContract.HalfwayLineX (PhysicsContract.PitchWidth / 2.0)

    let getTeam (state: SimState) (side: ClubSide) =
        if side = HomeClub then state.Home else state.Away

    let getTeamByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        if clubId = ctx.Home.Id then state.Home else state.Away

    let updateTeamByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (f: TeamSimState -> TeamSimState) =
        if clubId = ctx.Home.Id then state.Home <- f state.Home else state.Away <- f state.Away

    let setTeam (state: SimState) (side: ClubSide) (team: TeamSimState) =
        if side = HomeClub then state.Home <- team else state.Away <- team

    let updateTeam (state: SimState) (side: ClubSide) (f: TeamSimState -> TeamSimState) =
        if side = HomeClub then state.Home <- f state.Home else state.Away <- f state.Away

    let getSlots (state: SimState) (side: ClubSide) = (getTeam state side).Slots
    let setSlots (state: SimState) (side: ClubSide) (slots: PlayerSlot[]) =
        updateTeam state side (fun t -> { t with Slots = slots })

    let getSidelined (state: SimState) (side: ClubSide) = (getTeam state side).Sidelined
    let setSidelined (state: SimState) (side: ClubSide) (m: Map<PlayerId, PlayerOut>) =
        updateTeam state side (fun t -> { t with Sidelined = m })

    let getYellows (state: SimState) (side: ClubSide) = (getTeam state side).Yellows
    let setYellows (state: SimState) (side: ClubSide) (m: Map<PlayerId, int>) =
        updateTeam state side (fun t -> { t with Yellows = m })

    let getSubsUsed (state: SimState) (side: ClubSide) = (getTeam state side).SubsUsed
    let setSubsUsed (state: SimState) (side: ClubSide) (n: int) =
        updateTeam state side (fun t -> { t with SubsUsed = n })

    let getTactics (state: SimState) (side: ClubSide) = (getTeam state side).Tactics
    let setTactics (state: SimState) (side: ClubSide) (tac: TeamTactics) =
        updateTeam state side (fun ts -> { ts with Tactics = tac })

    let getInstructions (state: SimState) (side: ClubSide) = (getTeam state side).Instructions
    let setInstructions (state: SimState) (side: ClubSide) (i: TacticalInstructions option) =
        updateTeam state side (fun t -> { t with Instructions = i })

    let getBasePositions (state: SimState) (side: ClubSide) =
        if side = HomeClub then state.HomeBasePositions else state.AwayBasePositions

    let getActiveRuns (state: SimState) (side: ClubSide) = (getTeam state side).ActiveRuns
    let setActiveRuns (state: SimState) (side: ClubSide) (runs: RunAssignment list) =
        updateTeam state side (fun t -> { t with ActiveRuns = runs })

    let getChemistry (ctx: MatchContext) (side: ClubSide) =
        if side = HomeClub then ctx.HomeChemistry else ctx.AwayChemistry

    let getEmergentState (state: SimState) (side: ClubSide) = (getTeam state side).EmergentState
    let setEmergentState (state: SimState) (side: ClubSide) (s: EmergentState) =
        updateTeam state side (fun t -> { t with EmergentState = s })

    let getAdaptiveState (state: SimState) (side: ClubSide) = (getTeam state side).AdaptiveState
    let setAdaptiveState (state: SimState) (side: ClubSide) (s: AdaptiveState) =
        updateTeam state side (fun t -> { t with AdaptiveState = s })

    let getLastCognitiveSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastCognitiveSubTick
    let setLastCognitiveSubTick (state: SimState) (side: ClubSide) (t: int) =
        updateTeam state side (fun ts -> { ts with LastCognitiveSubTick = t })

    let getLastShapeSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastShapeSubTick
    let setLastShapeSubTick (state: SimState) (side: ClubSide) (t: int) =
        updateTeam state side (fun ts -> { ts with LastShapeSubTick = t })

    let getLastMarkingSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastMarkingSubTick
    let setLastMarkingSubTick (state: SimState) (side: ClubSide) (t: int) =
        updateTeam state side (fun ts -> { ts with LastMarkingSubTick = t })

    let getLastAdaptiveSubTick (state: SimState) (side: ClubSide) = (getTeam state side).LastAdaptiveSubTick
    let setLastAdaptiveSubTick (state: SimState) (side: ClubSide) (t: int) =
        updateTeam state side (fun ts -> { ts with LastAdaptiveSubTick = t })

    let getSlotsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Slots

    let getTacticsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Tactics

    let getInstructionsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Instructions

    let setTacticsByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (tac: TeamTactics) =
        updateTeamByClubId clubId ctx state (fun ts -> { ts with Tactics = tac })

    let getSidelinedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).Sidelined

    let setSidelinedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (m: Map<PlayerId, PlayerOut>) =
        updateTeamByClubId clubId ctx state (fun ts -> { ts with Sidelined = m })

    let getSubsUsedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        (getTeamByClubId clubId ctx state).SubsUsed

    let setSubsUsedByClubId (clubId: ClubId) (ctx: MatchContext) (state: SimState) (n: int) =
        updateTeamByClubId clubId ctx state (fun ts -> { ts with SubsUsed = n })

    let activePlayers (slots: PlayerSlot[]) =
        slots
        |> Array.choose (function
            | Active s -> Some s.Player
            | Sidelined _ -> None)

    let defaultBall =
        { Position = kickOffSpatial
          Spin = Spin.zero
          ControlledBy = None
          LastTouchBy = None
          IsInPlay = true
          Phase = PossessionPhase.Contest HomeClub
          PendingOffsideSnapshot = None }

    let resetBallToCenter (state: SimState) =
        state.Ball <-
            { state.Ball with
                Position = kickOffSpatial
                Spin = Spin.zero
                ControlledBy = None
                LastTouchBy = None
                IsInPlay = true
                PendingOffsideSnapshot = None }

    let clearOffsideSnapshot (state: SimState) =
        state.Ball <- { state.Ball with PendingOffsideSnapshot = None }

    let hasPossession (state: SimState) (pid: PlayerId) : bool =
        match state.Ball.ControlledBy with
        | Some c -> c = pid
        | None -> false

    let losePossession (state: SimState) =
        let club =
            match state.Ball.Phase with
            | PossessionPhase.InPossession s | PossessionPhase.Transition s | PossessionPhase.Contest s | PossessionPhase.InFlight s | PossessionPhase.SetPiece s -> s
        state.Ball <- { state.Ball with ControlledBy = None; Phase = PossessionPhase.Contest club }

    let flipPossession (state: SimState) =
        let club =
            match state.Ball.Phase with
            | PossessionPhase.InPossession s | PossessionPhase.Transition s | PossessionPhase.Contest s | PossessionPhase.InFlight s | PossessionPhase.SetPiece s ->
                ClubSide.flip s
        state.Ball <-
            { state.Ball with
                ControlledBy = None
                Phase = PossessionPhase.Contest club
                PendingOffsideSnapshot = None }

    let awardGoal
        (scoringClub: ClubSide)
        (scorerId: PlayerId option)
        (subTick: int)
        (ctx: MatchContext)
        (state: SimState)
        =
        if scoringClub = HomeClub then
            state.HomeScore <- state.HomeScore + 1
            state.Momentum <- Math.Clamp(state.Momentum + 3.0, -10.0, 10.0)
        else
            state.AwayScore <- state.AwayScore + 1
            state.Momentum <- Math.Clamp(state.Momentum - 3.0, -10.0, 10.0)

        resetBallToCenter state
        state.Ball <- { state.Ball with Phase = PossessionPhase.SetPiece(ClubSide.flip scoringClub) }

        let clubId = if scoringClub = HomeClub then ctx.Home.Id else ctx.Away.Id

        match scorerId with
        | Some pid ->
            [ { SubTick = subTick
                PlayerId = pid
                ClubId = clubId
                Type = Goal } ]
        | None -> []

    let adjustMomentum (dir: AttackDir) (delta: float) (state: SimState) =
        state.Momentum <- Math.Clamp(state.Momentum + AttackDir.momentumDelta dir delta, -10.0, 10.0)





    let goalDiff (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        if clubId = ctx.Home.Id then
            state.HomeScore - state.AwayScore
        else
            state.AwayScore - state.HomeScore

    let pressureMultiplier (clubId: ClubId) (ctx: MatchContext) (state: SimState) =
        1.1 - float (max -2 (min 2 (goalDiff clubId ctx state))) * 0.25

    let matchUrgency (clubId: ClubId) (ctx: MatchContext) (state: SimState) : float =
        let elapsedSeconds = PhysicsContract.subTicksToSeconds state.SubTick
        let late = elapsedSeconds > 60.0 * 60.0

        let ts =
            tacticsConfig
                (getTacticsByClubId clubId ctx state)
                (getInstructionsByClubId clubId ctx state)

        match goalDiff clubId ctx state, late with
        | d, true when d < 0 -> 1.35 * ts.UrgencyMultiplier
        | d, false when d < 0 -> 1.15 * ts.UrgencyMultiplier
        | d, _ when d > 0 -> 0.85 * ts.UrgencyMultiplier
        | _, true -> 1.10 * ts.UrgencyMultiplier
        | _ -> 1.00 * ts.UrgencyMultiplier

    let phaseFromBallZone (dir: AttackDir) (x: float) =
        let effectiveX =
            match dir with
            | LeftToRight -> x
            | RightToLeft -> PhysicsContract.PitchLength - x

        let third = PhysicsContract.PitchLength / 3.0

        if effectiveX < third then BuildUp
        elif effectiveX < third * 2.0 then Midfield
        else Attack

    let attackDirFor (clubSide: ClubSide) (state: SimState) =
        match clubSide with
        | HomeClub -> state.HomeAttackDir
        | AwayClub ->
            match state.HomeAttackDir with
            | LeftToRight -> RightToLeft
            | RightToLeft -> LeftToRight

    let currentPhase (state: SimState) =
        let dir = attackDirFor state.AttackingClub state
        phaseFromBallZone dir state.Ball.Position.X

    let clubSideOf (state: SimState) (pid: PlayerId) : ClubSide option =
        let foundHome =
            state.Home.Slots
            |> Array.exists (function
                | PlayerSlot.Active s -> s.Player.Id = pid
                | _ -> false)

        if foundHome then
            Some HomeClub
        else
            let foundAway =
                state.Away.Slots
                |> Array.exists (function
                    | PlayerSlot.Active s -> s.Player.Id = pid
                    | _ -> false)

            if foundAway then Some AwayClub else None

    let conditionsArray (slots: PlayerSlot[]) : int[] =
        Array.init slots.Length (fun i ->
            match slots[i] with
            | PlayerSlot.Active s -> s.Condition
            | _ -> 0)

    let playersArray (slots: PlayerSlot[]) : Player[] =
        Array.init slots.Length (fun i ->
            match slots[i] with
            | PlayerSlot.Active s -> s.Player
            | _ -> Unchecked.defaultof<Player>)

    let activeRunsFilter currentSubTick (runs: RunAssignment list) =
        runs |> List.filter (RunAssignment.isActive currentSubTick)

    let createEvent subTick playerId clubId t : MatchEvent =
        { SubTick = subTick
          PlayerId = playerId
          ClubId = clubId
          Type = t }
