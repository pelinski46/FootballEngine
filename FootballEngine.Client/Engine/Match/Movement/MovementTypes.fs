namespace FootballEngine.Movement

open FootballEngine.Domain

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
      ExpirySecond: int
      Source: string }

module Directive =
    let expired currentSecond (d: Directive) = currentSecond > d.ExpirySecond

    let create kind targetX targetY weight urgency expiry source =
        { Kind = kind
          TargetX = targetX
          TargetY = targetY
          Weight = weight
          Urgency = urgency
          ExpirySecond = expiry
          Source = source }

    let composeDirectives currentSecond (directives: Directive list) (modifiers: DirectiveModifiers) =
        let applyModifier (kind: DirectiveKind) weight =
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

        let activeDirectives =
            directives
            |> List.filter (fun d -> not (expired currentSecond d))
            |> List.filter (fun d -> d.Weight > 0.0)

        if List.isEmpty activeDirectives then
            (50.0, 50.0)
        else
            let totalWeight, sumX, sumY =
                activeDirectives
                |> List.fold
                    (fun (tw, sx, sy) d ->
                        let w = applyModifier d.Kind d.Weight
                        tw + w, sx + d.TargetX * w, sy + d.TargetY * w)
                    (0.0, 0.0, 0.0)

            if totalWeight = 0.0 then
                (50.0, 50.0)
            else
                (sumX / totalWeight, sumY / totalWeight)

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
      StartSecond: int
      DurationSeconds: int
      Intensity: float
      Priority: int }

module RunAssignment =
    let isActive currentSecond (r: RunAssignment) =
        currentSecond >= r.StartSecond
        && currentSecond < r.StartSecond + r.DurationSeconds

    let progress currentSecond (r: RunAssignment) =
        if not (isActive currentSecond r) then
            0.0
        else
            let elapsed = float (currentSecond - r.StartSecond)
            let total = float r.DurationSeconds
            min 1.0 (elapsed / total)

    let evaluateTrajectory t trajectory =
        match trajectory with
        | Linear(sx, sy, ex, ey) -> (sx + (ex - sx) * t, sy + (ey - sy) * t)
        | Waypoints points ->
            if Array.isEmpty points then
                (50.0, 50.0)
            elif points.Length = 1 then
                points[0]
            else
                let scaledT = t * float (points.Length - 1)
                let idx = int scaledT
                let frac = scaledT - float idx
                let i = min idx (points.Length - 2)
                let ax, ay = points[i]
                let bx, by = points[i + 1]
                (ax + (bx - ax) * frac, ay + (by - ay) * frac)

type MovementMode =
    | OpenPlay
    | SetPiece of SetPieceRole

and SetPieceRole =
    | CornerAttack
    | CornerDefend
    | FreeKickAttack
    | FreeKickDefend
    | ThrowInNear
    | ThrowInFar

type MovementRole =
    | BallCarrier
    | SupportPlayer
    | ShadowPlayer
    | PressingPlayer
    | CoveringPlayer
    | MarkerPlayer
    | RunPlayer
    | SetPiecePlayer

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
