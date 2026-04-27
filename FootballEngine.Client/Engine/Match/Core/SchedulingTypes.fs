namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.SimulationClock

type PlayerAction =
    | Shoot
    | Pass       of target: Player
    | Dribble
    | Cross
    | LongBall
    | Tackle     of opponent: Player
    | FreeKick
    | Corner
    | ThrowIn    of side: ClubSide
    | Penalty    of kicker: Player * side: ClubSide * kickNum: int

module SchedulingTypes =

    type TickPriority =
        | Physics = 0
        | Referee = 1
        | SetPiece = 2
        | Manager = 3
        | MatchControl = 4

    type PlayState =
        | LivePlay
        | Stopped of StopReason
        | SetPiece of SetPieceKind

    and StopReason =
        | Foul
        | Goal
        | BallOut
        | Injury

    type TickKind =
        | PhysicsTick
        | CognitiveTick
        | SetPieceTick of SetPieceKind * ClubSide
        | ManagerTick of ClubId
        | KickOffTick
        | HalfTimeTick
        | FullTimeTick
        | MatchEndTick
        | InjuryTick of player: PlayerId * severity: int
        | ResumePlayTick
        | SubstitutionTick of clubId: ClubId
        | ManagerReactionTick of trigger: ReactionTrigger
        | RefereeTick

    and ReactionTrigger =
        | RedCardTrigger of PlayerId
        | InjuryTrigger of PlayerId
        | GoalConceded
        | GoalScored
        | FatigueAlert of clubId: ClubId * playerId: PlayerId * condition: int
        | MomentumSwing of clubId: ClubSide

    [<Struct>]
    type PendingTick =
        { SubTick: int
          Priority: TickPriority
          Kind: TickKind }

    [<Struct; CustomComparison; CustomEquality>]
    type ScheduledTick =
        { SubTick: int
          Priority: TickPriority
          SequenceId: int64
          Kind: TickKind }

        interface IComparable<ScheduledTick> with
            member this.CompareTo o =
                match compare this.SubTick o.SubTick with
                | 0 ->
                    match compare this.Priority o.Priority with
                    | 0 -> compare this.SequenceId o.SequenceId
                    | c -> c
                | c -> c

        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? ScheduledTick as o -> (this :> IComparable<ScheduledTick>).CompareTo(o)
                | _ -> invalidArg "other" "Not a ScheduledTick"

        override this.Equals other =
            match other with
            | :? ScheduledTick as o ->
                this.SubTick = o.SubTick
                && this.Priority = o.Priority
                && this.SequenceId = o.SequenceId
                && this.Kind = o.Kind
            | _ -> false

        override this.GetHashCode() =
            HashCode.Combine(this.SubTick, int this.Priority, this.SequenceId)

    type OnBallIntent =
        | Shoot
        | Pass of target: PlayerId
        | Dribble
        | Cross
        | LongBall of target: PlayerId
        | Tackle of opponent: PlayerId

    type IntentContext =
        | NormalPlay
        | BuildUpPhase
        | PressingTrap

    type PlayerIntent =
        { Movement: MovementIntent
          Action: OnBallIntent option
          Context: IntentContext
          Urgency: float
          Confidence: float }

    type TickIntent =
        | NoOp
        | ScheduleInjury of player: PlayerId * severity: int
        | ScheduleSubstitution of ClubId
        | StopPlay of StopReason

    type PlayerResult =
        { NextTick: PendingTick option
          Events: MatchEvent list
          Transition: PlayState option }

    type RefereeResult =
        { NextTick: PendingTick option
          Actions: RefereeAction list
          Transition: PlayState option }


