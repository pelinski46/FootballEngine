namespace FootballEngine

open System
open FootballEngine.Domain

module SchedulingTypes =

    // -------------------------------------------------------------------------
    // Priority — lower value = higher priority within the same SubTick
    // -------------------------------------------------------------------------

    type TickPriority =
        | Physics = 0 // ball + player positions — must run before any decision
        | Referee = 1 // goal detection, offside, out-of-play
        | Duel = 2 // contested ball actions
        | SetPiece = 3
        | Manager = 4
        | MatchControl = 5 // halftime, fulltime, structure

    // -------------------------------------------------------------------------
    // Play state machine
    // -------------------------------------------------------------------------

    type PlayState =
        | LivePlay
        | Stopped of StopReason
        | SetPiece of SetPieceKind

    and StopReason =
        | Foul
        | Goal
        | BallOut
        | Injury



    // -------------------------------------------------------------------------
    // Tick kinds
    // -------------------------------------------------------------------------

    type TickKind =
        | PhysicsTick
        | DuelTick of chainDepth: int
        | DecisionTick of depth: int * controllerId: PlayerId option
        | PlayerActionTick of chainDepth: int * action: PlayerAction * attackerId: PlayerId option
        | FreeKickTick of kicker: PlayerId * position: Spatial * chainDepth: int
        | CornerTick of club: ClubSide * chainDepth: int
        | PenaltyTick of kicker: PlayerId * isHome: bool
        | ThrowInTick of club: ClubSide * chainDepth: int
        | GoalKickTick
        | KickOffTick
        | HalfTimeTick
        | FullTimeTick
        | ExtraTimeTick of period: int
        | MatchEndTick
        | InjuryTick of player: PlayerId * severity: int
        | ResumePlayTick
        | SubstitutionTick of clubId: ClubId
        | ManagerReactionTick of trigger: ReactionTrigger
        | RefereeTick
        | PossessionChangeTick of ClubSide



    and ReactionTrigger =
        | RedCardTrigger of PlayerId
        | InjuryTrigger of PlayerId
        | GoalConceded
        | GoalScored
        | FatigueAlert of clubId: ClubId * playerId: PlayerId * condition: int
        | MomentumSwing of clubId: ClubSide

    // -------------------------------------------------------------------------
    // ScheduledTick — the unit the scheduler operates on
    // -------------------------------------------------------------------------

    [<Struct; CustomComparison; CustomEquality>]
    type ScheduledTick =
        { SubTick: int // timeline position
          Priority: TickPriority
          SequenceId: int64 // tiebreaker within same SubTick + Priority
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

    // -------------------------------------------------------------------------
    // Agent contract
    // -------------------------------------------------------------------------

    type AgentOutput =
        { Events: MatchEvent list
          Spawned: ScheduledTick list
          Transition: PlayState option }

    type TickResult = AgentOutput

    type Agent =
        ClubId
            -> Player list
            -> Player list
            -> ScheduledTick
            -> MatchContext
            -> SimState
            -> SimulationClock
            -> AgentOutput

    type LoopState =
        { Context: MatchContext
          State: SimState
          Events: ResizeArray<MatchEvent>
          PlayState: PlayState
          Snapshots: System.Collections.Generic.List<SimSnapshot> option
          MatchEndScheduled: bool
          SequenceCounter: int64 }
