namespace FootballEngine

open System
open FootballEngine.Domain

module SchedulingTypes =

    type TickPriority =
        | Physics = 0
        | Referee = 1
        | Duel = 2
        | SetPiece = 3
        | Manager = 4
        | MatchControl = 5

    type PlayState =
        | LivePlay
        | Stopped of StopReason
        | SetPiece of SetPieceKind

    and StopReason =
        | Foul
        | Goal
        | BallOut
        | Injury

    and SetPieceKind =
        | FreeKick
        | Corner
        | ThrowIn
        | GoalKick
        | KickOff
        | Penalty

    type TickKind =
        | DuelTick of chainDepth: int
        | PlayerActionTick of chainDepth: int * action: PlayerAction * attackerId: PlayerId option
        | PhysicsTick
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

    and ReactionTrigger =
        | RedCardTrigger of PlayerId
        | InjuryTrigger of PlayerId
        | GoalConceded
        | GoalScored
        | FatigueAlert of clubId: ClubId * playerId: PlayerId * condition: int
        | MomentumSwing of clubId: ClubSide

    [<Struct; CustomComparison; CustomEquality>]
    type ScheduledTick =
        { Second: int
          Priority: TickPriority
          SequenceId: int64
          Kind: TickKind }

        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? ScheduledTick as o ->
                    match compare this.Second o.Second with
                    | 0 ->
                        match compare this.Priority o.Priority with
                        | 0 -> compare this.SequenceId o.SequenceId
                        | c -> c
                    | c -> c
                | _ -> invalidArg "other" "Not a ScheduledTick"

        override this.Equals other =
            match other with
            | :? ScheduledTick as o ->
                this.Second = o.Second
                && this.Priority = o.Priority
                && this.SequenceId = o.SequenceId
                && this.Kind = o.Kind
            | _ -> false

        override this.GetHashCode() =
            HashCode.Combine(this.Second, int this.Priority, this.SequenceId)

    type AgentOutput =
        { State       : MatchState
          Events      : MatchEvent list
          Spawned     : ScheduledTick list
          Transition  : PlayState option }

    type TickResult = AgentOutput

    type Agent =
        ClubId            // homeId
          -> Player list  // homeSquad
          -> Player list  // awaySquad
          -> ScheduledTick
          -> MatchState
          -> AgentOutput

    type LoopState =
        { MatchState: MatchState
          ReversedEvents: MatchEvent list
          PlayState: PlayState
          Snapshots: System.Collections.Generic.List<MatchState> option
          MatchEndScheduled: bool
          SequenceCounter: int64 }
