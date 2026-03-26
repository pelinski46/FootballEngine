namespace FootballEngine.Domain

type TeamTactics =
    | Balanced
    | Attacking
    | Defensive
    | Pressing
    | Counter

type LineupSlot =
    { Index: int
      Role: Position
      X: float
      Y: float
      PlayerId: PlayerId option }

type Lineup =
    { Formation: Formation
      Tactics: TeamTactics
      Slots: LineupSlot list }
