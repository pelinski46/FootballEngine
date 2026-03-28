namespace FootballEngine.Domain

type TeamTactics =
    | Balanced
    | Attacking
    | Defensive
    | Pressing
    | Counter

type TacticalInstructions =
    { Mentality: int
      DefensiveLine: int
      PressingIntensity: int }



type LineupSlot =
    { Index: int
      Role: Position
      X: float
      Y: float
      PlayerId: PlayerId option }

type Lineup =
    { Formation: Formation
      Tactics: TeamTactics
      Instructions: TacticalInstructions option
      Slots: LineupSlot list }

module TacticalInstructions =
    let defaultInstructions =
        { Mentality = 2
          DefensiveLine = 2
          PressingIntensity = 2 }
