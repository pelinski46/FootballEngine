namespace FootballEngine.Domain

type FormationSlot =
    { Index: int
      Role: Position
      X: float
      Y: float }

type LineupSlot =
    { Index: int
      Role: Position
      X: float
      Y: float
      PlayerId: PlayerId option }

type ClubLineup =
    { Formation: Formation
      TeamTactics: string
      Slots: LineupSlot list }

type Club =
    { Id: ClubId
      Name: string
      Nationality: CountryCode
      Reputation: int
      Players: Player list
      CurrentLineup: ClubLineup option
      Budget: decimal
      Morale: int }
