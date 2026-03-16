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

module ClubFormation =
    let all =
        [ F442
          F442Diamond
          F433
          F433Flat
          F451
          F4141
          F4231
          F4312
          F4321
          F352
          F343
          F3421
          F532
          F541
          F523 ]
