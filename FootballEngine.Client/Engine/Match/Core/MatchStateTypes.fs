namespace FootballEngine

open System
open FootballEngine.Domain
open FootballEngine.Domain.TacticalInstructions

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

module ClubSide =
    let toClubId (side: ClubSide) (ctx: MatchContext) : ClubId =
        match side with
        | HomeClub -> ctx.Home.Id
        | AwayClub -> ctx.Away.Id

    let ofClubId (clubId: ClubId) (ctx: MatchContext) : ClubSide =
        if clubId = ctx.Home.Id then HomeClub else AwayClub
