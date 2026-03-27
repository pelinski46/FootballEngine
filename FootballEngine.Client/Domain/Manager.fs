namespace FootballEngine.Domain

type SquadNeed =
    | NeedsPosition of Position
    | NeedsDepth of Position
    | NeedsQualityUpgrade of aboveSkill: int

type TransferTarget =
    { PlayerId: PlayerId
      Priority: float
      MaxFee: decimal }

type ManagerIntent =
    { ClubId: ClubId
      CoachId: StaffId
      Needs: SquadNeed list
      Targets: TransferTarget list
      PlayersToSell: PlayerId list
      BudgetAllocation: decimal }
