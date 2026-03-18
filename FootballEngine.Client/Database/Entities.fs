namespace FootballEngine.Database

open System
open SQLite

[<CLIMutable>]
type PlayerEntity =
    { [<PrimaryKey>]
      Id: int
      ClubId: int
      Name: string
      Birthday: DateTime
      Nationality: string
      Position: string
      PreferredFoot: string
      Height: int
      Weight: int
      Acceleration: int
      Pace: int
      Agility: int
      Balance: int
      JumpingReach: int
      Stamina: int
      Strength: int
      Finishing: int
      LongShots: int
      Dribbling: int
      BallControl: int
      Passing: int
      Crossing: int
      Tackling: int
      Marking: int
      Heading: int
      FreeKick: int
      Penalty: int
      Aggression: int
      Composure: int
      Vision: int
      Positioning: int
      Bravery: int
      WorkRate: int
      Concentration: int
      Leadership: int
      Reflexes: int
      Handling: int
      Kicking: int
      OneOnOne: int
      AerialReach: int
      Condition: int
      MatchFitness: int
      Morale: int
      StatusType: string
      StatusParamInt: int
      StatusParamDate: DateTime
      CurrentSkill: int
      PotentialSkill: int
      Reputation: int
      Value: decimal
      Salary: decimal
      ContractExpiry: int }

[<CLIMutable>]
type ClubEntity =
    { [<PrimaryKey>]
      Id: int
      Name: string
      Nationality: string
      Reputation: int
      Budget: decimal
      Morale: int }

[<CLIMutable>]
type LineupSlotEntity =
    { [<PrimaryKey; AutoIncrement>]
      Id: int
      ClubId: int
      FormationName: string
      TacticsName: string
      SlotIndex: int
      Role: string
      X: float
      Y: float
      PlayerId: int }

[<CLIMutable>]
type CompetitionEntity =
    { [<PrimaryKey>]
      Id: int
      Name: string
      TypeTag: string
      TypeParam1: string
      TypeParam2: string
      Country: string
      Season: int }

[<CLIMutable>]
type CompetitionClubEntity =
    { [<PrimaryKey; AutoIncrement>]
      Id: int
      CompetitionId: int
      ClubId: int }

[<CLIMutable>]
type LeagueStandingEntity =
    { [<PrimaryKey; AutoIncrement>]
      Id: int
      CompetitionId: int
      ClubId: int
      Played: int
      Won: int
      Drawn: int
      Lost: int
      GoalsFor: int
      GoalsAgainst: int
      Points: int }

[<CLIMutable>]
type KnockoutTieEntity =
    { [<PrimaryKey>]
      TieId: int
      CompetitionId: int
      Round: string
      HomeClubId: int
      AwayClubId: int
      Leg1FixtureId: int
      Leg2FixtureId: int
      AggHome: int
      AggAway: int
      WinnerId: int }

[<CLIMutable>]
type MatchFixtureEntity =
    { [<PrimaryKey>]
      Id: int
      CompetitionId: int
      Round: string
      HomeClubId: int
      AwayClubId: int
      ScheduledDate: DateTime
      HomeScore: int
      AwayScore: int
      Played: bool }

[<CLIMutable>]
type CountryEntity =
    { [<PrimaryKey>]
      Code: string
      Name: string
      Confederation: string }

[<CLIMutable>]
type GameSaveMeta =
    { [<PrimaryKey>]
      Id: int
      CurrentDate: DateTime
      Season: int
      UserClubId: int
      ManagerName: string
      PrimaryCountry: string }
