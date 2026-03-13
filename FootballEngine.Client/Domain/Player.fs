namespace FootballEngine.Domain

open System
open FootballEngine.DomainTypes

type PreferredFoot =
    | Left
    | Right
    | Both

type InjurySeverity =
    | Minor
    | Moderate
    | Major
    | Severe

type PlayerStatus =
    | Available
    | Suspended of int
    | Injured of InjurySeverity * DateTime

type PhysicalStats =
    { Acceleration: int
      Pace: int
      Agility: int
      Balance: int
      JumpingReach: int
      Stamina: int
      Strength: int }

type TechnicalStats =
    { Finishing: int
      LongShots: int
      Dribbling: int
      BallControl: int
      Passing: int
      Crossing: int
      Tackling: int
      Marking: int
      Heading: int
      FreeKick: int
      Penalty: int }

type MentalStats =
    { Aggression: int
      Composure: int
      Vision: int
      Positioning: int
      Bravery: int
      WorkRate: int
      Concentration: int
      Leadership: int }

type GoalkeeperStats =
    { Reflexes: int
      Handling: int
      Kicking: int
      OneOnOne: int
      AerialReach: int }

type Player =
    { Id: PlayerId
      ClubId: ClubId
      Name: string
      Birthday: DateTime
      Nationality: CountryCode
      Position: Position
      PreferredFoot: PreferredFoot
      Height: int
      Weight: int
      Physical: PhysicalStats
      Technical: TechnicalStats
      Mental: MentalStats
      Goalkeeping: GoalkeeperStats
      Condition: int
      MatchFitness: int
      Morale: int
      Status: PlayerStatus
      CurrentSkill: int
      PotentialSkill: int
      Reputation: int
      Value: decimal
      Salary: decimal
      ContractExpiry: int
      TeamId: ClubId }

module Player =
    let age (currentDate: DateTime) (p: Player) =
        let age = currentDate.Year - p.Birthday.Year

        if currentDate < p.Birthday.AddYears(age) then
            age - 1
        else
            age
