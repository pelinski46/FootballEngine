namespace FootballEngine.Domain

open System

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
    | Suspended of matchesBanned: int
    | Injured of InjurySeverity * until: DateTime

type ContractInfo = { Salary: decimal; ExpiryYear: int }

type PlayerAffiliation =
    | Contracted of clubId: ClubId * contract: ContractInfo
    | FreeAgent
    | YouthProspect of clubId: ClubId
    | Retired

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
      Affiliation: PlayerAffiliation
      TrainingSchedule: TrainingSchedule }

module Player =
    let age (currentDate: DateTime) (p: Player) =
        let years = currentDate.Year - p.Birthday.Year

        if currentDate < p.Birthday.AddYears(years) then
            years - 1
        else
            years

    let playerValue (skill: int) =
        decimal (max 0 (skill - 30) |> fun x -> x * x * 800)

    let playerSalary (skill: int) =
        decimal (max 0 (skill - 20) |> fun x -> x * x * 5 + 500)

    let clubOf (p: Player) : ClubId option =
        match p.Affiliation with
        | Contracted(clubId, _) | YouthProspect clubId -> Some clubId
        | FreeAgent | Retired -> None

    let contractOf (p: Player) : ContractInfo option =
        match p.Affiliation with
        | Contracted(_, contract) -> Some contract
        | _ -> None
