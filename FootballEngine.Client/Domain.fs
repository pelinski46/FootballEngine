namespace FootballEngine.Domain

open System

type PlayerId = int
type ClubId = int
type MatchId = int
type LeagueId = int
type CountryCode = string

type Position =
    | GK
    | DR
    | DC
    | DL
    | WBR
    | WBL
    | DM
    | MR
    | MC
    | ML
    | AMR
    | AMC
    | AML
    | ST

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

type Formation =

    | F442
    | F442Diamond
    | F433
    | F433Flat
    | F451
    | F4141
    | F4231
    | F4312
    | F4321

    | F352
    | F343
    | F3421

    | F532
    | F541
    | F523


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
      Morale: int
      Wins: int
      Draws: int
      Losses: int
      GoalsFor: int
      GoalsAgainst: int }

type LeagueLevel =
    | First
    | Second

type League =
    { Id: LeagueId
      Name: string
      Level: LeagueLevel
      ClubIds: ClubId list
      Season: int
      Nationality: CountryCode
      IsPlayable: bool }

type MatchEventType =
    | Goal
    | OwnGoal
    | Assist
    | YellowCard
    | RedCard
    | Injury of string
    | SubstitutionIn
    | SubstitutionOut

type MatchEvent =
    { Second: int
      PlayerId: PlayerId
      ClubId: ClubId
      Type: MatchEventType }

type MatchFixture =
    { Id: MatchId
      HomeClubId: ClubId
      AwayClubId: ClubId
      ScheduledDate: DateTime
      Played: bool
      HomeScore: int option
      AwayScore: int option
      Events: MatchEvent list }

type GameState =
    { CurrentDate: DateTime
      Clubs: Map<ClubId, Club>
      Players: Map<PlayerId, Player>
      Leagues: Map<LeagueId, League>
      Fixtures: Map<MatchId, MatchFixture>
      UserClubId: ClubId
      ManagerName: string
      PrimaryCountry: CountryCode }

module Player =
    let age (currentDate: DateTime) (p: Player) =
        let age = currentDate.Year - p.Birthday.Year

        if currentDate < p.Birthday.AddYears(age) then
            age - 1
        else
            age

    let calculateCurrentAbility (p: Player) =
        let relevantStats =
            let physical =
                [ p.Physical.Acceleration
                  p.Physical.Pace
                  p.Physical.Agility
                  p.Physical.Balance
                  p.Physical.JumpingReach
                  p.Physical.Stamina
                  p.Physical.Strength ]

            let mental =
                [ p.Mental.Aggression
                  p.Mental.Composure
                  p.Mental.Vision
                  p.Mental.Positioning
                  p.Mental.Bravery
                  p.Mental.WorkRate
                  p.Mental.Concentration
                  p.Mental.Leadership ]

            match p.Position with
            | GK ->
                let gkTech =
                    [ p.Goalkeeping.Reflexes
                      p.Goalkeeping.Handling
                      p.Goalkeeping.Kicking
                      p.Goalkeeping.OneOnOne
                      p.Goalkeeping.AerialReach ]

                physical @ mental @ gkTech
            | _ ->
                let technical =
                    [ p.Technical.Finishing
                      p.Technical.LongShots
                      p.Technical.Dribbling
                      p.Technical.BallControl
                      p.Technical.Passing
                      p.Technical.Crossing
                      p.Technical.Tackling
                      p.Technical.Marking
                      p.Technical.Heading
                      p.Technical.FreeKick
                      p.Technical.Penalty ]

                physical @ mental @ technical


        let sum = relevantStats |> List.sumBy float
        let count = float relevantStats.Length
        let average = sum / count


        let finalSkill = int (Math.Round(average * 10.0))

        Math.Clamp(finalSkill, 1, 200)

module Formation =
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
