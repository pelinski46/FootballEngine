namespace FootballEngine.Domain

type PlayerId = int
type ClubId = int
type MatchId = int
type CompetitionId = int
type CountryCode = string
type StaffId = int

type Round =
    | GroupStage of groupIndex: int
    | KnockoutRound of teamsRemaining: int
    | ThirdPlace
    | Final


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

type BehavioralProfile =
    { PositionalFreedom: float
      AttackingDepth: float
      LateralTendency: float
      DefensiveHeight: float
      PressingIntensity: float
      RiskAppetite: float
      Directness: float
      CreativityWeight: float
      AerialThreat: float
      HoldUpPlay: float }

module BehavioralProfile =
    let neutral =
        { PositionalFreedom = 0.5
          AttackingDepth = 0.5
          LateralTendency = 0.0
          DefensiveHeight = 0.5
          PressingIntensity = 0.5
          RiskAppetite = 0.5
          Directness = 0.5
          CreativityWeight = 0.5
          AerialThreat = 0.5
          HoldUpPlay = 0.5 }
