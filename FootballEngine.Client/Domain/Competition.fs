namespace FootballEngine.Domain



type LeagueLevel = LeagueLevel of int

type TiebreakerRule =
    | GoalDifference
    | GoalsScored
    | HeadToHead
    | HeadToHeadGoalDifference

type PromotionRule =
    | AutomaticPromotion of slots: int
    | PlayoffPromotion of slots: int

type RelegationRule =
    | AutomaticRelegation of slots: int
    | PlayoffRelegation of slots: int

type LeagueRules =
    { PointsForWin: int
      PointsForDraw: int
      Tiebreakers: TiebreakerRule list
      Promotion: PromotionRule list
      Relegation: RelegationRule list }

type TieResolution =
    | ReplayAtNeutral
    | ExtraTimeThenPenalties
    | AwayGoals
    | PenaltiesOnly

type LegFormat =
    | SingleLeg of TieResolution
    | TwoLegs of TieResolution

type GroupRules =
    { GroupCount: int
      TeamsPerGroup: int
      QualifyPerGroup: int
      PointsForWin: int
      PointsForDraw: int
      Tiebreakers: TiebreakerRule list }

type CupFormat =
    | StraightKnockout of LegFormat
    | GroupThenKnockout of GroupRules * LegFormat

type QualificationSlot =
    | LeaguePosition of level: LeagueLevel * fromPos: int * toPos: int
    | CupWinner of competitionName: string
    | TitleHolder
    | ConfederationSlot of slots: int

type CompetitionType =
    | NationalLeague of level: LeagueLevel * rules: LeagueRules
    | NationalCup of format: CupFormat * qualification: QualificationSlot list
    | InternationalCup of
        confederation: Confederation option *
        format: CupFormat *
        qualification: QualificationSlot list

type LeagueStanding =
    { ClubId: ClubId
      Played: int
      Won: int
      Drawn: int
      Lost: int
      GoalsFor: int
      GoalsAgainst: int
      Points: int }

type KnockoutTie =
    { TieId: int
      Round: Round
      HomeClubId: ClubId
      AwayClubId: ClubId
      Leg1FixtureId: MatchId option
      Leg2FixtureId: MatchId option
      AggregateScore: (int * int) option
      WinnerId: ClubId option }

type Competition =
    { Id: CompetitionId
      Name: string
      Type: CompetitionType
      Country: CountryCode option
      Season: int
      ClubIds: ClubId list
      Fixtures: Map<MatchId, MatchFixture>
      Standings: Map<ClubId, LeagueStanding>
      KnockoutTies: Map<int, KnockoutTie> }
