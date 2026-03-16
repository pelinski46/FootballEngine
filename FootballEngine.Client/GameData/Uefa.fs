module FootballEngine.Data.International.UEFA

open FootballEngine.Domain

let championsLeague: CompetitionType =
    InternationalCup(
        Some UEFA,
        GroupThenKnockout(
            { GroupCount = 8
              TeamsPerGroup = 4
              QualifyPerGroup = 2
              PointsForWin = 3
              PointsForDraw = 1
              Tiebreakers = [ HeadToHead; HeadToHeadGoalDifference; GoalDifference; GoalsScored ] },
            TwoLegs ExtraTimeThenPenalties
        ),
        [ LeaguePosition(LeagueLevel 0, 1, 4); TitleHolder ]
    )

let europaLeague: CompetitionType =
    InternationalCup(
        Some UEFA,
        GroupThenKnockout(
            { GroupCount = 8
              TeamsPerGroup = 4
              QualifyPerGroup = 2
              PointsForWin = 3
              PointsForDraw = 1
              Tiebreakers = [ HeadToHead; HeadToHeadGoalDifference; GoalDifference; GoalsScored ] },
            TwoLegs ExtraTimeThenPenalties
        ),
        [ LeaguePosition(LeagueLevel 0, 5, 6)
          CupWinner "FA Cup"
          CupWinner "Copa del Rey" ]
    )
