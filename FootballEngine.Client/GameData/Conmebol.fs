module FootballEngine.Data.International.CONMEBOL

open FootballEngine.Domain

let libertadores: CompetitionType =
    InternationalCup(
        Some CONMEBOL,
        GroupThenKnockout(
            { GroupCount = 8
              TeamsPerGroup = 4
              QualifyPerGroup = 2
              PointsForWin = 3
              PointsForDraw = 1
              Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ] },
            TwoLegs ExtraTimeThenPenalties
        ),
        [ LeaguePosition(LeagueLevel 0, 1, 3)
          CupWinner "Copa Argentina"
          LeaguePosition(LeagueLevel 0, 1, 4)
          CupWinner "Copa do Brasil" ]
    )

let sudamericana: CompetitionType =
    InternationalCup(
        Some CONMEBOL,
        GroupThenKnockout(
            { GroupCount = 8
              TeamsPerGroup = 4
              QualifyPerGroup = 2
              PointsForWin = 3
              PointsForDraw = 1
              Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ] },
            TwoLegs ExtraTimeThenPenalties
        ),
        [ LeaguePosition(LeagueLevel 0, 4, 7) ]
    )
