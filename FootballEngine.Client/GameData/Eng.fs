module FootballEngine.Data.Countries.ENG

open FootballEngine.Domain
open FootballEngine.Data

let data: CountryData =
    { Country =
        { Code = "ENG"
          Name = "England"
          Confederation = UEFA }

      Names =
        { FirstNames =
            [ "Harry"
              "Jack"
              "George"
              "Charlie"
              "James"
              "Thomas"
              "Oliver"
              "William"
              "Noah"
              "Jude"
              "Mason"
              "Declan"
              "Ben"
              "Luke"
              "Marcus"
              "Harvey"
              "Cole"
              "Kyle"
              "Liam"
              "Ethan"
              "Aaron"
              "Ryan"
              "Connor"
              "Kieran"
              "Jordan"
              "Tyler"
              "Reece"
              "Trent"
              "Curtis"
              "Callum" ]

          LastNames =
            [ "Smith"
              "Jones"
              "Williams"
              "Brown"
              "Taylor"
              "Davies"
              "Wilson"
              "Evans"
              "Thomas"
              "Johnson"
              "Walker"
              "Alexander"
              "Rice"
              "Foden"
              "Palmer"
              "Saka"
              "Kane"
              "Bellingham"
              "Sterling"
              "Mount"
              "Gallagher"
              "Trippier"
              "Maguire"
              "Shaw" ] }

      LeagueNames = [ "Premier League"; "Championship" ]

      LeagueRules =
        [ { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = []
            Relegation = [ AutomaticRelegation 3 ] }
          { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = [ AutomaticPromotion 2; PlayoffPromotion 1 ]
            Relegation = [ AutomaticRelegation 3 ] } ]

      Clubs =
        [ { Name = "Man City"
            LeagueLevel = 0
            Reputation = Some 9800 }
          { Name = "Arsenal"
            LeagueLevel = 0
            Reputation = Some 9600 }
          { Name = "Liverpool"
            LeagueLevel = 0
            Reputation = Some 9700 }
          { Name = "Aston Villa"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Tottenham"
            LeagueLevel = 0
            Reputation = Some 9200 }
          { Name = "Chelsea"
            LeagueLevel = 0
            Reputation = Some 9300 }
          { Name = "Newcastle"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Man Utd"
            LeagueLevel = 0
            Reputation = Some 9500 }
          { Name = "West Ham"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Brighton"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Bournemouth"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Wolves"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Fulham"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Crystal Palace"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Everton"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Brentford"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Nottm Forest"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Leicester"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Ipswich"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Southampton"
            LeagueLevel = 0
            Reputation = None }

          { Name = "Burnley"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Luton"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Sheffield Utd"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Leeds"
            LeagueLevel = 1
            Reputation = None }
          { Name = "West Brom"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Norwich"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Hull City"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Middlesbrough"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Coventry"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Preston"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Bristol City"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Cardiff"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Swansea"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Watford"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Sunderland"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Millwall"
            LeagueLevel = 1
            Reputation = None }
          { Name = "QPR"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Stoke City"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Blackburn"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Portsmouth"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Derby County"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Oxford Utd"
            LeagueLevel = 1
            Reputation = None } ]

      Cups =
        [ StraightKnockout(SingleLeg ExtraTimeThenPenalties)
          StraightKnockout(SingleLeg ExtraTimeThenPenalties) ] }
