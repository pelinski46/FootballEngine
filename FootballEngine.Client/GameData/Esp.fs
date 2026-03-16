module FootballEngine.Data.Countries.ESP

open FootballEngine.Domain
open FootballEngine.Data

let data: CountryData =
    { Country =
        { Code = "ESP"
          Name = "Spain"
          Confederation = UEFA }

      Names =
        { FirstNames =
            [ "Alvaro"
              "Diego"
              "Iker"
              "Javier"
              "Pablo"
              "Sergio"
              "Adrian"
              "Borja"
              "Hugo"
              "Pau"
              "Ferran"
              "Lamine"
              "Gavi"
              "Dani"
              "Mikel"
              "Nico"
              "Alex"
              "Yeray"
              "Carlos"
              "Antonio"
              "Jose"
              "Juan"
              "Manuel"
              "David"
              "Raul"
              "Victor"
              "Ruben"
              "Marc"
              "Gerard"
              "Pedri" ]

          LastNames =
            [ "Garcia"
              "Rodriguez"
              "Gonzalez"
              "Fernandez"
              "Lopez"
              "Sanchez"
              "Perez"
              "Gomez"
              "Martin"
              "Ruiz"
              "Hernandez"
              "Torres"
              "Navarro"
              "Marin"
              "Vazquez"
              "Ramos"
              "Gil"
              "Serrano"
              "Moreno"
              "Jimenez"
              "Diaz"
              "Alvarez"
              "Romero"
              "Castro" ] }

      LeagueNames = [ "LaLiga"; "LaLiga Hypermotion" ]

      LeagueRules =
        [ { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ HeadToHead; HeadToHeadGoalDifference; GoalDifference; GoalsScored ]
            Promotion = []
            Relegation = [ AutomaticRelegation 3 ] }
          { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = [ AutomaticPromotion 2; PlayoffPromotion 2 ]
            Relegation = [ AutomaticRelegation 4 ] } ]

      Clubs =
        [ { Name = "Real Madrid"
            LeagueLevel = 0
            Reputation = Some 9999 }
          { Name = "Barcelona"
            LeagueLevel = 0
            Reputation = Some 9900 }
          { Name = "Atletico Madrid"
            LeagueLevel = 0
            Reputation = Some 9500 }
          { Name = "Girona"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Athletic Club"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Real Sociedad"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Real Betis"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Valencia"
            LeagueLevel = 0
            Reputation = Some 8800 }
          { Name = "Villarreal"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Getafe"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Las Palmas"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Osasuna"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Alaves"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Sevilla"
            LeagueLevel = 0
            Reputation = Some 9000 }
          { Name = "Mallorca"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Rayo Vallecano"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Celta Vigo"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Leganes"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Valladolid"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Espanyol"
            LeagueLevel = 0
            Reputation = None }

          { Name = "Cadiz"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Almeria"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Granada"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Eibar"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Sporting Gijon"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Real Oviedo"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Elche"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Levante"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Burgos"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Racing Santander"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Tenerife"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Albacete"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Cartagena"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Zaragoza"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Huesca"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Mirandes"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Castellon"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Deportivo"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Malaga"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Cordoba"
            LeagueLevel = 1
            Reputation = None } ]

      Cups = [ StraightKnockout(TwoLegs ExtraTimeThenPenalties) ] }
