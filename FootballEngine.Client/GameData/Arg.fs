module FootballEngine.Data.Countries.ARG

open FootballEngine.Domain
open FootballEngine.Data

let data: CountryData =
    { Country =
        { Code = "ARG"
          Name = "Argentina"
          Confederation = CONMEBOL }

      Names =
        { FirstNames =
            [ "Bautista"
              "Mateo"
              "Enzo"
              "Nahuel"
              "Julian"
              "Franco"
              "Lautaro"
              "Tiago"
              "Geronimo"
              "Facundo"
              "Ignacio"
              "Nicolas"
              "Lucas"
              "Santiago"
              "Tomas"
              "Valentin"
              "Agustin"
              "Marcos"
              "Luis"
              "Julio"
              "Rodrigo"
              "Joaquin"
              "Thiago"
              "Benjamin"
              "Maximo"
              "Santino"
              "Luciano"
              "Emiliano"
              "Gonzalo"
              "Mauricio"
              "Felipe"
              "Alexis"
              "Damian"
              "Ezequiel"
              "Gaston"
              "Cristian"
              "Sebastian"
              "Martin"
              "Maximiliano"
              "Leandro"
              "Guillermo"
              "Pablo"
              "Fernando"
              "Andres"
              "Diego"
              "Mariano"
              "Adrian"
              "Claudio"
              "Gabriel"
              "Ramiro"
              "Matias"
              "Ivan"
              "Oscar"
              "Raul"
              "Bruno"
              "Alan"
              "Dylan"
              "Kevin"
              "Brian"
              "Emanuel"
              "Axel"
              "Mathias"
              "Joel"
              "Aaron"
              "Elias"
              "Ian"
              "Liam"
              "Gael"
              "Leon"
              "Noah"
              "Dante"
              "Milo"
              "Benicio"
              "Oliver"
              "Lorenzo"
              "Emilio"
              "Vicente"
              "Simon"
              "Samuel"
              "Alejo"
              "Lisandro"
              "Jeronimo"
              "Octavio"
              "Renato"
              "Gino"
              "Iker"
              "Rafael"
              "Ismael"
              "Camilo"
              "Tadeo"
              "Luca"
              "Adriel"
              "Aron"
              "Uriel"
              "Emir"
              "Izan"
              "Karim" ]

          LastNames =
            [ "Martinez"
              "Rodriguez"
              "Gomez"
              "Fernandez"
              "Lopez"
              "Diaz"
              "Perez"
              "Romero"
              "Sosa"
              "Alvarez"
              "Torres"
              "Garcia"
              "Acosta"
              "Benitez"
              "Medina"
              "Vazquez"
              "Blanco"
              "Cano" ] }

      LeagueNames = [ "Liga Profesional"; "Primera Nacional" ]

      LeagueRules =
        [ { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = []
            Relegation = [ AutomaticRelegation 3 ] }
          { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = [ AutomaticPromotion 2; PlayoffPromotion 4 ]
            Relegation = [ AutomaticRelegation 2 ] } ]

      Clubs =
        [ { Name = "River Plate"
            LeagueLevel = 0
            Reputation = Some 9500 }
          { Name = "Boca Juniors"
            LeagueLevel = 0
            Reputation = Some 9400 }
          { Name = "Racing Club"
            LeagueLevel = 0
            Reputation = Some 8800 }
          { Name = "Independiente"
            LeagueLevel = 0
            Reputation = Some 8700 }
          { Name = "San Lorenzo"
            LeagueLevel = 0
            Reputation = Some 8600 }
          { Name = "Talleres"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Estudiantes LP"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Defensa y Justicia"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Lanus"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Newell's"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Rosario Central"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Argentinos Jrs"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Huracan"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Godoy Cruz"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Belgrano"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Union SF"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Platense"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Velez Sarsfield"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Instituto"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Banfield"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Gimnasia LP"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Barracas Central"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Tigre"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Atl. Tucuman"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Sarmiento"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Ind. Rivadavia"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Dep. Riestra"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Central Cordoba"
            LeagueLevel = 0
            Reputation = None }

          { Name = "Quilmes"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Ferro"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Chacarita"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Colon"
            LeagueLevel = 1
            Reputation = None }
          { Name = "San Martin T"
            LeagueLevel = 1
            Reputation = None }
          { Name = "San Martin SJ"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Nueva Chicago"
            LeagueLevel = 1
            Reputation = None }
          { Name = "All Boys"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Aldosivi"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Atlanta"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Almagro"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Temperley"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Gimnasia M"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Def. de Belgrano"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Almirante Brown"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Dep. Moron"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Chaco For Ever"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Rafaela"
            LeagueLevel = 1
            Reputation = None } ]

      Cups = [ StraightKnockout(SingleLeg ExtraTimeThenPenalties) ] }
