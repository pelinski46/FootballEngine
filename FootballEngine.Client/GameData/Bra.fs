module FootballEngine.Data.Countries.BRA

open FootballEngine.Domain
open FootballEngine.Data

let data: CountryData =
    { Country =
        { Code = "BRA"
          Name = "Brazil"
          Confederation = CONMEBOL }

      Names =
        { FirstNames =
            [ "Gabriel"
              "Lucas"
              "Matheus"
              "Thiago"
              "Vinicius"
              "Felipe"
              "Guilherme"
              "Joao"
              "Caua"
              "Arthur"
              "Vitor"
              "Igor"
              "Douglas"
              "Bruno"
              "Rodrigo"
              "Ricardo"
              "Murilo"
              "Endrick"
              "Neymar"
              "Richarlison"
              "Raphinha"
              "Rodrygo"
              "Militao"
              "Danilo"
              "Casemiro"
              "Fabinho"
              "Marquinhos"
              "Alisson"
              "Ederson"
              "Alex" ]

          LastNames =
            [ "Silva"
              "Santos"
              "Oliveira"
              "Souza"
              "Pereira"
              "Alves"
              "Ferreira"
              "Ribeiro"
              "Gomes"
              "Barbosa"
              "Martins"
              "Rocha"
              "Carvalho"
              "Mendes"
              "Soares"
              "Cardoso"
              "Teixeira"
              "Lima"
              "Costa"
              "Nascimento"
              "Araujo"
              "Cavalcanti"
              "Batista"
              "Dias" ] }

      LeagueNames = [ "Série A"; "Série B" ]

      LeagueRules =
        [ { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = []
            Relegation = [ AutomaticRelegation 4 ] }
          { PointsForWin = 3
            PointsForDraw = 1
            Tiebreakers = [ GoalDifference; GoalsScored; HeadToHead ]
            Promotion = [ AutomaticPromotion 4 ]
            Relegation = [ AutomaticRelegation 4 ] } ]

      Clubs =
        [ { Name = "Palmeiras"
            LeagueLevel = 0
            Reputation = Some 9500 }
          { Name = "Flamengo"
            LeagueLevel = 0
            Reputation = Some 9600 }
          { Name = "Botafogo"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Atletico Mineiro"
            LeagueLevel = 0
            Reputation = Some 9000 }
          { Name = "Sao Paulo"
            LeagueLevel = 0
            Reputation = Some 9200 }
          { Name = "Gremio"
            LeagueLevel = 0
            Reputation = Some 9000 }
          { Name = "Internacional"
            LeagueLevel = 0
            Reputation = Some 9000 }
          { Name = "Fluminense"
            LeagueLevel = 0
            Reputation = Some 8800 }
          { Name = "Red Bull Bragantino"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Athletico Paranaense"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Fortaleza"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Cuiaba"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Corinthians"
            LeagueLevel = 0
            Reputation = Some 9300 }
          { Name = "Cruzeiro"
            LeagueLevel = 0
            Reputation = Some 8900 }
          { Name = "Vasco da Gama"
            LeagueLevel = 0
            Reputation = Some 8700 }
          { Name = "Bahia"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Vitoria"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Juventude"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Atletico Goianiense"
            LeagueLevel = 0
            Reputation = None }
          { Name = "Criciuma"
            LeagueLevel = 0
            Reputation = None }

          { Name = "Santos"
            LeagueLevel = 1
            Reputation = Some 8500 }
          { Name = "America Mineiro"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Coritiba"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Goias"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Sport Recife"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Ceara"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Vila Nova"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Mirassol"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Novorizontino"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Avai"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Ponte Preta"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Botafogo-SP"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Ituano"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Brusque"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Paysandu"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Guarani"
            LeagueLevel = 1
            Reputation = None }
          { Name = "Chapecoense"
            LeagueLevel = 1
            Reputation = None }
          { Name = "CRB"
            LeagueLevel = 1
            Reputation = None } ]

      Cups = [ StraightKnockout(TwoLegs ExtraTimeThenPenalties) ] }
