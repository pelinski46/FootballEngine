namespace FootballEngine

open FootballEngine.Domain
open MatchState

module MatchReferee =

    let cardProbability (p: Player) =
        0.010 + float p.Mental.Aggression * 0.0001

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let processCard (player: Player) (clubId: ClubId) (second: int) (s: MatchState) : MatchState =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s
        let count = ts.Yellows |> Map.tryFind player.Id |> Option.defaultValue 0

        let ts' =
            if count >= 1 then
                { ts with
                    Yellows = Map.add player.Id (count + 1) ts.Yellows
                    Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined }
            else
                { ts with
                    Yellows = Map.add player.Id (count + 1) ts.Yellows }

        let s' = withSide isHome ts' s

        let ev t =
            addEvent
                { Second = second
                  PlayerId = player.Id
                  ClubId = clubId
                  Type = t }

        if count >= 1 then
            s' |> ev YellowCard |> ev RedCard
        else
            s' |> ev YellowCard

    let processInjury (player: Player) (clubId: ClubId) (second: int) (s: MatchState) : MatchState =
        let isHome = clubId = s.Home.Id
        let ts = side isHome s

        withSide
            isHome
            { ts with
                Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined }
            s
        |> addEvent
            { Second = second
              PlayerId = player.Id
              ClubId = clubId
              Type = Injury "match" }
