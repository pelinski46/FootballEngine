namespace FootballEngine

open FootballEngine.Domain

module ClubSide =
    let ofClubId (clubId: ClubId) (s: MatchState) : ClubSide =
        if clubId = s.Home.Id then HomeClub else AwayClub

    let toClubId (side: ClubSide) (s: MatchState) : ClubId =
        match side with
        | HomeClub -> s.Home.Id
        | AwayClub -> s.Away.Id

    let teamSide (side: ClubSide) (s: MatchState) : TeamSide =
        match side with
        | HomeClub -> s.HomeSide
        | AwayClub -> s.AwaySide
