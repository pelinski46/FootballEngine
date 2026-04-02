namespace FootballEngine

open FootballEngine.Domain
open MatchStateOps

module MatchReferee =

    type RefereeIntent =
        | BlowWhistleGoal of scoringClub: ClubSide * scorerId: PlayerId option
        | IssueCard of player: Player * clubId: ClubId
        | IssueInjury of player: Player * clubId: ClubId
        | AwardThrowIn of team: ClubSide
        | RefereeIdle

    [<Literal>]
    let private goalLineHome = 100.0

    [<Literal>]
    let private goalLineAway = 0.0

    [<Literal>]
    let private goalPostMin = 36.8

    [<Literal>]
    let private goalPostMax = 63.2

    [<Literal>]
    let private crossbarHeight = 2.44

    [<Literal>]
    let private sidelineMin = 2.0

    [<Literal>]
    let private sidelineMax = 98.0

    let cardProbability (playerClub: ClubSide) (p: Player) =
        let baseProb = 0.010 + float p.Mental.Aggression * 0.0004

        match playerClub with
        | HomeClub -> baseProb * (1.0 - BalanceConfig.HomeCardReduction)
        | AwayClub -> baseProb

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let private ballCrossedGoalLine (s: MatchState) : ClubSide option =
        let pos = s.Ball.Position
        let inGoalY = pos.Y >= goalPostMin && pos.Y <= goalPostMax
        let inGoalZ = pos.Z >= 0.0 && pos.Z <= crossbarHeight

        if pos.X >= goalLineHome && inGoalY && inGoalZ then
            Some HomeClub
        elif pos.X <= goalLineAway && inGoalY && inGoalZ then
            Some AwayClub
        else
            None

    let private ballOutOfBounds (s: MatchState) : ClubSide option =
        let pos = s.Ball.Position
        let outX = pos.X < sidelineMin || pos.X > sidelineMax
        let outY = pos.Y < 2.0 || pos.Y > 98.0

        if outX || outY then
            let lastTouchClub =
                s.Ball.LastTouchBy
                |> Option.bind (fun pid ->
                    if s.HomeSide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some s.Home.Id
                    elif s.AwaySide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some s.Away.Id
                    else
                        None)

            lastTouchClub
            |> Option.map (fun clubId -> ClubSide.flip (ClubSide.ofClubId clubId s))
        else
            None

    let decide (second: int) (att: Player option) (def: Player option) (s: MatchState) : RefereeIntent list =
        let goalIntent =
            ballCrossedGoalLine s
            |> Option.map (fun scoringClub -> BlowWhistleGoal(scoringClub, s.Ball.LastTouchBy))
            |> Option.toList

        let throwInIntent = ballOutOfBounds s |> Option.map AwardThrowIn |> Option.toList

        let cardIntent =
            def
            |> Option.filter (fun d ->
                let defenderClubId = clubIdOf d s
                let defenderClub = ClubSide.ofClubId defenderClubId s
                Stats.bernoulli (cardProbability defenderClub d))
            |> Option.map (fun d ->
                let clubId = clubIdOf d s
                IssueCard(d, clubId))
            |> Option.toList

        let injuryIntent =
            att
            |> Option.filter (fun a -> Stats.bernoulli (injuryProbability a))
            |> Option.map (fun a ->
                let clubId = clubIdOf a s
                IssueInjury(a, clubId))
            |> Option.toList

        goalIntent @ throwInIntent @ cardIntent @ injuryIntent

    let resolve (second: int) (intent: RefereeIntent) (s: MatchState) : MatchState * MatchEvent list =
        match intent with

        | RefereeIdle -> s, []

        | BlowWhistleGoal(scoringClub, scorerId) -> awardGoal scoringClub scorerId second s

        | AwardThrowIn throwClub ->
            let throwX =
                match throwClub with
                | HomeClub -> 5.0
                | AwayClub -> 95.0

            { s with
                AttackingClub = throwClub
                Ball =
                    { s.Ball with
                        Position =
                            { s.Ball.Position with
                                X = throwX
                                Y = 50.0
                                Z = 0.0
                                Vx = 0.0
                                Vy = 0.0
                                Vz = 0.0 } } },
            []

        | IssueCard(player, clubId) ->
            let ts = side clubId s
            let count = ts.Yellows |> Map.tryFind player.Id |> Option.defaultValue 0

            let ts' =
                if count >= 1 then
                    { ts with
                        Yellows = Map.add player.Id (count + 1) ts.Yellows
                        Sidelined = Map.add player.Id SidelinedByRedCard ts.Sidelined }
                else
                    { ts with
                        Yellows = Map.add player.Id (count + 1) ts.Yellows }

            let events =
                if count >= 1 then
                    [ { Second = second
                        PlayerId = player.Id
                        ClubId = clubId
                        Type = YellowCard }
                      { Second = second
                        PlayerId = player.Id
                        ClubId = clubId
                        Type = RedCard } ]
                else
                    [ { Second = second
                        PlayerId = player.Id
                        ClubId = clubId
                        Type = YellowCard } ]

            withSide clubId ts' s, events

        | IssueInjury(player, clubId) ->
            let ts = side clubId s

            withSide
                clubId
                { ts with
                    Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined }
                s,
            [ { Second = second
                PlayerId = player.Id
                ClubId = clubId
                Type = Injury "match" } ]
