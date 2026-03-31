namespace FootballEngine

open FootballEngine.Domain
open MatchState

module MatchReferee =

    type RefereeIntent =
        | BlowWhistleGoal of scoringTeam: Possession * scorerId: PlayerId option
        | IssueCard of player: Player * clubId: ClubId
        | IssueInjury of player: Player * clubId: ClubId
        | AwardThrowIn of team: Possession
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

    let cardProbability (isHome: bool) (p: Player) =
        let baseProb = 0.010 + float p.Mental.Aggression * 0.0001

        if isHome then
            baseProb * (1.0 - BalanceConfig.HomeCardReduction)
        else
            baseProb

    let injuryProbability (p: Player) =
        0.0008 + float (100 - p.Physical.Strength) * 0.00002

    let private ballCrossedGoalLine (s: MatchState) : Possession option =
        let pos = s.Ball.Position
        let inGoalY = pos.Y >= goalPostMin && pos.Y <= goalPostMax
        let inGoalZ = pos.Z >= 0.0 && pos.Z <= crossbarHeight

        if pos.X >= goalLineHome && inGoalY && inGoalZ then
            Some Home
        elif pos.X <= goalLineAway && inGoalY && inGoalZ then
            Some Away
        else
            None

    let private ballOutOfBounds (s: MatchState) : Possession option =
        let pos = s.Ball.Position
        let outX = pos.X < sidelineMin || pos.X > sidelineMax
        let outY = pos.Y < 2.0 || pos.Y > 98.0

        if outX || outY then
            let lastTouchTeam =
                s.Ball.LastTouchBy
                |> Option.bind (fun pid ->
                    if s.HomeSide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some Home
                    elif s.AwaySide.Players |> Array.exists (fun p -> p.Id = pid) then
                        Some Away
                    else
                        None)

            lastTouchTeam |> Option.map flipPossession
        else
            None

    let decide (second: int) (att: Player option) (def: Player option) (s: MatchState) : RefereeIntent list =
        let goalIntent =
            ballCrossedGoalLine s
            |> Option.map (fun team -> BlowWhistleGoal(team, s.Ball.LastTouchBy))
            |> Option.toList

        let throwInIntent = ballOutOfBounds s |> Option.map AwardThrowIn |> Option.toList

        let cardIntent =
            def
            |> Option.filter (fun d ->
                let isHomeDef = clubIdOf d s = s.Home.Id
                Stats.bernoulli (cardProbability isHomeDef d))
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

        | BlowWhistleGoal(scoringTeam, scorerId) -> awardGoal scoringTeam scorerId second s

        | AwardThrowIn throwTeam ->
            let isHome = throwTeam = Home
            let throwX = if isHome then 5.0 else 95.0

            { s with
                Possession = throwTeam
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

            withSide isHome ts' s, events

        | IssueInjury(player, clubId) ->
            let isHome = clubId = s.Home.Id
            let ts = side isHome s

            withSide
                isHome
                { ts with
                    Sidelined = Map.add player.Id SidelinedByInjury ts.Sidelined }
                s,
            [ { Second = second
                PlayerId = player.Id
                ClubId = clubId
                Type = Injury "match" } ]
