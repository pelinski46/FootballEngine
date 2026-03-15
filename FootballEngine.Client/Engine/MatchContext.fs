namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.DomainTypes

module MatchContext =

    type Possession =
        | Home
        | Away

    type MatchPhase =
        | BuildUp
        | Midfield
        | Attack

    type PlayerOut =
        | SidelinedByRedCard
        | SidelinedByInjury
        | SidelinedBySub // came off as a substitution

    /// Full immutable state of the match at any point in simulation time.
    ///
    /// HomePlayers/AwayPlayers are the CURRENT active squads — substitutes
    /// are appended here when they come on, and the player going off is added
    /// to HomeSidelined/AwaySidelined. This keeps resolveView correct even
    /// after multiple substitutions.
    type MatchState =
        {
            Home: Club
            Away: Club
            Second: int
            HomeScore: int
            AwayScore: int
            BallPosition: float * float
            Possession: Possession
            Momentum: float
            /// Current squad (starters + any substitutes already on).
            HomePlayers: Player[]
            AwayPlayers: Player[]
            HomeConditions: int[]
            AwayConditions: int[]
            HomeSidelined: Map<PlayerId, PlayerOut>
            AwaySidelined: Map<PlayerId, PlayerOut>
            HomeYellows: Map<PlayerId, int>
            AwayYellows: Map<PlayerId, int>
            HomeSubsUsed: int
            AwaySubsUsed: int
            EventsRev: MatchEvent list
            HomePositions: Map<PlayerId, float * float>
            AwayPositions: Map<PlayerId, float * float>
            HomeBasePositions: Map<PlayerId, float * float>
            AwayBasePositions: Map<PlayerId, float * float>
        }

    /// Pre-computed spatial data. Positions are indexed by PlayerId so they
    /// survive substitutions (new players get a default centre position).
    type MatchContext =
        { HomePositions: Map<PlayerId, float * float>
          AwayPositions: Map<PlayerId, float * float> }

    type MatchReplay =
        { Final: MatchState
          Snapshots: MatchState[] }
    // ------------------------------------------------------------------ //
    //  Scheduled events                                                    //
    // ------------------------------------------------------------------ //

    type ScheduledEvent =
        | Duel
        | FatigueCheck
        | ShotAttempt of attacker: Player
        | CardCheck of player: Player * clubId: ClubId * isPossessingTeam: bool
        | InjuryCheck of player: Player * clubId: ClubId
        | SubstitutionCheck of clubId: ClubId
        | MatchEnd

    // ------------------------------------------------------------------ //
    //  Helpers                                                             //
    // ------------------------------------------------------------------ //

    let inline addEvent (ev: MatchEvent) (s: MatchState) =
        { s with EventsRev = ev :: s.EventsRev }

    let flipPossession =
        function
        | Home -> Away
        | Away -> Home

    let inline phaseFromBallZone (z: float) =
        if z < 30.0 || z > 70.0 then BuildUp
        elif z >= 40.0 && z <= 60.0 then Midfield
        else Attack

    /// Indices of players not currently sidelined.
    let activeIndices (players: Player[]) (sidelined: Map<PlayerId, PlayerOut>) =
        players
        |> Array.mapi (fun i p -> i, p)
        |> Array.filter (fun (_, p) -> not (Map.containsKey p.Id sidelined))
        |> Array.map fst

    /// Position for a player, defaulting to centre-pitch if not found
    /// (happens for substitutes whose slot wasn't in the original lineup).
    let positionOf (positions: Map<PlayerId, float * float>) (p: Player) =
        positions |> Map.tryFind p.Id |> Option.defaultValue (50.0, 50.0)

    // ------------------------------------------------------------------ //
    //  Score pressure helpers                                              //
    // ------------------------------------------------------------------ //

    let goalDiff (isHome: bool) (s: MatchState) =
        if isHome then
            s.HomeScore - s.AwayScore
        else
            s.AwayScore - s.HomeScore

    /// Multiplier in [0.6, 1.6]: losing → presses more, winning → manages game.
    let pressureMultiplier (isHome: bool) (s: MatchState) =
        let clamped = float (max -2 (min 2 (goalDiff isHome s)))
        1.1 - clamped * 0.25
