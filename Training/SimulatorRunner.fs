namespace Training

open FootballEngine
open FootballEngine.Domain
open FootballEngine.Simulation

type SimResult = {
    HomeScore: int
    AwayScore: int
    Events: MatchEvent list
    FinalState: SimState
    HomeId: ClubId
    AwayId: ClubId
}

module SimulatorRunner =

    let runOneMatch
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (profileMap: Map<PlayerId, BehavioralProfile>)
        : Result<SimResult, string> =
        match MatchSimulator.trySimulateMatch home away players staff profileMap with
        | Ok (homeScore, awayScore, events, finalState) ->
            Ok {
                HomeScore = homeScore
                AwayScore = awayScore
                Events = events
                FinalState = finalState
                HomeId = home.Id
                AwayId = away.Id
            }
        | Error err ->
            Error (sprintf "Simulation error: %A" err)

    let runBatch
        (n: int)
        (home: Club)
        (away: Club)
        (players: Map<PlayerId, Player>)
        (staff: Map<StaffId, Staff>)
        (profileMap: Map<PlayerId, BehavioralProfile>)
        : SimResult list =
        let rec loop i acc =
            if i >= n then List.rev acc
            else
                match runOneMatch home away players staff profileMap with
                | Ok result -> loop (i + 1) (result :: acc)
                | Error _ -> loop (i + 1) acc
        loop 0 []
