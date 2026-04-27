namespace FootballEngine

open FootballEngine.Domain

type PendingGoal =
    { ScoringClub: ClubSide
      ScorerId: PlayerId option }

type ActionResult =
    { Events: MatchEvent list
      PendingGoal: PendingGoal option }

module ActionResult =
    let empty = { Events = []; PendingGoal = None }

    let ofEvents events = { Events = events; PendingGoal = None }

    let withGoal scoringClub scorerId events =
        { Events = events; PendingGoal = Some { ScoringClub = scoringClub; ScorerId = scorerId } }

    let combine (results: ActionResult list) =
        { Events = results |> List.collect _.Events
          PendingGoal = results |> List.tryPick _.PendingGoal }
