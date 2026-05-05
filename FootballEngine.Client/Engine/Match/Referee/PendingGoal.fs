namespace FootballEngine.Referee

open FootballEngine.Domain
open FootballEngine.Types

type ActionResult =
    { Events: MatchEvent list
      PendingRefereeActions: RefereeAction list }

module ActionResult =
    let empty = { Events = []; PendingRefereeActions = [] }

    let ofEvents events = { Events = events; PendingRefereeActions = [] }

    let combine (results: ActionResult list) =
        { Events = results |> List.collect _.Events
          PendingRefereeActions = results |> List.collect _.PendingRefereeActions }
