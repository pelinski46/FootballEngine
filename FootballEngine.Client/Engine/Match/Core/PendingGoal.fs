namespace FootballEngine

open FootballEngine.Domain

type ActionResult =
    { Events: MatchEvent list }

module ActionResult =
    let empty = { Events = [] }

    let ofEvents events = { Events = events }

    let combine (results: ActionResult list) =
        { Events = results |> List.collect _.Events }
