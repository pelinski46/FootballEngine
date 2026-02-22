namespace FootballEngine

open FootballEngine.Domain

module Lineup =

    let swapPlayer (targetIdx: int) (pId: PlayerId) (lineup: ClubLineup) : ClubLineup =
        let slotsMap = lineup.Slots |> List.map (fun s -> s.Index, s.PlayerId) |> Map.ofList

        let sourceIdx =
            slotsMap
            |> Map.tryPick (fun idx idOpt -> if idOpt = Some pId then Some idx else None)

        let occupant = slotsMap |> Map.tryFind targetIdx |> Option.flatten

        let newSlotsMap =
            match sourceIdx with
            | Some src ->

                slotsMap |> Map.add targetIdx (Some pId) |> Map.add src occupant
            | None ->

                slotsMap
                |> Map.add targetIdx (Some pId)
                |> Map.map (fun idx idOpt -> if idx <> targetIdx && idOpt = Some pId then None else idOpt)

        let newSlots =
            newSlotsMap
            |> Map.toList
            |> List.map (fun (idx, pidOpt) ->

                let originalSlot = lineup.Slots |> List.find (fun s -> s.Index = idx)

                { Index = idx
                  Role = originalSlot.Role
                  X = originalSlot.X
                  Y = originalSlot.Y
                  PlayerId = pidOpt })
            |> List.sortBy _.Index

        { lineup with Slots = newSlots }
