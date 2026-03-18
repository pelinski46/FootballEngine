namespace FootballEngine

open FootballEngine.Domain

module FormationLineUps =
    let all =
        [ F442
          F442Diamond
          F433
          F433Flat
          F451
          F4141
          F4231
          F4312
          F4321
          F352
          F343
          F3421
          F532
          F541
          F523 ]


module Lineup =
    let private available (club: Club) =
        club.Players |> List.filter (fun p -> p.Status = Available)

    let bestForPosition (players: Player list) (pos: Position) : Player option =
        match players |> List.filter (fun p -> p.Position = pos) with
        | [] -> players |> List.sortByDescending _.CurrentSkill |> List.tryHead
        | exact -> exact |> List.sortByDescending _.CurrentSkill |> List.tryHead

    let bestFormation (club: Club) : Formation =
        let bench = available club

        FormationLineUps.all
        |> List.maxBy (fun formation ->
            FormationData.getFormation formation
            |> List.sumBy (fun slot ->
                bench
                |> List.filter (fun p -> p.Position = slot.Role)
                |> List.sortByDescending _.CurrentSkill
                |> List.tryHead
                |> Option.map _.CurrentSkill
                |> Option.defaultValue 0))

    let autoLineup (club: Club) (formation: Formation) : Club =
        match club.CurrentLineup with
        | Some _ -> club
        | None ->
            let slots = FormationData.getFormation formation
            let bench = available club

            let rec assign (remaining: FormationSlot list) (pool: Player list) (acc: LineupSlot list) =
                match remaining with
                | [] -> List.rev acc
                | slot :: rest ->
                    let lineSlot =
                        { Index = slot.Index
                          Role = slot.Role
                          X = slot.X
                          Y = slot.Y
                          PlayerId = None }

                    match bestForPosition pool slot.Role with
                    | None -> assign rest pool (lineSlot :: acc)
                    | Some p ->
                        assign
                            rest
                            (pool |> List.filter (fun x -> x.Id <> p.Id))
                            ({ lineSlot with PlayerId = Some p.Id } :: acc)

            { club with
                CurrentLineup =
                    Some
                        { Formation = formation
                          TeamTactics = "Balanced"
                          Slots = assign slots bench [] } }

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
