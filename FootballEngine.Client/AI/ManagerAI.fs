namespace FootballEngine.Client.AI

open FootballEngine
open FootballEngine.Domain
open FootballEngine.DomainTypes

module ManagerAI =


    let pickBestForPosition (players: Player list) (pos: Position) : Player option =
        // Primero busca jugadores de la posición exacta y disponibles
        match players |> List.filter (fun p -> p.Position = pos && p.Status = Available) with
        | [] ->
            // Si no hay, busca cualquier jugador disponible (el de mayor skill)
            players
            |> List.filter (fun p -> p.Status = Available)
            |> List.sortByDescending _.CurrentSkill
            |> List.tryHead
        | available -> available |> List.sortByDescending _.CurrentSkill |> List.tryHead

    let pickBestFormation (club: Club) =
        let available = club.Players |> List.filter (fun p -> p.Status = Available)

        Formation.all
        |> List.maxBy (fun formation ->
            FormationData.getFormation formation
            |> List.sumBy (fun slot ->
                available
                |> List.filter (fun p -> p.Position = slot.Role)
                |> List.sortByDescending _.CurrentSkill
                |> List.tryHead
                |> Option.map _.CurrentSkill
                |> Option.defaultValue 0))

    let ensureLineup (club: Club) (formation: Formation) : Club =
        match club.CurrentLineup with
        | Some _ -> club
        | None ->
            let formationSlots = FormationData.getFormation formation
            let available = club.Players |> List.filter (fun p -> p.Status = Available)

            let rec assign (slots: FormationSlot list) (remaining: Player list) (acc: LineupSlot list) =
                match slots with
                | [] -> List.rev acc
                | slot :: rest ->
                    let lineSlot =
                        { Index = slot.Index
                          Role = slot.Role
                          X = slot.X
                          Y = slot.Y
                          PlayerId = None }

                    match pickBestForPosition remaining slot.Role with
                    | Some p ->
                        assign
                            rest
                            (remaining |> List.filter (fun x -> x.Id <> p.Id))
                            ({ lineSlot with PlayerId = Some p.Id } :: acc)
                    | None -> assign rest remaining (lineSlot :: acc)

            { club with
                CurrentLineup =
                    Some
                        { Formation = pickBestFormation club
                          TeamTactics = "Balanced"
                          Slots = assign formationSlots available [] } }
