namespace FootballEngine.Client.AI

open FootballEngine.Domain

module ManagerAI =

    let defaultFormationSlots: FormationSlot list =
        [ { Index = 0
            Role = GK
            X = 5.0
            Y = 50.0 }
          { Index = 1
            Role = DL
            X = 20.0
            Y = 15.0 }
          { Index = 2
            Role = DC
            X = 20.0
            Y = 35.0 }
          { Index = 3
            Role = DC
            X = 20.0
            Y = 65.0 }
          { Index = 4
            Role = DR
            X = 20.0
            Y = 85.0 }
          { Index = 5
            Role = ML
            X = 50.0
            Y = 15.0 }
          { Index = 6
            Role = MC
            X = 50.0
            Y = 35.0 }
          { Index = 7
            Role = MC
            X = 50.0
            Y = 65.0 }
          { Index = 8
            Role = MR
            X = 50.0
            Y = 85.0 }
          { Index = 9
            Role = ST
            X = 80.0
            Y = 35.0 }
          { Index = 10
            Role = ST
            X = 80.0
            Y = 65.0 } ]

    let pickBestForPosition (players: Player list) (pos: Position) : Player option =
        // Primero busca jugadores de la posición exacta y disponibles
        match players |> List.filter (fun p -> p.Position = pos && p.Status = Available) with
        | [] ->
            // Si no hay, busca cualquier jugador disponible (el de mayor skill)
            players
            |> List.filter (fun p -> p.Status = Available)
            |> List.sortByDescending (fun p -> p.CurrentSkill)
            |> List.tryHead
        | available -> available |> List.sortByDescending (fun p -> p.CurrentSkill) |> List.tryHead

    let ensureLineup (club: Club) (formationSlots: FormationSlot list) : Club =
        match club.CurrentLineup with
        | Some _ -> club
        | None ->
            let available = club.Players |> List.filter (fun p -> p.Status = Available)

            let rec assign (slots: FormationSlot list) (remaining: Player list) (acc: LineupSlot list) =
                match slots with
                | [] -> List.rev acc
                | slot :: rest ->
                    match pickBestForPosition remaining slot.Role with
                    | Some p ->
                        let newRemaining = remaining |> List.filter (fun x -> x.Id <> p.Id)

                        let lineSlot =
                            { Index = slot.Index
                              Role = slot.Role
                              X = slot.X
                              Y = slot.Y
                              PlayerId = Some p.Id }

                        assign rest newRemaining (lineSlot :: acc)
                    | None ->

                        let lineSlot =
                            { Index = slot.Index
                              Role = slot.Role
                              X = slot.X
                              Y = slot.Y
                              PlayerId = None }

                        assign rest remaining (lineSlot :: acc)

            let lineupSlots = assign formationSlots available []

            let newLineup =
                { FormationName = "4-4-2"
                  TeamTactics = "Balanced"
                  Slots = lineupSlots }

            { club with
                CurrentLineup = Some newLineup }

    let ensureAwayLineup (club: Club) (formationSlots: FormationSlot list) : Club =
        let mirroredSlots = formationSlots |> List.map (fun s -> { s with X = 100.0 - s.X })

        ensureLineup { club with CurrentLineup = None } mirroredSlots
