namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

module TacticalPipeline =

    /// Calculate marking assignments for a team
    /// Returns MarkMan directive for a specific player index if applicable
    let calculateMarking
        (playerIdx: int)
        (slots: PlayerSlot[])
        (oppSlots: PlayerSlot[])
        (currentSubTick: int)
        : Directive option =

        match slots[playerIdx] with
        | PlayerSlot.Active s when s.Player.Position <> GK ->
            // Basic pairing: outfield players match by approximate index
            // In a full implementation, we'd sort oppSlots by position first
            let mutable oppCount = 0
            let mutable targetOppPos = None
            
            for i = 0 to oppSlots.Length - 1 do
                match oppSlots[i] with
                | PlayerSlot.Active os when os.Player.Position <> GK ->
                    if oppCount = playerIdx - 1 then // playerIdx-1 because GK is 0
                        targetOppPos <- Some os.Pos
                    oppCount <- oppCount + 1
                | _ -> ()

            match targetOppPos with
            | Some pos ->
                Some (Directive.create MarkMan pos.X pos.Y 0.6 0.5 (currentSubTick + 80) "tactical-marking" Directive.tacticalPriority)
            | None -> None
        | _ -> None

    /// Calculate ball-chase (pressing) directive for a specific player
    let calculateBallChase
        (playerIdx: int)
        (chasingSet: int[])
        (ballPos: Spatial)
        (currentSubTick: int)
        : Directive option =
        
        let mutable rank = -1
        for i = 0 to chasingSet.Length - 1 do
            if chasingSet[i] = playerIdx then rank <- i
            
        if rank >= 0 then
            // Aumentamos los pesos drásticamente (de 10.0 a 50.0 para el primero)
            let weight = if rank = 0 then 50.0 else if rank = 1 then 15.0 else 5.0
            // Reducimos el tiempo de expiración para recalcular rápido y usamos prioridad de emergencia
            Some (Directive.create Press ballPos.X ballPos.Y weight 1.0 (currentSubTick + 10) "ball-chase" Directive.emergencyPriority)
        else
            None

    /// Update tactical state (marking, pressing, ball chase)
    /// Called at CognitiveRate (200 subTicks = 5 seconds)
    let update
        (currentSubTick: int)
        (state: SimState)
        (clubSide: ClubSide)
        : unit =

        let slots = getSlots state clubSide
        let oppSlots = getSlots state (ClubSide.flip clubSide)
        let ballPos = state.Ball.Position

        // Identify top 3 chasers once
        let mutable top1 = (999999.0<meter^2>, -1)
        let mutable top2 = (999999.0<meter^2>, -1)
        let mutable top3 = (999999.0<meter^2>, -1)

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let distSq = s.Pos.DistSqTo2D ballPos
                if distSq < fst top1 then
                    top3 <- top2
                    top2 <- top1
                    top1 <- (distSq, i)
                elif distSq < fst top2 then
                    top3 <- top2
                    top2 <- (distSq, i)
                elif distSq < fst top3 then
                    top3 <- (distSq, i)
            | _ -> ()
            
        let chasingSet = [| (snd top1); (snd top2); (snd top3) |] |> Array.filter (fun i -> i >= 0)

        // Update each slot
        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Sidelined _ -> ()
            | PlayerSlot.Active s ->
                let existing = s.Directives
                let markingOpt = calculateMarking i slots oppSlots currentSubTick
                let chaseOpt = calculateBallChase i chasingSet ballPos currentSubTick
                
                // Still using a List for merging, but we've removed the intermediate markingDirs/chaseDirs arrays
                let buf = System.Collections.Generic.List<Directive>(existing.Length + 2)
                for d in existing do
                    if not (Directive.expired currentSubTick d) then
                        match d.Kind with
                        | MarkMan | Press when d.Source = "tactical-marking" || d.Source = "ball-chase" -> ()
                        | _ -> buf.Add(d)
                
                markingOpt |> Option.iter buf.Add
                chaseOpt |> Option.iter buf.Add
                
                slots[i] <- PlayerSlot.Active { s with Directives = buf.ToArray() }
