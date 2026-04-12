namespace FootballEngine.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Order
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Stats
open FootballEngine.MatchSimulator
open FootballEngine.Movement
open FootballEngine.SchedulingTypes
open FootballEngine.Benchmarks.Helpers


[<AbstractClass>]
type BenchBase() =
    let mutable state: SimState = Unchecked.defaultof<_>
    let mutable homeSquad: Player list = []
    let mutable awaySquad: Player list = []
    let mutable homeId: ClubId = 0
    let mutable attacker: Player = Unchecked.defaultof<_>
    let mutable target: Player = Unchecked.defaultof<_>

    member _.State
        with get () = state
        and set v = state <- v

    member _.HomeSquad
        with get () = homeSquad
        and set v = homeSquad <- v

    member _.AwaySquad
        with get () = awaySquad
        and set v = awaySquad <- v

    member _.HomeId
        with get () = homeId
        and set v = homeId <- v

    member _.Attacker
        with get () = attacker
        and set v = attacker <- v

    member _.Target
        with get () = target
        and set v = target <- v

    member this.BaseSetup() =
        let gs, c, p, st = loadClubs ()

        match setup c[0] c[1] p st false gs.ProfileCache with
        | Ok(ctx, st, h, a) ->
            this.State <- st
            this.HomeSquad <- h
            this.AwaySquad <- a
            this.HomeId <- c[0].Id
            let hp = SimStateOps.activePlayers st.Home.Slots
            this.Attacker <- hp[9]
            this.Target <- hp[10]
        | Error e -> failwithf "Setup failed: %A" e


[<MemoryDiagnoser>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
type MatchEngineE2EBenchmarks() =
    inherit BenchBase()

    [<GlobalSetup>]
    member this.Setup() = this.BaseSetup()

    [<Benchmark(Baseline = true, Description = "E2E: Full 90-Min Match")>]
    member this.FullMatchFast() =
        runLoopDes this.HomeId this.HomeSquad this.AwaySquad this.State

    [<Benchmark(Description = "E2E: Match State Init Only")>]
    member _.MatchStateInitOnly() =
        let gs, c, p, st = loadClubs ()

        match setup c[0] c[1] p st false gs.ProfileCache with
        | Ok(ctx, _, _, _) -> ctx
        | Error e -> failwithf "%A" e


[<MemoryDiagnoser>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
[<SimpleJob>]
type MovementBenchmarks() =
    inherit BenchBase()

    let mutable emptyEvents = ResizeArray<MatchEvent>()
    let mutable mockDirectives: Directive[][] = [||]

    [<GlobalSetup>]
    member this.Setup() =
        this.BaseSetup()

        mockDirectives <-
            this.State.HomeSide.Players
            |> Array.map (fun _ ->
                [| Directive.create Shape 52.5 34.0 1.0 1.0 999999 "shape"
                   Directive.create Press 30.0 20.0 0.8 1.2 999999 "press"
                   Directive.create Cover 60.0 50.0 0.6 0.9 999999 "cover" |])


    [<Benchmark(Baseline = true, Description = "Steering: 11 Players (1 Tick)")>]
    member this.TeamSteering() =
        MovementEngine.updateTeamSide 40 this.State HomeClub 0.1

    [<Benchmark(Description = "Cognitive AI: 11 Players (1 Tick)")>]
    member this.TeamCognitive() =
        MovementEngine.updateCognitive 40 this.State HomeClub emptyEvents

    [<Benchmark(Description = "GameRead: Spatial Analysis (1 Player)")>]
    member this.SinglePlayerGameRead() =
        GameRead.compute 5 this.State.HomeSide this.State.AwaySide 50.0 34.0 LeftToRight 0.0

    [<Benchmark(Description = "[DIAG] Array.copy Positions only")>]
    member this.PositionsArrayCopyOnly() =
        let newPos = Array.copy this.State.HomeSide.Positions
        newPos

    [<Benchmark(Description = "[DIAG] Record-with: TeamSide update")>]
    member this.TeamSideRecordWith() =
        let newPos = Array.copy this.State.HomeSide.Positions

        { this.State with
            HomeSide =
                { this.State.HomeSide with
                    Positions = newPos } }


    [<Benchmark(Description = "[DIAG] Steering: 1 Player only (overhead fijo)")>]
    member this.SinglePlayerSteering() =
        let side = this.State.HomeSide

        let singleSide =
            { side with
                Players = side.Players[0..0]
                Conditions = side.Conditions[0..0]
                Positions = side.Positions[0..0]
                BasePositions = side.BasePositions[0..0] }

        let singleState =
            { this.State with
                HomeSide = singleSide }

        MovementEngine.updateTeamSide 40 singleState HomeClub 0.1

    [<Benchmark(Description = "[DIAG] Directive composition (3 directives)")>]
    member this.DirectivesComposeOnly() =
        let directives = mockDirectives[0]
        Directive.composeDirectives 40 directives DirectiveModifiers.neutral


    [<Benchmark(Description = "[DIAG] Cognitive: with active directives")>]
    member this.TeamCognitiveWithDirectives() =

        let stateWithDirectives =
            { this.State with
                HomeDirectives = mockDirectives }

        MovementEngine.updateCognitive 40 stateWithDirectives HomeClub emptyEvents


    [<Benchmark(Description = "[DIAG] GameRead: full 22-player scan")>]
    member this.GameReadFull22Players() =

        GameRead.compute
            5
            this.State.HomeSide
            this.State.AwaySide
            this.State.Ball.Position.X
            this.State.Ball.Position.Y
            LeftToRight
            this.State.Momentum



[<MemoryDiagnoser>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
[<SimpleJob>]
type ActionBenchmarks() =
    inherit BenchBase()

    let mutable attackerIdx = 0

    [<GlobalSetup>]
    member this.Setup() =
        this.BaseSetup()

        attackerIdx <-
            this.State.HomeSide.Players
            |> Array.findIndex (fun p -> p.Id = this.Attacker.Id)


    [<Benchmark(Baseline = true, Description = "Action: Resolve Pass")>]
    member this.ResolvePass() =
        PassAction.resolve 100 this.State this.Target

    [<Benchmark(Description = "Action: Resolve Duel")>]
    member this.ResolveDuel() = DuelAction.resolve 100 this.State

    [<Benchmark(Description = "Spatial: Find Best Pass Target")>]
    member this.FindPassTarget() =
        MatchSpatial.findBestPassTarget this.Attacker this.State LeftToRight

    [<Benchmark(Description = "[DIAG] RNG: betaSample (mean=0.72, conc=10)")>]
    member _.RngBetaSampleOnly() = betaSample 0.72 10.0

    [<Benchmark(Description = "[DIAG] Raycast: single passLaneClear check (vs defSide)")>]
    member this.SingleRaycastOnly() =
        let ballX = this.State.Ball.Position.X
        let ballY = this.State.Ball.Position.Y
        let targetX = 70.0 // posición fija de target (compilador no la puede eliminar)
        let targetY = 34.0
        let defSide = this.State.AwaySide
        let tdx = targetX - ballX
        let tdy = targetY - ballY
        let lenSq = tdx * tdx + tdy * tdy
        let mutable defendersNearLine = 0.0

        for i = 0 to defSide.Players.Length - 1 do
            if defSide.Players[i].Position <> GK then
                let dSp = defSide.Positions[i]
                let dx = dSp.X - ballX
                let dy = dSp.Y - ballY

                if lenSq >= 0.01 then
                    let t = (dx * tdx + dy * tdy) / lenSq

                    if t >= 0.0 && t <= 1.0 then
                        let projX = ballX + t * tdx
                        let projY = ballY + t * tdy
                        let dist = sqrt ((dSp.X - projX) ** 2.0 + (dSp.Y - projY) ** 2.0)

                        if dist < 3.0 then
                            defendersNearLine <- defendersNearLine + 1.0

        defendersNearLine = 0.0

    [<Benchmark(Description = "[DIAG] FindPassTarget: open field (no defenders)")>]
    member this.FindPassTargetOpenField() =
        // Usa AwaySide vacío simulando campo abierto — nos dice el costo mínimo del algoritmo
        let emptyAway =
            { this.State.AwaySide with
                Positions = Array.zeroCreate this.State.AwaySide.Positions.Length }

        let stateOpen = { this.State with AwaySide = emptyAway }
        MatchSpatial.findBestPassTarget this.Attacker stateOpen LeftToRight

    [<Benchmark(Description = "[DIAG] Pass candidate scoring only (sin raycast)")>]
    member this.PassCandidateScoringOnly() =
        let attSide = this.State.HomeSide
        let ballX = this.State.Ball.Position.X
        let ballY = this.State.Ball.Position.Y
        let visionWeight = float this.Attacker.Mental.Vision / 100.0
        let mutable bestScore = System.Double.MinValue
        let mutable bestIdx = -1

        for i = 0 to attSide.Players.Length - 1 do
            let p = attSide.Players[i]

            if p.Id <> this.Attacker.Id then
                let sp = attSide.Positions[i]
                let dist = sqrt ((sp.X - ballX) ** 2.0 + (sp.Y - ballY) ** 2.0)
                let forwardBonus = if sp.X > ballX then 0.15 else 0.0


                let score = (1.0 / (1.0 + dist * 0.1)) + forwardBonus + visionWeight * 0.1

                if score > bestScore then
                    bestScore <- score
                    bestIdx <- i

        bestIdx // evita dead-code elimination


[<MemoryDiagnoser>]
[<Orderer(SummaryOrderPolicy.FastestToSlowest)>]
[<SimpleJob>]
type InfrastructureBenchmarks() =
    inherit BenchBase()

    let mutable scheduler: TickScheduler = Unchecked.defaultof<_>

    let mutable hotScheduler: TickScheduler = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member this.Setup() =
        this.BaseSetup()

        hotScheduler <- TickScheduler(PhysicsContract.MaxMatchSubTicks)

        for i in 0..500 do
            hotScheduler.Insert(
                { SubTick = i * 40
                  Priority = TickPriority.Physics
                  SequenceId = int64 i
                  Kind = PhysicsTick }
            )

            hotScheduler.Insert(
                { SubTick = i * 40 + 20
                  Priority = TickPriority.Duel
                  SequenceId = int64 (i + 1000)
                  Kind = DuelTick 0 }
            )

    [<IterationSetup(Target = "SchedulerColdStart")>]
    member _.SetupFreshScheduler() =
        scheduler <- TickScheduler(PhysicsContract.MaxMatchSubTicks)



    [<Benchmark(Baseline = true, Description = "Record: MatchState record-with (shallow)")>]
    member this.DeepRecordUpdate() =
        { this.State with
            SubTick = 100
            Momentum = 5.0
            Ball =
                { this.State.Ball with
                    IsInPlay = false } }

    [<Benchmark(Description = "Record: TeamSide with Array.copy (Positions)")>]
    member this.ArrayUpdate() =
        let newPos = Array.copy this.State.HomeSide.Positions
        newPos[0] <- { newPos[0] with X = 10.0 }

        { this.State with
            HomeSide =
                { this.State.HomeSide with
                    Positions = newPos } }

    [<Benchmark(Description = "Scheduler: cold-start (fresh per iteration)")>]
    member _.SchedulerColdStart() =
        for i in 0..1000 do
            scheduler.Insert(
                { SubTick = i + 10
                  Priority = TickPriority.Physics
                  SequenceId = 0L
                  Kind = PhysicsTick }
            )

            scheduler.Insert(
                { SubTick = i + 15
                  Priority = TickPriority.Duel
                  SequenceId = 1L
                  Kind = DuelTick 0 }
            )

            scheduler.Dequeue() |> ignore
            scheduler.Dequeue() |> ignore

    [<Benchmark(Description = "[DIAG] Scheduler: hot (pre-populated, steady state)")>]
    member _.SchedulerHot() =
        for i in 1001..2001 do
            hotScheduler.Insert(
                { SubTick = i + 10
                  Priority = TickPriority.Physics
                  SequenceId = 0L
                  Kind = PhysicsTick }
            )

            hotScheduler.Insert(
                { SubTick = i + 15
                  Priority = TickPriority.Duel
                  SequenceId = 1L
                  Kind = DuelTick 0 }
            )

            hotScheduler.Dequeue() |> ignore
            hotScheduler.Dequeue() |> ignore


    [<Benchmark(Description = "[DIAG] Scheduler: single Insert + Dequeue")>]
    member _.SchedulerSingleOp() =
        hotScheduler.Insert(
            { SubTick = 100000
              Priority = TickPriority.Physics
              SequenceId = 0L
              Kind = PhysicsTick }
        )

        hotScheduler.Dequeue() |> ignore

    [<Benchmark(Description = "[DIAG] ChemistryGraph: Array2D familiarity access (11×11)")>]
    member this.ChemistryGraphAccess() =
        let cg = this.Ctx.HomeChemistry
        let mutable sum = 0.0

        for i = 0 to cg.PlayerCount - 1 do
            for j = 0 to cg.PlayerCount - 1 do
                sum <- sum + cg.Familiarity[i, j]

        sum // evita dead-code elimination


    [<Benchmark(Description = "[DIAG] Directive list[]: create + filter expired (11 players)")>]
    member this.DirectiveListGCPressure() =
        let players = this.State.HomeSide.Players

        players
        |> Array.map (fun _ ->
            let existing =
                [ Directive.create Shape 52.5 34.0 1.0 1.0 100 "s" // expira en subtick 100
                  Directive.create Press 30.0 20.0 0.8 1.2 999999 "p" ]

            existing |> List.filter (fun d -> not (Directive.expired 40 d)) // subtick 40 → filtra el Shape
        )


    [<Benchmark(Description = "[DIAG] Record chain: 1000 sequential MatchState updates")>]
    member this.RecordChain1000Updates() =
        let mutable s = this.State

        for i = 0 to 999 do
            s <-
                { s with
                    SubTick = s.SubTick + 1
                    Momentum = s.Momentum + 0.001 }

        s.SubTick
