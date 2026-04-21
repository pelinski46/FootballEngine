namespace FootballEngine.Movement

open FootballEngine
open FootballEngine.Domain
open SimStateOps
open FootballEngine.PhysicsContract

module MovementEngine =

    let refreshCache (currentSubTick: int) (state: SimState) (clubSide: ClubSide) =
        let slots = getSlots state clubSide
        let emergentState = getEmergentState state clubSide
        let modifiers = EmergentLoops.toDirectiveModifiers emergentState

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let target = Directive.composeDirectives currentSubTick s.Directives modifiers

                let exec =
                    ActionMath.calcMovementExecution
                        s.Player.Physical.Agility
                        s.Player.Physical.Balance
                        s.Player.Physical.Acceleration
                        s.Condition

                slots[i] <-
                    PlayerSlot.Active
                        { s with
                            CachedTarget = target
                            CachedExecution = float exec }
            | _ -> ()

    let updateStrategic (currentSubTick: int) (state: SimState) (clubSide: ClubSide) =
        let slots = getSlots state clubSide
        let basePos = getBasePositions state clubSide



        let dir = attackDirFor clubSide state
        let phase = phaseFromBallZone dir state.Ball.Position.X
        let modifiers = DirectiveModifiers.neutral

        let shapeDirectives =
            ShapePipeline.calculate
                basePos
                dir
                phase
                state.Ball.Position.X
                (tacticsConfig (getTactics state clubSide) (getInstructions state clubSide))
                currentSubTick


        for i = 0 to min slots.Length shapeDirectives.Length - 1 do
            match slots[i] with
            | PlayerSlot.Active s ->
                let existing = s.Directives
                let buf = System.Collections.Generic.List<Directive>(existing.Length + 1)

                for d in existing do
                    if not (Directive.expired currentSubTick d) then
                        match d.Kind with
                        | Shape -> ()
                        | _ -> buf.Add(d)


                buf.Add(shapeDirectives[i])
                slots[i] <- PlayerSlot.Active { s with Directives = buf.ToArray() }
            | _ -> ()

        setLastShapeSubTick state clubSide currentSubTick


    let updateTactical (currentSubTick: int) (state: SimState) (clubSide: ClubSide) =
        TacticalPipeline.update currentSubTick state clubSide

    let updatePhysics (currentSubTick: int) (state: SimState) (clubSide: ClubSide) (dt: float<second>) =
        let slots = getSlots state clubSide

        for i = 0 to slots.Length - 1 do
            match slots[i] with
            | PlayerSlot.Sidelined _ -> ()
            | PlayerSlot.Active s ->


                let newPos =
                    SteeringPipeline.calculate s.Player s.Profile s dt currentSubTick state.Ball state

                slots[i] <- PlayerSlot.Active { s with Pos = newPos }


    let private hasActiveShapeDirective (currentSubTick: int) (state: SimState) (clubSide: ClubSide) =
        getSlots state clubSide
        |> Array.exists (fun slot ->
            match slot with
            | PlayerSlot.Active s ->
                s.Directives
                |> Array.exists (fun d -> d.Kind = Shape && not (Directive.expired currentSubTick d))
            | _ -> false)

    let updateTeamSide
        (currentSubTick: int)
        (_ctx: MatchContext)
        (state: SimState)
        (clubSide: ClubSide)
        (dt: float<second>)
        (steeringRate: int)
        (cognitiveRate: int)
        =
        let shouldRunStrategic =
            currentSubTick % 600 = 0
            || not (hasActiveShapeDirective currentSubTick state clubSide)

        if shouldRunStrategic then
            updateStrategic currentSubTick state clubSide

        if currentSubTick % cognitiveRate = 0 then
            updateTactical currentSubTick state clubSide

        updatePhysics currentSubTick state clubSide dt
        refreshCache currentSubTick state clubSide
