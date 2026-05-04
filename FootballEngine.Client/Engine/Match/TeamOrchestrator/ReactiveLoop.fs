namespace FootballEngine.TeamOrchestrator

open FootballEngine
open FootballEngine.Domain
open FootballEngine.Types

type PlanDeviation =
    | OnTrack
    | Drifting of magnitude: float
    | Critical of magnitude: float

module ReactiveLoop =

    let run (ctx: MatchContext) (state: SimState) (clock: SimulationClock) : PlanDeviation =
        let emergentHome = SimStateOps.getEmergentState state HomeClub
        let emergentAway = SimStateOps.getEmergentState state AwayClub

        // Compactness: ¿el equipo está manteniendo la forma?
        let shapeDeviation = 1.0 - emergentHome.CompactnessLevel

        // Pressing: ¿se está ejecutando el pressing planeado?
        let pressDeviation =
            abs (emergentHome.PressingIntensity - emergentAway.PressingIntensity)

        // Fatigue: leer Condition del frame directamente (igual que runAdaptive)
        let frame = SimStateOps.getFrame state HomeClub
        let mutable totalCondition = 0
        let mutable activeCount = 0

        for i = 0 to frame.SlotCount - 1 do
            match frame.Physics.Occupancy[i] with
            | OccupancyKind.Active _ ->
                totalCondition <- totalCondition + int frame.Condition[i]
                activeCount <- activeCount + 1
            | _ -> ()

        let avgCondition =
            if activeCount > 0 then
                float totalCondition / float activeCount
            else
                50.0

        let fatigueDeviation = 1.0 - (avgCondition / 100.0)

        let total = shapeDeviation * 0.4 + pressDeviation * 0.3 + fatigueDeviation * 0.3

        if total < 0.15 then OnTrack
        elif total < 0.35 then Drifting total
        else Critical total
