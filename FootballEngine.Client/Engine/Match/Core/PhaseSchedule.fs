namespace FootballEngine

open SimulationClock

[<Struct>]
type SubTickPhases = {
    Physics: bool
    Cognitive: bool
    Steering: bool
    Referee: bool
    Action: bool
    SetPiece: bool
    Adaptive: bool
}

type PhaseSchedule = {
    Phases: SubTickPhases[]
    AdaptiveCheckpoints: int[]
}

module PhaseSchedule =
    let build (clock: SimulationClock) (maxSubTick: int) : PhaseSchedule =
        let phases = Array.zeroCreate (maxSubTick + 1)
        let mutable adaptiveCheckpoints = ResizeArray()

        for t = 0 to maxSubTick do
            let isPhysics = t % clock.PhysicsRate = 0
            let isSteering = t % clock.SteeringRate = 0
            let isCognitive = t % clock.CognitiveRate = 0
            let isReferee = t % clock.PhysicsRate = 0
            let isAction = t % clock.ActionRate = 0
            let isAdaptive = t % clock.AdaptiveRate = 0

            if isAdaptive then
                adaptiveCheckpoints.Add(t)

            phases[t] <- {
                Physics = isPhysics
                Cognitive = isCognitive
                Steering = isSteering
                Referee = isReferee
                Action = isAction
                SetPiece = false
                Adaptive = isAdaptive
            }

        { Phases = phases; AdaptiveCheckpoints = adaptiveCheckpoints.ToArray() }

    let setSetPiece (ps: PhaseSchedule) (subTick: int) (isSetPiece: bool) =
        if subTick >= 0 && subTick < ps.Phases.Length then
            let p = ps.Phases[subTick]
            ps.Phases[subTick] <- { p with SetPiece = isSetPiece }
