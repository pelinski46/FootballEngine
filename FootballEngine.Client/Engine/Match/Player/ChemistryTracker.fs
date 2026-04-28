namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract

type FamiliarityPair = {
    PlayerA: PlayerId
    PlayerB: PlayerId
    Familiarity: float
}

type TeamCohesion = {
    OverallCohesion: float
    PressingCoordination: float
    OffsideTrapEffectiveness: float
    TransitionSpeed: float
}

module ChemistryTracker =

    let updateFamiliarity
        (current: float)
        (successfulPass: bool)
        (minutesPlayed: float)
        : float =

        let passBonus = if successfulPass then 0.02 else -0.005
        let timeBonus = minutesPlayed * 0.001
        min 1.0 (current + passBonus + timeBonus)

    let calculateCohesion
        (familiarity: float[,])
        (playerCount: int)
        : TeamCohesion =

        let mutable totalFam = 0.0
        let mutable count = 0

        for i = 0 to playerCount - 1 do
            for j = i + 1 to playerCount - 1 do
                totalFam <- totalFam + familiarity[i, j]
                count <- count + 1

        let avgFam = if count > 0 then totalFam / float count else 0.5

        { OverallCohesion = avgFam
          PressingCoordination = avgFam * 0.8 + 0.1
          OffsideTrapEffectiveness = avgFam * 0.7 + 0.15
          TransitionSpeed = avgFam * 0.9 + 0.05 }

    let familiarityBonus (familiarity: float) : float =
        familiarity * 0.15
