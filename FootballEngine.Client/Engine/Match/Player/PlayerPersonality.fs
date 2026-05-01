namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract
open Stats

type PlayerPersonality = {
    Flair: float
    Consistency: float
    ImportantMatches: float
    Controversy: float
    Leadership: float
    Teamwork: float
    Ambition: float
    Pressure: float
    Sportsmanship: float
    Temperament: float
}

module PlayerPersonality =

    let derive (player: Player) : PlayerPersonality =
        let mental = player.Mental
        let norm (v: int) = float v / 20.0

        { Flair = norm mental.Vision * 0.6 + norm player.Technical.Dribbling * 0.4
          Consistency = norm mental.Concentration * 0.5 + norm mental.Composure * 0.5
          ImportantMatches = norm mental.Composure * 0.4 + norm mental.Leadership * 0.3 + norm player.Morale * 0.3
          Controversy = norm mental.Aggression * 0.5 + (1.0 - norm mental.Composure) * 0.5
          Leadership = norm mental.Leadership
          Teamwork = norm mental.WorkRate * 0.6 + norm mental.Positioning * 0.4
          Ambition = norm player.Morale * 0.5 + norm mental.WorkRate * 0.5
          Pressure = norm mental.Composure * 0.7 + norm mental.Concentration * 0.3
          Sportsmanship = 1.0 - norm mental.Aggression * 0.6
          Temperament = norm mental.Composure * 0.5 + norm mental.Concentration * 0.5 }

    let defaultPersonality = {
        Flair = 0.5; Consistency = 0.5; ImportantMatches = 0.5
        Controversy = 0.5; Leadership = 0.5; Teamwork = 0.5
        Ambition = 0.5; Pressure = 0.5; Sportsmanship = 0.5; Temperament = 0.5 }

module OpponentModel =

    type OpponentInsight = {
        IsSlow: bool
        IsAggressive: bool
        GKWeakSide: ClubSide
        HighPress: bool
    }

    let analyze (opponentFrame: TeamFrame) (opponentRoster: PlayerRoster) : OpponentInsight =
        let mutable avgPace = 0.0
        let mutable avgAggression = 0.0
        let mutable count = 0

        for i = 0 to opponentFrame.SlotCount - 1 do
            match opponentFrame.Physics.Occupancy[i] with
            | OccupancyKind.Active rosterIdx ->
                let p = opponentRoster.Players[rosterIdx]
                avgPace <- avgPace + float p.Physical.Pace
                avgAggression <- avgAggression + float p.Mental.Aggression
                count <- count + 1
            | _ -> ()

        let avgPaceNorm = if count > 0 then avgPace / float count / 20.0 else 0.5
        let avgAggNorm = if count > 0 then avgAggression / float count / 20.0 else 0.5

        { IsSlow = avgPaceNorm < 0.4
          IsAggressive = avgAggNorm > 0.6
          GKWeakSide = HomeClub
          HighPress = avgAggNorm > 0.5 }
