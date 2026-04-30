namespace FootballEngine

open FootballEngine.Domain
open PhysicsContract
open Stats

type WallPosition = {
    PlayerIds: PlayerId list
    X: float<meter>
    Y: float<meter>
    JumpProbability: float
}

module WallBehavior =

    let calculateWallSize (distToGoal: float<meter>) : int =
        let dist = float distToGoal
        if dist < 18.0 then 4
        elif dist < 25.0 then 3
        elif dist < 30.0 then 2
        else 1

    let selectWallPlayers
        (defenders: Player[])
        (ballX: float<meter>)
        (goalX: float<meter>)
        (ballY: float<meter>)
        (wallSize: int)
        : PlayerId list =

        let gx = float goalX
        let bx = float ballX
        let by = float ballY

        defenders
        |> Array.filter (fun d -> d.Position <> Domain.GK)
        |> fun filtered ->
            let actualWallSize = min wallSize filtered.Length
            filtered
            |> Array.sortBy (fun d ->
                let px = float d.Id
                abs (bx - px))
            |> Array.take actualWallSize
        |> Array.map _.Id
        |> Array.toList

    let wallPosition
        (ballX: float<meter>)
        (goalX: float<meter>)
        (ballY: float<meter>)
        (wallSize: int)
        : WallPosition =

        let wallX = ballX + (goalX - ballX) / 3.0

        { PlayerIds = []
          X = wallX
          Y = ballY
          JumpProbability = 0.0 }

    let calculateJumpProbability (player: Player) : float =
        let bravery = float player.Mental.Bravery / 20.0
        let jumping = float player.Physical.JumpingReach / 20.0
        0.4 * bravery + 0.6 * jumping

    let shouldWallJump (player: Player) : bool =
        let jumpProb = calculateJumpProbability player
        bernoulli (jumpProb * 0.7)
