namespace FootballEngine

open FootballEngine.Domain
open MatchState

module MatchBall =

    [<Literal>]
    let private gravity = -9.8

    [<Literal>]
    let private friction = 0.98

    [<Literal>]
    let private bounceDamping = 0.5

    let private stepPhysics (dt: float) (sp: Spatial) : Spatial =
        let x'  = sp.X  + sp.Vx * dt
        let y'  = sp.Y  + sp.Vy * dt
        let z'  = sp.Z  + sp.Vz * dt
        let vx' = sp.Vx * friction
        let vy' = sp.Vy * friction
        let vz' = (sp.Vz + gravity * dt) * friction

        if z' <= 0.0 then
            { X = x'; Y = y'; Z = 0.0
              Vx = vx'; Vy = vy'; Vz = -vz' * bounceDamping }
        else
            { X = x'; Y = y'; Z = z'
              Vx = vx'; Vy = vy'; Vz = vz' }

    let updatePhysics (dt: float) (s: MatchState) : MatchState =
        let ball' = { s.Ball with Position = stepPhysics dt s.Ball.Position }
        { s with Ball = ball' }
