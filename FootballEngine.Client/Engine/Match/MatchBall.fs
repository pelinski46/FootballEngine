namespace FootballEngine

module MatchBall =

    [<Literal>]
    let private gravity = -9.8

    let private stepPhysics (dt: float) (sp: Spatial) : Spatial =
        let x' = sp.X + sp.Vx * dt
        let y' = sp.Y + sp.Vy * dt
        let z' = sp.Z + sp.Vz * dt
        let vx' = sp.Vx * BalanceConfig.BallFriction
        let vy' = sp.Vy * BalanceConfig.BallFriction
        let vz' = (sp.Vz + gravity * dt) * BalanceConfig.BallFriction

        if z' <= 0.0 then
            { X = x'
              Y = y'
              Z = 0.0
              Vx = vx'
              Vy = vy'
              Vz = -vz' * BalanceConfig.BallBounceDamping }
        else
            { X = x'
              Y = y'
              Z = z'
              Vx = vx'
              Vy = vy'
              Vz = vz' }

    let updatePhysics (dt: float) (s: MatchState) : MatchState =
        let ball' =
            { s.Ball with
                Position = stepPhysics dt s.Ball.Position }

        { s with Ball = ball' }
