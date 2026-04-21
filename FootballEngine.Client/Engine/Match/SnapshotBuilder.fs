namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract

type SimSnapshot =
    { SubTick: int
      HomePositions: Spatial[]
      AwayPositions: Spatial[]
      BallX: float<meter>
      BallY: float<meter>
      BallVx: float<meter / second>
      BallVy: float<meter / second>
      BallZ: float<meter>
      BallVz: float<meter / second>
      BallSpinTop: float<radianPerSecond>
      BallSpinSide: float<radianPerSecond>
      Possession: Possession
      HomeScore: int
      AwayScore: int
      HomeConditions: int[]
      AwayConditions: int[]
      HomeSidelined: Map<PlayerId, PlayerOut>
      AwaySidelined: Map<PlayerId, PlayerOut>
      Momentum: float }

type MatchReplay =
    { Context: MatchContext
      Final: SimState
      Events: MatchEvent list
      Snapshots: SimSnapshot[] }

module SnapshotBuilder =

    /// Takes a snapshot of the current sim state.
    /// Only called from runLoopFull — never from runLoopFast.
    let take (state: SimState) : SimSnapshot =
        { SubTick = state.SubTick
          HomePositions =
            state.Home.Slots |> Array.map (function
                | PlayerSlot.Active s -> s.Pos
                | Sidelined _ -> SimStateOps.kickOffSpatial)
          AwayPositions =
            state.Away.Slots |> Array.map (function
                | PlayerSlot.Active s -> s.Pos
                | Sidelined _ -> SimStateOps.kickOffSpatial)
          BallX = state.Ball.Position.X
          BallY = state.Ball.Position.Y
          BallVx = state.Ball.Position.Vx
          BallVy = state.Ball.Position.Vy
          BallZ = state.Ball.Position.Z
          BallVz = state.Ball.Position.Vz
          BallSpinTop = state.Ball.Spin.Top
          BallSpinSide = state.Ball.Spin.Side
          Possession = state.Ball.Possession
          HomeScore = state.HomeScore
          AwayScore = state.AwayScore
          HomeConditions =
            state.Home.Slots |> Array.map (function
                | PlayerSlot.Active s -> s.Condition
                | Sidelined _ -> 0)
          AwayConditions =
            state.Away.Slots |> Array.map (function
                | PlayerSlot.Active s -> s.Condition
                | Sidelined _ -> 0)
          HomeSidelined = state.Home.Sidelined
          AwaySidelined = state.Away.Sidelined
          Momentum = state.Momentum }
