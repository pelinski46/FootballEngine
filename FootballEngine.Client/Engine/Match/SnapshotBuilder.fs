namespace FootballEngine

open FootballEngine.Domain
open FootballEngine.PhysicsContract
open SimStateOps

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

    let private positionsFromFrame (frame: TeamFrame) : Spatial[] =
        Array.init frame.SlotCount (fun i ->
            match frame.Occupancy[i] with
            | OccupancyKind.Active _ ->
                { X = float frame.PosX[i] * 1.0<meter>
                  Y = float frame.PosY[i] * 1.0<meter>
                  Z = 0.0<meter>
                  Vx = float frame.VelX[i] * 1.0<meter/second>
                  Vy = float frame.VelY[i] * 1.0<meter/second>
                  Vz = 0.0<meter/second> }
            | _ -> SimStateOps.kickOffSpatial)

    let private conditionsFromFrame (frame: TeamFrame) : int[] =
        Array.init frame.SlotCount (fun i -> int frame.Condition[i])

    /// Takes a snapshot of the current sim state.
    /// Only called from runLoopFull — never from runLoopFast.
    let take (state: SimState) : SimSnapshot =
        { SubTick = state.SubTick
          HomePositions = positionsFromFrame state.Home.Frame
          AwayPositions = positionsFromFrame state.Away.Frame
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
          HomeConditions = conditionsFromFrame state.Home.Frame
          AwayConditions = conditionsFromFrame state.Away.Frame
          HomeSidelined = state.Home.Sidelined
          AwaySidelined = state.Away.Sidelined
          Momentum = state.Momentum }
