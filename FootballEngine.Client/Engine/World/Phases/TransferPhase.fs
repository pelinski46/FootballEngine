namespace FootballEngine.World.Phases

open FootballEngine.Domain
open FootballEngine.World
open FootballEngine.World.AgentTypes
open FootballEngine.World.Agents
open FootballEngine.World.Supervisors

module TransferPhase =

    let make (state: GameState) : WorldPhase =
        let clubIds = state.Clubs |> Map.toList |> List.map fst
        let playerIds = state.Players |> Map.toList |> List.map fst

        let managerAgents = clubIds |> List.map ManagerAgent.make
        let clubAgents = clubIds |> List.map ClubAgent.makeFinance
        let playerAgents = playerIds |> List.map PlayerAgent.make

        let managerPhase = mkPhase managerAgents TransferSupervisor.resolve Weekly
        let clubPhase = mkPhase clubAgents FinanceSupervisor.resolve OnDemand
        let playerPhase = mkPhase playerAgents (fun s _ -> s) OnDemand

        { Frequency = Weekly
          Run = fun clock state -> state |> managerPhase.Run clock |> clubPhase.Run clock |> playerPhase.Run clock }
