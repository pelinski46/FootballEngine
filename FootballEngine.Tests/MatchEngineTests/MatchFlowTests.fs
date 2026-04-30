module FootballEngine.Tests.MatchEngineTests.MatchFlowTests

open Expecto
open FootballEngine
open FootballEngine.Domain

let matchFlowTests =
    testList
        "MatchFlow"
        [ test "updateOne advances exactly one subTick while a state timer is active" {
              let state = SimState()
              state.Flow <-
                  GoalPause
                      { ScoringTeam = HomeClub
                        ScorerId = None
                        IsOwnGoal = false
                        RemainingTicks = 3
                        VARRequested = false }

              let result =
                  MatchStepper.updateOne
                      (Unchecked.defaultof<MatchContext>)
                      SimulationClock.defaultClock
                      [||]
                      state

              Expect.equal result.State.SubTick 1 "one call must advance exactly one sub-tick"

              match result.State.Flow with
              | GoalPause g -> Expect.equal g.RemainingTicks 2 "timer should decrement by one"
              | other -> failtestf "expected GoalPause, got %A" other
          }

          test "command ordering is deterministic for same target tick" {
              let commands =
                  [| { CommandId = 2L
                       IssuedForSubTick = 10
                       Source = User
                       Command = ChangeTactics(1, TeamTactics.Attacking) }
                     { CommandId = 1L
                       IssuedForSubTick = 10
                       Source = User
                       Command = PauseSimulation } |]

              let ordered = MatchCommands.orderForTick 10 commands

              match ordered |> Array.map _.Command with
              | [| PauseSimulation; ChangeTactics _ |] -> ()
              | other -> failtestf "unexpected command order: %A" other
          }

          test "VAR review is represented directly in SimState" {
              let state = SimState()
              state.Flow <-
                  VARReview
                      { Incident = GoalCheck(HomeClub, None, false, 42)
                        Phase = CheckingIncident
                        RemainingTicks = 20
                        TotalTicks = 20 }

              match state.Flow with
              | VARReview review ->
                  Expect.equal review.RemainingTicks 20 "UI can read VAR timer from SimState"
              | other -> failtestf "expected VARReview, got %A" other
          } ]
