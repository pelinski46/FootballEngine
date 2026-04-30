module FootballEngine.Tests.MatchEngineTests.MatchInvariantsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests

let matchInvariantsTests =
    testList
        "MatchInvariants"
        [ testCase "all invariants hold over 20 matches" <| fun () ->
              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2

              let mutable errorOpt: string option = None

              for seed in 1..20 do
                  if errorOpt.IsSome then ()
                  else
                      let result = MatchSimulator.trySimulateMatch clubs[0] clubs[1] game.Players game.Staff game.ProfileCache
                      match result with
                      | Ok(h, a, events, final) ->
                          Expect.isGreaterThanOrEqual h 0 $"match seed {seed}: HomeScore = {h}, expected >= 0"
                          Expect.isGreaterThanOrEqual a 0 $"match seed {seed}: AwayScore = {a}, expected >= 0"
                          Expect.isLessThanOrEqual h 10 $"match seed {seed}: HomeScore = {h}, expected <= 10"
                          Expect.isLessThanOrEqual a 10 $"match seed {seed}: AwayScore = {a}, expected <= 10"

                          let goalCount = events |> List.filter (fun e -> e.Type = MatchEventType.Goal) |> List.length
                          let ownGoalCount = events |> List.filter (fun e -> e.Type = MatchEventType.OwnGoal) |> List.length
                          let totalGoals = goalCount + ownGoalCount
                          let totalScore = h + a
                          Expect.equal totalGoals totalScore $"match seed {seed}: {goalCount} Goal + {ownGoalCount} OwnGoal = {totalGoals}, but score = {totalScore}"

                          for ev in events do
                              Expect.isGreaterThanOrEqual ev.SubTick 0 $"match seed {seed}: event SubTick {ev.SubTick} >= 0"
                              Expect.isLessThanOrEqual ev.SubTick 342000 $"match seed {seed}: event SubTick {ev.SubTick} <= 342000"

                          let sorted = events |> List.sortBy _.SubTick
                          Expect.equal (List.map _.SubTick events) (List.map _.SubTick sorted) $"match seed {seed}: events not in order"

                          let fkCount = events |> List.filter (fun e -> match e.Type with MatchEventType.FreeKick _ -> true | _ -> false) |> List.length
                          let foulCount = events |> List.filter (fun e -> e.Type = MatchEventType.FoulCommitted) |> List.length
                          Expect.isLessThanOrEqual fkCount foulCount $"match seed {seed}: {fkCount} FK <= {foulCount} fouls"

                          Expect.isLessThanOrEqual final.Home.SubsUsed 3 $"match seed {seed}: Home subs {final.Home.SubsUsed} <= 3"
                          Expect.isLessThanOrEqual final.Away.SubsUsed 3 $"match seed {seed}: Away subs {final.Away.SubsUsed} <= 3"

                          let redCards = events |> List.filter (fun e -> e.Type = MatchEventType.RedCard)
                          let byPlayer = redCards |> List.groupBy _.PlayerId
                          for pid, cards in byPlayer do
                              Expect.isLessThanOrEqual (List.length cards) 1 $"match seed {seed}: player {pid} got {List.length cards} reds"

                          Expect.isGreaterThanOrEqual final.Momentum -10.0 $"match seed {seed}: momentum {final.Momentum} >= -10"
                          Expect.isLessThanOrEqual final.Momentum 10.0 $"match seed {seed}: momentum {final.Momentum} <= 10"

                          for i = 0 to final.Home.Frame.SlotCount - 1 do
                              match final.Home.Frame.Occupancy[i] with
                              | OccupancyKind.Active _ ->
                                  let cond = int final.Home.Frame.Condition[i]
                                  Expect.isGreaterThanOrEqual cond 0 $"match seed {seed}: Home cond {cond} >= 0"
                                  Expect.isLessThanOrEqual cond 100 $"match seed {seed}: Home cond {cond} <= 100"
                              | _ -> ()
                          for i = 0 to final.Away.Frame.SlotCount - 1 do
                              match final.Away.Frame.Occupancy[i] with
                              | OccupancyKind.Active _ ->
                                  let cond = int final.Away.Frame.Condition[i]
                                  Expect.isGreaterThanOrEqual cond 0 $"match seed {seed}: Away cond {cond} >= 0"
                                  Expect.isLessThanOrEqual cond 100 $"match seed {seed}: Away cond {cond} <= 100"
                              | _ -> ()
                      | Error e -> errorOpt <- Some $"match seed {seed}: simulation error: %A{e}"

              match errorOpt with
              | Some msg -> failtest msg
              | None -> () ]
