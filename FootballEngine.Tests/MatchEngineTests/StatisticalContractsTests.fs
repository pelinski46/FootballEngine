module FootballEngine.Tests.MatchEngineTests.StatisticalContractsTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.Tests

// ── helpers ────────────────────────────────────────────────────────────────

let private countWhere f xs = xs |> Seq.filter f |> Seq.length

let private pct num den =
    if den = 0 then
        "N/A"
    else
        sprintf "%.1f%%" (float num / float den * 100.0)

let private avg total n =
    if n = 0 then 0.0 else float total / float n

let private divLine () = printfn "%s" (System.String('-', 60))

// ── main test ──────────────────────────────────────────────────────────────

let statisticalContractsTests =
    testList
        "StatisticalContracts"
        [ testCase "all statistical contracts hold over 20 matches"
          <| fun () ->

              let game = Helpers.loadGame ()
              let clubs = game.Clubs |> Map.toArray |> Array.map snd |> Array.take 2

              // Full replay so we get ALL events, not just key events
              let replays =
                  Array.Parallel.init 20 (fun i ->
                      let hi = i % clubs.Length
                      let ai = (hi + 1) % clubs.Length
                      MatchSimulator.trySimulateMatchFull clubs[hi] clubs[ai] game.Players game.Staff game.ProfileCache)

              let okReplays =
                  replays
                  |> Array.choose (function
                      | Ok r -> Some r
                      | _ -> None)

              let nMatches = okReplays.Length

              // ── accumulators ──────────────────────────────────────────────

              let mutable totalGoals = 0
              let mutable totalOwnGoals = 0
              let mutable homeWins = 0
              let mutable awayWins = 0
              let mutable draws = 0

              // Shot pipeline
              let mutable nShotLaunched = 0
              let mutable nShotOnTarget = 0
              let mutable nShotOffTarget = 0
              let mutable nShotBlocked = 0
              let mutable nSave = 0
              let mutable nSaveCaught = 0
              let mutable nSaveParried = 0

              // Goal path tracing
              let mutable nGoalWithNoShot = 0
              let mutable nGoalFromLaunched = 0

              // Passing
              let mutable nPassCompleted = 0
              let mutable nPassMisplaced = 0
              let mutable nPassIntercepted = 0
              let mutable nPassDeflected = 0
              let mutable nPassLaunched = 0

              // Duels
              let mutable nDribbleSuccess = 0
              let mutable nDribbleFail = 0
              let mutable nTackleSuccess = 0
              let mutable nTackleFail = 0

              // Set pieces
              let mutable nCorner = 0
              let mutable nFreeKick = 0
              let mutable nPenaltyScored = 0
              let mutable nPenaltyMissed = 0
              let mutable nCrossAttempt = 0
              let mutable nCrossSuccess = 0
              let mutable nLongBall = 0

              // Discipline
              let mutable nFoul = 0
              let mutable nYellow = 0
              let mutable nRed = 0
              let mutable nInjury = 0

              // GK
              let mutable nGKDistribution = 0

              let perMatchGoals = System.Collections.Generic.List<int>()
              let perMatchShots = System.Collections.Generic.List<int>()

              // ── accumulate ────────────────────────────────────────────────

              for replay in okReplays do
                  let h = replay.Final.HomeScore
                  let a = replay.Final.AwayScore
                  let events = replay.Events

                  if h > a then homeWins <- homeWins + 1
                  elif a > h then awayWins <- awayWins + 1
                  else draws <- draws + 1

                  totalGoals <- totalGoals + h + a
                  perMatchGoals.Add(h + a)

                  let matchShotLaunched = countWhere (fun e -> e.Type = ShotLaunched) events
                  let matchGoals = countWhere (fun e -> e.Type = Goal || e.Type = OwnGoal) events

                  if matchShotLaunched = 0 && matchGoals > 0 then
                      nGoalWithNoShot <- nGoalWithNoShot + matchGoals
                  elif matchShotLaunched > 0 then
                      nGoalFromLaunched <- nGoalFromLaunched + matchGoals

                  let matchShots =
                      countWhere
                          (fun e ->
                              match e.Type with
                              | ShotLaunched
                              | ShotOnTarget
                              | ShotOffTarget
                              | ShotBlocked -> true
                              | _ -> false)
                          events

                  perMatchShots.Add(matchShots)

                  for e in events do
                      match e.Type with
                      | OwnGoal -> totalOwnGoals <- totalOwnGoals + 1
                      | ShotLaunched -> nShotLaunched <- nShotLaunched + 1
                      | ShotOnTarget -> nShotOnTarget <- nShotOnTarget + 1
                      | ShotOffTarget -> nShotOffTarget <- nShotOffTarget + 1
                      | ShotBlocked -> nShotBlocked <- nShotBlocked + 1
                      | Save -> nSave <- nSave + 1
                      | SaveCaught _ -> nSaveCaught <- nSaveCaught + 1
                      | SaveParried _ -> nSaveParried <- nSaveParried + 1
                      | PassCompleted _ -> nPassCompleted <- nPassCompleted + 1
                      | PassLaunched _ -> nPassLaunched <- nPassLaunched + 1
                      | PassMisplaced _ -> nPassMisplaced <- nPassMisplaced + 1
                      | PassIntercepted _ -> nPassIntercepted <- nPassIntercepted + 1
                      | PassDeflected _ -> nPassDeflected <- nPassDeflected + 1
                      | DribbleSuccess -> nDribbleSuccess <- nDribbleSuccess + 1
                      | DribbleFail -> nDribbleFail <- nDribbleFail + 1
                      | TackleSuccess -> nTackleSuccess <- nTackleSuccess + 1
                      | TackleFail -> nTackleFail <- nTackleFail + 1
                      | Corner -> nCorner <- nCorner + 1
                      | FreeKick _ -> nFreeKick <- nFreeKick + 1
                      | PenaltyAwarded s ->
                          if s then
                              nPenaltyScored <- nPenaltyScored + 1
                          else
                              nPenaltyMissed <- nPenaltyMissed + 1
                      | CrossAttempt s ->
                          nCrossAttempt <- nCrossAttempt + 1

                          if s then
                              nCrossSuccess <- nCrossSuccess + 1
                      | LongBall _ -> nLongBall <- nLongBall + 1
                      | FoulCommitted -> nFoul <- nFoul + 1
                      | YellowCard -> nYellow <- nYellow + 1
                      | RedCard -> nRed <- nRed + 1
                      | Injury _ -> nInjury <- nInjury + 1
                      | GKDistribution _ -> nGKDistribution <- nGKDistribution + 1
                      | _ -> ()

              // ── diagnostic output ─────────────────────────────────────────

              divLine ()
              printfn "STATISTICAL CONTRACTS — %d matches" nMatches
              divLine ()

              printfn ""
              printfn "[ RESULTS ]"
              printfn "  Home wins: %d  Away wins: %d  Draws: %d" homeWins awayWins draws
              printfn "  Total goals: %d  Own goals: %d" totalGoals totalOwnGoals
              printfn "  Avg goals/match: %.2f" (avg totalGoals nMatches)
              printfn "  Goals range: %d - %d per match" (Seq.min perMatchGoals) (Seq.max perMatchGoals)

              printfn ""
              printfn "[ SHOT PIPELINE ]  (should flow: Launched -> OnTarget/OffTarget -> Save/Goal)"
              printfn "  ShotLaunched  : %d  (%.1f/match)" nShotLaunched (avg nShotLaunched nMatches)
              printfn "  ShotOnTarget  : %d  (%.1f/match)" nShotOnTarget (avg nShotOnTarget nMatches)
              printfn "  ShotOffTarget : %d  (%.1f/match)" nShotOffTarget (avg nShotOffTarget nMatches)
              printfn "  ShotBlocked   : %d  (%.1f/match)" nShotBlocked (avg nShotBlocked nMatches)
              printfn "  Save          : %d  (%.1f/match)" nSave (avg nSave nMatches)
              printfn "  SaveCaught    : %d" nSaveCaught
              printfn "  SaveParried   : %d" nSaveParried
              printfn "  Goals         : %d  (%.1f/match)" totalGoals (avg totalGoals nMatches)
              printfn ""
              let totalShotEvents = nShotLaunched + nShotOnTarget + nShotOffTarget + nShotBlocked
              printfn "  Shot->Goal conversion    : %s  (expected ~5-15%%)" (pct totalGoals totalShotEvents)
              printfn "  OnTarget->Goal conversion: %s  (expected ~20-40%%)" (pct totalGoals nShotOnTarget)
              printfn "  Goals with NO ShotLaunched in match: %d  <- BUG if > 0" nGoalWithNoShot
              printfn "  Goals with ShotLaunched present    : %d" nGoalFromLaunched

              printfn ""
              printfn "[ PASSING ]"
              let nPassAll = nPassCompleted + nPassMisplaced + nPassIntercepted + nPassDeflected
              printfn "  PassLaunched  : %d  (%.1f/match)" nPassLaunched (avg nPassLaunched nMatches)
              printfn "  PassCompleted : %d  (%.1f/match)" nPassCompleted (avg nPassCompleted nMatches)
              printfn "  PassMisplaced : %d" nPassMisplaced
              printfn "  PassIntercept : %d" nPassIntercepted
              printfn "  PassDeflected : %d" nPassDeflected
              printfn "  Completion rate: %s  (expected ~65-85%%)" (pct nPassCompleted nPassAll)

              printfn ""
              printfn "[ DUELS ]"

              printfn
                  "  DribbleSuccess: %d  DribbleFail: %d  rate: %s"
                  nDribbleSuccess
                  nDribbleFail
                  (pct nDribbleSuccess (nDribbleSuccess + nDribbleFail))

              printfn
                  "  TackleSuccess : %d  TackleFail : %d  rate: %s"
                  nTackleSuccess
                  nTackleFail
                  (pct nTackleSuccess (nTackleSuccess + nTackleFail))

              printfn ""
              printfn "[ SET PIECES ]"
              printfn "  Corners   : %d  (%.1f/match)" nCorner (avg nCorner nMatches)
              printfn "  FreeKicks : %d  (%.1f/match)" nFreeKick (avg nFreeKick nMatches)
              printfn "  Penalties : scored=%d  missed=%d" nPenaltyScored nPenaltyMissed

              printfn
                  "  Cross att : %d  success=%d  rate: %s"
                  nCrossAttempt
                  nCrossSuccess
                  (pct nCrossSuccess nCrossAttempt)

              printfn "  LongBalls : %d  (%.1f/match)" nLongBall (avg nLongBall nMatches)

              printfn ""
              printfn "[ DISCIPLINE ]"
              printfn "  Fouls  : %d  (%.1f/match)" nFoul (avg nFoul nMatches)
              printfn "  Yellow : %d  Red: %d  Injury: %d" nYellow nRed nInjury

              printfn ""
              printfn "[ GK ]"
              printfn "  GKDistribution: %d  (%.1f/match)" nGKDistribution (avg nGKDistribution nMatches)

              divLine ()

              // ── contracts ─────────────────────────────────────────────────

              let avgGoals = avg totalGoals nMatches
              Expect.isGreaterThanOrEqual avgGoals 1.5 $"avg goals/match = {avgGoals:F2}, expected >= 1.5"
              Expect.isLessThanOrEqual avgGoals 10.0 $"avg goals/match = {avgGoals:F2}, expected <= 10.0"

              Expect.equal
                  nGoalWithNoShot
                  0
                  $"goals in matches with zero ShotLaunched = {nGoalWithNoShot}. Goals bypassing ShotAction."

              let avgShotEvents = avg totalShotEvents nMatches

              Expect.isGreaterThanOrEqual
                  avgShotEvents
                  5.0
                  $"avg shot-pipeline events/match = {avgShotEvents:F1}, expected >= 5"

              if totalShotEvents > 0 then
                  let conv = float totalGoals / float totalShotEvents * 100.0

                  Expect.isLessThanOrEqual
                      conv
                      25.0
                      $"shot conversion = {conv:F1}%%, expected <= 25%%. Goals bypassing GK."

              let avgPasses = avg nPassCompleted nMatches
              Expect.isGreaterThanOrEqual avgPasses 20.0 $"avg completed passes/match = {avgPasses:F1}, expected >= 20"

              if nPassAll > 0 then
                  let passRate = float nPassCompleted / float nPassAll * 100.0
                  Expect.isGreaterThanOrEqual passRate 55.0 $"pass completion = {passRate:F1}%%, expected >= 55%%"
                  Expect.isLessThanOrEqual passRate 95.0 $"pass completion = {passRate:F1}%%, expected <= 95%%"

              Expect.isGreaterThanOrEqual
                  (avg nCorner nMatches)
                  2.0
                  $"avg corners/match = {avg nCorner nMatches:F2}, expected >= 2"

              Expect.isGreaterThanOrEqual
                  (avg nFoul nMatches)
                  5.0
                  $"avg fouls/match = {avg nFoul nMatches:F2}, expected >= 5"

              Expect.isGreaterThan
                  homeWins
                  awayWins
                  $"home wins = {homeWins}, away wins = {awayWins}: expected home advantage" ]
