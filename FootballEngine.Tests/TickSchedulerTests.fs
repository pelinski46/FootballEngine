module FootballEngine.Tests.TickSchedulerTests

open Expecto
open FootballEngine
open FootballEngine.SchedulingTypes

let private mkTick subTick priority seqId kind =
    { SubTick = subTick
      Priority = priority
      SequenceId = seqId
      Kind = kind }

let tickSchedulerTests =
    testList
        "TickScheduler"
        [ test "Insert increases Count" {
              let s = TickScheduler(100)
              Expect.isTrue s.IsEmpty "should be empty"
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              Expect.equal s.Count 1 "count should be 1"
          }

          test "Dequeue returns ticks in SubTick order" {
              let s = TickScheduler(100)
              s.Insert(mkTick 30 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 10 TickPriority.Duel 1L (DuelTick 0))
              s.Insert(mkTick 20 TickPriority.Duel 2L (DuelTick 0))
              let t1 = s.Dequeue()
              let t2 = s.Dequeue()
              let t3 = s.Dequeue()

              match t1, t2, t3 with
              | ValueSome a, ValueSome b, ValueSome c ->
                  Expect.equal a.SubTick 10 ""
                  Expect.equal b.SubTick 20 ""
                  Expect.equal c.SubTick 30 ""
              | _ -> failwith "expected three ticks"
          }

          test "Dequeue respects Priority within same SubTick" {
              let s = TickScheduler(100)
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 10 TickPriority.Physics 1L PhysicsTick)
              s.Insert(mkTick 10 TickPriority.Referee 2L PhysicsTick)
              let t1 = s.Dequeue()
              let t2 = s.Dequeue()
              let t3 = s.Dequeue()

              match t1, t2, t3 with
              | ValueSome a, ValueSome b, ValueSome c ->
                  Expect.equal a.Priority TickPriority.Physics ""
                  Expect.equal b.Priority TickPriority.Referee ""
                  Expect.equal c.Priority TickPriority.Duel ""
              | _ -> failwith "expected three ticks"
          }

          test "Dequeue respects SequenceId as tiebreaker" {
              let s = TickScheduler(100)
              s.Insert(mkTick 10 TickPriority.Duel 5L (DuelTick 0))
              s.Insert(mkTick 10 TickPriority.Duel 1L (DuelTick 0))
              s.Insert(mkTick 10 TickPriority.Duel 3L (DuelTick 0))
              let t1 = s.Dequeue()
              let t2 = s.Dequeue()
              let t3 = s.Dequeue()

              match t1, t2, t3 with
              | ValueSome a, ValueSome b, ValueSome c ->
                  Expect.equal a.SequenceId 1L ""
                  Expect.equal b.SequenceId 3L ""
                  Expect.equal c.SequenceId 5L ""
              | _ -> failwith "expected three ticks"
          }

          test "Dequeue from empty returns ValueNone" {
              let s = TickScheduler(100)
              Expect.equal (s.Dequeue()) ValueNone ""
          }

          test "CancelTicks removes matching" {
              let s = TickScheduler(100)
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 20 TickPriority.Physics 1L PhysicsTick)
              s.Insert(mkTick 30 TickPriority.Duel 2L (DuelTick 0))

              s.CancelTicks (function
                  | DuelTick _ -> true
                  | _ -> false)

              Expect.equal s.Count 1 ""

              match s.Dequeue() with
              | ValueSome tick -> Expect.equal tick.Kind PhysicsTick ""
              | ValueNone -> failwith "expected PhysicsTick"
          }

          test "PurgeAfter removes future ticks" {
              let s = TickScheduler(100)
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 50 TickPriority.Duel 1L (DuelTick 0))
              s.Insert(mkTick 90 TickPriority.Duel 2L (DuelTick 0))
              s.PurgeAfter 50
              Expect.equal s.Count 2 ""
          }

          test "Large volume stress — realistic match load" {
              let s = TickScheduler(342000)

              for i = 0 to 199 do
                  s.Insert(mkTick i TickPriority.Physics (int64 i) PhysicsTick)

              for i = 0 to 2 do
                  let t = 960 * (i + 1)
                  s.Insert(mkTick t TickPriority.Duel (int64 (200 + i)) (DuelTick 0))

              for i = 0 to 2 do
                  let t = 80 * (i + 1)
                  s.Insert(mkTick t TickPriority.Manager (int64 (300 + i)) RefereeTick)

              s.Insert(mkTick 2400 TickPriority.Manager 400L (SubstitutionTick 1))
              s.Insert(mkTick 2400 TickPriority.Manager 401L (SubstitutionTick 2))
              s.Insert(mkTick 108000 TickPriority.MatchControl 500L HalfTimeTick)
              s.Insert(mkTick 228000 TickPriority.MatchControl 501L FullTimeTick)

              let totalExpected = 200 + 3 + 3 + 2 + 1 + 1
              Expect.equal s.Count totalExpected "should have all ticks"

              let mutable prevSubTick = -1
              let mutable dequeued = 0
              let mutable finished = false

              while not finished do
                  match s.Dequeue() with
                  | ValueSome t ->
                      // SubTick order must be non-decreasing
                      Expect.isTrue (t.SubTick >= prevSubTick) "ticks must be in order"
                      // Within same SubTick, priority must be non-decreasing
                      prevSubTick <- t.SubTick
                      dequeued <- dequeued + 1
                  | ValueNone -> finished <- true

              Expect.equal dequeued totalExpected "all ticks should be dequeued"
              Expect.isTrue s.IsEmpty "scheduler should be empty after full drain"
          } ]
