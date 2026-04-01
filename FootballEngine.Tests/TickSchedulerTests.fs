module FootballEngine.Tests.TickSchedulerTests

open Expecto
open FootballEngine
open FootballEngine.Domain
open FootballEngine.SchedulingTypes

let private mkTick second priority seqId kind =
    { Second = second; Priority = priority; SequenceId = seqId; Kind = kind }

let tickSchedulerTests =
    testList
        "TickScheduler"
        [ test "Insert increases Count" {
              let s = TickScheduler(100)
              Expect.isTrue s.IsEmpty "should be empty"
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              Expect.equal s.Count 1 "count should be 1"
          }

          test "Dequeue returns ticks in Second order" {
              let s = TickScheduler(100)
              s.Insert(mkTick 30 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 10 TickPriority.Duel 1L (DuelTick 0))
              s.Insert(mkTick 20 TickPriority.Duel 2L (DuelTick 0))
              let t1 = s.Dequeue()
              let t2 = s.Dequeue()
              let t3 = s.Dequeue()
              match t1, t2, t3 with
              | ValueSome a, ValueSome b, ValueSome c ->
                  Expect.equal a.Second 10 ""
                  Expect.equal b.Second 20 ""
                  Expect.equal c.Second 30 ""
              | _ -> failwith "expected three ticks"
          }

          test "Dequeue respects Priority within same Second" {
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
              s.CancelTicks(function | DuelTick _ -> true | _ -> false)
              Expect.equal s.Count 1 ""
              match s.Dequeue() with
              | ValueSome tick -> Expect.equal tick.Kind PhysicsTick ""
              | ValueNone -> failwith "expected PhysicsTick"
          }

          test "PurgeAfterSecond removes future ticks" {
              let s = TickScheduler(100)
              s.Insert(mkTick 10 TickPriority.Duel 0L (DuelTick 0))
              s.Insert(mkTick 50 TickPriority.Duel 1L (DuelTick 0))
              s.Insert(mkTick 90 TickPriority.Duel 2L (DuelTick 0))
              s.PurgeAfterSecond 50
              Expect.equal s.Count 2 ""
          }

          test "Large volume stress" {
              let s = TickScheduler(5700)
              for i = 0 to 5700 do
                  s.Insert(mkTick i TickPriority.Duel (int64 i) (DuelTick 0))
              Expect.equal s.Count 5701 ""
              let mutable prev = -1
              let mutable dequeued = 0
              let mutable finished = false
              while not finished do
                  match s.Dequeue() with
                  | ValueSome t ->
                      Expect.isTrue (t.Second >= prev) ""
                      prev <- t.Second
                      dequeued <- dequeued + 1
                  | ValueNone -> finished <- true
              Expect.equal dequeued 5701 ""
              Expect.isTrue s.IsEmpty ""
          } ]
