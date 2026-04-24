namespace FootballEngine

open FootballEngine.SchedulingTypes

// FASE 9: BitArray reemplaza HashSet<int64> para cancelled — zero allocations durante el partido.
// 10M bits = 1.25 MB, suficiente para ~10M ticks (un partido real usa < 500K).
type TickScheduler(maxSubTick: int) =
    let pq = System.Collections.Generic.PriorityQueue<ScheduledTick, ScheduledTick>()
    let cancelled = System.Collections.BitArray(10_000_000)
    let mutable cancelledCount = 0

    member _.Insert(tick: ScheduledTick) =
        if tick.SubTick <= maxSubTick then
            pq.Enqueue(tick, tick)

    member _.Dequeue() : ScheduledTick voption =
        let mutable result = ValueNone
        let mutable found = false

        while not found && pq.Count > 0 do
            let tick = pq.Dequeue()
            let idx = int tick.SequenceId
            if idx >= 0 && idx < cancelled.Length && cancelled.Get(idx) then
                cancelled.Set(idx, false)
                cancelledCount <- cancelledCount - 1
            else
                result <- ValueSome tick
                found <- true

        result

    member _.CancelTicks(predicate: TickKind -> bool) =
        for struct (tick, _) in pq.UnorderedItems do
            if predicate tick.Kind then
                let idx = int tick.SequenceId
                if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
                    cancelled.Set(idx, true)
                    cancelledCount <- cancelledCount + 1

    member _.CancelTick(sequenceId: int64) =
        let idx = int sequenceId
        if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
            cancelled.Set(idx, true)
            cancelledCount <- cancelledCount + 1

    member _.PurgeAfter(subTick: int) : unit =
        for struct (tick, _) in pq.UnorderedItems do
            if tick.SubTick > subTick then
                let idx = int tick.SequenceId
                if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
                    cancelled.Set(idx, true)
                    cancelledCount <- cancelledCount + 1

    member _.IsEmpty: bool = pq.Count = cancelledCount
    member _.Count: int = pq.Count - cancelledCount
