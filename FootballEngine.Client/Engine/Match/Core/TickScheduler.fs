namespace FootballEngine

open System.Collections.Generic
open FootballEngine.SchedulingTypes

type TickScheduler(maxSubTick: int) =
    let pq = PriorityQueue<ScheduledTick, ScheduledTick>()
    let cancelled = HashSet<int64>()

    member _.Insert(tick: ScheduledTick) =
        if tick.SubTick <= maxSubTick then
            pq.Enqueue(tick, tick)

    member _.Dequeue() : ScheduledTick voption =
        let mutable result = ValueNone
        let mutable found = false
        
        while not found && pq.Count > 0 do
            let tick = pq.Dequeue()
            if not (cancelled.Remove(tick.SequenceId)) then
                result <- ValueSome tick
                found <- true
        
        if pq.Count = 0 && cancelled.Count > 0 then
            cancelled.Clear()
            
        result

    member _.CancelTicks(predicate: TickKind -> bool) =
        // Mark for lazy removal
        for struct (tick, _) in pq.UnorderedItems do
            if predicate tick.Kind then
                cancelled.Add(tick.SequenceId) |> ignore

    member _.CancelTick(sequenceId: int64) =
        cancelled.Add(sequenceId) |> ignore

    member _.PurgeAfter(subTick: int) : unit =
        for struct (tick, _) in pq.UnorderedItems do
            if tick.SubTick > subTick then
                cancelled.Add(tick.SequenceId) |> ignore

    member _.IsEmpty: bool = pq.Count = 0
    member _.Count: int = pq.Count
