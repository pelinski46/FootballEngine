namespace FootballEngine

open System.Collections.Generic
open FootballEngine.SchedulingTypes

type TickScheduler(maxSubTick: int) =
    let pq = PriorityQueue<ScheduledTick, ScheduledTick>()

    member _.Insert(tick: ScheduledTick) =
        if tick.SubTick <= maxSubTick then
            pq.Enqueue(tick, tick)

    member _.Dequeue() : ScheduledTick voption =
        if pq.Count > 0 then ValueSome(pq.Dequeue()) else ValueNone

    member _.CancelTicks(predicate: TickKind -> bool) =
        let items =
            pq.UnorderedItems
            |> Seq.map (fun struct (element, _) -> element)
            |> Seq.filter (fun t -> not (predicate t.Kind))
            |> Seq.toArray

        pq.Clear()

        for item in items do
            pq.Enqueue(item, item)

    member _.PurgeAfter(subTick: int) : unit =
        let items =
            pq.UnorderedItems
            |> Seq.map (fun struct (element, _) -> element)
            |> Seq.filter (fun t -> t.SubTick <= subTick)
            |> Seq.toArray

        pq.Clear()

        for item in items do
            pq.Enqueue(item, item)

    member _.IsEmpty: bool = pq.Count = 0
    member _.Count: int = pq.Count
