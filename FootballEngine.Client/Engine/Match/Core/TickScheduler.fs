namespace FootballEngine

open System.Collections.Generic
open FootballEngine.SchedulingTypes

type TickScheduler(maxSecond: int) =
    let buckets = Array.init (maxSecond + 1) (fun _ -> List<ScheduledTick>())
    let mutable count = 0
    let mutable minSecond = maxSecond

    member _.Insert(tick: ScheduledTick) : unit =
        let idx = tick.Second

        if idx >= 0 && idx <= maxSecond then
            buckets[idx].Add(tick)
            count <- count + 1

            if idx < minSecond then
                minSecond <- idx

    member _.Dequeue() : ScheduledTick voption =
        let rec search i =
            if i > maxSecond then
                ValueNone
            elif buckets[i].Count > 0 then
                let bucket = buckets[i]
                let mutable bestIdx = 0
                let mutable best = bucket[0]

                for j = 1 to bucket.Count - 1 do
                    let candidate = bucket[j]

                    if
                        compare candidate.Priority best.Priority < 0
                        || (candidate.Priority = best.Priority && candidate.SequenceId < best.SequenceId)
                    then
                        best <- candidate
                        bestIdx <- j

                bucket.RemoveAt(bestIdx)
                count <- count - 1

                if bucket.Count = 0 && i = minSecond then
                    let rec findNext n =
                        if n > maxSecond then n
                        elif buckets[n].Count > 0 then n
                        else findNext (n + 1)

                    minSecond <- findNext (i + 1)

                ValueSome best
            else
                search (i + 1)

        search minSecond

    member _.CancelTicks(predicate: TickKind -> bool) : unit =
        for i = 0 to maxSecond do
            let bucket = buckets[i]

            if bucket.Count > 0 then
                let removed = bucket.RemoveAll(fun t -> predicate t.Kind)
                count <- count - removed

        if count = 0 then
            minSecond <- maxSecond

    member _.PurgeAfterSecond(second: int) : unit =
        let start = min (second + 1) maxSecond

        for i = start to maxSecond do
            count <- count - buckets[i].Count
            buckets[i].Clear()

        if count = 0 then
            minSecond <- maxSecond

    member _.IsEmpty: bool = count = 0
    member _.Count: int = count
