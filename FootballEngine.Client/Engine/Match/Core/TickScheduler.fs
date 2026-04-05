namespace FootballEngine

open System.Collections.Generic
open FootballEngine.SchedulingTypes

/// Priority queue backed by a bucket array indexed by SubTick.
/// O(1) insert, O(k) dequeue where k = ticks in the current SubTick bucket.
type TickScheduler(maxSubTick: int) =

    let buckets   = Array.init (maxSubTick + 1) (fun _ -> List<ScheduledTick>())
    let mutable count      = 0
    let mutable minSubTick = maxSubTick

    member _.Insert(tick: ScheduledTick) : unit =
        let idx = tick.SubTick
        if idx >= 0 && idx <= maxSubTick then
            buckets[idx].Add(tick)
            count <- count + 1
            if idx < minSubTick then
                minSubTick <- idx

    member _.Dequeue() : ScheduledTick voption =
        let rec search i =
            if i > maxSubTick then
                ValueNone
            elif buckets[i].Count > 0 then
                let bucket = buckets[i]
                let mutable bestIdx = 0
                let mutable best    = bucket[0]

                for j = 1 to bucket.Count - 1 do
                    let candidate = bucket[j]
                    if  compare candidate.Priority best.Priority < 0
                     || (candidate.Priority = best.Priority && candidate.SequenceId < best.SequenceId)
                    then
                        best    <- candidate
                        bestIdx <- j

                bucket.RemoveAt(bestIdx)
                count <- count - 1

                if bucket.Count = 0 && i = minSubTick then
                    let rec findNext n =
                        if n > maxSubTick       then n
                        elif buckets[n].Count > 0 then n
                        else findNext (n + 1)
                    minSubTick <- findNext (i + 1)

                ValueSome best
            else
                search (i + 1)

        search minSubTick

    /// Remove all ticks matching the predicate across all buckets.
    member _.CancelTicks(predicate: TickKind -> bool) : unit =
        for i = 0 to maxSubTick do
            let bucket = buckets[i]
            if bucket.Count > 0 then
                let removed = bucket.RemoveAll(fun t -> predicate t.Kind)
                count <- count - removed
        if count = 0 then
            minSubTick <- maxSubTick

    /// Discard all ticks scheduled strictly after the given SubTick.
    member _.PurgeAfter(subTick: int) : unit =
        let start = min (subTick + 1) maxSubTick
        for i = start to maxSubTick do
            count <- count - buckets[i].Count
            buckets[i].Clear()
        if count = 0 then
            minSubTick <- maxSubTick

    member _.IsEmpty : bool = count = 0
    member _.Count   : int  = count
