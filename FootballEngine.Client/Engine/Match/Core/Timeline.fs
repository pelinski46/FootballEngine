namespace FootballEngine

open SchedulingTypes

type TickBucket = {
    mutable Count: int
    mutable Capacity: int
    mutable Kinds: TickKind[]
    mutable Priorities: TickPriority[]
    mutable SequenceIds: int64[]
}

module TickBucket =
    let create initialCapacity = {
        Count = 0
        Capacity = initialCapacity
        Kinds = Array.zeroCreate initialCapacity
        Priorities = Array.zeroCreate initialCapacity
        SequenceIds = Array.zeroCreate initialCapacity
    }

    let private ensureCapacity (b: TickBucket) minCapacity =
        if b.Capacity < minCapacity then
            let newCap = max minCapacity (b.Capacity * 2)
            let newKinds = Array.zeroCreate newCap
            let newPriorities = Array.zeroCreate newCap
            let newSeqIds = Array.zeroCreate newCap
            Array.blit b.Kinds 0 newKinds 0 b.Count
            Array.blit b.Priorities 0 newPriorities 0 b.Count
            Array.blit b.SequenceIds 0 newSeqIds 0 b.Count
            b.Kinds <- newKinds
            b.Priorities <- newPriorities
            b.SequenceIds <- newSeqIds
            b.Capacity <- newCap

    let add (b: TickBucket) kind priority seqId =
        ensureCapacity b (b.Count + 1)
        b.Kinds[b.Count] <- kind
        b.Priorities[b.Count] <- priority
        b.SequenceIds[b.Count] <- seqId
        b.Count <- b.Count + 1

    let sort (b: TickBucket) =
        for i = 1 to b.Count - 1 do
            let k = b.Kinds[i]
            let p = b.Priorities[i]
            let s = b.SequenceIds[i]
            let mutable j = i - 1
            while j >= 0 && (b.Priorities[j] > p || (b.Priorities[j] = p && b.SequenceIds[j] > s)) do
                b.Kinds[j + 1] <- b.Kinds[j]
                b.Priorities[j + 1] <- b.Priorities[j]
                b.SequenceIds[j + 1] <- b.SequenceIds[j]
                j <- j - 1
            b.Kinds[j + 1] <- k
            b.Priorities[j + 1] <- p
            b.SequenceIds[j + 1] <- s

    let clear (b: TickBucket) = b.Count <- 0

type Timeline(maxSubTick: int) =
    let buckets = Array.init (maxSubTick + 1) (fun _ -> TickBucket.create 4)
    let cancelled = System.Collections.BitArray(10_000_000)
    let mutable cancelledCount = 0
    let mutable currentSubTick = 0

    member _.Insert(subTick: int, kind: TickKind, priority: TickPriority, seqId: int64) =
        if subTick >= 0 && subTick <= maxSubTick then
            TickBucket.add buckets[subTick] kind priority seqId

    member _.Dequeue() : (TickKind * TickPriority * int64) voption =
        if currentSubTick > maxSubTick then
            ValueNone
        else
            let bucket = buckets[currentSubTick]
            if bucket.Count = 0 then
                currentSubTick <- currentSubTick + 1
                ValueNone
            else
                TickBucket.sort bucket
                let mutable result: (TickKind * TickPriority * int64) voption = ValueNone
                let mutable found = false
                for i = 0 to bucket.Count - 1 do
                    if not found then
                        let seqId = bucket.SequenceIds[i]
                        let idx = int seqId
                        if idx >= 0 && idx < cancelled.Length && cancelled.Get(idx) then
                            cancelled.Set(idx, false)
                            cancelledCount <- cancelledCount - 1
                        else
                            result <- ValueSome(bucket.Kinds[i], bucket.Priorities[i], seqId)
                            found <- true
                if not found then
                    result <- ValueNone
                currentSubTick <- currentSubTick + 1
                result

    member _.CancelTicks(predicate: TickKind -> bool) =
        for subTick = currentSubTick to maxSubTick do
            let bucket = buckets[subTick]
            for i = 0 to bucket.Count - 1 do
                if predicate bucket.Kinds[i] then
                    let idx = int bucket.SequenceIds[i]
                    if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
                        cancelled.Set(idx, true)
                        cancelledCount <- cancelledCount + 1

    member _.CancelTick(sequenceId: int64) =
        let idx = int sequenceId
        if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
            cancelled.Set(idx, true)
            cancelledCount <- cancelledCount + 1

    member _.PurgeAfter(subTick: int) : unit =
        for st = subTick + 1 to maxSubTick do
            let bucket = buckets[st]
            for i = 0 to bucket.Count - 1 do
                let idx = int bucket.SequenceIds[i]
                if idx >= 0 && idx < cancelled.Length && not (cancelled.Get(idx)) then
                    cancelled.Set(idx, true)
                    cancelledCount <- cancelledCount + 1

    member _.Advance() =
        if currentSubTick <= maxSubTick then
            currentSubTick <- currentSubTick + 1

    member _.DequeueAllAtCurrent() : (TickKind * TickPriority * int64) list =
        if currentSubTick > maxSubTick then []
        else
            let bucket = buckets[currentSubTick]
            if bucket.Count = 0 then []
            else
                TickBucket.sort bucket
                let results = ResizeArray()
                for i = 0 to bucket.Count - 1 do
                    let seqId = bucket.SequenceIds[i]
                    let idx = int seqId
                    if idx >= 0 && idx < cancelled.Length && cancelled.Get(idx) then
                        cancelled.Set(idx, false)
                        cancelledCount <- cancelledCount - 1
                    else
                        results.Add(bucket.Kinds[i], bucket.Priorities[i], seqId)
                bucket.Count <- 0
                results |> Seq.toList

    member _.CurrentSubTick = currentSubTick
    member _.MaxSubTick = maxSubTick
    member _.IsEmpty: bool =
        let mutable empty = true
        let mutable st = currentSubTick
        while empty && st <= maxSubTick do
            if buckets[st].Count > 0 then empty <- false
            st <- st + 1
        empty && cancelledCount = 0
