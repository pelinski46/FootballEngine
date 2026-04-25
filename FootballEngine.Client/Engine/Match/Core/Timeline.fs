namespace FootballEngine

open SchedulingTypes

type Timeline(maxSubTick: int) =
    let mutable size = 0
    let mutable capacity = 64
    let mutable kinds: TickKind[] = Array.zeroCreate capacity
    let mutable priorities: TickPriority[] = Array.zeroCreate capacity
    let mutable sequenceIds: int64[] = Array.zeroCreate capacity
    let mutable currentSubTick = 0
    let cancelled = System.Collections.Generic.HashSet<int64>()

    let swap (i: int) (j: int) =
        let tk = kinds[i]
        let tp = priorities[i]
        let ts = sequenceIds[i]
        kinds[i] <- kinds[j]
        priorities[i] <- priorities[j]
        sequenceIds[i] <- sequenceIds[j]
        kinds[j] <- tk
        priorities[j] <- tp
        sequenceIds[j] <- ts

    let isLess (i: int) (j: int) =
        let pi = priorities[i]
        let pj = priorities[j]
        pi < pj || (pi = pj && sequenceIds[i] < sequenceIds[j])

    let bubbleUp (i: int) =
        let mutable idx = i
        let mutable running = true

        while idx > 0 && running do
            let parent = (idx - 1) >>> 1

            if isLess idx parent then
                swap idx parent
                idx <- parent
            else
                running <- false

    let bubbleDown (i: int) =
        let mutable idx = i
        let mutable running = true

        while running do
            let left = idx * 2 + 1
            let right = idx * 2 + 2
            let mutable smallest = idx

            if left < size && isLess left smallest then
                smallest <- left

            if right < size && isLess right smallest then
                smallest <- right

            if smallest <> idx then
                swap idx smallest
                idx <- smallest
            else
                running <- false

    let ensureCapacity () =
        if size >= capacity then
            let newCap = capacity * 2
            let newKinds = Array.zeroCreate newCap
            let newPriorities = Array.zeroCreate newCap
            let newSeqIds = Array.zeroCreate newCap
            Array.blit kinds 0 newKinds 0 size
            Array.blit priorities 0 newPriorities 0 size
            Array.blit sequenceIds 0 newSeqIds 0 size
            kinds <- newKinds
            priorities <- newPriorities
            sequenceIds <- newSeqIds
            capacity <- newCap

    let removeRoot () =
        size <- size - 1

        if size > 0 then
            kinds[0] <- kinds[size]
            priorities[0] <- priorities[size]
            sequenceIds[0] <- sequenceIds[size]
            bubbleDown 0

    member _.Insert(subTick: int, kind: TickKind, priority: TickPriority, seqId: int64) =
        if subTick >= 0 && subTick <= maxSubTick then
            ensureCapacity ()
            kinds[size] <- kind
            priorities[size] <- priority
            sequenceIds[size] <- seqId + (int64 subTick <<< 32)
            bubbleUp size
            size <- size + 1

    member _.Dequeue() : (TickKind * TickPriority * int64) voption =
        let mutable result = ValueNone
        let mutable found = false

        while size > 0 && not found do
            let seqId = sequenceIds[0]

            if cancelled.Remove(seqId) then
                removeRoot ()
            else
                result <- ValueSome(kinds[0], priorities[0], seqId)
                found <- true
                removeRoot ()

        result

    member _.CancelTick(sequenceId: int64) = cancelled.Add(sequenceId) |> ignore

    member _.CancelTicks(predicate: TickKind -> bool) =
        for i in 0 .. size - 1 do
            if predicate kinds[i] then
                cancelled.Add(sequenceIds[i]) |> ignore

    member _.PurgeAfter(subTick: int) =
        let threshold = int64 subTick <<< 32

        for i in 0 .. size - 1 do
            if sequenceIds[i] >= threshold then
                cancelled.Add(sequenceIds[i]) |> ignore

    member _.Advance() = currentSubTick <- currentSubTick + 1

    member this.DequeueAllInto(buffer: ResizeArray<TickKind * TickPriority * int64>) =
        buffer.Clear()
        let mutable running = true

        while running do
            match this.Dequeue() with
            | ValueSome tick -> buffer.Add(tick)
            | ValueNone -> running <- false

    member this.DequeueAll() : (TickKind * TickPriority * int64) list =
        let results = ResizeArray()
        let mutable running = true

        while running do
            match this.Dequeue() with
            | ValueSome tick -> results.Add(tick)
            | ValueNone -> running <- false

        results |> Seq.toList

    member _.CurrentSubTick = currentSubTick
    member _.MaxSubTick = maxSubTick
    member _.IsEmpty = size = 0
    member _.Count = size
