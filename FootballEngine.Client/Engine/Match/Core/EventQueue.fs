namespace FootballEngine

open FootballEngine.Domain
open SchedulingTypes

type EventKind =
    | SetPieceEvent of SetPieceKind * ClubSide * PlayerId option
    | InjuryEvent of PlayerId * int
    | SubstitutionEvent of ClubId
    | ManagerReaction of ReactionTrigger

type EventQueue(maxSize: int) =
    let subTicks = Array.zeroCreate<int> maxSize
    let kinds = Array.zeroCreate<EventKind> maxSize
    let seqIds = Array.zeroCreate<int64> maxSize
    let mutable count = 0

    member _.Insert(subTick: int, kind: EventKind, seqId: int64) =
        if count < maxSize then
            let mutable i = count - 1
            while i >= 0 && (subTicks[i] > subTick || (subTicks[i] = subTick && seqIds[i] > seqId)) do
                subTicks[i + 1] <- subTicks[i]
                kinds[i + 1] <- kinds[i]
                seqIds[i + 1] <- seqIds[i]
                i <- i - 1
            subTicks[i + 1] <- subTick
            kinds[i + 1] <- kind
            seqIds[i + 1] <- seqId
            count <- count + 1

    member _.Dequeue(currentSubTick: int) : (EventKind * int64) option =
        if count > 0 && subTicks[0] <= currentSubTick then
            let kind = kinds[0]
            let seqId = seqIds[0]
            for i = 0 to count - 2 do
                subTicks[i] <- subTicks[i + 1]
                kinds[i] <- kinds[i + 1]
                seqIds[i] <- seqIds[i + 1]
            count <- count - 1
            Some(kind, seqId)
        else
            None

    member _.IsEmpty = count = 0
    member _.Count = count
