module [PriorityQueue, make, makeQ, sizeOf, pop, push]

# PriorityQueue a := {
#     size: U64,
#     comparator: Comparator a,
#     store: List a
# }
# make : Comparator a, U64 -> PriorityQueue a
Comparator a : a, a -> [LT, EQ, GT]
PriorityQueue a := {
    comparator : Comparator a,
    store : List a,
}
make = makeQ
makeQ = \comparator -> @PriorityQueue {
        comparator,
        store: [],
    }

sizeOf = \@PriorityQueue { store } -> List.len store
push = \@PriorityQueue { store, comparator }, a ->
    newStore =
        List.append store a
        |> swim (List.len store) a comparator
    @PriorityQueue {
        comparator,
        store: newStore,
    }

getStore = \@PriorityQueue { store } -> store

pop = \@PriorityQueue { store, comparator } ->
    List.first store
    |> Result.map_ok \value -> (
            value,
            @PriorityQueue {
                comparator,
                store: List.drop_first store 1 |> sink 0 comparator,
            },
        )
    |> Result.map_err \_ -> EmptyQueue
# |> Result.with_default (Err EmptyQueue, @PriorityQueue { store, comparator, size })

sink = \store, index, comparator ->
    (leftIdx, rightIdx) = (index * 2 + 1, index * 2 + 2)
    left =
        List.get store leftIdx
        |> Result.map_ok Value
        |> Result.with_default LeftMissing
    right =
        List.get store rightIdx
        |> Result.map_ok Value
        |> Result.with_default RightMissing

    when (left, right) is
        (LeftMissing, RightMissing) ->
            store

        (_, _) ->
            (newIndex, newValue) =
                when (left, right) is
                    (LeftMissing, Value rightValue) -> (rightIdx, rightValue)
                    (Value leftValue, RightMissing) -> (leftIdx, leftValue)
                    (Value leftValue, Value rightValue) if comparator leftValue rightValue == GT -> (rightIdx, rightValue)
                    (Value leftValue, Value _) -> (leftIdx, leftValue)
                    (_, _) -> crash "should have bailed earlier"
            List.get store index
            |> Result.map_ok \oldValue ->
                if comparator oldValue newValue == GT then
                    List.swap store index newIndex
                    |> sink newIndex comparator
                else
                    store
            |> Result.with_default store

swim = \store, index, currentValue, comparator ->
    if index == 0 then
        store
    else
        parentIndex = (index - 1) |> Num.to_frac |> Num.div 2 |> Num.floor
        List.get store parentIndex
        |> Result.map_ok \parent ->
            if comparator currentValue parent == LT then
                List.swap store index parentIndex
                |> swim parentIndex currentValue comparator
            else
                store
        |> Result.with_default store

expect
    queue =
        makeQ Num.compare
        |> push 15
        |> push 10
    actual = getStore queue
    actual == [10, 15]

expect
    queue =
        makeQ Num.compare
        |> push 65
        |> push 30
        |> push 10
        |> push 20
        |> push 90

    actual = getStore queue |> List.first
    actual == Ok 10

expect
    queue =
        makeQ Num.compare
        |> push 65
        |> push 30
        |> push 10
        |> push 20
        |> push 90

    values =
        List.range { start: At 0, end: At 5 }
        |> List.walk_until (queue, []) \(aq, accum), _ ->
            pop aq
            |> Result.map_ok \(v, q) -> (q, List.append accum v)
            |> Result.map_ok Continue
            |> Result.with_default (Break (aq, accum))
    values.1 == [10, 20, 30, 65, 90] && (sizeOf values.0) == 0

# popping an empty queue returns Err EmptyQueue
expect
    actual = makeQ Num.compare |> pop |> Result.map_ok \(v, _) -> v
    expected = Err EmptyQueue
    actual == expected

# Popping queue with one element returns an empty queue
expect
    actual =
        makeQ Num.compare
        |> push 15
        |> pop
        |> Result.map_ok \(_, q) -> sizeOf q
        |> Result.with_default 12

    actual == 0

## popping a queue with one item twice returns an empty queue
expect
    actual =
        makeQ Num.compare
        |> push 65
        |> pop
        |> Result.map_ok .1
        |> Result.try pop
        |> Result.map_ok .0
    actual == Err EmptyQueue
# expect
#     (_, actual) =
#         makeQ Num.compare
#         |> push 15
#         |> push 15
#         |> push 15
#         |> push 15
#         |> pop

#     sizeOf (actual) == 3

# expect
#     (queue) =
#         makeQ Num.compare
#         |> push 33
#         |> push 32
#         |> push 34
#         |> push 9
#     dbg (getStore queue)
#     sizeOf queue == 4

# expect
#     (value, _) =
#         makeQ Num.compare
#         |> push 33
#         |> push 32
#         |> push 34
#         |> push 650
#         |> pop
#         |> \(_, q) -> pop q

#     value == (Ok 33)
# expect
#     (_, queue) =
#         makeQ Num.compare
#         |> push 33
#         |> push 32
#         |> push 34
#         |> push 650
#         |> push 0
#         |> pop
#     sizeOf queue == 4
# expect
#     (_, actual) =
#         makeQ Num.compare
#         |> push 15
#         |> push 10
#         |> push 12
#         |> pop

#     sizeOf (actual) == 2

# expect
#     (actual, _) =
#         makeQ Num.compare
#         |> push 15
#         |> pop

#     expected = Ok 15
#     actual == expected

# # PriorityQueue should dequeue in order
# expect
#     (value, _) =
#         makeQ Num.compare
#         |> push 15
#         |> push 10
#         |> push 33
#         |> push 20
#         |> pop
#     value == Ok 10

# expect
#     (value, actual) =
#         makeQ Num.compare
#         |> push 15
#         |> pop

#     sizeOf (actual) == 0
#     && value == Ok 15
