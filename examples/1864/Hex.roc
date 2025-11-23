module [
    Doubled,
    Point,
    Line,
    add,
    dot,
    addPoint,
    subPoint,
    pixelToHex,
    clamped,
    closestNeighbors,
    clamp,
    findGraph,
    findGraph2,
    hexDistance,
    neighborsOf,
    doubled_is_eq,
    pathToLine,
    hexSize,
    verticalSpacing,
    horizontalSpace,
    hexToPixel,
    halfHorizontalSpacing,
    halfVerticalSpacing,
    lerp,
    normalize,
    cubeLerp,
    doubled,
    findPath,
    hexPoints,
    pointLerp,
    magnitude,
]
import Graph

Point : { x : F32, y : F32 }
Doubled : {
    row : I32,
    column : I32,
}

Line : List [ End, Segment Doubled Doubled ]

hexSize = 64
horizontalSpace = 1.5f32 * hexSize
halfHorizontalSpacing = horizontalSpace / 2f32
verticalSpacing = 128 # Num.sqrt 3.0 |> Num.mul hexSize |> Num.add 16
halfVerticalSpacing = verticalSpacing / 2f32

hexCorner = \center, size, corner ->
    angle = Num.pi / 180 * (60 * corner)
    { x: center.x + size * (Num.cos angle), y: center.y + size * (Num.sin angle) }

hexPoints = \center, size -> [
    hexCorner center size 0,
    hexCorner center size 1,
    hexCorner center size 2,
    hexCorner center size 3,
    hexCorner center size 4,
    hexCorner center size 5,
]

normalize = \vec ->
    mag = magnitude vec
    { x: vec.x / mag, y: vec.y / mag }

doubled : I32, I32 -> Doubled
doubled = \column, row -> { column, row }

axialToCube = \{ q, r } -> {
    q, r, s: (-1 * q) - r
}
toAxial: Doubled -> { q: I32, r: I32 }
toAxial = \{ column, row } -> {
    q: column |> Num.to_i32,
    r: ((row - column) |> Num.to_frac) / 2 |> Num.round
}

doubled_is_eq = \a, b ->
    a.column == b.column && a.row == b.row


cubeToDouble : { q: F32, r: F32 }* -> Doubled
cubeToDouble = \cube ->
    col = Num.round cube.q
    row = Num.round (2f32 * cube.r + cube.q)
    doubled col row

cubeRound = |frac|
    q = Num.round frac.q |> Num.to_f32
    r = Num.round frac.r |> Num.to_f32
    s = Num.round frac.s |> Num.to_f32

    q_diff = Num.abs (q - Num.to_frac frac.q)
    r_diff = Num.abs (r - frac.r)
    s_diff = Num.abs (s - frac.s)

    if q_diff > r_diff && q_diff > s_diff then
        { q: (r * -1) - s, r, s }
    else if r_diff > s_diff then
        { r: (q * -1) - s, s, q }
    else
        { q, r, s: (q * -1) - r }

addPoint = \a, b -> {
    x: a.x + b.x,
    y: a.y + b.y,
}
subPoint = |a, b| {
    x: a.x - b.x,
    y: a.y - b.y
}

hexToPixel : Doubled -> Point
hexToPixel = \{ row, column } -> {
    x:  Num.to_f32 column |> Num.mul 1.5f32 |> Num.mul hexSize,
    y:  Num.to_f32 row |> Num.mul hexSize
}

pixelToHex = |{ x, y }|
    base = 1/Num.sqrt 3
    baseq = 2/3
    baser = -1/3
    xx = x / hexSize
    yy = y / (hexSize * 2 / Num.sqrt 3) |> Num.to_f32

    q = xx * baseq
    r = baser * xx + base * yy
    rounded = cubeRound (axialToCube { q, r })
    cubeToDouble rounded

add = \a, b -> doubled (a.column + b.column) (a.row + b.row)
clampCube = \min, max -> \cell ->
        cell.column
        >= min.column
        && cell.row
        >= min.row
        && cell.column
        <= max.column
        && cell.row
        <= max.row

minCell = doubled -8 -8
maxCell = doubled 8 8
clamped = clampCube minCell maxCell

expect
    clamped (doubled 0 0)
expect
    clamped (doubled -1 0)
    |> Bool.not
expect
    clamped (doubled 15 0)
    |> Bool.not
expect
    clamped (doubled 0 17)
    |> Bool.not
expect
    clamped (doubled 0 -1)
    |> Bool.not

clamp = \test ->
    c =
        if test.column < minCell.column then
            if minCell.column - test.column == 1 then
                minCell.column + 1
            else
                minCell.column
        else if test.column > maxCell.column then
            if (test.column - maxCell.column) == 1 then
                maxCell.column - 1
            else
                maxCell.column
        else
            test.column
    r =
        if test.row < minCell.row then
            if minCell.row - test.row == 1 then
                minCell.row + 1
            else
                minCell.row
        else if test.row > maxCell.row then
            if test.row - maxCell.row == 1 then
                maxCell.row - 1
            else
                maxCell.row
        else
            test.row
    { column: c, row: r }

expect
    actual = clamp (doubled 11 17)
    expected = doubled 11 15
    actual == expected

# expect
#     actual = clamp (doubled 14 18)
#     expected = doubled 12 16
#     actual == expected

# expect
#     actual = clamp (doubled 14 14)
#     expected = doubled 12 14
#     actual == expected
expect
    actual = clamp (doubled 1 17)
    expected = doubled 1 15
    actual == expected
expect
    actual = clamp (doubled -2 8)
    expected = doubled 0 8
    actual == expected

expect
    actual = clamp (doubled 14 8)
    expected = doubled 12 8
    actual == expected

expect
    actual = clamp (doubled 14 -2)
    expected = doubled 12 0
    actual == expected
# expect
#     clamp (doubled 12 18) == (doubled 12 16)

expect
    actual = clamp (doubled -4 -2)
    expected = doubled 0 0
    actual == expected
## Clamp should maintain (r + c) % 2 == 0 invariant
expect
    actual = clamp (doubled -1 1)
    expected = doubled 1 1
    actual == expected
expect
    actual = clamp (doubled 3 -1)
    expected = doubled 3 1
    actual == expected

doubleNeighbors = [
    doubled 1 1,
    doubled 0 2,
    doubled 1 -1,
    doubled -1 -1,
    doubled 0 -2,
    doubled -1 1,
]

neighborsOf: Doubled -> List Doubled
neighborsOf = \cell ->
    List.map doubleNeighbors \n -> add n cell
    |> List.keep_if clamped

expect
    actual = neighborsOf (doubled 6 6)
    expected = [
        doubled 6 4,
        doubled 7 5,
        doubled 5 5,
        doubled 6 8,
        doubled 5 7,
        doubled 7 7,
    ]
    List.all expected \expec -> List.contains actual expec

expect
    n = neighborsOf (doubled 0 0)
    List.contains n (doubled 0 2)
    &&
    !(List.contains n (doubled 2 0))

expect
    actual = neighborsOf (doubled 12 6)
    expected = [
        doubled 12 8,
        doubled 11 5,
        doubled 12 4,
        doubled 11 7,
    ]
    List.all expected \expec -> List.contains actual expec

# graph = \isBlocked -> \cell -> Ok (neighborsOf cell |> List.drop_if isBlocked)
graph = |isBlocked| |cell| Ok(neighborsOf cell |> List.drop_if isBlocked)

findPath = \from, to -> cubeLerp from to
magnitude = |{ x, y }|
    Num.sqrt(x * x + y * y)

dot = |a, b|
    a.x * b.x + a.y * b.y


findGraph : Doubled, Doubled, (Doubled -> Bool) -> _
findGraph = \from, to, isBlocked ->
    dest = hexToPixel to
    estimator = |candidate|
        pos = hexToPixel candidate
        delta = subPoint dest pos
        magnitude delta / (Hex.hexSize * 2) |> Num.round
    Graph.astar {
        isTarget: \c -> c == to,
        estimator,
        root: from,
        graph: graph isBlocked,
    }
    |> Result.map_ok .1


findGraph2 : Doubled, Doubled, (Doubled -> Bool) -> Result (List Doubled) [NotFound]
findGraph2 = \from, to, isBlocked ->
    cost_fn: Doubled, Doubled -> I32
    cost_fn = \start, end ->
        lerpPath = cubeLerp start to |> List.get 1
        when lerpPath is
            Ok next if next == end -> 1
            Ok _ -> 2
            Err _ -> 5

    estimator = |candidate|
        hexDistance candidate to

    Graph.astar3 {
        isTarget: |c| c == to,
        estimator,
        cost_fn: cost_fn,
        root: from,
        graph: graph isBlocked,
    }
    |> Result.map_err \_ -> NotFound
    |> Result.map_ok .1

## Should Err when all blocked
# expect
#     actual =
#         findGraph
#             (doubled 12 12)
#             (doubled 14 12)
#             (\_ -> Bool.true)
#     expected = Err NotFound
#     actual == expected

## Should find shortest path when one blocked
# expect
#     actual =
#         findGraph
#             (doubled 1 3)
#             (doubled 3 3)
#             (\{ column, row } -> column == 2 && row == 4)
#     expected = [doubled 1 3, doubled 2 2, doubled 3 3]
#     actual == Ok expected
## Should find shortest three-step path when one blocked
# expect
#     actual =
#         findGraph
#             (doubled 3 7)
#             (doubled 1 3)
#             # \_ -> Bool.false
#             (\{ column, row } -> column == 2 && row == 4)
#     expected = [doubled 3 7, doubled 2 6, doubled 1 5, doubled 1 3]
#     actual == Ok expected
## findGraph should return a single item
## when from and to are equal
# expect
#     cell = doubled 12 12
#     actual = findGraph cell cell (\_ -> Bool.false)
#     expected = Ok [cell]
#     actual == expected

## findGraph should Err when out of bounds
expect
    actual =
        findGraph
            (doubled 0 0)
            (doubled 2 0)
            (\_ -> Bool.true)
    expected = Err NotFound
    actual == expected

## findGraph should find one hop away
expect
    actual =
        findGraph
            (doubled 0 0)
            (doubled 0 2)
            (\_ -> Bool.false)
    expected = Ok [doubled 0 0, doubled 0 2]
    actual == expected

## findGraph should return straight line when none blocked
expect
    actual =
        findGraph
            (doubled 0 0)
            (doubled 0 4)
            (\_ -> Bool.false)
    expected = Ok [doubled 0 0, doubled 0 2, doubled 0 4]
    actual == expected

expect
    actual =
        findGraph
            (doubled 1 1)
            (doubled 3 0)
            (\_ -> Bool.false)
    expected = Err NotFound
    actual == expected
## findGraph should return straight line horizontally
expect
    actual =
        findGraph
            (doubled 0 0)
            (doubled 2 0)
            (\_ -> Bool.false)
    expected = Ok [doubled 0 0, doubled 1 1, doubled 2 0]
    actual == expected

pathToLine = \path ->
    List.map_with_index path \from, i ->
        List.get path (i + 1)
        |> Result.map_ok \to -> Segment from to
        |> Result.with_default End

hexDistance : Doubled, Doubled -> I32
hexDistance = \from, to ->
    dcol = Num.sub from.column to.column |> Num.abs
    drow = Num.sub from.row to.row |> Num.abs
    Num.sub drow dcol
    |> Num.to_frac
    |> Num.div 2
    |> Num.max 0.0
    |> Num.round
    |> Num.add dcol
# dcol + (Num.max 0 ((Num.sub drow dcol) |> Num.to_frac |> Num.div 2 |> Num.round))

expect
    actual = hexDistance (doubled 1 5) (doubled 1 3)
    expected = 1
    actual == expected
expect
    List.all
        (neighborsOf (doubled 1 5))
        \neighbor -> hexDistance (doubled 1 5) neighbor == 1

pointLerp : Point, Point, F32 -> Point
pointLerp = \a, b, progress -> {
    x: lerp (a.x) (b.x) progress |> Num.round |> Num.to_f32,
    y: lerp (a.y) (b.y) progress |> Num.round |> Num.to_f32,
}

## Lerping with 1.0


cubeLerp : Doubled, Doubled -> List Doubled
cubeLerp = \a, b ->
    n = hexDistance a b
    xs = List.range { start: At 0, end: At n }
    mul = 1 |> Num.div (Num.to_frac n)
    aCube = toAxial a |> axialToCube
    bCube = toAxial b |> axialToCube
    if n == 0 then
        [a]
    else
        List.walk xs [] \accum, i ->
            ii = mul |> Num.mul (Num.to_frac i)
            q = lerp (Num.to_f32 aCube.q + 0.001) (Num.to_f32 bCube.q) ii
            r = lerp (Num.to_f32 aCube.r + 0.001) (Num.to_f32 bCube.r) ii
            s = lerp (Num.to_f32 aCube.s + 0.001) (Num.to_f32 bCube.s) ii
            cube = cubeRound { q, r, s  }
            List.append accum (cubeToDouble cube)
            # row_ = (lerp (Num.to_f32 a.row ) (Num.to_f32 b.row) ii)
            # row = Num.round row_
            # col_ = (lerp (Num.to_f32 a.column) (Num.to_f32 b.column) ii)
            # col = Num.round col_
            # cell =
            #     if (row + col) % 2 == 0 then
            #         doubled col row
            #     else
            #         dbg "Row = $(row |> Num.to_str), Column = $(col |> Num.to_str)"
            #         doubled (col - 1) (row)
            # List.append accum cell

lerp : F32, F32, F32 -> F32
lerp = \a, b, t ->
    aa = Num.to_frac a
    bb = Num.to_frac b
    aa + (bb - aa) * t

closestNeighbors = \from, to ->
    neighborsOf to
    |> List.sort_with \a, b -> Num.compare (hexDistance from a) (hexDistance from b)

