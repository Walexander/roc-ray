module [Unit, MoveChoice, Id, combatOrder, isAlive, takeHit, summary, reroute, make, moveTo, initial, updateReadiness]
import Hex exposing [Doubled, Point, doubled]
import Utils exposing [frameCountToSeconds]
import PointyHex
import Health
# import Assets
# import w4.Sprite exposing [Sprite]
Id : I8
Unit : {
    id : Id,
    moveRate : F32,
    cooldownRate : F32,
    attackDamage : U32,
    army : [Union, Confederates],
    health : [Living Health.Health, Dead U64],
    position : Hex.Point,
    cell : Doubled,
    dest : Doubled,
    velocity: Point,
    # sprite : Sprite,
    lastPath : List Doubled,
    type: [Artillery, Cavalry, Infantry],
    readiness : [Cooldown U32, Ready, Moving({start: Hex.Point, end: Hex.Point, t: F32})],
    range : U8,
}
MoveChoice : [
    Selected Id (List Doubled),
    Destination Id Doubled (List Doubled),
    Finished,
]
make : _ -> Unit
make = \{ type, id: inId, army, cell } ->
    position = PointyHex.hex_to_pixel cell
    lastPath = []
    id = Num.to_i8 inId
    when type is
        Artillery ->
            {
                id,
                attackDamage: 18u32,
                type,
                army,
                position,
                readiness: Ready,
                health: Living (Health.make 125),
                cell,
                dest: cell,
                lastPath,
                moveRate: 0.45,
                cooldownRate: 60.0 * 4,
                range: 2,
                velocity: { x: 0, y: 0 },
                # sprite: Assets.cannon,
            }

        Infantry ->
            {
                id,
                attackDamage: 15u32,
                type,
                army,
                position,
                health: Living (Health.make 100),
                readiness: Ready,
                cell,
                dest: cell,
                lastPath,
                moveRate: 0.6,
                cooldownRate: 60.0 * 3,
                velocity: { x: 0, y: 0 },
                range: 8,
                # sprite: Assets.infantry,
            }

        Cavalry ->
            {
                id,
                attackDamage: 11u32,
                type,
                army,
                position,
                readiness: Ready,
                health: Living (Health.make 80),
                cell,
                dest: cell,
                lastPath,
                moveRate: 0.75,
                cooldownRate: 60.0 * 2.125,
                velocity: { x: 0, y: 0 },
                range: 1,
                # sprite: Assets.horsey,
            }

moveTo = |unit, dest, path_finder|
    from_cell = unit.cell

    # path = when unit.army is
    #     Confederates -> Hex.findGraph2(from_cell, dest, isOccupied) ?? unit.lastPath
    #     Union -> Hex.findGraph2(from_cell, dest, isOccupied) ?? unit.lastPath
    path = path_finder(from_cell, dest)

    prev_cell = List.get(unit.lastPath, 1) |> Result.with_default(unit.cell)
    next_cell = List.get(path, 1) |> Result.with_default(unit.cell)

    (readiness, lastPath) = when unit.readiness is
        Cooldown _ | Ready -> (Moving { start: PointyHex.hex_to_pixel unit.cell, end: PointyHex.hex_to_pixel(next_cell), t: 0  }, path)
        Moving { start, end, t } ->
            start_cell = PointyHex.pixel_to_hex start
            end_cell = PointyHex.pixel_to_hex end
            if start_cell == from_cell && next_cell != end_cell then
                (Moving { start: end, end: start, t: 1 - t }, List.prepend(path, end_cell))
            else if next_cell == PointyHex.pixel_to_hex end  then
                (Moving {start, end, t}, path)
            else
                (Moving {start, end, t}, List.prepend(path, prev_cell))

    { unit & readiness, lastPath, dest }
isAlive = \{ health } ->
    when health is
        Living hp -> Health.isAlive hp
        Dead _ -> Bool.false

takeHit = \{ health }, damage ->
    when health is
        Living hp -> Health.takeHit hp damage |> Living
        other -> other

combatOrder = \unit, modifier, units ->
    canFire =
        when unit.readiness is
            Ready -> Bool.true
            _ -> Bool.false
    if !(isAlive unit) || !canFire then
        Err NoEnemy
    else
        getTargeting units unit
        |> Result.map_ok \target ->
            attack = Num.to_f32 unit.attackDamage
            mod = Num.to_f32 modifier |> Num.div 100f32 |> Num.add 1f32
            damage =
                (mod)
                |> Num.mul (attack / 2)
                |> Num.round
                |> Num.to_u32
            (unit, target, damage)

getTargeting = \units, unit ->
    PointyHex.neighbors unit.cell
    |> List.join_map \cell ->
        List.find_first units \u ->
            when u.health is
                Living _ -> u.cell == cell && u.army != unit.army
                Dead _ -> Bool.false
        |> Result.map_ok \u -> [u]
        |> Result.with_default []
    |> List.first
    |> Result.map_err \_ -> NoEnemies

expect
    health = Living (Health.make 100)
    for = { army: Confederates, cell: doubled 2 2, health }
    units = [
        { army: Union, cell: doubled 2 4, health },
        { army: Union, cell: doubled 3 3, health },
        for,
    ]
    actual =
        getTargeting units for
        |> Result.map_ok \{ health: h } ->
            when h is
                Living hp -> Health.health hp
                Dead _ -> 0
    expected =
        List.first units
        |> Result.map_ok \{ health: h } ->
            when h is
                Living hp -> Health.health hp
                Dead _ -> 0
    actual == expected

# initial : ( \Hex.Doubled -> Bool ) -> List Unit
initial = [
    make { id: 5, type: Cavalry, army: Union, cell: doubled -9 -3 },
    make { id: 4, type: Infantry, army: Union, cell: doubled -8 -2 },
    make { id: 3, type: Infantry, army: Union, cell: doubled -10 -2 },
    make { id: 6, type: Artillery, army: Confederates, cell: doubled 9 -3 },
    make { id: 7, type: Cavalry, army: Confederates, cell: doubled 8 -4 },
    make { id: 8, type: Infantry, army: Confederates, cell: doubled 10 -2 },
]

updateReadiness: Unit -> Unit
updateReadiness = |unit|
    readiness = when unit.readiness is
        Ready | Moving _ -> unit.readiness
        Cooldown countdown if countdown > 0 -> Cooldown(Num.to_u32(countdown - 1))
        Cooldown _ -> Ready
    { unit & readiness }

update : Unit, U64, MoveChoice, (Doubled -> Bool) -> Unit
update = \original, frameCount, move, cannotMoveTo ->
    { cell, dest, moveRate } = original
    (newDest, newPath) = unitPathFromMove original move cannotMoveTo
    currentCellPosition = PointyHex.pixel_to_hex original.position
    moveCountDown = frameCount % (Num.round moveRate)

    marchingOrder =
        when newPath is
            [] -> Stopped
            [_] if moveCountDown == 0 -> Stopped
            [nextCell] -> DoneMoving nextCell
            [nextCell, destination] if cannotMoveTo destination -> DoneMoving nextCell
            [_, nextCell, ..] if cannotMoveTo nextCell -> UpdatePathTo newDest newPath
            [from, next, ..] -> ProceedTo from next newDest newPath


    (velocity, position) = when marchingOrder is
            Stopped -> ({ x: 0, y: 0 }, original.position)
            DoneMoving finalDest -> ({x: 0, y: 0}, PointyHex.hex_to_pixel(finalDest))
            ProceedTo from to _ _ ->
                from_pos = PointyHex.hex_to_pixel from
                to_pos = PointyHex.hex_to_pixel to
                v_ = Hex.subPoint to_pos from_pos |> |v| { x: v.x / moveRate |> Num.to_f32, y: v.y / moveRate  |> Num.to_f32}
                (v_, Hex.addPoint(original.position, v_))
            UpdatePathTo _ _ -> ({ x: 0, y: 0}, original.position)

    when marchingOrder is
        Stopped ->
            readiness =
                when original.readiness is
                    Cooldown countdown if countdown > 0 -> Cooldown (countdown - 1)
                    Cooldown _ -> Ready
                    Moving _ -> Cooldown (original.cooldownRate |> Num.round)
                    other -> other
            { original & readiness, position, velocity, lastPath: [] }

        DoneMoving _ ->
            { original & position, velocity, lastPath: [] }

        ProceedTo _ nextCell destination path if moveCountDown == 0 ->
            { original &
                cell: nextCell,
                position: PointyHex.hex_to_pixel nextCell,
                lastPath: path |> List.drop_first 1,
                dest: destination,
            }

        ProceedTo _ _ destination path ->
            { original &
                dest: destination,
                lastPath: path,
                velocity,
                cell: currentCellPosition,
                position,
            }

        UpdatePathTo destination nextPath ->
            lastPath =
                Hex.findGraph cell dest cannotMoveTo
                |> Result.on_err \_ ->
                    Hex.closestNeighbors cell dest
                    |> List.drop_if \neighbor -> cannotMoveTo neighbor
                    |> List.first
                    |> Result.try \aDest -> Hex.findGraph cell aDest cannotMoveTo
                |> Result.on_err \_ -> Hex.findGraph cell (doubled 1 1) cannotMoveTo
                |> Result.with_default (List.drop_last nextPath 1)

            { original &
                dest: List.last lastPath |> Result.with_default destination,
                lastPath,
            }

unitPathFromMove = \unit, move, isblocked ->
    when move is
        Destination id chosen _ if id == unit.id ->
            if isblocked chosen then
                (unit.cell, unit.lastPath)
            else
                path =
                    Hex.findGraph unit.cell chosen isblocked
                    |> Result.map_ok \p -> reroutePath p unit.lastPath unit.cell
                    |> Result.with_default []
                (chosen, path)

        _ -> (unit.dest, unit.lastPath)

summary : Unit, _ ->  _
summary = \unit, planned ->
    id = unit.id
    next =
        List.get unit.lastPath 1
        |> Result.map_ok \n -> "$(Num.to_str n.column),$(Num.to_str n.row)"
        |> Result.with_default "none"

    readyState =
        when unit.readiness is
            Cooldown ticked ->
                tickSeconds =
                    ticked
                    |> frameCountToSeconds
                    |> Num.mul 10
                    |> Num.round
                    |> Num.to_frac
                    |> Num.div 10
                    |> Num.to_str
                "C<$(tickSeconds)>"

            Moving _ -> "M {${Num.to_str unit.velocity.x}, ${Num.to_str unit.velocity.y}"
            Ready -> "R"

    health =
        when unit.health is
            Living hp ->
                Health.health hp
                |> Num.mul 100
                |> Num.round
                |> Num.to_frac
                |> Num.div 100
                |> Num.to_str

            Dead time -> "Dead since $(time |> Num.to_str)"
    unitPos = "$(unit.position.x |> Num.round |> Num.to_str),$(unit.position.y|> Num.round |> Num.to_str )"
    distance = "$(Hex.hexDistance unit.cell unit.dest |> Num.to_str)]/$(List.len unit.lastPath |> Num.to_str)"
    """
    #$(Num.to_str id)/H:$(health)
    $(Num.to_str unit.cell.column),$(Num.to_str unit.cell.row)->$(next) $(unitPos)
    dest: {$(Num.to_str unit.dest.column),$(Num.to_str unit.dest.row)} $(distance)
    [$(List.len unit.lastPath |> Num.to_str):$(List.len planned |> Num.to_str)] {$(readyState)}
    """
reroute = |unit, isOccupied, path_finder|
    when unit.readiness is
        Moving { end } ->
            if isOccupied(PointyHex.pixel_to_hex(end)) then
                dbg "Re-routing unit ${Inspect.to_str unit.id} to ${Inspect.to_str unit.dest}"
                moveTo(unit, unit.dest, path_finder)
            else
                unit
        _ -> unit
reroutePath = \newPath, lastPath, currentCell ->
    lastNextStep =
        List.get lastPath 1
        |> Result.map_ok Moving
        |> Result.with_default (StandingStill currentCell)

    newNextStep =
        List.get newPath 1
        |> Result.map_ok Moving
        |> Result.with_default (NotMoving currentCell)

    when (lastNextStep, newNextStep) is
        (Moving prevCell, Moving nextCell) if prevCell == nextCell -> newPath
        (Moving prevCell, Moving _) ->
            List.concat [currentCell, prevCell] newPath

        (_, _) -> newPath

## reroutePath should no op when paths are the same
expect
    last = [Hex.doubled 0 0, Hex.doubled 0 2]
    next = [Hex.doubled 0 0, Hex.doubled 0 2]
    actual = reroutePath next last (Hex.doubled 0 0)
    actual == next

## reroutePath returns new path when next step is same in both
expect
    last = [Hex.doubled 0 0, Hex.doubled 0 2]
    next = [Hex.doubled 0 0, Hex.doubled 0 2, Hex.doubled 0 4]
    actual = reroutePath next last (Hex.doubled 0 0)
    actual == next
## reroutePath prefixes current and next cell onto path when different
expect
    last = [Hex.doubled 0 0, Hex.doubled 0 2]
    next = [Hex.doubled 0 0, Hex.doubled 2 0, Hex.doubled 4 0]
    actual = reroutePath next last (Hex.doubled 0 0)
    expected = [
        Hex.doubled 0 0,
        Hex.doubled 0 2,
        Hex.doubled 0 0,
        Hex.doubled 2 0,
        Hex.doubled 4 0,
    ]
    actual == expected
