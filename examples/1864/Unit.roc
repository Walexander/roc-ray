module [Unit, MoveChoice, Id, combatOrder, isAlive, takeHit, summary, reroutePath, update, make, moveTo, initial]
import Hex exposing [Doubled, Point, doubled, cubeLerp]
import Utils exposing [frameCountToSeconds]
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
    health : [Living Health.Health, Dead U32],
    position : Hex.Point,
    cell : Doubled,
    dest : Doubled,
    velocity: Point,
    # sprite : Sprite,
    lastPath : List Doubled,
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
    position = Hex.hexToPixel cell
    lastPath = []
    id = Num.to_i8 inId
    when type is
        Artillery ->
            {
                id,
                attackDamage: 18u32,
                army,
                position,
                readiness: Ready,
                health: Living (Health.make 125),
                cell,
                dest: cell,
                lastPath,
                moveRate: 120,
                cooldownRate: 60.0 * 4,
                range: 2,
                velocity: { x: 0, y: 0 },
                # sprite: Assets.cannon,
            }

        Infantry ->
            {
                id,
                attackDamage: 15u32,
                army,
                position,
                health: Living (Health.make 100),
                readiness: Ready,
                cell,
                dest: cell,
                lastPath,
                moveRate: 90,
                cooldownRate: 60.0 * 3,
                velocity: { x: 0, y: 0 },
                range: 8,
                # sprite: Assets.infantry,
            }

        Cavalry ->
            {
                id,
                attackDamage: 11u32,
                army,
                position,
                readiness: Ready,
                health: Living (Health.make 80),
                cell,
                dest: cell,
                lastPath,
                moveRate: 50,
                cooldownRate: 60.0 * 2.125,
                velocity: { x: 0, y: 0 },
                range: 1,
                # sprite: Assets.horsey,
            }

moveTo = |unit, dest, isOccupied|
    path = Hex.findGraph(unit.cell, dest, isOccupied) ?? [ ]
    readiness = when path is
        [first, second, ..] -> Moving { start: Hex.hexToPixel first, end: Hex.hexToPixel second, t: 0 }
        _ -> Ready
    { unit &
        dest,
        readiness,
        lastPath: path,
    }
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
    Hex.neighborsOf unit.cell
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
initial = |isOccupied| [
    Unit.make { id: 5, type: Cavalry, army: Union, cell: doubled 1 1 },
    Unit.make { id: 4, type: Infantry, army: Union, cell: doubled 1 5 },
    Unit.make { id: 3, type: Infantry, army: Union, cell: doubled 0 4 },
    Unit.make { id: 6, type: Artillery, army: Confederates, cell: doubled 10 6 }
    |> Unit.moveTo (doubled 6 2) isOccupied,
    Unit.make { id: 7, type: Cavalry, army: Confederates, cell: doubled 12 8 }
    |> Unit.moveTo (doubled 5 1) isOccupied,
    Unit.make { id: 8, type: Infantry, army: Confederates, cell: doubled 12 0 }
    |> Unit.moveTo (doubled 4 10) isOccupied,
]

update : Unit, U64, MoveChoice, (Doubled -> Bool) -> Unit
update = \original, frameCount, move, cannotMoveTo ->
    { cell, dest, moveRate } = original
    (newDest, newPath) = unitPathFromMove original move cannotMoveTo
    currentCellPosition = Hex.pixelToHex original.position
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
            DoneMoving finalDest -> ({x: 0, y: 0}, Hex.hexToPixel(finalDest))
            ProceedTo from to _ _ ->
                from_pos = Hex.hexToPixel from
                to_pos = Hex.hexToPixel to
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
            dest_coords = Hex.hexToPixel nextCell
            f = Hex.subPoint original.position dest_coords
            dot_to = Hex.dot (Hex.subPoint position dest_coords) f

            dbg "Proceeding to next cell $(nextCell.row |> Num.to_str), $(nextCell.column |> Num.to_str) $(dot_to |> Num.to_str)"
            { original &
                cell: nextCell,
                position: Hex.hexToPixel nextCell,
                # position: Hex.pointLerp original.position (Hex.hexToPixel nextCell) (1 / moveRate),
                # position,
                lastPath: path |> List.drop_first 1,
                dest: destination,
            }

        ProceedTo _ _ destination path ->
            # from_pos = Hex.hexToPixel fromCell
            # to_pos = Hex.hexToPixel nextCell
            # _position =
            #     Hex.pointLerp from_pos to_pos
            #         (
            #             moveCountDown
            #             |> Num.to_frac
            #             |> Num.div moveRate
            #         )
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
