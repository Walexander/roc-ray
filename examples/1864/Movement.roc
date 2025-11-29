module [update, move_to]
import Hex
import HexTile exposing [HexMap]

move_to = |from, dest, curr_path, curr_state, path_finder|
  new_path = path_finder(from, dest)
  prev = List.get(curr_path, 1) ?? from
  next = List.get(new_path, 1) ?? from
  (order, path) = when curr_state is
    Moving { start, end , t } ->
      start_cell = Hex.pixelToHex start
      end_cell = Hex.pixelToHex end
      if start_cell == from && next != end_cell then
        (Moving {start: end, end: start, t: 1 - t}, List.prepend new_path end_cell)
      else if next == end_cell then
        (Moving { start, end, t }, new_path)
      else
        (Moving { start, end, t }, List.prepend new_path prev)
    _ ->
      (Moving { start: Hex.hexToPixel next, end: Hex.hexToPixel next, t: 0 }, new_path)
  { order, path }


update = |world, dt, is_occupied, path_finder|
  units =  List.map(world.units, |unit|
    can_move = |cell| if unit.cell == cell then Bool.false else is_occupied cell
    updateMovement(unit, world.map, dt)
    |> reroute can_move path_finder
  )
  { world & units }

updateMovement = |unit, map, dt_|
    dt = Num.to_f32 dt_ |> Num.div 1000
    get_cost = HexTile.get_cell_cost map
    when unit.readiness is
        Ready | Cooldown _ -> unit
        Moving { start, end, t } ->
            movement_cost = get_cost unit.cell
            newT = t + dt * unit.moveRate / movement_cost
            newPos = Hex.pointLerp(start, end, newT)
            if newT >= 1 then
                cell = Hex.pixelToHex(end)
                position = Hex.hexToPixel cell
                updated = when unit.lastPath is
                    [_, _] | [_] | [] -> { unit & position, cell, lastPath: [], readiness: Cooldown(unit.cooldownRate |> Num.round) }
                    [_, to, next, ..] -> {unit &
                        cell: to,
                        position: Hex.hexToPixel to,
                        lastPath: List.drop_first unit.lastPath 1,
                        readiness: Moving({ start: Hex.hexToPixel to, end: Hex.hexToPixel next, t: 0 })
                    }

                updated
            else
                { unit &
                    position: newPos,
                    cell: Hex.pixelToHex(unit.position),
                    readiness: Moving {
                        start, end, t: newT
                    }
                }
# move_to = |from, dest, curr_path, curr_state, path_finder|
reroute = |unit, is_occupied, path_finder|
    when unit.readiness is
        Moving { end } ->
            if is_occupied(Hex.pixelToHex(end)) then
                dbg "Re-routing unit ${Inspect.to_str unit.id} to ${Inspect.to_str unit.dest}"
                { order, path } = move_to(unit.cell, unit.dest, unit.lastPath, unit.readiness, path_finder)
                { unit & lastPath: path, readiness: order }
            else
                unit
        _ -> unit
