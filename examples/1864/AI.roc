module [ update, ]
import Unit
import PointyHex

# update : YearOfDecision, _, I32 -> YearOfDecision
update = |world, path_finder, is_occupied|
  pads = List.join world.map.launch_pads

  orders = List.keep_if(world.units, |u| u.army == world.ai_army)
    |> List.walk world.ai_intents |dict, u|
      List.keep_if pads |cell| (cell == u.cell || is_occupied cell |> Bool.not)
      |> List.sort_with(|a, b|
        a_d = PointyHex.hex_distance u.cell a
        b_d = PointyHex.hex_distance u.cell b
        if a_d < b_d then LT
        else if b_d < a_d then GT
        else EQ
      )
        |> List.first
        |> Result.map_ok |dest| Dict.insert dict u.id (TakePad dest)
        |> Result.with_default dict

  {
    world &
    ai_intents: orders,
    units: process_orders(world.units, orders, is_occupied, path_finder)
  }

process_orders : List Unit.Unit, _, _, _ -> List Unit.Unit
process_orders = |units, orders, is_occupied, path_finder|
  List.map units |unit|
    Dict.get(orders, unit.id)
      |> Result.try |order|
        when order is
          TakePad cell -> Ok cell
          _ -> Err NotFound
      |> Result.map_ok |dest| Unit.moveTo unit dest path_finder
      |> Result.with_default unit
