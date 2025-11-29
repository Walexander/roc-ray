module [update]
import Hex exposing [Doubled, lerp]
import Unit exposing [Unit]
LaunchPad : List Doubled

update = |world|
  {world & launch_state: get_launch_state world.map world.units }


get_launch_state = |hex_map, units|
    padOwners = List.map hex_map.launch_pads \pad ->
        getPadOwner units pad
    getLaunchStatus padOwners

getPadOwner : List Unit, LaunchPad -> [Owned [Union, Confederates], Neutral]
getPadOwner = |units, pad|
    byArmy = List.walk pad [] |accum, cell|
        List.find_first units |u| u.cell == cell
        |> Result.map_ok |u| List.append accum u.army
        |> Result.with_default accum

    (union, confederates) = List.walk byArmy (0, 0) |accum, army|
        when army is
            Union -> (accum.0 + 1, accum.1)
            Confederates -> (accum.0, accum.1 + 1)

    if union == confederates then
        Neutral
    else if union > 0 and confederates == 0 then
        Owned Union
    else if confederates > 0 and union == 0 then
        Owned Confederates
    else
        Neutral

getLaunchStatus = \owners ->
    (union, confederates) = countPadsByOwner owners
    if union > confederates then
        InControl Union
    else if confederates > union then
        InControl Confederates
    else
        Stalemate

countPadsByOwner = \owners ->
    List.walk owners (0, 0) \accum, owner ->
        when owner is
            Owned Union -> (accum.0 + 1, accum.1)
            Owned Confederates -> (accum.0, accum.1 + 1)
            Neutral -> accum

