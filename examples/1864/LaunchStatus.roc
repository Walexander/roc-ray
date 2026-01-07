module [update, for_pad]
import Model
import Hex exposing [Doubled]
import Particle
LaunchPad : List Doubled

update = |world|
    { world &
        launch_state: overall world,
    }
for_pad = |model, pad|
    get_pad_owner get_occupants(model.ecs) pad
get_occupants = |ecs|
    Particle.get_by_component ecs [Occupies]
    |> Dict.walk [] |accum, _, value|
        when value is
            [Occupies occupancy] -> List.append accum occupancy
            _ -> accum

get_launch_state_ecs = |ecs, launch_pads|
    occupants = get_occupants ecs
    List.map launch_pads |pad| get_pad_owner occupants pad

overall : Model.YearOfDecision -> [InControl Model.Army, Stalemate]
overall = |model|
    get_launch_state_ecs model.ecs model.map.launch_pads
    |> get_overall_status


get_pad_owner : List Particle.CompOccupies, LaunchPad -> [Owned [Union, Confederates], Neutral]
get_pad_owner = |occupants, pad|
    List.walk pad [] |accum, cell|
        List.find_first occupants |u| u.cell == cell
        |> Result.map_ok |u| List.append accum u.army
        |> Result.with_default accum
    |> List.walk (0, 0) |accum, army|
        when army is
            Union -> (accum.0 + 1, accum.1)
            Confederates -> (accum.0, accum.1 + 1)
    |> |(union, confederates)|
        if union == confederates then
            Neutral
        else if union > 0 and confederates == 0 then
            Owned Union
        else if confederates > 0 and union == 0 then
            Owned Confederates
        else
            Neutral

get_overall_status = |owners|
    (union, confederates) = count_pads owners
    if union > confederates then
        InControl Union
    else if confederates > union then
        InControl Confederates
    else
        Stalemate

count_pads = |owners|
    List.walk owners (0, 0) |accum, owner|
        when owner is
            Owned Union -> (accum.0 + 1, accum.1)
            Owned Confederates -> (accum.0, accum.1 + 1)
            Neutral -> accum
