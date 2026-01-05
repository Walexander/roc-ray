module [PlayerMove, inputs_to_move, update!]
import rr.Mouse
import rr.Keys
import rr.Effect
import Hex
import Unit
import HexTile
import PointyHex
import Particle
import Model exposing [ YearOfDecision ]

PlayerMove : [ IncreaseTimer,
  DecreaseTimer,
  ResetGame,
  MoveUnit MoveUnitData,
  OrderMove {
    to: Hex.Doubled,
    id: I32,
  },
  SelectUnit I8,
  AddTrauma,
  ToggleTerrain Hex.Doubled HexTile.Terrain,
  NoMove
]

MoveUnitData : {
  unit_index: I8,
  to: Hex.Doubled
}


inputs_to_move : YearOfDecision, _, _, _, _, _ -> PlayerMove
inputs_to_move = |model, is_occupied, unit_from_cell, hover_coords, keys, buttons|
  if Mouse.pressed buttons.left then
    hover_cell = PointyHex.pixel_to_hex(hover_coords)
    if Keys.down keys KeyLeftShift then
      terrain = HexTile.get_terrain model.map hover_cell
      ToggleTerrain hover_cell (HexTile.next_terrain terrain)
    else if (is_occupied hover_cell) then
      (unit_from_cell hover_cell)
        |> Result.map_ok(\u -> SelectUnit u.id)
        |> Result.with_default NoMove
    else if hover_cell == model.hoverCell then
      if Keys.down keys KeyLeftControl then
        OrderMove { id: 2, to: hover_cell }
      else
        MoveUnit { unit_index: model.selectedIndex, to: hover_cell }
    else
      NoMove
  else if Keys.pressed keys KeySpace then
    AddTrauma
  else if Keys.pressed keys KeyEnter then
    ResetGame
  else if Keys.pressed keys KeyR then
    IncreaseTimer
  else if Keys.pressed keys KeyT then
    DecreaseTimer
  else
    NoMove



update! : YearOfDecision, PlayerMove, _ => _
update! = |model, move, path_finder|
  when move is
    NoMove -> model
    SelectUnit unit_index -> { model & selectedIndex: unit_index }
    AddTrauma -> {
      model &
      trauma: Hex.lerp model.trauma 1 0.5,
      ecs: Particle.spawn(model.ecs, { position: PointyHex.hex_to_pixel model.hoverCell, num_particles: 0, max_lifetime: 180 })

    }
    ToggleTerrain cell terrain ->
      map = HexTile.toggle_terrain model.map cell terrain
      dbg "Toggling like ${Inspect.to_str terrain}"
      { model & map }
    MoveUnit { unit_index, to } ->
      units = List.map model.units |u|
          if u.id == unit_index then
            Unit.moveTo u to path_finder
          else u
      { model & units }
    IncreaseTimer ->
      { model & countdown: Num.max(0, model.countdown + 1_000) }
    DecreaseTimer ->
      { model & countdown: if model.countdown >= 1000 then
          Num.max(0, model.countdown |> Num.sub_wrap 1_000)
      else 0 }
    ResetGame ->
      Model.initialize({
        seed: Effect.random_i32! 1 10_000,
        camera: model.camera,
        textures:  model.textures,
        render_textures: model.render_textures,
        shaders: model.shaders,
        sounds: model.sounds,
        base_camera: model.base_camera})
    OrderMove { id, to } -> {model&
        ecs: Particle.move_to model.ecs id to
    }
