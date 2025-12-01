module [
  HexTile,
  HexMap,
  Terrain,
  neighbors,
  is_in_bounds,
  toggle_terrain,
  get_cell_cost,
  get_terrain,
  texture_position,
  make_path_finder,
  random_terrain,
  next_terrain,
  init,
  make_tile
]
import PointyHex
import Hex exposing [Doubled, doubled]
import Graph

Terrain : [Sand, Gravel, Field, Rocky, Stone, Pasture, Mountain, Granite, Forest, Basalt, Lava, Water]

HexTile : {
  cell: Doubled,
  terrain: Terrain
}

HexMap : {
  tiles: Dict Doubled HexTile,
  min_cell: Doubled,
  max_cell: Doubled,
  # clamped: Doubled -> Bool,
  # neighbors: Doubled -> List Doubled,
  border: List Doubled,
  center: List Doubled,
  launch_pads: List (List Doubled),
}
# empty = {
#   tiles: Dict.empty {},
#   min_cell: doubled(0, 0),
#   max_cell: doubled(0, 0),
#   border: [],
#   center: [],
#   launch_pads: [],
# }

init: Doubled, Doubled, _ -> HexMap
init = |min_cell, max_cell, noise_fn|
  border =
    top_left = doubled(min_cell.column - 1, min_cell.row - 1)
    top_right = doubled(max_cell.column + 1, top_left.row)
    bottom_right = doubled(top_right.column, max_cell.row + 1)
    bottom_left = doubled(top_left.column, bottom_right.row)
    []
    # List.join([
    #   Hex.cubeLerp top_left top_right,
    #   Hex.cubeLerp top_left bottom_left,
    #   Hex.cubeLerp top_right bottom_right,
    #   Hex.cubeLerp bottom_left bottom_right,
    # ])
  # column_count = max_cell.column - min_cell.column |> Num.to_f32
  # row_count = max_cell.row - min_cell.row |> Num.to_f32
  tile_maker = terrain_from_tile { min_cell, max_cell, noise_fn }
  tiles =
    ## for each column from min to max
    List.range {
      start: At (min_cell.column |> Num.to_i32),
      end: Before(max_cell.column + 1 |> Num.to_i32), step: 1
    }
    |> List.join_map |column|
      ## from start to end row
      ## (odd columns are pushed down by one at the top and up by one at the bottom)
      (min_row, max_row) =
        if column % 2 == 0 then
          (min_cell.row, max_cell.row)
        else
          (min_cell.row + 1, max_cell.row - 1)

      # col_norm = (Num.to_f32(column) / column_count)
      List.range({
        start: At min_row,
        end: Before(max_row + 1 |> Num.to_i32), step: 2
      })
      |> List.map |row|
        cell = doubled(column, row)
        tile_maker cell
        # row_norm = Num.to_f32(row) / row_count
        # noise = Noise.perlin2d(2.5 * row_norm, 1 * col_norm)
        # terrain= terrain_from_height(noise)
        # { terrain, cell, }
    |> List.walk (Dict.empty {}) |dict, tile| Dict.insert dict tile.cell tile

  {
    min_cell,
    max_cell,
    border,
    tiles,
    center: [ doubled(0, 0) ],
    launch_pads: [
      [ doubled(-1, -3), doubled(0, -4), doubled(1, -3) ],
      [ doubled(-1, 3), doubled(0, 4), doubled(1, 3) ],
    ]
  }

is_in_bounds = |map, cell|
  clamped = Hex.clampCube map.min_cell map.max_cell
  clamped cell && Bool.not( List.contains map.center cell )

terrain_from_tile = |{ min_cell, max_cell, noise_fn }|
  col_scale = 1/Num.to_f32(max_cell.column - min_cell.column)
  row_scale = 1/Num.to_f32(max_cell.row - min_cell.row)

  |cell|
    col_n = Num.to_f32(cell.column) * col_scale
    row_n = Num.to_f32(cell.row) * row_scale
    noise = 2 * noise_fn(1.05 * col_n, 1.125 * row_n)

    # dbg "Got noise from ${col_n |> Num.to_str}, ${row_n|> Num.to_str} = ${Inspect.to_str noise}"
    { cell, terrain: terrain_from_height noise }

make_tile : Doubled, Terrain -> HexTile
make_tile = |cell, terrain_|
  { cell, terrain: terrain_ }

toggle_terrain = |map, cell, terrain|
  { map &
    tiles: Dict.update(map.tiles, cell, |result|
      Result.map_ok result |_| make_tile cell terrain
  )
}

neighbors = |map|
  center = map.center
  clamped = Hex.clampCube map.min_cell map.max_cell
  |cell|
    (
        PointyHex.neighbors cell
        |> List.keep_if clamped
        |> List.keep_if |c| List.contains center c |> Bool.not
    )


terrain_from_height = |height|
  if height < -0.9 then Water
  else if height < -0.5 then Sand
  else if height < -0.20 then Gravel
  else if height < 0.25 then Pasture
  else if height < 0.7 then Forest
  else if height < 0.9 then Stone
  else Mountain

get_terrain : HexMap, Doubled -> Terrain
get_terrain = |map, cell|
  Dict.get map.tiles cell
    |> Result.map_ok .terrain
    |> Result.with_default Sand

terrains = [Sand,
  Gravel,
  Field,
  Rocky,
  Stone,
  Pasture,
  Mountain,
  Granite,
  Forest,
  Basalt, Lava, Water]

random_terrain = |num|
  idx = num % List.len(terrains)
  List.get terrains idx |> Result.with_default Sand

terrain_cost : Terrain -> F32
terrain_cost = |terrain|
  when terrain is
    Field | Pasture | Gravel -> 0.95
    Stone | Granite | Rocky -> 1.15
    Sand -> 1.020
    Forest -> 1.125
    Mountain -> 1.6
    Water -> 2
    Lava -> 5
    Basalt -> 5

next_terrain : Terrain -> Terrain
next_terrain = |terrain|
  when terrain is
    Sand -> Gravel
    Gravel -> Rocky
    Rocky -> Stone
    Stone -> Pasture
    Pasture -> Mountain
    Mountain -> Granite
    Granite -> Forest
    Forest -> Field
    Field -> Lava
    Basalt -> Basalt
    Lava -> Water
    Water -> Sand

texture_position = |tile, width, height|
  when tile.terrain is
    Sand -> {x: 0, y: 0}
    # _ -> { x: 0, y: 0 }
    Gravel -> {x: 1 * width, y: 0 }
    Field -> {x: 3 * width, y: 3 * height }

    Rocky -> {x: 0, y: height * 1 }
    Stone -> {x: 1 * width, y: height * 1 }
    Pasture -> {x: 3 * width, y: height * 3 }

    Mountain -> {x: 2 * width, y: height * 0 }
    Granite -> { x: 3 * width, y: height * 3 }
    Forest -> { x: 3 * width, y: height * 3  }

    Basalt -> {x: 2 * width, y: height * 3 }
    Lava -> {x: 3 * width, y: height * 0 }
    Water -> {x: 0 * width, y: height * 4 }


get_cell_cost = |map|
  |cell|
    Dict.get map.tiles cell
    |> Result.map_ok .terrain
    |> Result.map_ok terrain_cost
    |> Result.with_default 100_000_000.0

make_path_finder = |map, is_occupied|
  neighbors_ = neighbors map
  graph = |start|
    neighbors_ start
    |> List.drop_if(|cell| is_occupied(cell))
  get_cost = get_cell_cost map

  |from, dest|
    estimator = |candidate|
      Hex.hexDistance candidate dest |> Num.to_f32
    cost_fn = |_, end|
      get_cost end

    Graph.astar3 {
      isTarget: \c -> c == dest,
      estimator,
      cost_fn,
      root: from,
      graph,
    }
    |> Result.map_ok( .1 )
    |> Result.with_default []
