module [
  HexTile,
  Terrain,
  texture_position,
  random_terrain,
  next_terrain
]
import Hex exposing [Doubled]

Terrain : [Sand, Gravel, Field, Rocky, Stone, Pasture, Mountain, Granite, Forest, Basalt, Lava, Water]

HexTile : {
  cell: Doubled,
  terrain: Terrain
}

terrains = [Sand, Gravel, Field, Rocky, Stone, Pasture, Mountain, Granite, Forest, Basalt, Lava, Water]

random_terrain = |num|
  idx = num % List.len(terrains)
  List.get terrains idx |> Result.with_default Sand

next_terrain : HexTile -> Terrain
next_terrain = |tile|
  when tile.terrain is
    Sand -> Gravel
    Gravel -> Rocky
    Rocky -> Stone
    Stone -> Pasture
    Pasture -> Mountain
    Mountain -> Granite
    Granite -> Forest
    Forest -> Basalt
    Field -> Rocky
    Basalt -> Lava
    Lava -> Water
    Water -> Sand

texture_position = |tile, size|
  when tile.terrain is
    Sand -> {x: 0, y: 0}
    Gravel -> {x: 1 * size, y: 0 }
    Field -> {x: 2 * size, y: 0 }

    Rocky -> {x: 0, y: size * 1 }
    Stone -> {x: 1 * size, y: size * 1 }
    Pasture -> {x: 2 * size, y: size * 1 }

    Mountain -> {x: 0 * size, y: size * 2 }
    Granite -> { x: 1 * size, y: size * 2 }
    Forest -> { x: 2 * size, y: size * 2  }

    Basalt -> {x: 0 * size, y: size * 3 }
    Lava -> {x: 1 * size, y: size * 3 }
    Water -> {x: 2 * size, y: size * 3 }
