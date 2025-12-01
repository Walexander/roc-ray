module [cell_sorter, points, hex_to_pixel, vertical_spacing, horizontal_spacing, pixel_to_hex, neighbors, hex_width, hex_height]

import Hex  exposing [Doubled, Point, doubled]

hex_size = 32
hex_width = 64
hex_height = 64
vertical_spacing = 45
horizontal_spacing = 33

double_width_neighbors = [
  doubled(1, -1),
  doubled(-1, -1),
  doubled(-2, 0),
  doubled(2, 0),
  doubled(-1, 1),
  doubled(1, 1)
]

points = |center|
  List.range { start: At 0, end: Before 7 }
  |> List.map |c| corner center c

corner = |center, c|
  angle_deg = 60 * c - 30
  angle = Num.pi / 180 * angle_deg
  {
    x: center.x + hex_size * (Num.cos angle),
    y: center.y + hex_size * (Num.sin angle) - 8,
  }

neighbors = |cell|
  List.map double_width_neighbors |n| Hex.add n cell

hex_to_pixel : Doubled -> Point
hex_to_pixel = |{column, row}|
  y = 1.5 * (Num.to_f32 row)
  x = 0.866025 * (Num.to_f32 column)

  { x: x * hex_width / 1.732050, y: y * hex_height / 2 }

pixel_to_hex : Point -> Doubled
pixel_to_hex = |{x, y}|
  x_ = x / (hex_width  / Num.sqrt 3)
  y_ = y / (hex_height / 2)

  q = 0.577350 * x_ - 0.33333 * y_
  r = y_ * 2 / 3
  rounded = Hex.cubeRound( Hex.axialToCube { q, r } )
  axial_to_double_width rounded

axial_to_double_width : { q: F32, r: F32 }* -> Doubled
axial_to_double_width = |{q, r}|
  doubled(Num.round(2 * q + r), Num.round r)


cell_sorter = |a, b|
      if a.row < b.row then
          LT
      else if b.row < a.row then
          GT
      else if a.column < b.column then
          LT
      else if b.column < a.column then
          GT
      else
          EQ
