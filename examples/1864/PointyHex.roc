module [cell_sorter, hex_distance, points, hex_to_pixel, lerp_path, vertical_spacing, horizontal_spacing, pixel_to_hex, neighbors, hex_width, hex_height]

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

double_width_to_axial : Doubled -> _
double_width_to_axial = |cell|
  q = Num.to_frac(cell.column - cell.row) / 2 |> Num.round
  r = cell.row
  { q, r }

lerp_path = |from, to|
  n = hex_distance from to
  if n == 0 then
      [ from ]
  else
    xs = List.range { start: At 0, end: At n }
    multiple = 1 |> Num.div(Num.to_frac n)
    from_cube = double_width_to_axial from |> Hex.axialToCube
    to_cube = double_width_to_axial to |> Hex.axialToCube
    List.walk xs [] |accum, i|
        ii = Num.to_frac i |> Num.mul multiple
        q = Hex.lerp(Num.to_frac from_cube.q + 0.1, Num.to_frac to_cube.q, ii)
        r = Hex.lerp(Num.to_frac from_cube.r + 0.1, Num.to_frac to_cube.r, ii)
        s = Hex.lerp(Num.to_frac from_cube.s + 0.1, Num.to_frac to_cube.s, ii)
        List.append accum axial_to_double_width(Hex.cubeRound { q, r, s })

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
hex_distance : Doubled, Doubled -> I32
hex_distance = |from, to|
  d_col = Num.abs(from.column - to.column)
  d_row = Num.abs(from.row - to.row)
  d_col1 = (d_col - d_row) |> Num.to_frac |> Num.div 2 |> Num.round
  d_row + Num.max(0, d_col1)

