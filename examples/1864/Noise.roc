module [perlin2d, seeded_perlin2d]
import Hex
perm_ : List I32
perm_ = [
        151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,
        140,36,103,30,69,142,8,99,37,240,21,10,23,190, 6,148,
        247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,
        57,177,33,88,237,149,56,87,174,20,125,136,171,168, 68,175,
        74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,
        60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,
        65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,
        200,196,135,130,116,188,159,86,164,100,109,198,173,186, 3,64,
        52,217,226,250,124,123,5,202,38,147,118,126,255,82,85,212,
        207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,213,
        119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185,112,104,
        218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241,
        81,51,145,235,249,14,239,107,49,192,214, 31,181,199,106,157,
        184, 84,204,176,115,121,50,45,127,  4,150,254,138,236,205, 93,
        222,114,67,29,24,72,243,141,128,195,78, 66,215,61,156,180
    ]

perm = List.concat(perm_, perm_)


fade = |t|
  t * t * t * (t * (t * 6 - 15) + 10)

grad_seed = |seed|
  |hash, x, y|
    # h = Num.bitwise_and hash 15
    h =  Num.round x |> Num.add (Num.round y) |> Num.add_wrap(hash + seed) |> Num.bitwise_and 15
    # x_ = Num.to_i32(x) |> Num.add_wrap (Num.to_i32 y) |> Num.add_wrap(hash + seed * 3746193)
    # h_ = Num.bitwise_xor(x, Num.shift_left_by x 13) |> Num.mul 15731 |> Num.add 789221
    #   |> Num.rem 1000
    #   |> Num.to_f32
    #   |> Num.div 1000
    #   |> Num.sub 0.5
    # dbg "H is ${h_|> Num.to_str} vs ${Inspect.to_str h}"
    when h is
        0 ->  x + y
        1 -> -x + y
        2 ->  x - y
        3 -> -x - y
        4 ->  x
        5 -> -x
        6 ->  y
        7 -> -y
        8 ->  x + 0.5 * y
        9 -> -x + 0.5 * y
        10 -> 0.5 * x - y
        11 -> -0.5 * x - y
        _ -> x + y

# seeded_perlin2d : I32 -> F32, F32 -> F32
seeded_perlin2d = |seed|
  grad_ = grad_seed seed
  |xin, yin|
    perlin2d_ grad_ xin yin

perlin2d : F32, F32 -> F32

perlin2d = |xin, yin|
  perlin2d_ (grad_seed 1) xin yin

perlin2d_ = |grad, xin, yin|
  xi = Num.floor xin |> Num.bitwise_and 255
  yi = Num.floor yin |> Num.bitwise_and 255
  xf = xin - (Num.floor xin |> Num.to_f32)
  yf = yin - (Num.floor yin |> Num.to_f32)
  u = fade xf
  v = fade yf
  aa = List.get perm xi
    |> Result.map_ok |p| Num.to_u64(p + yi)
    |> Result.try |i| List.get perm i
    |> Result.with_default 0
  ab = List.get perm xi
    |> Result.try |p| List.get perm Num.to_u64(p + yi + 1)
    |> Result.with_default 0

  ba = List.get perm (xi + 1)
    |> Result.try |p| List.get perm Num.to_u64(p + yi)
    |> Result.with_default 0

  bb = List.get perm (xi + 1)
    |> Result.try(|p| List.get perm (Num.to_u64(p + yi + 1)))
    |> Result.with_default 0

  x1 = Hex.lerp(grad(aa, xf, yf), grad(ba, (xf - 1.0), yf), u)
  x2 = Hex.lerp(grad(ab, xf, (yf - 1)), grad(bb, (xf - 1.0), (yf - 1)), u)
  Hex.lerp x1 x2 v
