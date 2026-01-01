module [Matrix4, identity, translate, multiply, scale, rotate, value]

Matrix4 := {
    m0: F32, m4: F32, m8: F32, m12: F32,
    m1: F32, m5: F32, m9: F32, m13: F32,
    m2: F32, m6: F32, m10: F32, m14: F32,
    m3: F32, m7: F32, m11: F32, m15: F32,
} implements [
    Eq { is_eq: mat4_eq },

]

Vector2: { x: F32, y: F32 }

mat4_eq : Matrix4, Matrix4 -> Bool
mat4_eq = |@Matrix4 a, @Matrix4 b|
  Num.is_approx_eq a.m0 b.m0 {}
  && Num.is_approx_eq a.m1 b.m1 {}
  && Num.is_approx_eq a.m2 b.m2 {}
  && Num.is_approx_eq a.m3 b.m3 {}
  && Num.is_approx_eq a.m4 b.m4 {}
  && Num.is_approx_eq a.m5 b.m5 {}
  && Num.is_approx_eq a.m6 b.m6 {}
  && Num.is_approx_eq a.m7 b.m7 {}
  && Num.is_approx_eq a.m8 b.m8 {}
  && Num.is_approx_eq a.m9 b.m9 {}
  && Num.is_approx_eq a.m10 b.m10 {}
  && Num.is_approx_eq a.m11 b.m11 {}
  && Num.is_approx_eq a.m12 b.m12 {}
  && Num.is_approx_eq a.m13 b.m13 {}
  && Num.is_approx_eq a.m14 b.m14 {}
  && Num.is_approx_eq a.m15 b.m15 {}



identity_ = {
    m0:1, m4: 0, m8: 0, m12:0,
    m1:0, m5: 1, m9: 0, m13:0,
    m2:0, m6: 0, m10:1, m14:0,
    m3:0, m7: 0, m11:0, m15:1 }
identity : Matrix4
identity = @Matrix4 identity_

# to_list : Matrix4 -> List F32
# to_list = |@Matrix4 { m0, m4, m8, m12, m1, m5, m9, m13, m2, m6, m10, m14, m3, m7, m11, m15 }|
#    [ m0, m4, m8, m12, m1, m5, m9, m13, m2, m6, m10, m14, m3, m7, m11, m15 ]


value = |@Matrix4 record| record

from_list : List F32 -> Matrix4
from_list = |list|
    when list is
    [ m0, m4, m8, m12, m1, m5, m9, m13, m2, m6, m10, m14, m3, m7, m11, m15 ] ->
        @Matrix4 { m0, m4, m8, m12, m1, m5, m9, m13, m2, m6, m10, m14, m3, m7, m11, m15 }
    _ ->
        identity

multiply : Matrix4, Matrix4 -> Matrix4
multiply = |@Matrix4 left, @Matrix4 right|
    @Matrix4 {
      m0: left.m0*right.m0 + left.m1*right.m4 + left.m2*right.m8 + left.m3*right.m12,
      m1: left.m0*right.m1 + left.m1*right.m5 + left.m2*right.m9 + left.m3*right.m13,
      m2: left.m0*right.m2 + left.m1*right.m6 + left.m2*right.m10 + left.m3*right.m14,
      m3: left.m0*right.m3 + left.m1*right.m7 + left.m2*right.m11 + left.m3*right.m15,
      m4: left.m4*right.m0 + left.m5*right.m4 + left.m6*right.m8 + left.m7*right.m12,
      m5: left.m4*right.m1 + left.m5*right.m5 + left.m6*right.m9 + left.m7*right.m13,
      m6: left.m4*right.m2 + left.m5*right.m6 + left.m6*right.m10 + left.m7*right.m14,
      m7: left.m4*right.m3 + left.m5*right.m7 + left.m6*right.m11 + left.m7*right.m15,
      m8: left.m8*right.m0 + left.m9*right.m4 + left.m10*right.m8 + left.m11*right.m12,
      m9: left.m8*right.m1 + left.m9*right.m5 + left.m10*right.m9 + left.m11*right.m13,
      m10: left.m8*right.m2 + left.m9*right.m6 + left.m10*right.m10 + left.m11*right.m14,
      m11: left.m8*right.m3 + left.m9*right.m7 + left.m10*right.m11 + left.m11*right.m15,
      m12: left.m12*right.m0 + left.m13*right.m4 + left.m14*right.m8 + left.m15*right.m12,
      m13: left.m12*right.m1 + left.m13*right.m5 + left.m14*right.m9 + left.m15*right.m13,
      m14: left.m12*right.m2 + left.m13*right.m6 + left.m14*right.m10 + left.m15*right.m14,
      m15: left.m12*right.m3 + left.m13*right.m7 + left.m14*right.m11 + left.m15*right.m15,
  }

scale : Vector2 -> Matrix4
scale = |by|

  @Matrix4 { identity_ &
    m0: by.x,
    m5: by.y,
  }

## m3:0, m7: 0, m11:0, m15:1 }
translate : Vector2 -> Matrix4
translate = |by| @Matrix4 {
  identity_ &
  m12: by.x, m13: by.y, m14: 0,
}

    # m0:1, m4: 0, m8: 0, m12:0,
    # m1:0, m5: 1, m9: 0, m13:0,
    # m2:0, m6: 0, m10:1, m14:0,
    # m3:0, m7: 0, m11:0, m15:1 }
rotate : F32 -> Matrix4
rotate = |radians|
  c = Num.cos radians
  s = Num.sin radians
  @Matrix4 {
    identity_ &
    m0: c,
    m1: s,
    m4: -1 * s,
    m5: c,
  }
diagonals = |@Matrix4 { m0, m5, m10, m15 }|
  (m0, m5, m10, m15)
expect multiply Matrix4.identity Matrix4.identity
  |> |@Matrix4 {m0, m5, m10, m15}|
    List.all [m0, m5, m10, m15] |x| Num.is_approx_eq x 1.0 {}



expect
  identity == identity
  && multiply (translate { x: 2, y: 3 }) identity == (translate { x: 2, y: 3 })
  && multiply identity (translate { x: 2, y: 3 }) == (translate { x: 2, y: 3 })

expect
  r = rotate 0
  r == identity
expect
  actual = translate { x: 2, y: 3 }
  expected = from_list [
    1, 0, 0, 2,
    0, 1, 0, 3,
    0, 0, 1, 0,
    0, 0, 0, 1,
  ]

  expected == actual

expect
  actual = scale { x: 2, y: 3 }
  expected = from_list [
      2, 0, 0, 0
    , 0, 3, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, 1
  ]
  expected == actual

expect
  t = translate { x: 2, y: 3 }
  s = scale { x: 2, y: 3 }
  result = multiply t s
  should_be = from_list [
    2.0, 0, 0, 2,
    0, 3, 0, 3,
    0, 0, 1, 0,
    0, 0, 0, 1
  ]
  dbg "Should be ${Inspect.to_str (diagonals should_be)}"
  dbg "result = ${Inspect.to_str (diagonals result)}"
  result == should_be
