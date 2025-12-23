module [
    Shader,
    ShaderLocation,
    load!,
    get_location!,
    set_value!,
    set_value_vec2!,
    set_value_matrix!,
]

import InternalMatrix  exposing [ Matrix ]
import InternalVector
import Effect

Shader : Effect.Shader

ShaderLocation := { loc: I32 }
# ShaderLocation := { loc: I32 }

## Sleep the main thread for a given number of milliseconds.
load! : Str, Str => Result Shader [ LoadErr Str ]_
load! = |vertex, fragment|
    Effect.load_shader! vertex fragment
    |> Result.map_err(LoadErr)

get_location! : Shader, Str => Result ShaderLocation [LoadErr Str]_
get_location! = |shader, identifier|
    Effect.get_shader_location! shader identifier
    |> Result.map_ok |loc| @ShaderLocation({ loc })
    |> Result.map_err LoadErr

set_value! : Shader, ShaderLocation, F32 => {}
set_value! = |shader, @ShaderLocation { loc }, value|
    Effect.set_shader_value! shader loc value

set_value_vec2! : Shader, ShaderLocation, { x: F32, y: F32 } => {}
set_value_vec2! = |shader, @ShaderLocation { loc }, {x, y}|
    Effect.set_shader_value_vec2! shader loc InternalVector.from_xy(x, y)

set_value_matrix! : Shader, ShaderLocation,  Matrix => {}
set_value_matrix! = |shader, @ShaderLocation { loc }, matrix|
    matrix_ = InternalMatrix.from_matrix(matrix)
    Effect.set_shader_value_matrix! shader loc matrix_
