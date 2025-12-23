app [Model, init!, render!] { rr: platform "../platform/main.roc" }

import rr.RocRay
import rr.Camera
import rr.Draw
import rr.Effect
import rr.RenderTexture
import rr.Keys
import rr.Shader

import rr.Texture

width = 512
height = 512
col_count = 65
row_count = 65
render_width = col_count
render_height =  row_count

Model : {
    shader : Effect.Shader,
    offset_loc: Shader.ShaderLocation,
    direction: [Right, Left],
    offset: { x: F32, y: F32 },
}

center = { x: width / 2, y: height / 2 }
init! : {} => Result Model _
init! = |{}|

    RocRay.init_window!({ title: "Basic Shapes", width, height })
    shader = (
        Shader.load!(
            "examples/assets/uniform-test.vs",
            "examples/assets/fragment-noop.fs",
        )
    )?

    Ok(
        {
            shader,
            offset_loc: Shader.get_location!(shader, "u_offset")?,
            direction: Right,
            offset: { x: 0, y: 0 }
        },
    )
render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|


    _ = if pf.frame_count % 60 == 0 then
        dbg "offset is ${Inspect.to_str model.offset}"
    else
            ""
    Draw.draw!(
        Teal,
        |{}|
            Draw.with_mode_shader!(
                model.shader,
                |{}|
                    Shader.set_value_vec2!(model.shader, model.offset_loc, model.offset)
                    Draw.rectangle! {
                        color: White,
                        rect: {
                            x: 0, y: height / 2 - 75, width: 100, height: 150,
                        }
                    }
                )

            # Draw.render_texture_rec!({ texture: model.fog, source: { width, height, x: 0, y: 0 }, pos: { x: 0, y: 0 }, tint: (RGBA 128 128 128 128) })
    )
    game_time = (pf.timestamp.render_start - pf.timestamp.init_start) |> Num.to_f32 |> Num.div 1e3
    Ok({ model & offset: {
                x: when model.direction is
                    Right -> model.offset.x + 1
                    Left -> model.offset.x - 1,
                y: 40 * Num.sin(15 - (game_time * 4.0)),
            },

            direction: when model.direction is
                Right if model.offset.x >= (width - 100) -> Left
                Left if model.offset.x <= 0 -> Right
                _ -> model.direction,

        })

