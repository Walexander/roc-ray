app [Model, init!, render!] { rr: platform "../platform/main.roc" }
import rr.RocRay exposing [Texture, Camera]

import rr.RocRay
import rr.Camera
import rr.Draw
import rr.Effect
import rr.Keys

import rr.Texture

width = 512 * 2
height = 512 * 2

Model : { camera: Camera, time_loc: I32, freq_loc: I32, freq: F32, amplitude_loc: I32, amplitude: F32, texture: Texture, shader : Effect.Shader }

center = { x: width / 2, y: height / 2}
init! : {} => Result Model _
init! = |{}|

    RocRay.init_window!({ title: "Basic Shapes", width, height })

    shader = (Effect.load_shader!("examples/assets/vertex-shader.vs",
        "examples/assets/fragment-shader.fs"
        # "examples/assets/raylib/examples/shaders/resources/shaders/glsl100/grayscale.fs"
    ) |> Result.map_err |e| LoadErr "Failed to load shader: ${Inspect.to_str e}")?

    amplitude = 0.333
    texture = Texture.load!("examples/assets/raylib/examples/shaders/resources/plasma.png")?
    time_loc = (Effect.get_shader_location!(shader, "time") |> Result.map_err LoadErr)?
    freq_loc = (Effect.get_shader_location!(shader, "frequency") |> Result.map_err LoadErr)?
    amplitude_loc = (Effect.get_shader_location!(shader, "amplitude") |> Result.map_err LoadErr)?
    camera = Camera.create!({
        zoom: 1.0,
        offset: center,
        target: { x: 0, y: 0 },
        rotation: 0,
    })?

    Effect.set_shader_value! shader amplitude_loc amplitude
    freq = 2.0
    Effect.set_shader_value! shader freq_loc freq

    Ok({ shader , texture, camera, time_loc, amplitude_loc, amplitude, freq_loc, freq })

render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|
    game_time = (pf.timestamp.render_start - pf.timestamp.init_start) |> Num.to_f32 |> Num.div 1e3
    Effect.set_shader_value! model.shader model.time_loc game_time
    freq = if Keys.pressed(pf.keys, KeyUp) then
        model.freq + 0.05
    else if Keys.pressed(pf.keys, KeyDown) then
        model.freq - 0.05
    else model.freq

    amplitude = if Keys.pressed(pf.keys, KeyLeft) then
        model.amplitude - 0.05
    else if Keys.pressed(pf.keys, KeyRight) then
        model.amplitude + 0.05
    else
        model.amplitude

    Effect.set_shader_value! model.shader model.freq_loc model.freq
    Effect.set_shader_value! model.shader model.amplitude_loc model.amplitude

    Draw.draw!(
        White,
        |{}|
            Draw.text! {
                text: "Frequency = ${Inspect.to_str freq}\nAmp = ${Inspect.to_str amplitude}",
                size: 16,
                pos: { x: 20, y: 20 },
                color: Black,

            }
            Draw.with_mode_2d! model.camera |{}|
                draw_center_texture! model.texture {x: width / -2, y: 0} (width /2) (height / 2)
                Draw.with_mode_shader!(
                    model.shader,
                    |{}|
                        draw_center_texture! model.texture {x: 0, y: height / -2} (width /2) (height / 2)
                )
    )
    Ok({ model & freq, amplitude })

draw_center_texture! = |texture, pos, width_, height_|

    Draw.texture_rec! {
        pos,
        texture: texture,
        source: { x: 0, y: 0, width: width_, height: height_ },
        tint: White,
    }
