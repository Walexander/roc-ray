app [Model, init!, render!] { rr: platform "../platform/main.roc" }
import rr.RocRay exposing [Texture, Camera]

import rr.RocRay
import rr.Camera
import rr.Draw
import rr.Effect
import rr.RenderTexture
import rr.Keys
import rr.Shader exposing [ShaderLocation]

import rr.Texture

width = 512
height = 512
col_count = 65
row_count = 65
render_width = col_count
render_height =  row_count

Model : {
    camera : Camera,
    freq : F32,
    amplitude : F32,
    texture : Texture,
    shader : Effect.Shader,
    paused: Bool,
    fog: RocRay.RenderTexture,
    locations : {
        mvp : ShaderLocation,
        time : ShaderLocation,
        frequency : ShaderLocation,
        amplitude : ShaderLocation,
    },
}

center = { x: width / 2, y: height / 2 }
init! : {} => Result Model _
init! = |{}|

    RocRay.init_window!({ title: "Basic Shapes", width, height })

    shader = (
        Effect.load_shader!(
            "examples/assets/vertex-shader.vs",
            "examples/assets/fragment-shader.fs",
            # "examples/assets/raylib/examples/shaders/resources/shaders/glsl100/grayscale.fs"
        )
        |> Result.map_err |e| LoadErr "Failed to load shader: ${Inspect.to_str e}"
    )?

    amplitude = 10.333
    freq = 2.0

    texture = Texture.load!("examples/assets/raylib/examples/shaders/resources/plasma.png")?

    fog_texture = RenderTexture.create!({ width: render_width, height: render_height })?

    RenderTexture.set_render_texture_filter!(fog_texture, Bilinear)

    format = Texture.get_format! texture
    dbg "Texture format is ${Inspect.to_str format}"

    camera = Camera.create!(
        {
            zoom: 2,
            offset: center,
            target: { x: 0, y: 0 },
            rotation: 0,
        },
    )?

    locations = {
        mvp: Shader.get_location!(shader, "mvp")?,
        time: Shader.get_location!(shader, "time")?,
        amplitude: Shader.get_location!(shader, "amplitude")?,
        frequency: Shader.get_location!(shader, "frequency")?,
    }

    Shader.set_value! shader locations.amplitude amplitude
    Shader.set_value! shader locations.frequency freq

    matrix = Camera.to_matrix! camera
    Shader.set_value_matrix! shader locations.mvp matrix

    Ok(
        {
            shader,
            texture,
            camera,
            freq,
            fog: fog_texture,
            amplitude,
            locations,
            paused: Bool.false,
        },
    )

lerp : F32, F32, F32 -> F32
lerp = |from, to, t|
    from + (to - from) * t
render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|
    game_time = (pf.timestamp.render_start - pf.timestamp.init_start) |> Num.to_f32 |> Num.div 1e3
    if Bool.not(model.paused) then
        Shader.set_value! model.shader model.locations.time game_time
    else {}

    freq =
        (
            if Keys.down(pf.keys, KeyUp) then
                model.freq + 0.05
            else if Keys.down(pf.keys, KeyDown) then
                model.freq - 0.05
            else
                model.freq
        )
        |> Num.max 0

    amplitude =
        (

            if Keys.down(pf.keys, KeyLeft) then
                model.amplitude - 0.05
            else if Keys.down(pf.keys, KeyRight) then
                model.amplitude + 0.05
            else if Keys.pressed(pf.keys, KeySpace) then
                model.amplitude + 10
            else
                model.amplitude
        )
        |> |a| if Bool.not(model.paused) then lerp a 0 (1/ 60) else a
        |> Num.max 0.01

    Shader.set_value! model.shader model.locations.frequency model.freq
    Shader.set_value! model.shader model.locations.amplitude model.amplitude

    # right_half = Num.to_f32 col_count |> Num.div 2 |> Num.floor
    # _ = Draw.with_texture! model.fog Clear |{}|
    #     List.range  { start: At(0), end: Before(col_count) }
    #     |> List.join_map |x|
    #         List.range { start: At(0), end: Before(row_count) }
    #         |> List.map |y| (x, y)
    #     # |> List.drop_if |(x, y)| x > 8 && y > 8
    #     |> List.for_each! |(x, y)|
    #         color = if x > right_half && y > right_half then Clear else if x < y  then RocRay.fade(Black, 0.95) else RocRay.fade(Black, 0.85)
    #         Draw.rectangle! {rect: { x: Num.to_f32 x, y: Num.to_f32 y, width: 1, height: 1 }, color }

    Draw.draw!(
        Teal,
        |{}|
            Draw.with_mode_2d! model.camera |{}|
                Draw.with_mode_shader!(
                    model.shader,
                    |{}|
                        draw_center_texture! model.texture { x: -width / 2, y: height / -2 } (width) (height)
                    )
                # Draw.render_texture_pro!({
                #     texture: model.fog,
                #     source: { width: Num.to_f32(render_width), height: Num.to_f32(render_height), x: 0, y: 0 },
                #     dest: { width, height, x: width / -2, y: height / -2 },
                #     origin: { x: 0, y: 0 },
                #     rotation: 0,
                #     tint: White })
                # Draw.poly! {
                #     center: { x: 0, y: 00 },
                #     sides: 6,
                #     radius: 25,
                #     rotation: 90,
                #     color: Blue,
                # }
            # draw_center_texture! model.texture { x: width / -2, y: 0 } (width) (height / 2)
            Draw.text! {
                text: "[Up Dn] Frequency = ${Inspect.to_str freq}\n[L R] Amp = ${Inspect.to_str amplitude}\n[Enter]Paused = ${Inspect.to_str model.paused}",
                size: 16,
                pos: { x: 20, y: 20 },
                color: White,
            }

            # Draw.render_texture_rec!({ texture: model.fog, source: { width, height, x: 0, y: 0 }, pos: { x: 0, y: 0 }, tint: (RGBA 128 128 128 128) })
    )
    Ok({ model & freq, amplitude, paused: if Keys.pressed(pf.keys, KeyEnter) then Bool.not model.paused else model.paused })

draw_center_texture! = |texture, pos, width_, height_|

    Draw.texture_rec! {
        pos,
        texture: texture,
        source: { x: 0, y: 0, width: width_, height: height_ },
        tint: Teal,
    }
