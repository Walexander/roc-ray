app [Model, init!, render!] { rr: platform "../platform/main.roc" }
import rr.RocRay exposing [Texture, Camera]

import rr.RocRay
import rr.Camera
import rr.Draw
import rr.RenderTexture
import rr.Keys
import rr.Shader exposing []
import rr.Mouse

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
    paused: Bool,
    marker_pos: RocRay.Vector2,
    fog_shader: Shader.RenderShader,
    scale_shader: Shader.RenderShader,
    fog: RocRay.RenderTexture,
    animation_frames: U32,
}

center = { x: width / 2, y: height / 2 }
init! : {} => Result Model _
init! = |{}|

    RocRay.set_target_fps! 60
    RocRay.display_fps! { fps: Visible, pos: { x: 10, y: 10 } }
    RocRay.init_window!({ title: "Basic Shapes", width, height })

    fog_shader = Shader.new!(
        "examples/assets/vertex-shader.vs",
        "examples/assets/fragment-shader.fs",
        ["mvp", "time", "frequency", "amplitude"]
    )?

    amplitude = 10.333
    freq = 2.0

    texture = Texture.load!("examples/assets/plasma.png")?

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

    Ok(
        {
            marker_pos: { x: 0, y: 0 },
            fog_shader,
            scale_shader: Shader.new!("examples/assets/scale.vert", "examples/assets/fragment-noop.fs", ["time", "max", "center"])?,
            texture,
            camera,
            freq,
            fog: fog_texture,
            amplitude,
            animation_frames: 0,
            paused: Bool.false,
        }
    )

lerp : F32, F32, F32 -> F32
lerp = |from, to, t|
    from + (to - from) * t
render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|
    game_time = (pf.timestamp.render_start - pf.timestamp.init_start) |> Num.to_f32 |> Num.div 1e3


    marker_pos =
        if Mouse.pressed(pf.mouse.buttons.left) then
            RocRay.get_screen_to_world_2d! pf.mouse.position model.camera
        else model.marker_pos

    _ = if Bool.not(model.paused) then
        Shader.set_f32!(model.fog_shader, "time", game_time)
        |> \_ -> {}
    else {}

    animation_frames =
        if Mouse.pressed(pf.mouse.buttons.left) then 0
        else if Bool.not(model.paused) then
            model.animation_frames + 1
        else model.animation_frames

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
    iteration_duration = (60 * 2.5) |> Num.floor
    duration_f = Num.to_f32 iteration_duration

    rounds = animation_frames |> Num.to_f32 |> Num.div duration_f
    t = Num.min rounds 1.0
    Draw.draw!(
        Teal,
        |{}|
            Draw.with_mode_2d! model.camera |{}|
                _ = Shader.set_f32!(model.fog_shader, "amplitude", model.amplitude)
                    |> Shader.set_f32!("frequency", model.freq)
                Draw.with_mode_shader!(
                    model.fog_shader.shader,
                    |{}|
                        draw_center_texture! model.texture { x: -width / 2, y: height / -2 } (width) (height)
                    )
                # iterations = pf.frame_count |> Num.to_f32 |> Num.div 60 * 5 |> Num.floor
                # dbg "t = ${Inspect.to_str t}"
                Draw.with_mode_shader! model.scale_shader.shader |{}|
                    Shader.set_f32! model.scale_shader "time" t
                    |> Shader.set_f32! "max" 3.0
                    |> Shader.set_vec2! "center" marker_pos
                    |> \_ -> ({})
                    Draw.ring! {
                        center: marker_pos,
                        inner: 12,
                        outer: 14,
                        start: 90,
                        end: 360 + 90,
                        segments: 6,
                        color: RocRay.fade(Black, 0.8)
                    }
            text =
                """
                [Up Dn] Frequency = ${Inspect.to_str freq}
                [L R] Amp = ${Inspect.to_str amplitude}
                [Enter]Paused = ${Inspect.to_str model.paused}
                Center = ${Inspect.to_str marker_pos}
                """
            Draw.text! {
                size: 16,
                text,
                pos: { x: 20, y: 20 },
                color: White,
            }

    )
    Ok({ model & freq, amplitude,
            marker_pos,
            paused: if Keys.pressed(pf.keys, KeyEnter) then Bool.not model.paused else model.paused,
            animation_frames
    })

draw_center_texture! = |texture, pos, width_, height_|

    Draw.texture_rec! {
        pos,
        texture: texture,
        source: { x: 0, y: 0, width: width_, height: height_ },
        tint: Teal,
    }
