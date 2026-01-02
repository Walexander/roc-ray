## a dissolve animation driven by perlin noise image
app [Model, init!, render!] { rr: platform "../platform/main.roc" }
import rr.RocRay exposing [Texture]
import rr.Draw
import rr.Keys
import rr.Shader
import rr.Mouse
import rr.Texture

width = 512 + 64 + 16
height = 512 + 64

Model : {
    noise_texture: Texture,
    animation_frames: U64,
    plasma: Texture,
    shader: Shader.RenderShader,
    paused: Bool,
    scale: F32,
    direction: [Forward, Reverse]
}

noise_dims = { x: 64, y: 64 }
init! : {} => Result Model _
init! = |{}|
    RocRay.set_target_fps! 60
    RocRay.display_fps! { fps: Visible, pos: { x: 10, y: 10 } }
    RocRay.init_window!({ title: "Basic Image", width, height })
    scale = 1.125
    plasma = Texture.load!("examples/assets/plasma.png")?
    noise_image = RocRay.gen_image_perlin_noise!(noise_dims, {x: 0, y: 0}, scale)?
    noise_texture = Texture.from_image!(noise_image)?
    Ok({noise_texture, plasma, animation_frames: 0, paused: Bool.false,
        shader: Shader.new!("examples/assets/vertex-shader.vs", "examples/assets/shaders/dissolve.frag",
            ["progress", "softness", "noiseTex"])?,
        direction: Forward,
        scale,
    })

render! : Model, RocRay.PlatformState => Result Model [LoadErr Str]
render! = |model, pf|
    animation_frames =
        if Mouse.pressed(pf.mouse.buttons.left) then 0
        else if Bool.not(model.paused) then
            model.animation_frames + 1
        else model.animation_frames

    animation_time = 60 * 2.5
    base_progress = animation_frames % Num.floor(animation_time) |> Num.to_f32 |> Num.div animation_time
    progress = if model.direction == Forward then base_progress else 1 - base_progress
    direction =
        if animation_frames == 0 then Forward
        else if model.direction == Forward && base_progress >= 0.99  then Reverse
        else if model.direction == Reverse && base_progress >= 0.99 then
            Forward
        else model.direction

    scale = if Keys.pressed pf.keys KeySpace then
        # scale_ = RocRay.random_i32! { min: 10, max: 1000 } |> Num.to_f32 |> Num.div 1000 |> Num.mul 15
        scale_ = model.scale
        image = RocRay.gen_image_perlin_noise!(noise_dims, {x: Num.to_f32 animation_frames, y: 0}, scale_)
        when image is
            Ok img ->
                Texture.update_from_image! model.noise_texture img
                scale_
            _ -> model.scale
    else model.scale
    Draw.draw!(
        RGBA 192 192 192 255,
        |{}|
            Draw.texture_pro! {
                texture: model.noise_texture,
                dest: { x: 32, y: 32, width: width - 64, height: height - 64},
                origin: { x: 0, y: 0 },
                source: {
                    width: 64,
                    height: 64,
                    x: 0,
                    y: 0
                },
                rotation: 0,
                tint: White
            }
            Draw.text! {
                text: scale |> Num.mul 100 |> Num.round |> Num.to_f32 |> Num.div 100 |> Inspect.to_str,
                size: 16,
                pos: {x: width - 64, y: 8 },
                color: Green,
            }
            Draw.with_mode_shader! model.shader.shader |{}|
                Shader.set_f32! model.shader "progress" progress
                |> Shader.set_f32! "softness" 0.02
                |> Shader.set_texture! "noiseTex" model.noise_texture
                |> \_ -> {}

                Draw.texture_pro! {
                    texture: model.plasma,
                    origin: { x: 0, y: 0 },
                    dest: {
                        x: 32,
                        y: 32,
                        width: width - 64,
                        height: height - 64,
                    },
                    source: {
                        width: 512,
                        height: 512,
                        x: 0, y: 0,
                    },
                    rotation: 0,
                    tint: White,
                }
    )
    Ok({ model &
        paused: if Keys.pressed(pf.keys, KeyEnter) then Bool.not model.paused else model.paused,
        direction,
        animation_frames,
        scale,
    })

