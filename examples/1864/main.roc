app [Model, init!, render!] {
    rr: platform "../../platform/main.roc",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
}
import Matrix4
# import Utils exposing [frameCountToSeconds]
import Unit exposing [Unit]
import GameActions
import rand.Random
# import Assets
import Hex
import PointyHex
import AI
import Health
import HexTile

import Noise
import rr.Draw
import rr.RocRay exposing [ PlatformState ]
import rr.InternalVector
import rr.RenderTexture
import rr.Effect
import rr.Shader
import rr.Keys
import rr.Camera
import rr.Mouse
import rr.Sound
import rr.Texture

import Model exposing [ YearOfDecision ]
import Movement
import Trauma
import LaunchStatus
import LaunchCountdown
import Animation
import Particle


Model : YearOfDecision

screen = {
    width: 1024,
    height: 768
}

base_camera = {
    target: { x: 0.0, y: 0 },
    offset: {x: screen.width / 2, y: screen.height / 2 },
    zoom: 1.125,
    rotation: 0
}
fog_scale = 0.25
init! : {} => Result YearOfDecision _
init! = |{}|
    RocRay.set_target_fps! 60
    RocRay.display_fps! { fps: Visible, pos: { x: 10, y: 10 } }
    RocRay.init_window! { width: screen.width, height: screen.height, title: "Silly Things" }

    camera = Camera.create!(base_camera)?

    hexTexture = Texture.load!("examples/1864/assets/kenney-tiles.png")?
    top_tiles = Texture.load!("examples/1864/assets/topTiles.png")?
    units = Texture.load!("examples/1864/assets/aliens.png")?

    power_up = Sound.load!("examples/1864/assets/power-up.mp3")?
    power_down = Sound.load!("examples/1864/assets/power-down.mp3")?
    ok =  Sound.load!("examples/1864/assets/yessir.mp3")?
    horse = Sound.load!("examples/1864/assets/horse.mp3")?
    wagon = Sound.load!("examples/1864/assets/wagon.mp3")?

    render_textures = {
        fog: RenderTexture.create!({
            width: screen.width * fog_scale,
            height: screen.height * fog_scale,
        })?
    }
    shaders = {
        fog: Shader.new!("examples/1864/assets/shaders/default.vs",
            "examples/1864/assets/shaders/fog-blur.frag",
            ["texelSize", "blurStrength"])?,
        # ring: Shader.new!("examples/1864/assets/shaders/debug.vs", "examples/1864/assets/shaders/ring-select.fs",
        #     ["time", "center", "duration", "thickness", "color", "center"]
        # )?,
        particle: Shader.new!(
            "examples/1864/assets/shaders/default.vs",
            "examples/1864/assets/shaders/explosion.frag",
            [ "u_time", "u_duration",]
        )?,
        ring: Shader.new!("examples/1864/assets/shaders/scale.vert",
            "examples/1864/assets/shaders/noop.frag",
            ["time", "max", "center"]
        )?,
    }
    RenderTexture.set_render_texture_filter! render_textures.fog Bilinear
    image = RocRay.gen_image_color!(1, 1, White)?
    textures = {
            empty: Texture.from_image!(image)?,
            units,
            top_tiles,
            full_tiles: hexTexture
        }
    baseState : YearOfDecision
    baseState = Model.initialize({
        camera,
        seed: Effect.random_i32! 1 10_000,
        textures,
        render_textures,
        shaders,
        sounds: { power_up, power_down, ok, horse, wagon },
        base_camera
    })
    Ok baseState

render! : YearOfDecision, RocRay.PlatformState => Result YearOfDecision []
render! = |model, pf|
    dt = pf.timestamp.last_render_end - pf.timestamp.last_render_start

    isOccupied = |cube| List.contains (model.units |> List.map .cell) cube

    path_finder = HexTile.make_path_finder model.map isOccupied

    unit_from_cell = |cell| List.find_first model.units |u| u.cell == cell
    Camera.update! model.camera model.base_camera
    mouse_world = RocRay.get_screen_to_world_2d! pf.mouse.position model.camera
    hover_cell = PointyHex.pixel_to_hex mouse_world

    mouseCell2 = if HexTile.is_in_bounds model.map (hover_cell) then
        hover_cell
    else
        model.hoverCell

    selectedCell = if Mouse.released pf.mouse.buttons.left then
        mouseCell2
    else
        model.selectedCell

    rand = Random.step model.rand Random.bounded_u32(0, 1_000_000)
    player_move_ = GameActions.inputs_to_move(
        model,
        isOccupied,
        unit_from_cell,
        mouse_world,
        pf.keys,
        pf.mouse.buttons
    )
    summary_unit = List.find_first(model.units, |unit| unit.id == model.selectedIndex)
        |> Result.on_err |_| List.first model.units
        |> |result|
            when result is
                Ok u -> u
                Err _ -> crash "must have non empty unit list"

    world: YearOfDecision
    world = { model& game_time: model.game_time + dt, rand: rand.state }
        |> Trauma.process_trauma player_move_
        |> LaunchStatus.update
        |> LaunchCountdown.update(Num.to_u32 dt)
        |> Particle.update dt path_finder HexTile.get_movement_cost(model.map)
        |> Movement.update(dt, isOccupied, path_finder)
        |> GameActions.update!(player_move_, path_finder)
        |> Health.update
        |> |world_|
            if world_.game_time >= 3_500 then
                AI.update world_ path_finder isOccupied
            else
                world_
        |> |world_|  { world_ & hoverCell: mouseCell2, selectedCell }
        |> |world_| { world_ &
            glowing:
                when player_move_ is
                    SelectUnit unit_index ->
                        List.find_first(model.units, |{id}| id == unit_index)
                        |> Result.map_ok |unit| Following (unit, Animation.make {
                            start_time: pf.timestamp.last_render_end,
                            duration: 2.5,
                        })
                        |> Result.with_default None
                    MoveUnit { to } -> Running (to, Animation.make {
                        start_time: pf.timestamp.last_render_end,
                        duration: 1.5,
                    })
                    _ -> when world_.glowing is
                        Running (cell, anim) ->
                            if (anim.finished) then
                                None
                            else
                                processed = Animation.process anim dt
                                Running (cell, processed)
                        Following (unit, anim) ->
                            u_ = List.find_first(model.units, |{id}| id == unit.id)
                                |> Result.with_default summary_unit
                            if anim.finished then None
                            else Following (u_, Animation.process anim dt)
                        None -> None

        }
    visible_cells = model.units
        |> List.drop_if |u| u.army == Confederates
        |> List.join_map |unit| PointyHex.neighbors unit.cell |> List.append unit.cell
    countdown_pct = Num.max(0, 1 - (Num.to_f32 model.countdown / Num.to_f32 model.countdown_start))
    (new_settings, intensity) = update_camera model pf

    debugText =
        """
            Random: ${rand.value |> Num.to_f32 |> Num.div 1_000_000 |> Num.to_str}
            ECS Size: ${model.ecs.current_size |> Num.to_str}
            Game Time: ${Inspect.to_str model.game_time}
            Mouse to World: ( $(Num.round mouse_world.x|>Num.to_str), ${ Num.round mouse_world.y |> Num.to_str } )
            Hover Cell: ${ Inspect.to_str hover_cell }
            Hover Cell pixel: ${ Inspect.to_str(PointyHex.hex_to_pixel hover_cell) }
            Countdown = ${model.countdown |> Num.to_str}
            Pct=${countdown_pct |> Num.mul 100 |> to_fixed 0}%,
            Seed = ${model.seed |> Num.to_str};
            A=${intensity|>to_fixed 3}
            Trauma=${model.trauma |> to_fixed 3}
        """

    bg_color = RGBA 64 64 64 255
    debug_mode = Keys.down pf.keys KeyLeftControl
    Draw.draw! bg_color |{}|
        render_map! world visible_cells new_settings debug_mode
        render_game! world  pf path_finder
        if Keys.down pf.keys KeyLeftShift then
            render_trauma_bar! world.trauma intensity
        else {}

        if debug_mode then
            render_debug! world  pf.mouse.position debugText
        else {}

    render_sound! world player_move_

    if world.launch_state != model.launch_state then
        when world.launch_state is
            InControl _ -> Sound.play! world.sounds.power_up
            Stalemate -> Sound.play! world.sounds.power_down
    else {}


    Ok world

render_map! = |model, visible_cells, new_settings, show_fog|

    Camera.update!(model.camera, {
        new_settings &
        offset: {
            x: screen.width / 2 * fog_scale,
            y: screen.height / 2 * fog_scale,
        },
        zoom: fog_scale * model.base_camera.zoom, })


    Draw.with_texture! model.render_textures.fog RocRay.fade(Black, 0.5) |{}|
        Draw.with_mode_2d! model.camera  |{}|
            List.drop_if model.units \u -> u.army == Confederates
            |> List.for_each! |unit|
               Draw.circle! { center: unit.position, radius: 64, color: White }

            render_map_fn! model.map |pad, tile_type|
                center = PointyHex.hex_to_pixel  pad.cell
                when tile_type is
                    Normal if List.contains visible_cells pad.cell ->
                        Draw.triangle_fan! PointyHex.points(center) RocRay.fade(White, 1.0)
                    _ -> {} #Draw.triangle_fan! PointyHex.points(center) RocRay.fade(Black, 1.0)
    Camera.update!(model.camera, new_settings)
    Draw.with_mode_2d!(
        model.camera,
        |{}|
           render_map_fn! model.map |tile, tile_type|
                tx_pos = HexTile.texture_position tile 65 89
                tint = when tile_type is
                    LaunchPad|Normal -> White
                    OutOfBounds -> Black
                drawHex! tile.cell model.hexTexture tx_pos tint
    )
    ## For some reason, rendering this with the camera is **slow**
    if show_fog then render_fog! model else {}

## TODO: scissor the top of this so it doesn't sit above our FPS indicator
fog_texel_size = { x: 1/( fog_scale * Num.to_f32(screen.width)), y: 1/(fog_scale * Num.to_f32(screen.height)) }
render_fog! = |model|
    Draw.with_blend_mode! Multiplied |{}|
        Draw.with_mode_shader! model.shaders.fog.shader |{}|
            Shader.set_vec2! model.shaders.fog "texelSize" fog_texel_size
                |> Shader.set_f32! "blurStrength" 3.5
                |> \_ -> {}
            Draw.render_texture_pro!({
                texture: model.render_textures.fog,
                source:  {
                    width: screen.width * fog_scale,
                    height: screen.height * fog_scale * -1,
                    x: 0,
                    y: 0,
                },
                dest: {
                    width: screen.width,
                    height: screen.height,
                    x: 0,
                    y: 0,
                },
                origin: { x: 0, y: 0 },
                rotation: 0,
                tint: White })

render_debug! = |model, mouse_pos, debug_text|
    unit_finder = |id| |u| u.id == id
    Draw.circle! {
        center: mouse_pos,
        color: Red,
        radius: 5,
    }

    summary_unit_ = List.find_first(model.units, unit_finder model.selectedIndex)
    summary_unit = summary_unit_
        |> Result.on_err |_| List.first model.units
        |> |result|
            when result is
                Ok u -> u
                Err _ -> crash "must have non empty unit list"

    summary_text =
    """
    Unit Summary: ${ Unit.summary summary_unit [] }
    """
    summary_text_color = if summary_unit.army == Confederates then Red else Silver
    summary_text_dims =
        Effect.measure_text! summary_text 24 1 |> InternalVector.to_vector2
    Draw.text! { pos: { x: 128+10, y: screen.height - (Num.to_f32 summary_text_dims.y + 24) }, text: summary_text, size: 24, color: summary_text_color }
    debug_text_dims = Effect.measure_text! debug_text 16 1 |> InternalVector.to_vector2

    debug_pos = {
        x: screen.width - (Num.to_f32 debug_text_dims.x) - 48,
        y: screen.height - (Num.to_f32 debug_text_dims.y) - 24
    }
    Draw.rectangle! {
        rect: {
            x: debug_pos.x - 4,
            y: debug_pos.y - 4,
            width: debug_text_dims.x + 12,
            height: debug_text_dims.y + 8,
        },
        color: RocRay.fade(Black, 0.75),
    }
    Draw.text! { pos: debug_pos, text: debug_text, size: 16, color: White }
render_particle_debug! = |{x, y, scale, dead_frame, lifetime}|
    debug_text =
            """
            x: ${x |> Num.round |> Num.to_f32 |> Inspect.to_str}
            y: ${y |>  Num.round |> Num.to_f32 |> Inspect.to_str}
            s: ${scale |> Num.round |> Num.to_f32 |> Inspect.to_str}
            dead:  ${dead_frame |> Inspect.to_str}
            """
    dims = RocRay.measure_text! { size: 16, text: debug_text, spacing: 4 }
    t = Num.to_f32 dead_frame |> Num.div (Num.to_f32 lifetime)
    rect = {
        x: x,
        y: y - dims.y * 2,
        width: dims.x,
        height: dims.y + 16
    }
    Draw.rectangle! {
        rect,
        color: RGBA 128 128 128 255
    }
    Draw.text! {
        text: debug_text,
        size: 16,
        pos: { x: rect.x + 8, y: rect.y + 8},
        color: White,
    }
    Draw.line_ex! {
        end: { x, y },
        start: { y: rect.height + rect.y, x: rect.x + dims.x / 2 },
        thickness: 4,
        color: Green
    }
    Draw.circle! { center: { x, y }, radius: 4, color: RocRay.fade(Red, 0.6) }

render_particles! = |ecs, shader, texture, debug|
    Draw.with_blend_mode! Alpha |{}|
            Particle.get_by_component ecs [Position, Killable, Graphics]
            |> Dict.values
            |> List.for_each! |c| when c is
                [
                    Position {x, y},
                    Killable { dead_frame, lifetime },
                    Graphics { radius, rotation }
                ] ->
                    # Matrix4.value xform.transform
                    scale = Num.max(24, 10 * radius) |> Num.min 128
                    if debug then
                        render_particle_debug! { x, y, scale, lifetime, dead_frame }
                    else {}
                    Draw.with_mode_shader! shader.shader |{}|
                        Shader.set_f32! shader "u_duration" Num.to_f32(lifetime)
                            |> Shader.set_f32! "u_time" Num.to_f32(lifetime - dead_frame)
                            |> \_ -> {}

                        Draw.texture_pro! {
                            texture,
                            # origin: { x: 0.5, y: 0.5 },
                            origin: { x: 0, y: 0 },
                            rotation,
                            dest: {
                                width: scale, height: scale,
                                x: x - scale / 2,
                                y: y - scale / 2,
                            },
                            source: {
                                x: 0, y: 0, width: 1, height: 1,
                            },
                            tint: RocRay.fade(Black, 1.0)
                        }
                _ -> {}
    # Dict.map ecs.positionable |id, { x, y }|
    #     graphic = Dict.get ecs.scalable id
    #         |> Result.with_default { radius: 0, color: White, rotation: 0 }
    #     {
    #         x, y,
    #         rotation: 0,
    #         radius: graphic.radius,
    #         color: graphic.color
    #     }
    # |> Dict.values
    # |> List.for_each! |{x, y, radius, color}|
    #     Draw.circle! {center: { x, y }, radius: radius * 10, color }


render_hex_scaled! = |cell, color|
    Draw.triangle_fan! PointyHex.points_for(cell) color

render_hex_outline! = |center, color, thickness|
    drawPath! PointyHex.points(center) color thickness Bool.true

renderHexOutline! = \cell, color ->
    center = (PointyHex.hex_to_pixel cell)
    drawPath! PointyHex.points(center) color 3 Bool.true


to_fixed = |num, precision|
    mul = Num.pow 10 precision
    num |> Num.to_f32 |> Num.mul mul |> Num.round |> Num.to_f32 |> Num.div mul |> Num.to_str

# render_game! : YearOfDecision, PlatformState, _ -> _
render_game! : YearOfDecision, PlatformState, _ => _
render_game! = |model, pf, path_finder|
    summary_unit = List.find_first(model.units, |unit| unit.id == model.selectedIndex)
        |> Result.on_err |_| List.first model.units
        |> |result|
            when result is
                Ok u -> u
                Err _ -> crash "must have non empty unit list"

    # cubePath = path_finder(PointyHex.pixel_to_hex(summary_unit.position), model.hoverCell)


    planning_mode = Keys.down pf.keys KeyLeftControl
    debug_mode = Keys.down pf.keys KeyLeftShift
    unitPath_ =
        summary_unit.lastPath
        |> List.map PointyHex.hex_to_pixel
        |> List.drop_first 1
        |> List.prepend (summary_unit.position)
    Draw.with_mode_2d!(
        model.camera,
        |{}|
           render_launch_pads! model
           countdown_color = when model.launch_state is
                Stalemate -> Green
                InControl Union -> Blue
                InControl Confederates -> Red
           drawPath! unitPath_ White 5 Bool.false
           render_glow! model summary_unit
           render_units! model summary_unit
           renderHexOutline! model.hoverCell White

           if planning_mode then
                cube_path = path_finder summary_unit.cell model.hoverCell
                straightLine = cube_path |> List.map |point| PointyHex.hex_to_pixel point
                drawPath! straightLine RGBA(200, 200, 200, 255) 5 Bool.false
           else
                {}

           render_countdown!(model.countdown, countdown_color)
           render_particles! model.ecs model.shaders.particle model.textures.empty debug_mode
           render_system! model.ecs
    )
                    # )
    {}

render_sound! = |model, player_move|
    when player_move is
        MoveUnit {unit_index} ->
            List.find_first model.units |u| u.id == unit_index
            |> Result.map_ok |{type}|
                when type is
                    Cavalry -> model.sounds.horse
                    Infantry -> model.sounds.ok
                    Artillery -> model.sounds.wagon
            |> Result.with_default model.sounds.ok
            |> Sound.play!
        _ -> {}

update_camera = |model, _|
    old_settings = model.base_camera
    intensity = model.trauma * model.trauma * model.trauma
    amplitude = 5.0 * intensity
    frequency = model.game_time |> Num.to_f32 |> Num.mul 1.002
    updated_camera_settings = {
        old_settings &
        target: {
            x: (model.base_camera.target.x + Noise.perlin2d(frequency, 0) * amplitude)
                |> Num.round |> Num.to_f32,
            y: (model.base_camera.target.y + Noise.perlin2d(0, frequency) * amplitude)
                |> Num.round |> Num.to_f32,
        },
        rotation:
            model.base_camera.rotation +
            Noise.perlin2d(frequency, frequency) * 2.5 * intensity

    }
    (updated_camera_settings, intensity)

render_launch_pads! = |model|
    List.for_each! model.map.launch_pads |pad|
        state = LaunchStatus.get_launch_pad_state model.units pad
        tile_type = when state is
            Neutral -> Grey
            Owned Union -> Blue
            Owned Confederates -> Red
        List.for_each! pad |cell| draw_top_tile! model.textures.top_tiles cell tile_type

render_glow! = |model, summary_unit|
   (center, pct, color, shape, max) = when model.glowing is
        None -> (PointyHex.hex_to_pixel summary_unit.dest, 1.0, White, (Hex summary_unit.dest), 1.0) #PointyHex.hex_to_pixel (summary_unit.dest, 1.0)
        Following ({position}, anim) -> (position, Animation.percent anim, Black, Circle 28, 2.0)
        Running (cell, anim) -> (PointyHex.hex_to_pixel cell , Animation.percent anim, White, (Hex cell), 2.5)

    Draw.with_mode_shader! model.shaders.ring.shader |{}|
        Shader.set_f32! model.shaders.ring "time" pct
        |> Shader.set_vec2! "center" center
        |> Shader.set_f32! "max" max
        |> |_|
            when shape is
                Hex cell ->
                    render_hex_outline! PointyHex.hex_to_pixel(cell) RocRay.fade(Black, 0.8) 3.0
                    render_hex_scaled! cell RocRay.fade(color, 0.5)

                Circle radius ->
                    Draw.circle_gradient! {
                            center,
                            inner: RocRay.fade(White, 0.25),
                            outer: RocRay.fade(White, 1.0),
                            radius,
                    }
                    # Draw.circle_lines! {
                    #         center,
                    #         color: Black,
                    #         radius: radius + 2,
                    # }
                    Draw.ring! {
                        center,
                        inner: radius,
                        outer: radius + 3,
                        start: 90,
                        end: 360 + 90,
                        segments: 24,
                        color: RocRay.fade(Black, 0.8)
                    }
    {}

#    List.range { start: At 0, end: Before 4 }
#        |> List.for_each! |ii|
#            i = Num.to_f32 ii
#            Draw.circle_lines! {
#                center,
#                color: RGBA(0, 0, 0, 255),
#                radius: (radius - 4.0 - Num.to_f32(i))
#            }
#            Draw.circle_lines! {
#                center,
#                color: RGBA(240, 240, 240, 255),
#                radius: radius - i
#            }
#            Draw.circle_lines! {
#                center,
#                color: Black,
#                radius: max_radius - i
#            }
render_units! : Model, _ => {}
render_units! = |model, selected|
   model.units
    |> List.drop_if |u|
        when u.health is
            Living _ -> Bool.false
            Dead _ -> Bool.true
    |> List.for_each!(|unit| drawUnit!(unit, model.textures.units, selected.id == unit.id))

render_trauma_bar! = |trauma, intensity|
    gauge_size = 128
    gauge_width = 16

    y_offset = screen.height - gauge_size - 32

    Draw.rectangle!{
        rect: {
            x: 8,
            y: y_offset,
            height: gauge_size + 16,
            width: gauge_width * 2 + 3 * 4,
        },
        color: White,
    }
    Draw.rectangle!{
        rect: {
            x: 8 + 4,
            y: y_offset + 8,
            height: gauge_size + 4,
            width: gauge_width,
        },
        color: Black,
    }
    Draw.rectangle!{
        rect: {
            x: 8 + 6,
            y: y_offset + 8 + (gauge_size - gauge_size * trauma - 2) ,
            height: gauge_size * trauma + 2,
            width: gauge_width - 4,
        },
        color: Red,
    }
    Draw.rectangle!{
        rect: {
            x: 8 + 8 + gauge_width,
            y: y_offset + 8,
            height: gauge_size + 4,
            width: gauge_width,
        },
        color: Black,
    }
    Draw.rectangle!{
        rect: {
            x: 8 + 8 + gauge_width + 2,
            y: y_offset + 8 + ( gauge_size - gauge_size * intensity - 2) ,
            height: gauge_size * intensity + 2,
            width: gauge_width - 4,
        },
        color: Teal,
    }

render_countdown! = |countdown, color|
    diameter = 48
    text_size = 48
    center = { x: 0, y: 24 }
    text = countdown |> Num.to_f32
        |> Num.div 1_00
        |> Num.round
        |> |c|
            if c < 100 && c > 0 then
                (Num.to_f32 c / 10) |> Num.to_str |>
                |s| if Str.to_utf8 s |> List.len <= 1 then Str.concat s ".0" else s
            else
                Num.ceiling(Num.to_f32 c/10)|>Num.to_f32 |> Num.to_str
    text_dims = Effect.measure_text! text text_size 1 |> InternalVector.to_vector2

    Draw.circle!{ radius: diameter, center, color }
    Draw.circle_lines!{ radius: diameter - 2, center, color: Black }
    Draw.circle_lines!{ radius: diameter - 1, center, color: Black }
    Draw.circle_lines!{ radius: diameter + 1, center, color: Black }
    Draw.circle_lines!{ radius: diameter + 2, center, color: Black }
    Draw.text!{
        color: White,
        pos: {
            x: center.x - text_dims.x / 2,
            y: center.y - text_dims.y / 2,
        },
        size: text_size,
        text
    }


TileType : [ LaunchPad, OutOfBounds, Normal ]
render_map_fn! : HexTile.HexMap, (HexTile.HexTile, TileType => {}) => _
render_map_fn! = |hex_map, draw_tile!|
    all_pads = List.join hex_map.launch_pads
    launch_tiles = hex_map.launch_pads |> List.join_map |pads|
        List.map pads |cell| HexTile.make_tile cell Basalt
    tiles =
        Dict.values hex_map.tiles
        |> List.drop_if |tile| List.contains all_pads tile.cell
        |> List.concat(launch_tiles)
        |> List.sort_with |a, b| PointyHex.cell_sorter a.cell b.cell

    List.for_each!(
        tiles,
        |pad|
            # tile_type = if List.contains launch_tiles pad then LaunchPad
            #     else if List.contains hex_map.center pad.cell) then OutOfBounds
            #     else Normal
            tile_type =
                if List.contains(launch_tiles, pad) then
                    LaunchPad
                else if List.contains(hex_map.center, pad.cell) then OutOfBounds
                else Normal
            draw_tile! pad tile_type

    )

draw_top_tile! = |texture, cell, type|
    center = PointyHex.hex_to_pixel cell
    offset = { x: center.x - 27, y: center.y - 30 }
    source =
    when type is
        Blue -> { x: 55 * 0, y: 1 * 57, height: 57, width: 55 }
        Grey  -> { x: 55 * 0, y: 2 * 57, height: 57, width: 55 }
        Red  -> { x: 55 * 1, y: 3 * 57, height: 57, width: 55 }
    tint = when type is
        Blue -> RocRay.fade(Blue, 0.95)
        Grey -> Gray
        Red -> RocRay.fade(Red, 0.95)

    Draw.texture_rec! {
        texture,
        pos: offset,
        tint,
        source,
    }

drawPath! = |listOfPoints, color, thickness, connected|
    first = List.first listOfPoints ?? { x: 0, y: 0 }
    rest = List.drop_first listOfPoints 1
    radius = thickness / 2
    last = List.walk! rest first |start, end|
        Draw.line_ex! { color, thickness, start, end }
        Draw.circle! { color, radius, center: end }
        end
    if connected then
        Draw.line_ex! { color, thickness, start: last, end: first }
    else
        Draw.circle! { color, radius: thickness, center: first }

drawHex! = |cell, texture, pos, tint|

    center =
        # PointyHex.hex_to_pixel (doubled (cell.column) (cell.row))
        PointyHex.hex_to_pixel cell
    offset =
        center
        |> Hex.addPoint { x: (-1 * 32), y: -1 * 32 }
    # tint = if cell == doubled(0, 0) || cell == doubled(-1, 1) || cell == doubled(1, 1) then Black else White
    Draw.texture_rec! {
        texture,
        pos: offset,
        tint,
        source: {
            x: pos.x,
            y: pos.y,
            width: PointyHex.horizontal_spacing * 2,
            height: PointyHex.vertical_spacing * 2,
        },
    }

render_system! : Particle.ECS => {}
render_system! = |ecs|
    Particle.get_by_component ecs [Position, Renderable]
    |> Dict.values
    |> List.for_each! |component|
        when component is
            [Position { x, y }, Renderable render] ->
                when render is
                    Texture { texture, origin, source, scale } ->
                        width = source.width * scale.x
                        height = source.height * scale.y
                        Draw.texture_pro! {
                            dest: {
                                x: x - width / 2 - 2,
                                y: y - height / 2 - 2,
                                width: width + 4,
                                height: height + 4,
                            },
                            origin,
                            source,
                            texture,
                            rotation: 0,
                            tint: Teal,
                        }
                        Draw.texture_pro! {
                            dest: {
                                x: x - width / 2,
                                y: y - height / 2,
                                width,
                                height,
                            },
                            origin,
                            source,
                            texture,
                            rotation: 0,
                            tint: White,
                        }
                    _ -> {}
            _ -> {}
drawUnit! : Unit, RocRay.Texture, Bool => _
drawUnit! = |unit, texture, selected|
    point = Hex.addPoint unit.position { x: 0, y: 0 }
    drawTo = {
        x: point.x,
        y: point.y,
        flags: if unit.army == Union then
            []
        else
            [FlipX],
    }
    source = { x: 0, y: 0, width: 40, height: 62 }
    source_ = when unit.type is
        Infantry -> { source & x: source.width * 0, y: source.height * 0 }
        Cavalry -> { source & x: source.width * 1, y: source.height * 0 }
        Artillery -> { source & x: source.width * 0, y: source.height * 2 }

    unit_width = 12
    unit_height = 24
    Draw.texture_pro! {
        texture,
        source: source_,
        dest: {
            width: unit_width + 4,
            height: unit_height + 4,
            x: drawTo.x - unit_width / 2 - 2,
            y: drawTo.y - unit_height / 2 - 2},
        origin: { x: 0, y: 0 },
        rotation: 0,
        tint: if selected then Green else if unit.army == Confederates then Red else Blue,
    }
    Draw.texture_pro! {
        texture,
        source: source_,
        dest: {
            width: unit_width,
            height: unit_height,
            x: drawTo.x - unit_width / 2,
            y: drawTo.y - unit_height / 2},
        origin: { x: 0, y: 0 },
        rotation: 0,
        tint: White,
    }
