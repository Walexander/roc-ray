app [Model, init!, render!] {
    rr: platform "../../platform/main.roc",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
}
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
import rr.Effect
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

camera_settings = {
    target: { x: 0.0, y: 0 },
    offset: {x: screen.width / 2, y: screen.height / 2 - 80},
    zoom: 1.125,
    rotation: 0
}
init! : {} => Result YearOfDecision _
init! = |{}|
    RocRay.init_window! { width: screen.width, height: screen.height, title: "Stupid STuff" }
    RocRay.set_target_fps! 60
    hexTexture = Texture.load!("examples/1864/assets/kenney-tiles.png")?
    top_tiles = Texture.load!("examples/1864/assets/topTiles.png")?
    units = Texture.load!("examples/1864/assets/aliens.png")?
    camera = Camera.create!(camera_settings)?
    power_up = Sound.load!("examples/1864/assets/power-up.mp3")?
    power_down = Sound.load!("examples/1864/assets/power-down.mp3")?
    ok =  Sound.load!("examples/1864/assets/yessir.mp3")?
    horse = Sound.load!("examples/1864/assets/horse.mp3")?
    wagon = Sound.load!("examples/1864/assets/wagon.mp3")?
    baseState : YearOfDecision
    baseState = Model.initialize!(
        camera,
        { units, top_tiles, full_tiles: hexTexture },
        { power_up, power_down, ok, horse, wagon },
        camera_settings
    )
    Ok baseState

render! : YearOfDecision, RocRay.PlatformState => Result YearOfDecision []
render! = |model, pf|
    dt = pf.timestamp.last_render_end - pf.timestamp.last_render_start

    isOccupied = |cube| List.contains (model.units |> List.map .cell) cube

    path_finder = HexTile.make_path_finder model.map isOccupied

    unit_from_cell = |cell| List.find_first model.units |u| u.cell == cell
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

    countdown_pct = Num.max(0, 1 - (Num.to_f32 model.countdown / Num.to_f32 model.countdown_start))
    intensity = update_camera! model pf

    rand = Random.step model.rand Random.bounded_u32(0, 1_000_000)
    debugText =
        """
            Random: ${rand.value |> Num.to_f32 |> Num.div 1_000_000 |> Num.to_str}
            ECS Size: ${model.ecs.current_size |> Num.to_str}
            Game Time: ${Inspect.to_str model.game_time}
            Mouse to World: ( $(Num.round mouse_world.x|>Num.to_str), ${ Num.round mouse_world.y |> Num.to_str } )
            Hover Cell: ${ Inspect.to_str hover_cell }
            Hover Cell pixel: ${ Inspect.to_str(PointyHex.hex_to_pixel hover_cell) }
            Countdown = ${model.countdown |> Num.to_str}
            Pct=${countdown_pct |> Num.mul 100 |> to_fixed 0}%, Seed = ${model.seed |> Num.to_str}; A=${intensity|>to_fixed 3}
            Trauma=${model.trauma |> to_fixed 3}
        """
    player_move_ = GameActions.inputs_to_move(model, isOccupied, unit_from_cell, mouse_world, pf.keys, pf.mouse.buttons)
    _ = if pf.frame_count %  60 == 0 then
        dbg "ECS dump: ${Inspect.to_str model.ecs.scalable}\nMoveable: ${Inspect.to_str model.ecs.positionable}"
    else ""

    world: YearOfDecision

    world = { model& game_time: model.game_time + dt, rand: rand.state }
        |> Trauma.process_trauma player_move_
        |> LaunchStatus.update
        |> LaunchCountdown.update(Num.to_u32 dt)
        |> Particle.update dt
        |> Movement.update(dt, isOccupied, path_finder)
        |> GameActions.update!(player_move_, path_finder)
        |> Health.update
        |> |world_|
            if world_.game_time >= 2_500 then
                AI.update world_ path_finder isOccupied
            else
                world_
        |> |world_|  { world_ & hoverCell: mouseCell2, selectedCell }
        |> |world_| { world_ &
            glowing:
                when player_move_ is
                    MoveUnit { to } -> Running (to, Animation.make {
                        start_time: pf.timestamp.last_render_end,
                        frame_count: 8,
                        fps: 8,
                    })
                    _ -> when world_.glowing is
                        Running (cell, anim) ->
                            duration = pf.timestamp.last_render_end - anim.start_time
                            if (duration) >= 2_000 then
                                None
                            else
                                processed = Animation.process anim dt
                                Running (cell, processed)
                        None -> when player_move_ is
                            _ -> world_.glowing

        }


    Draw.draw! Black |{}|
        render_game! world  pf path_finder
        render_trauma_bar! world.trauma intensity
        render_debug! world  pf.mouse.position pf.keys debugText

    render_sound! world player_move_

    if world.launch_state != model.launch_state then
        when world.launch_state is
            InControl _ -> Sound.play! world.sounds.power_up
            Stalemate -> Sound.play! world.sounds.power_down
    else {}


    Ok world

render_debug! = |model, mouse_pos, keys, debug_text|
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
    summary_text_dims = Effect.measure_text! summary_text 24 1 |> InternalVector.to_vector2
    Draw.text! { pos: { x: 128+10, y: screen.height - (Num.to_f32 summary_text_dims.y + 24) }, text: summary_text, size: 24, color: summary_text_color }
    debug_text_dims = Effect.measure_text! debug_text 16 1 |> InternalVector.to_vector2

    if Keys.down keys KeyLeftShift then
        debug_pos = {
            x: screen.width - (Num.to_f32 debug_text_dims.x) - 48,
            y: screen.height - (Num.to_f32 debug_text_dims.y) - 24
        }
        Draw.text! {
            pos: debug_pos, text: debug_text, size: 16, color: White }
    else
        {}

render_particles! = |ecs|
    Dict.map ecs.positionable |id, { x, y }|
        graphic = Dict.get ecs.scalable id
            |> Result.with_default { radius: 0, color: White }
        {
            x, y,
            radius: graphic.radius,
            color: graphic.color
        }
    |> Dict.values
    |> List.for_each! |{x, y, radius, color}|
        Draw.circle! {center: { x, y }, radius: radius * 10, color }

renderHexOutline! = \cell, color ->
    points = PointyHex.points (PointyHex.hex_to_pixel cell)
    drawPath! points color 3 Bool.true


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


    unitPath_ =
        summary_unit.lastPath
        |> List.map PointyHex.hex_to_pixel
        |> List.drop_first 1
        |> List.prepend (summary_unit.position)

    Draw.with_mode_2d!(
        model.camera,
        |{}|
           render_map! model.map model.hexTexture
           render_launch_pads! model
           drawPath! unitPath_ White 5 Bool.false
           renderHexOutline! model.hoverCell Navy
           countdown_color = when model.launch_state is
                Stalemate -> Green
                InControl Union -> Blue
                InControl Confederates -> Red
           render_countdown! model.countdown countdown_color

           render_glow! model summary_unit

           render_units! model

           if Keys.down pf.keys KeyLeftControl then
                cube_path = path_finder summary_unit.cell model.hoverCell
                straightLine = cube_path |> List.map |point| PointyHex.hex_to_pixel point
                drawPath! straightLine Navy 5 Bool.false
           else {}

           render_particles! model.ecs
        )
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

update_camera! = |model, _|
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
    Camera.update!(model.camera, updated_camera_settings)
    intensity

render_launch_pads! = |model|
    List.for_each! model.map.launch_pads |pad|
        state = LaunchStatus.get_launch_pad_state model.units pad
        tile_type = when state is
            Neutral -> Grey
            Owned Union -> Blue
            Owned Confederates -> Red
        List.for_each! pad |cell| draw_top_tile! model.textures.top_tiles cell tile_type

render_glow! = |model, summary_unit|
   max_radius = 28
   # List.range { start: At 0, end: Before 8 }
   # |> List.for_each! |i|
   #  Draw.circle_lines! {
   #      center: PointyHex.hex_to_pixel model.hoverCell |> Hex.addPoint { y: -8, x: 0 },
   #      color: Navy,
   #      radius: Num.to_f32 (max_radius - i)
   #  }

   (center_, radius) = when model.glowing is
       None ->
           c = PointyHex.hex_to_pixel summary_unit.dest
           (c, max_radius)
       Running (cell, anim) ->
           c = PointyHex.hex_to_pixel cell
           (c, max_radius |> Num.to_f32 |> Num.div 8 |> Num.mul (Num.to_f32 anim.frame_index) |> Num.round)

   center = Hex.addPoint center_ { y: -8, x: 0 }
   List.range { start: At 0, end: Before 4 }
       |> List.for_each! |i|
           Draw.circle_lines! {
               center,
               color: RGBA(255, 250, 250, 255),
               radius: Num.to_f32 (radius - 4 - i)
           }
           Draw.circle_lines! {
               center,
               color: RGBA(255, 250, 250, 255),
               radius: Num.to_f32 (radius - i)
           }
           Draw.circle_lines! {
               center,
               color: Black,
               radius: Num.to_f32 (max_radius - i)
           }

render_units! = |model|
   model.units
    |> List.drop_if |u|
        when u.health is
            Living _ -> Bool.false
            Dead _ -> Bool.true
    |> List.for_each!(|unit| drawUnit! unit model.textures.units)
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
    Draw.circle!{ radius: diameter, center, color: White }
    Draw.text!{
        color,
        pos: {
            x: center.x - text_dims.x / 2,
            y: center.y - text_dims.y / 2,
        },
        size: text_size,
        text
    }

render_map! : HexTile.HexMap, _ => _
render_map! = |hex_map, texture|
    # foo_ = List.concat(
    #     hex_map.border
    #         |> List.map(|c|
    #             HexTile.make_tile(c, Basalt)
    #         ),
    #     Dict.values hex_map.tiles
    # )
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
            tx_pos = HexTile.texture_position pad 65 89
            tint = if List.contains hex_map.center pad.cell then Black else White
            drawHex! pad.cell texture tx_pos tint
    )

draw_top_tile! = |texture, cell, type|
    center = PointyHex.hex_to_pixel cell
    offset = { x: center.x - 27, y: center.y - 40 }
    source =
    when type is
        Blue -> { x: 55 * 0, y: 1 * 57, height: 57, width: 55 }
        Grey  -> { x: 55 * 0, y: 2 * 57, height: 57, width: 55 }
        Red  -> { x: 55 * 1, y: 3 * 57, height: 57, width: 55 }
    tint = when type is
        Blue -> Blue
        Grey -> Gray
        Red -> Red

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
        |> Hex.addPoint { x: (-1 * 32), y: -1 * 44 }
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

    # cellText =
    #     """
    #     ${Inspect.to_str i}
    #     ${cell.column |> Num.to_str}, ${cell.row |> Num.to_str}
    #     """
    # Draw.text! {
    #     color: Black,
    #     size: 12,
    #     text: cellText,
    #     pos: { x: center.x - 12, y: center.y - 12 },
    # }

# drawHexOutline! = \cell, translate, color ->
#     points = Hex.hexPoints translate
#     (rest, first) = List.drop_first points 1
#     List.walk_try! rest first |current, next| ->
#         Draw.line_ex! { start: current
# Draw.text! {
#     color,
#     size: 16,
#     text: "h2p: $(hoverCellPoint.x |> Num.add point.x |> Num.round |> Num.to_str), $(hoverCellPoint.y |> Num.add point.y |> Num.round |> Num.to_str)",
#     pos: { x: x + halfHorizontalSpacing - 16, y: y + halfVerticalSpacing + 16 }

# }
# colors <- W4.getDrawColors |> Task.await
# W4.setShapeColors!{ fill: Color1, border: Color1 }
# W4.setShapeColors! colors
# W4.setShapeColors! {border: Color4, fill: None }
# W4.oval! { x, y, height: Num.toU32 (2 * verticalSpacing), width: Num.round (Num.to_frac horizontalSpace |> Num.mul 1.33) }
# Sprite.blit! Assets.filledHex { x: x, y : y }
# W4.setTextColors! { fg: Color1, bg: None }
# Task.ok {x, y}
# updateTitle = \state, inputs, lastInputs, frameCount ->
#     netplay = W4.getNetplay!
#     thePlayer = getCurrentPlayer netplay
#     army = playerArmy thePlayer
#     W4.setPalette! (armyPalette army)
#     pressed = {
#         union: inputs.0.button1 && !lastInputs.0.button1,
#         confederates: inputs.1.button1 && !lastInputs.1.button1,
#     }

#     ready =
#         when state.ready is
#             Ready forAction ->
#                 when forAction is
#                     Union if pressed.confederates -> BothReady
#                     Confederates if pressed.union -> BothReady
#                     _ -> Ready forAction

#             WaitingBoth if pressed.union -> Ready Union
#             WaitingBoth if pressed.confederates -> Ready Confederates
#             WaitingBoth | BothReady -> state.ready
#     newState =
#         when ready is
#             BothReady -> InGame (newGame frameCount Union Confederates)
#             # BothReady -> GameOver { restartIn: 5, winner: Union, elapsed: frameCount }
#             _ -> TitleScreen { state & ready }
#     Task.ok newState

# updateGameOver = \state, frameCount ->
#     { restartIn } = state
#     elapsedSeconds = frameCountToSeconds state.elapsed |> Num.round
#     elapsedSince = frameCountToSeconds frameCount |> Num.round
#     sinceOver = (elapsedSince - elapsedSeconds)
#     next = if sinceOver >= restartIn then baseState.screenState else GameOver state
#     Task.ok next

# updateInGame = \model, frameCount, inputs, lastInputs ->
#     padOwners = List.map model.map.launchPads \pad ->
#         getPadOwner model.units pad
#     launchStatus = getLaunchStatus padOwners
#     launchTimer =
#         if model.launchTimer <= 0 then
#             model.launchTimer
#         else
#             when launchStatus is
#                 InControl _ -> Num.sub model.launchTimer 1
#                 StaleMate -> model.launchTimer

#     isOccupied = isCellOccupied model.units model.map.obstacles
#     isObstacle = cellObstacle model.map.obstacles
#     (unionInputs, confedInputs) =
#         when model.armies is
#             (Union, Confederates) ->
#                 ({ inputs: inputs.0, last: lastInputs.0 }, { inputs: inputs.1, last: lastInputs.1 })

#             _ -> ({ inputs: inputs.1, last: lastInputs.1 }, { inputs: inputs.0, last: lastInputs.0 })

#     pressed = {
#         union: unionInputs.inputs.button1 && !unionInputs.last.button1,
#         confederates: confedInputs.inputs.button1 && !confedInputs.last.button1,
#     }
#     pressedZ = {
#         union: unionInputs.inputs.button2 && !unionInputs.last.button2,
#         confederates: confedInputs.inputs.button2 && !confedInputs.last.button2,
#     }
#     getUnitById = makeUnitIdLocator model.units

#     unionHoverCell =
#         getHoverCell model.hovering.union unionInputs.inputs unionInputs.last isObstacle
#     (unionMove, nextUnionIndex) =
#         updateMoveChoice model.moves.0 {
#             isOccupied,
#             getUnitById,
#             wasPressed: pressed.union,
#             zPressed: pressedZ.union,
#             nextIndex: model.unitIndex.union,
#             hovering: unionHoverCell,
#             theArmy: Union,
#             units: List.keepIf model.units \u -> u.army == Union,
#         }

#     confedHover =
#         getHoverCell model.hovering.confederate confedInputs.inputs confedInputs.last isObstacle
#     (confedMove, nextConfedIndex) =
#         updateMoveChoice model.moves.1 {
#             isOccupied,
#             getUnitById,
#             wasPressed: pressed.confederates,
#             zPressed: pressedZ.confederates,
#             nextIndex: model.unitIndex.confederate,
#             hovering: confedHover,
#             theArmy: Confederates,
#             units: List.keepIf model.units \u -> u.army == Confederates,
#         }

#     units = List.walk model.units [] \accum, u ->
#         unitUpdate =
#             when u.army is
#                 Union -> Unit.update u frameCount unionMove isOccupied
#                 Confederates -> Unit.update u frameCount confedMove isOccupied

#         List.append
#             accum
#             { unitUpdate & position: unitUpdate.position }

#     combatModifiers = randList! { length: List.len units }
#     combatUnits =
#         if frameCount % 6 == 0 then
#             runCombat units combatModifiers
#             |> List.keepIf Unit.isAlive
#         else
#             units

#     hovering = {
#         union: maybeUpdateHoverCell isOccupied model.unitIndex.union nextUnionIndex unionHoverCell,
#         confederate: maybeUpdateHoverCell isOccupied model.unitIndex.confederate nextConfedIndex confedHover,
#     }
#     nextState =
#         if launchTimer > 0 then
#             InGame
#                 { model &
#                     moves: (unionMove, confedMove),
#                     hovering,
#                     unitIndex: {
#                         union: nextUnionIndex,
#                         confederate: nextConfedIndex,
#                     },
#                     launchTimer,
#                     units: combatUnits,
#                 }
#         else
#             when launchStatus is
#                 InControl winner ->
#                     GameOver { winner, restartIn: 5, elapsed: frameCount }

#                 StaleMate -> crash "launch timer should not tick without winner"
#     Task.ok nextState
# renderTitleScreen : TitleState, U64 -> _
# renderTitleScreen = \state, frameCount ->
#     netplay = W4.getNetplay!
#     thePlayer = getCurrentPlayer netplay
#     army = playerArmy thePlayer
#     textX = boardRect.x + 10

#     elapsedSeconds =
#         frameCount
#         |> frameCountToSeconds
#         |> Num.round
#         |> Num.toStr
#     Drawing.drawGameTime! elapsedSeconds
#     readyMessage =
#         when state.ready is
#             WaitingBoth -> "Press \u(80) to begin"
#             Ready readyArmy if readyArmy == army -> "...Waiting on..."
#             Ready _ -> "Press \u(80) already!"
#             BothReady -> "Let's roc"

#     title = armyName army
#     help =
#         """
#         Move units to the
#         launch pads.
#         """
#     disclaimer =
#         """
#         Be in control
#         when the timer
#         hits 0 to win!
#         """
#     gameName = " 1864! "
#     W4.setTextColors! { bg: Color4, fg: None }
#     gameName |> W4.text! { x: textX, y: boardRect.y }
#     W4.setTextColors! { fg: Color2, bg: None }
#     " $(title) " |> W4.text! { x: textX, y: boardRect.y + 10 }
#     W4.setTextColors! { bg: Color2, fg: Color3 }
#     " $(readyMessage) " |> W4.text! { x: textX - 12, y: boardRect.y + 20 }
#     W4.setTextColors! { fg: Color4, bg: None }
#     help |> W4.text! { x: 15, y: boardRect.y + 35 }
#     W4.setShapeColors! { border: Color4, fill: None }
#     Drawing.drawGrid!
#         [
#             doubled 3 11,
#             doubled 2 12,
#             doubled 3 13,
#             doubled 9 11,
#             doubled 10 12,
#             doubled 9 13,

#         ]
#         Assets.hex
#         (Hex.addPoint boardRect { x: 0, y: -5 })
#     Drawing.drawLaunchTimer!
#         15_000
#         20_000
#         { boardRect &
#             x: 80,
#             y: 90,
#         }
#     W4.setTextColors! { fg: Color3, bg: None }
#     disclaimer |> W4.text! { x: 15, y: boardRect.y + 35 + 60 }
#     Drawing.drawToolbar {
#         x: 0,
#         # y: boardRect.y + (Num.toI32 boardRect.height) |> Num.sub 25
#         y: 160 - 20,
#         # (Num.toI32 boardRect.height) |> Num.sub 25
#     }
# # Sprite.blit Assets.bloodMoon { y: 160 - 40, x: 0 }

# renderGameOver = \state, frameCount ->
#     restartIn = 5
#     { winner } = state
#     color = armyColor winner
#     name = armyName winner
#     netplay = W4.getNetplay!
#     thePlayer = getCurrentPlayer netplay
#     theArmy = playerArmy thePlayer

#     elapsed = Num.toFrac state.elapsed |> Num.div 60.0 |> Num.round # |> Num.toStr
#     elapsedSince = Num.toFrac frameCount |> Num.div 60.0 |> Num.round
#     # W4.debug! "Elapsed" elapsed
#     sinceOver = (elapsedSince - elapsed)

#     playerPalette = W4.getPalette!
#     W4.setPalette! playerPalette
#     W4.rect! { width: 140, height: 80, x: 10, y: 30 }
#     W4.setShapeColors! { border: color, fill: color }
#     W4.rect! { width: 138, height: 25, x: 11, y: 31 }
#     W4.setTextColors! { fg: Color4, bg: None }
#     outcome =
#         if theArmy == winner then
#             "WIN"
#         else
#             "LOSE"
#     "You $(outcome)!!" |> W4.text! { x: 17, y: 35 }
#     message =
#         """
#         After $(Num.toStr elapsed)
#         long seconds,the
#         $(name) Army
#         wins.
#         """
#     message |> W4.text! { x: 17, y: 60 }

#     countDown = restartIn - sinceOver
#     restartMessage = " Restart in $(Num.toStr countDown) "
#     size =
#         Str.countUtf8Bytes restartMessage
#         |> Num.toFrac
#         |> Num.div 2
#         |> Num.mul 8
#         |> Num.round
#     W4.setTextColors! { fg: Color1, bg: Color2 }
#     restartMessage |> W4.text! { x: Num.abs (80 - size), y: 100 }

# getUnitFromClickedCell = \units, selected, army ->
#     List.find_first units \u -> u.cell == selected && u.army == army && Unit.isAlive u

# basePoint : Hex.Point
# basePoint = { x: 5, y: 20 }

# boardRect = {
#     x: basePoint.x |> Num.toI32,
#     y: basePoint.y |> Num.toI32,
#     width: Num.toU32 150,
#     height: Num.toU32 100,
# }

drawUnit! : Unit, _ => _
drawUnit! = |unit, texture|
    point = Hex.addPoint unit.position { x: 4, y: 2 }
    drawTo = {
        x: point.x,
        y: point.y,
        flags: if unit.army == Union then
            []
        else
            [FlipX],
    }
    # border = armyColor unit.army

    outlinePoint = Hex.addPoint point { x: (PointyHex.hex_width / -2), y: 16 }
    (readyColors, width) =
        when unit.readiness is
            Cooldown timer ->
                t =
                    unit.cooldownRate
                    |> Num.sub (Num.to_f32 timer)
                    |> Num.div unit.cooldownRate
                w = Hex.lerp 0 64 t |> Num.round
                (
                    { start: Red, end: Green },
                    w,
                )

            Ready -> ({ start: White, end: Green }, Hex.horizontalSpace |> Num.round)
            Moving _ -> ({ start: White, end: White }, Hex.horizontalSpace |> Num.round)
    # Draw.rectangle! {
    #     color: if unit.army == Confederates then Black else Navy,
    #     rect: {
    #         width: 40,
    #         height: 40,
    #         x: drawTo.x - 24 - 4,
    #         y: drawTo.y - 24 - 8,
    #     }
    # }
    source = { x: 0, y: 0, width: 40, height: 62 }
    source_ = when unit.type is
        Infantry -> { source & x: source.width * 0, y: source.height * 0 }
        Cavalry -> { source & x: source.width * 1, y: source.height * 0 }
        Artillery -> { source & x: source.width * 0, y: source.height * 2 }

    Draw.rectangle! {
        color: if unit.army == Confederates then RGBA(192, 0, 0, 200) else RGBA(0, 128, 255, 200),
        rect: {
            width: 48,
            height: 24,
            x: drawTo.x - (20) - 4,
            y: drawTo.y ,
        }
    }
    Draw.texture_rec! {
        pos: { x: drawTo.x - 24, y: drawTo.y - 48},
        source: source_,
        texture,
        tint: White,
    }
    # _ = when unit.type is
    #     Cavalry  -> Draw.circle! {
    #         center: { x: drawTo.x, y: drawTo.y - 16 },
    #         radius: 6,
    #         color: Fuchsia,
    #     }
    #     Artillery -> Draw.circle! {
    #         center: { x: drawTo.x, y: drawTo.y - 16 },
    #         radius: 6,
    #         color: Maroon,
    #     }
    #     Infantry -> Draw.circle! {
    #         color: Purple,
    #         center: { x: drawTo.x, y: drawTo.y - 16 },
    #         radius: 6,
    #     }
    # Draw.rectangle_gradient_h! {
    #     right: readyColors.end,
    #     left: readyColors.start,
    #     rect: {
    #         x: outlinePoint.x + 4,
    #         y: outlinePoint.y - 12,
    #         width: Hex.horizontalSpace / 2,
    #         height: 8
    #     }
    # }
