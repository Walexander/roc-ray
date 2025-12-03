app [Model, init!, render!] { rr: platform "../../platform/main.roc" }

import rr.RocRay exposing [Camera]
import rr.Camera
import rr.Draw
import rr.Mouse
import PointyHex
import rr.Texture
import Hex

width = 800
height = 600

Model : {
    click_point: { x: F32, y: F32 },
    texture: RocRay.Texture,
    camera: Camera,
    top_texture: RocRay.Texture
}

init! : {} => Result Model _
init! = |{}|

    RocRay.init_window!({ title: "Basic Shapes", width, height })
    texture = Texture.load!("examples/1864/assets/kenney-tiles.png")?
    top_texture = Texture.load!("examples/1864/assets/topTiles.png")?

    camera = Camera.create!({
        target: { x: 0.0, y: 0 },
        offset: {x:  400, y: 400 },
        zoom: 5.0,
        rotation: 0
    })?
    Ok({click_point: { x: 0, y: 0 },
            texture,
        top_texture,
        camera,
    })

render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|

    hover_pos = RocRay.get_screen_to_world_2d! pf.mouse.position model.camera
    click_hex = PointyHex.pixel_to_hex model.click_point
    Draw.draw!(
        White,
        |{}|
            Draw.text!({ pos: { x: 10, y: 10 }, text: Inspect.to_str click_hex, size: 40, color: Navy })
            Draw.text!({ pos: { x: 10, y: 60 }, text: Inspect.to_str model.click_point, size: 40, color: Navy })
            Draw.text!({ pos: { x: 10, y: 100 }, text: Inspect.to_str (PointyHex.hex_to_pixel click_hex), size: 40, color: Navy })
            Draw.rectangle!({ rect: { x: 100, y: 150, width: 250, height: 100 }, color: Aqua })
            Draw.rectangle_gradient_h!({ rect: { x: 400, y: 150, width: 250, height: 100 }, left: Lime, right: Navy })
            Draw.rectangle_gradient_v!({ rect: { x: 300, y: 250, width: 250, height: 100 }, top: Maroon, bottom: Green })
            Draw.circle!({ center: { x: 200, y: 400 }, radius: 75, color: Fuchsia })
            Draw.circle_gradient!({ center: { x: 600, y: 400 }, radius: 75, inner: Yellow, outer: Maroon })
            Draw.line!({ start: { x: 100, y: 500 }, end: { x: 500, y: 500 }, color: Red })

            Draw.with_mode_2d!(model.camera, |{}|
                PointyHex.neighbors Hex.doubled(0, 0) |> List.prepend Hex.doubled(0, 0) |> List.prepend(click_hex)
                    |> List.sort_with PointyHex.cell_sorter
                    |> List.for_each! |hex|
                        source =
                            if hex == Hex.doubled(-1, -1) || hex == Hex.doubled(2, 0) || hex == click_hex then
                                { width: 65, height: 89, x: 1 * 65, y: 1 * 89 }
                            else
                                { width: 65, height: 89, x: 0, y: 0 * 89 }
                        draw_hex_tile! model.texture hex source
                draw_top_tile! model.top_texture click_hex Green
                draw_top_tile! model.top_texture Hex.doubled(-1, -1) Grey
                draw_top_tile! model.top_texture Hex.doubled(2, 0) Red
            )
    )



    Ok({model &
            click_point:
                if Mouse.pressed pf.mouse.buttons.left then hover_pos
                else model.click_point
    })

draw_top_tile! = |texture, cell, type|
    center = PointyHex.hex_to_pixel cell
    offset = { x: center.x - 27, y: center.y - 40 }
    source =
    when type is
        Green -> { x: 55 * 0, y: 1 * 57, height: 57, width: 55 }
        Grey  -> { x: 55 * 1, y: 1 * 57, height: 57, width: 55 }
        Red  -> { x: 55 * 1, y: 3 * 57, height: 57, width: 55 }

    Draw.texture_rec! {
        texture,
        pos: offset,
        tint: White,
        source,
    }

draw_hex_tile! = |texture, cell, source|
    center = PointyHex.hex_to_pixel cell
    offset = { x: center.x - 32, y: center.y - 44 }
    Draw.texture_rec! {
        texture,
        pos: offset,
        tint: White,
        source,
    }

