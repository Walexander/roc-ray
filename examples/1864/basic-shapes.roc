app [Model, init!, render!] { rr: platform "../../platform/main.roc" }

import rr.RocRay
import rr.Draw
import rr.Mouse
import PointyHex
import rr.Texture
import Hex

width = 800
height = 600

Model : {
    click_point: { x: F32, y: F32 },
    texture: RocRay.Texture
}

init! : {} => Result Model _
init! = |{}|

    RocRay.init_window!({ title: "Basic Shapes", width, height })
    texture = Texture.load!("examples/1864/assets/kenney-tiles.png")?

    Ok({click_point: { x: 0, y: 0 },
            texture,
    })

render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|

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
            PointyHex.neighbors Hex.doubled(6, 8) |> List.for_each! |hex|
                draw_hex_tile! model.texture hex
            draw_hex_tile! model.texture click_hex

    )

    Ok({model &
            click_point: if Mouse.pressed pf.mouse.buttons.left then
                pf.mouse.position
            else model.click_point
    })

draw_hex_tile! = |texture, cell|
    center = PointyHex.hex_to_pixel cell
    offset = { x: center.x - 32, y: center.y - 44 }
    Draw.texture_rec! {
        texture,
        pos: offset,
        tint: White,
        source: {
            x: 0, y: 0, width: 65, height: 88,
        },
    }

