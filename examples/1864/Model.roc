module [ Army, Orders, YearOfDecision, initialize ]
import rr.RocRay exposing [ Camera, Vector2 ]
import rr.Shader
import Unit exposing [Unit]
import HexTile exposing [HexMap]
import Hex exposing [ doubled, Doubled ]
import Noise
import Animation exposing [Animation]
import Particle
import PointyHex
import rand.Random

Army : [Union, Confederates]
CameraSettings: {
    target : Vector2,
    offset : Vector2,
    rotation : F32,
    zoom : F32,
}


Orders : [
  TakePad Doubled,
  BlockCell Doubled,
  Idle,
]

YearOfDecision : {
    frameCount : U64,
    ai_army: Army,
    ai_intents: Dict Unit.Id Orders,
    game_time: U64,
    selectedIndex: I8,
    border: List Doubled,
    hexTexture : RocRay.Texture,
    selectedCell : Doubled,
    hoverCell : Doubled,
    player: Hex.Point,
    playerVelocity: Hex.Point,
    map: HexMap,
    units: List Unit,
    camera: Camera,
    base_camera: CameraSettings,
    trauma: F32,
    seed: I32,
    rand: Random.State,
    countdown_start: U32,
    countdown: U32,
    launch_state: [InControl Army, Stalemate],
    animations: Dict Doubled Animation,
    glowing: [Running (Doubled, Animation), Following(Unit, Animation), None],
    textures: {
        full_tiles: RocRay.Texture,
        top_tiles: RocRay.Texture,
        units: RocRay.Texture,
        empty: RocRay.Texture,
    },
    render_textures: {
        fog: RocRay.RenderTexture
    },
    shaders: {
        fog: Shader.RenderShader,
        ring: Shader.RenderShader,
        particle: Shader.RenderShader,
    },
    sounds: {
        power_up: RocRay.Sound,
        power_down: RocRay.Sound,
        ok: RocRay.Sound,
        horse: RocRay.Sound,
        wagon: RocRay.Sound,
    },
    ecs: Particle.ECS,
}

initialize =  |{ seed, camera, textures, render_textures, shaders, sounds, base_camera }|
    noise_fn = Noise.seeded_perlin2d seed
    map = HexTile.init(doubled(-12, -4), doubled(12, 4), noise_fn)

    units = Unit.initial
    selectedIndex = List.first units
        |> |r|
            when r is
                Ok u -> u.id
                Err _ -> crash "units must be non-empty list"
    rand = Random.seed (seed |> Num.to_u32)
    source = { width: 40, height: 65, x: 40, y: 65 }
    renderable = {
        texture: textures.units,
        origin: { x: 0, y: 0 },
        source,
        # scale: { x: 0.3, y: 0.3870 },
        scale: { x: 1.0, y: 1.0 },
        flip: None,
        tint: Olive,
    }
    single_test_unit : List Particle.ComponentData
    single_test_unit = [
        Occupies { cell: Hex.doubled -9 -1, army: Union },
        Position(PointyHex.hex_to_pixel(Hex.doubled -9 -1)),
        Renderable Texture(renderable),
        MoveRequest { destination: Hex.doubled(4, 2), }
    ]
    countdown = 20_000

    {
        game_time: 0,
        ai_army: Confederates,
        ai_intents: Dict.empty {},
        frameCount: Num.to_u64 0,
        hoverCell: doubled 0 0,
        selectedCell: doubled 0 0,
        selectedIndex,
        player: { x: 0, y: 0 },
        trauma: 0f32,
        map,
        shaders,
        playerVelocity: { x: 0, y: 0 },
        border: Hex.border,
        base_camera,
        seed,
        rand,
        units: Unit.initial,
        sounds,
        camera,
        hexTexture: textures.full_tiles,
        countdown_start: countdown,
        countdown,
        animations: Dict.empty {},
        glowing: None,
        launch_state: Stalemate,
        textures,
        render_textures,
        ecs: Particle.make rand
            |> Particle.spawn({position: { x: 16, y: -64 }, num_particles: 16 })
            |> Particle.spawn({position: PointyHex.hex_to_pixel Hex.doubled(-7, 3), num_particles: 6 })
            |> Particle.add single_test_unit
            |> .1
            |> Unit.spawn {
                texture: textures.units,
                type: Infantry,
                army: Union,
                cell: Hex.doubled -8 2,
            }
            |> |(id, ecs0)| Particle.move_to ecs0 id Hex.doubled(3, 1)
            |> Unit.spawn {
                texture: textures.units,
                type: Artillery,
                army: Confederates,
                cell: Hex.doubled 10 0,
            }
            |> |(id, ecs0)| Particle.move_to ecs0 id Hex.doubled(-2, 2)
    }
