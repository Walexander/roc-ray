module [ Army, Orders, YearOfDecision, initialize! ]
import rr.RocRay exposing [ Camera, Vector2 ]
import rr.Effect
import Unit exposing [Unit]
import HexTile exposing [HexMap]
import Hex exposing [ doubled, Doubled ]
import Noise
import Health
import Animation exposing [Animation]
import Particle
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
    # inputs : (W4.Gamepad, W4.Gamepad),
    # lastInputs : (W4.Gamepad, W4.Gamepad),
    game_time: U64,
    health: Dict Unit.Id Health.Health,
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
    glowing: [Running (Doubled, Animation), None],

    textures: {
        full_tiles: RocRay.Texture,
        top_tiles: RocRay.Texture,
        units: RocRay.Texture,
    },
    # background: Sprite,
    # backgrounds: List Sprite,
    # screenState : ScreenState,
    sounds: {
        power_up: RocRay.Sound,
        power_down: RocRay.Sound,
        ok: RocRay.Sound,
        horse: RocRay.Sound,
        wagon: RocRay.Sound,
    },
    ecs: Particle.ECS,
}

initialize! : Camera, _, _, _ => YearOfDecision
initialize! =  |camera, textures, sounds, camera_settings|
    seed = Effect.random_i32! 1 10000

    noise_fn = Noise.seeded_perlin2d seed
    map = HexTile.init(doubled(-12, -4), doubled(12, 4), noise_fn)

    units = Unit.initial
    selectedIndex = List.first units
        |> |r|
            when r is
                Ok u -> u.id
                Err _ -> crash "units must be non-empty list"
    rand = Random.seed (seed |> Num.to_u32)
    countdown = 20_000
    {
        game_time: 0,
        ai_army: Confederates,
        ai_intents: Dict.empty {},
        frameCount: Num.to_u64 0,
        hoverCell: doubled 0 0,
        health: Dict.empty {},
        selectedCell: doubled 0 0,
        selectedIndex,
        player: { x: 0, y: 0 },
        trauma: 0f32,
        map,
        playerVelocity: { x: 0, y: 0 },
        border: Hex.border,
        base_camera: camera_settings,
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
        ecs: Particle.make rand |> Particle.spawn { x: 0, y: 0 } 32 #|> Particle.spawn { x: 50, y: 25 } 1,
    }
