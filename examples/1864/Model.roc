module [ Army, YearOfDecision, initialize! ]
import rr.RocRay exposing [ Camera, Vector2 ]
import rr.Effect
import Unit exposing [Unit]
import HexTile exposing [HexMap]
import Hex exposing [ doubled, Doubled ]
import Noise
import Animation exposing [Animation]

Army : [Union, Confederates]
CameraSettings: {
    target : Vector2,
    offset : Vector2,
    rotation : F32,
    zoom : F32,
}

YearOfDecision : {
    frameCount : U64,
    # inputs : (W4.Gamepad, W4.Gamepad),
    # lastInputs : (W4.Gamepad, W4.Gamepad),
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
    countdown_start: U32,
    countdown: U32,
    launch_state: [InControl Army, Stalemate],
    animations: Dict Doubled Animation,
    glowing: [Running (Doubled, Animation), None],
    # background: Sprite,
    # backgrounds: List Sprite,
    # screenState : ScreenState,
    sounds: {
        zap: RocRay.Sound,
        ok: RocRay.Sound,
        horse: RocRay.Sound,
        wagon: RocRay.Sound,
    },
}

initialize! : Camera, RocRay.Texture, _, _ => YearOfDecision
initialize! =  |camera, hexTexture, sounds, camera_settings|
    seed = Effect.random_i32! 1 10000
    noise_fn = Noise.seeded_perlin2d seed
    map = HexTile.init(doubled(-10, -4), doubled(10, 4), noise_fn)

    units = Unit.initial
    selectedIndex = List.first units
        |> |r|
            when r is
                Ok u -> u.id
                Err _ -> crash "units must be non-empty list"
    countdown = 20_000
    {
        frameCount: Num.to_u64 0,
        hoverCell: doubled 0 0,
        selectedCell: doubled 0 0,
        selectedIndex,
        player: { x: 0, y: 0 },
        trauma: 0f32,
        map,
        playerVelocity: { x: 0, y: 0 },
        border: Hex.border,
        base_camera: camera_settings,
        seed,
        units: Unit.initial,
        sounds,
        camera,hexTexture,
        countdown_start: countdown,
        countdown,
        animations: Dict.empty {},
        glowing: None,
        launch_state: Stalemate,
    }
