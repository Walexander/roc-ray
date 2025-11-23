app [Model, init!, render!] { rr: platform "../../platform/main.roc" }
# import Utils exposing [frameCountToSeconds]
import Unit exposing [Unit]
# import Assets
import Hex exposing [Doubled, doubled, hexToPixel, pixelToHex]
import HexTile exposing [HexTile]
import rr.Draw
import rr.RocRay exposing [ Camera, Vector2 ]
import rr.InternalVector
import rr.Effect
import rr.Keys
import rr.Camera
import rr.Mouse
import rr.Texture

# import Drawing
# import Health

Army : [Union, Confederates]
# Palette : {
#     color1 : U32,
#     color2 : U32,
#     color3 : U32,
#     color4 : U32,
# }
ScreenState : [
    TitleScreen TitleState,
    InGame GameState,
    GameOver GameOverState,
]

YearOfDecision : {
    frameCount : U64,
    # inputs : (W4.Gamepad, W4.Gamepad),
    # lastInputs : (W4.Gamepad, W4.Gamepad),
    selectedIndex: I8,
    hexTexture : RocRay.Texture,
    selectedCell : Doubled,
    hoverCell : Doubled,
    player: Hex.Point,
    playerVelocity: Hex.Point,
    playerPath: Hex.Line,
    unit: Unit,
    map_tiles: List HexTile,
    units: List Unit,
    camera: Camera,

    # background: Sprite,
    # backgrounds: List Sprite,
    screenState : ScreenState,
}
Model : YearOfDecision
TitleState : {
    ready : [
        WaitingBoth,
        Ready [Union, Confederates],
        BothReady,
    ],
}
GameOverState : {
    winner : [Union, Confederates],
    restartIn : U32,
    elapsed : U64,
}

LaunchPad : List Doubled
LaunchPads : List LaunchPad
Map : {
    obstacles : List Doubled,
    launchPads : LaunchPads,
}
GameState : {
    map : Map,
    startFrame : U64,
    launchTimer : U16,
    units : List Unit,
    armies : (Army, Army),
    launchIn : U16,

    hovering : {
        union : Doubled,
        confederate : Doubled,
    },
    unitIndex : {
        union : U64,
        confederate : U64,
    },
    moves : (Unit.MoveChoice, Unit.MoveChoice),
}

palette = {
    color1: 0xfce4a8,
    color2: 0x71969f,
    color3: 0xd71a21,
    color4: 0x01334e,
}
redPosterPalette = {
    color1: 0xe8d6c0,
    color2: 0x92938d,
    color3: 0xa1281c,
    color4: 0x000000,
}

# redAlert = 0xc4181f
# greenAlert = 0x426e5d
unionPalette = palette
confederatePalette = redPosterPalette

defaultGamepad = {
    up: Bool.false,
    down: Bool.false,
    left: Bool.false,
    right: Bool.false,
    button1: Bool.false,
    button2: Bool.false,
}

screenDims = {
    width: 1024,
    height: 768
}
init! : {} => Result Model _
init! = |{}|
    RocRay.init_window! { width: screenDims.width, height: screenDims.height, title: "Stupid STuff" }
    RocRay.set_target_fps! 24
    hexTexture = Texture.load!("examples/1864/assets/Flat/hex-tiles-2px-outline.png")?
    camera_settings = {
        target: { x: 0, y: 0},
        offset: {x: screenDims.width / 2, y: screenDims.height / 2},
        zoom: 0.55,
        rotation: 0
    }
    camera = Camera.create!(camera_settings)?

    obstacles = [ doubled(0, 4), ] #, (doubled -3 -1), (doubled 3 -1) ]
        |> List.map |seed| List.append (Hex.neighborsOf seed) seed
        |> List.join

    map_tiles = obstacles
        |> List.map |cell| { cell, terrain: Sand }

    isOccupied = \cube -> List.contains obstacles cube

    unit = Unit.make {
            id: 1,
            army: Union,
            cell: Hex.doubled(7, -7),
            type: Cavalry,
        } |> Unit.moveTo(doubled(-6, 0), isOccupied)

    units = Unit.initial isOccupied
    baseState : Model
    baseState = {
        frameCount: Num.to_u64 0,
        map_tiles,
        hoverCell: doubled 0 0,
        selectedCell: doubled 0 0,
        selectedIndex: 0,
        player: { x: 0, y: 0 },
        playerVelocity: { x: 0, y: 0 },
        playerPath: Hex.findPath (doubled 0 0) (doubled 0 0) |> Hex.pathToLine,
        screenState: TitleScreen { ready: WaitingBoth },
        camera,
        unit: List.first(units) |> Result.with_default unit,
        units ,
        hexTexture,
    }
    Ok baseState

render! : Model, RocRay.PlatformState => Result Model []
render! = |model, pf|
    # inputs = getPlayerInputs!
    # netplay = W4.getNetplay!

    # screenState =
    #     when model.screenState is
    #         TitleScreen state ->
    #             renderTitleScreen state model.frameCount
    #             |> Task.await \_ -> updateTitle state inputs model.inputs model.frameCount

    #         GameOver state ->
    #             renderGameOver state model.frameCount
    #             |> Task.await \_ -> updateGameOver state model.frameCount

    #         InGame state ->
    #             renderInGame state netplay model.frameCount
    #             |> Task.await \_ -> updateInGame state model.frameCount inputs model.inputs


    obstacles = List.map model.map_tiles .cell

    isOccupied = \cube -> List.contains obstacles cube
        || List.contains (model.units |> List.map .cell) cube

    mouse_world = RocRay.get_screen_to_world_2d! pf.mouse.position model.camera

    mouseCell2 = pixelToHex mouse_world
    selectedCell = if Mouse.released pf.mouse.buttons.left then mouseCell2 else model.selectedCell

    summary_unit = if Mouse.released pf.mouse.buttons.left && isOccupied selectedCell then
        List.find_first(model.units, \unit -> unit.cell == selectedCell)
        |> Result.on_err |_| List.first model.units
        |> Result.with_default model.unit
    else
        List.find_first(model.units, |unit|
            unit.id == model.selectedIndex
        )
        |> Result.on_err |_| List.first model.units
        |> Result.with_default model.unit

    summary_text =
    """
    Unit Summary: ${ Unit.summary summary_unit [] }
    ${Inspect.to_str summary_unit.position} ${straightLine |> List.len|> Num.to_str}
    """
    summary_text_color = if summary_unit.army == Confederates then Red else Black

    # updatedUnit = Unit.update model.unit pf.frame_count Finished isOccupied
    player_move = if Mouse.pressed pf.mouse.buttons.left then
        if Keys.down pf.keys KeyLeftShift then
            dbg "toggling obstacle@${Inspect.to_str mouseCell2}"
            if Keys.down pf.keys KeyLeftControl then
                DeleteObstacle mouseCell2
            else
                ToggleCellObstacle mouseCell2
        else
            dbg "Moving ${Inspect.to_str model.selectedIndex} to ${Inspect.to_str mouseCell2}"
            MoveTo mouseCell2
    else
        NoMove

    updatedUnits =
        model.units
        |> List.map(|unit|
            Unit.updateMovement(unit)
            |> Unit.reroute (|u| if unit.cell == u then Bool.false else isOccupied u)
            |> Unit.updateReadiness
        )
        # |> List.map(|unit| Unit.reroute unit isOccupied)
        |> |units|
            when player_move is
                MoveTo dest -> List.map(units, |u| if u.id == model.selectedIndex then Unit.moveTo(u, dest, isOccupied) else u)
                _ -> units


    map_tiles =
        when player_move is
            DeleteObstacle cell ->
                List.drop_if model.map_tiles |tile| tile.cell == cell
            ToggleCellObstacle cell ->
                existing = List.find_first model.map_tiles |tile|
                    tile.cell == cell
# when existing
                when existing is
                    Ok tile ->
                        tiles_ = model.map_tiles
                            |> List.drop_if(|t| t.cell == cell)
                        if tile.terrain == Water then
                            tiles_
                        else
                            List.append tiles_ ({ tile & terrain: HexTile.next_terrain tile })
                    Err _ ->
                        List.append model.map_tiles { cell: cell, terrain: HexTile.random_terrain(pf.frame_count) }
            _ -> model.map_tiles
    updated = { model &
        hoverCell: mouseCell2,
        selectedCell,
        unit: summary_unit,
        selectedIndex: summary_unit.id,
        units: updatedUnits,
        map_tiles
    }

    hoverCellPoint = hexToPixel mouseCell2
    textColor = if Keys.down pf.keys KeyLeftShift then Navy else RGBA 0 0 0 0
    distance = Hex.hexDistance mouseCell2 selectedCell

    cubePath_ = if Keys.down pf.keys KeyLeftShift then
        Hex.findGraph2 Hex.pixelToHex(summary_unit.position) mouseCell2 isOccupied
    else
        Hex.findGraph Hex.pixelToHex(summary_unit.position) mouseCell2 isOccupied

    cubePath = cubePath_ |> Result.with_default []

    straightLine = cubePath
        |> List.map |point|
            Hex.hexToPixel point

    unitPath_ =
        summary_unit.lastPath
        |> List.map |point|
            Hex.hexToPixel point
        |> List.drop_first 1
        |> List.prepend (summary_unit.position)


    m_dist = Hex.hexToPixel summary_unit.cell |> Hex.subPoint (Hex.hexToPixel mouseCell2) |> Hex.magnitude
    debugText =
    """
        { $(Num.to_str mouseCell2.column), ${Num.to_str mouseCell2.row} } { ${Num.to_str selectedCell.column}, $(Num.to_str selectedCell.row) }; d=$( Num.to_str distance ), $( Num.to_str (Num.round (hoverCellPoint.y)) ) world
        Mouse to World: ( $(Num.round mouse_world.x|>Num.to_str), ${ Num.round mouse_world.y |> Num.to_str } )
        Mag: ${Inspect.to_str m_dist}
    """


    Draw.draw! White |{}|
        Draw.with_mode_2d!
            model.camera
            |{}|
                _ = renderMap! model.map_tiles selectedCell model.hexTexture
                drawPath! unitPath_ Navy 5 Bool.false
                path_color = if Keys.down pf.keys KeyLeftControl then Red else White
                drawPath! straightLine path_color 5 Bool.false
                renderHexOutline! summary_unit.cell Hex.hexSize  Black
                renderHexOutline! Hex.pixelToHex(summary_unit.position) Hex.hexSize  Navy
                renderHexOutline! summary_unit.dest Hex.hexSize Aqua
                renderHexOutline! mouseCell2  (Hex.hexSize + 4) Red
                List.for_each!(model.units, |unit| drawUnit! unit)
        Draw.text! { pos: { x: 10, y: 52 }, text: debugText, size: 40, color: textColor }
        Draw.circle! {
            center: pf.mouse.position,
            color: Red,
            radius: 5,
        }
        summary_text_dims = Effect.measure_text! summary_text 40 1 |> InternalVector.to_vector2
        Draw.text! { pos: { x: 10, y: screenDims.height - (Num.to_f32 summary_text_dims.y + 40) }, text: summary_text, size: 40, color: summary_text_color }

    Ok updated

renderHexOutline! = \cell, size, color ->
    points = Hex.hexPoints (Hex.hexToPixel cell) size
    drawPath! points color 3 Bool.true

# renderMap! : (List HexTile), Doubled, _ -> Result _
renderMap! = |tiles, selectedCell, texture|
    _ = List.for_each_try!(tiles, |tile|
        cell = tile.cell
        tx_pos = HexTile.texture_position tile (Hex.hexSize * 2)
        drawHex! cell texture tx_pos Black
        Ok {}
    )
    # drawHex! selectedCell texture { x: -128, y: 4 * 128 } White
    renderHexOutline! selectedCell Hex.hexSize Black
    # Draw.circle! {
    #     color: White,
    #     center: hexToPixel selectedCell |> Hex.addPoint translate,
    #     radius: 10,
    # }
    Ok {}

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

drawHex! = |cell, texture, pos, color|
    center =
        hexToPixel (doubled (cell.column) (cell.row))
    offset =
        center
        |> Hex.addPoint { x: (-1 * Hex.hexSize), y: -1 * Hex.hexSize }
    tint = White
    Draw.texture_rec! {
        texture,
        pos: offset,
        tint,
        source: {
            x: pos.x,
            y: pos.y,
            width: Hex.hexSize * 2,
            height: Hex.hexSize * 2,
        },
    }
    cellText = "${cell.column |> Num.to_str}, ${cell.row |> Num.to_str}"
    Draw.text! {
        color,
        size: 24,
        text: cellText,
        pos: { x: center.x - 24, y: center.y - 12 },
    }

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

updateFrameCount = |prev|
    frameCount = Num.add_wrap prev.frameCount 1
    { prev & frameCount }

# # getCurrentPlayer : _ -> [Player1, Player2]
# getCurrentPlayer = \netplay ->
#     when netplay is
#         Enabled p -> p
#         _ -> Player1

# getPlayerInputs =
#     p1 = W4.getGamepad! Player1
#     p2 = W4.getGamepad! Player2
#     Task.ok (p1, p2)

# padFor = \string, size ->
#     strLen = Str.countUtf8Bytes string
#     diff = (size - strLen)
#     if diff <= 0 then
#         ""
#     else
#         List.repeat " " diff
#         |> Str.joinWith ""

# leftPad = \string, size ->
#     padFor string size
#     |> Str.concat string

# rightPad = \string, size ->
#     Str.concat string (padFor string size)

# ## Should left pad
# expect
#     actual = leftPad "123" 5
#     expected = "  123"
#     actual == expected
# expect
#     actual = rightPad "123" 5
#     expected = "123  "
#     actual == expected

# expect
#     actual = Health.new {
#         type: Infantry,
#         entity: 1,
#         readiness: Defending,
#         lastFired: 0,
#         rate: 200,
#         range: 1,
#         damage: 25,
#     }
#     (Health.range actual) == 1

# getHoverCell = \hoverCell, gamePad, lastGamepad, isObstacle ->
#     go = \cell ->
#         cell
#         |> \{ row, column } ->
#             if gamePad.up && Bool.not lastGamepad.up then
#                 { row: row - 2, column }
#             else
#                 { row, column }
#         |> \{ row, column } ->
#             if gamePad.down && Bool.not lastGamepad.down then
#                 { row: row + 2, column }
#             else
#                 { row, column }
#         |> \{ row, column } ->
#             if gamePad.left && Bool.not lastGamepad.left then
#                 { row: row + 1, column: column - 1 }
#             else
#                 { row, column }
#         |> \{ row, column } ->
#             if gamePad.right && Bool.not lastGamepad.right then
#                 { row: row + 1, column: column + 1 }
#             else
#                 { row, column }
#     helper = \cell ->
#         next = go cell
#         if Hex.clamped next && isObstacle next then
#             helper (next)
#         else
#             next
#     helper hoverCell |> Hex.clamp

# testInput = {
#     up: Bool.false,
#     down: Bool.false,
#     left: Bool.false,
#     right: Bool.false,
# }

# expect
#     actual = getHoverCell (doubled 0 0) testInput testInput \_ -> Bool.false
#     expected = doubled 0 0
#     actual == expected
# expect
#     actual = getHoverCell (doubled 0 0) { testInput & down: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 0 2
#     actual == expected
# expect
#     actual = getHoverCell (doubled 1 1) { testInput & left: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 0 2
#     actual == expected
# expect
#     actual = getHoverCell (doubled 0 0) { testInput & right: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 1 1
#     actual == expected

# expect
#     actual = getHoverCell (doubled 0 12) { testInput & right: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 1 13
#     actual == expected

# # gethoverCell clamps when next cell is out of bounds
# expect
#     actual = getHoverCell (doubled 12 0) { testInput & right: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 11 1
#     actual == expected

# expect
#     actual = getHoverCell (doubled 0 0) { testInput & down: Bool.true } testInput \_ -> Bool.false
#     expected = doubled 0 2
#     actual == expected

# # getHoverCell skips blocked cells below
# expect
#     actual = getHoverCell (doubled 0 0) { testInput & down: Bool.true } testInput \cell -> cell.row == 2 && cell.column == 0
#     expected = doubled 0 4
#     actual == expected

# # getHoverCell skips multiple blocked cells below
# expect
#     isOccupied = \cell -> List.contains [doubled 0 2, doubled 0 4] cell
#     actual = getHoverCell
#         (doubled 0 0)
#         { testInput & down: Bool.true }
#         testInput
#         isOccupied
#     expected = doubled 0 6
#     actual == expected

# # getHoverCell up after down is no op
# expect
#     isOccupied = \cell -> List.contains [doubled 0 2, doubled 0 4] cell
#     actual =
#         getHoverCell (doubled 0 0) { testInput & down: Bool.true } testInput isOccupied
#         |> getHoverCell { testInput & up: Bool.true } testInput isOccupied
#     expected = doubled 0 0
#     actual == expected

# makeUnitIdLocator = \units -> \queryId -> List.find_first units \{ id } -> id == queryId
# makeUnitIdIndexer = \units -> \queryId -> List.find_firstIndex units \{ id } -> id == queryId

# maybeUpdateHoverCell = \isOccupied, oldIndex, newIndex, current ->
#     if oldIndex != newIndex then
#         Hex.neighborsOf current
#         |> List.drop_if \c -> isOccupied c
#         |> List.first
#         |> Result.with_default current
#     else
#         current

# renderInGame = \model, netplay, _frameCount ->
#     thePlayer = getCurrentPlayer netplay
#     theArmy = playerArmy thePlayer
#     getUnitById = makeUnitIdLocator model.units
#     totalMs =
#         model.launchIn
#         |> frameCountToSeconds
#         |> Num.mul 1000
#         |> Num.round
#     msRemaining =
#         model.launchTimer
#         |> frameCountToSeconds
#         |> Num.mul 1000
#         |> Num.round

#     (theMove, theHoverCell) =
#         if theArmy == Union then
#             (model.moves.0, model.hovering.union)
#         else
#             (model.moves.1, model.hovering.confederate)

#     drawObs = List.walk model.map.obstacles (Task.ok {}) \task, obs ->
#         task!
#         { x, y } = Hex.hexToPixel obs
#         W4.setShapeColors! { border: Color4, fill: None }
#         Drawing.blitHexagon! obs boardRect Assets.filledHex
#         W4.setTextColors { fg: Color1, bg: None }
#         |> Task.await \_ -> W4.text "@" { x: x + boardRect.x + 4, y: boardRect.y + y + 2 }
#     Drawing.drawBoardRect! boardRect
#     drawObs!

#     getOwner = \pad -> getPadOwner model.units pad
#     Drawing.drawPads! model.map.launchPads getOwner
#     Drawing.drawLaunchTimer! msRemaining totalMs {
#         x: (boardRect.x + (Num.toI32 boardRect.width)) |> Num.toFrac |> Num.div 2 |> Num.round,
#         y: (boardRect.y + (Num.toI32 boardRect.height)) |> Num.toFrac |> Num.div 2 |> Num.round,
#     }
#     # Drawing.drawPlayer! model.moves.0 getUnitById Color2 model.hovering.union
#     # Drawing.drawPlayer! model.moves.1 getUnitById Color3 model.hovering.confederate
#     np = W4.getNetplay!
#     moves = when np is
#         Disabled ->
#             Drawing.drawPlayer model.moves.0 getUnitById Color2 model.hovering.union
#             |> Task.await \_ -> Drawing.drawPlayer model.moves.1 getUnitById Color3 model.hovering.confederate
#         Enabled _ ->
#             if theArmy == Union then
#                 Drawing.drawPlayer model.moves.0 getUnitById Color2 model.hovering.union
#             else
#                 Drawing.drawPlayer model.moves.1 getUnitById Color3 model.hovering.confederate

#     moves!
#     # isNetplay = when np is
#     #     Disabled -> drawP1 |> Task.await \_ -> drawP2
#     #     _ -> Task.ok {}
#     # drawP1!
#     # drawP2!

# #     np = isNetplay
# #         |> Task.await \isNet ->
# #             if theArmy == Union then
# #                 drawP1
# #             else if isNet then
# #                 drawP2
# #             else
# #                 drawP1 |> Task.await \_ -> drawP2

# #     Drawing.drawHoverPositon! model.hovering.union (armyColor Union)
# #     W4.setShapeColors! { border: armyColor Confederates, fill: None }
# #     Drawing.drawHoverPositon! model.hovering.confederate (armyColor Confederates)

#     selectedUnit =
#         when theMove is
#             Selected id _ | Destination id _ _ ->
#                 getUnitById id
#                 |> Result.map_ok \unit -> Chosen unit
#                 |> Result.with_default None

#             _ -> None

#     _ <-
#         List.keepIf model.units \u -> Unit.isAlive u
#         |> \units -> Drawing.drawUnits units boardRect # isSelected theArmy
#         |> Task.await

#     task =
#         when selectedUnit is
#             Chosen u -> Drawing.drawSelectionIndicator (u.position) (armyColor u.army)
#             None -> Task.ok {}
#     task!

#     unitSummary =
#         when theMove is
#             Selected id path | Destination id _ path ->
#                 getUnitById id
#                 |> Result.map_ok \unit -> Unit.summary unit path
#                 |> Result.with_default "He dead ..."

#             Finished -> "Press \u(81)\nto choose\nnew unit"

#     shapeColors = { fill: Color1, border: Color4 }
#     W4.setShapeColors! shapeColors
#     infoY = boardRect.y + (Num.toI32 boardRect.height)
#     W4.rect! {
#         x: 0,
#         y: infoY,
#         height: Num.toU32 (160 - (boardRect.y + (Num.toI32 boardRect.height))),
#         width: 160,
#     }
#     W4.setTextColors! { fg: Color4, bg: None }
#     W4.text! unitSummary {
#         x: boardRect.x,
#         y: infoY + 2,
#     }
#     W4.setTextColors! { bg: Color4, fg: None }
#     currCellText = " $(Num.toStr theHoverCell.column),$(Num.toStr theHoverCell.row) "
#     width = Str.countUtf8Bytes currCellText
#     W4.text! currCellText {
#         x: boardRect.x + (Num.toI32 boardRect.width) - (Num.toI32 (width * 7)),
#         y: (boardRect.y + (Num.toI32 boardRect.height) + 2) |> Num.abs,
#     }
#     Drawing.drawToolbar {
#         x: 0,
#         y: 160 - 50,
#         # (Num.toI32 boardRect.height) |> Num.sub 25
#     }

# updateMoveChoice : Unit.MoveChoice, _ -> (Unit.MoveChoice, U64)
# updateMoveChoice = \currentChoice, { hovering, theArmy, getUnitById, zPressed, wasPressed, nextIndex, isOccupied, units } ->
#     selected = getUnitFromClickedCell units hovering theArmy
#     total = List.len units
#     idx = if total > 0 then nextIndex % total else 0
#     (resultChoice, newIndex) =
#         when currentChoice is
#             Destination _ _ _ | _ if zPressed ->
#                 List.get units idx
#                 |> Result.map_ok \u -> (Selected u.id u.lastPath, idx + 1)
#                 |> Result.with_default (currentChoice, nextIndex)

#             Destination _ _ _ ->
#                 List.get units idx
#                 |> Result.map_ok \u -> (Selected u.id u.lastPath, idx + 1)
#                 |> Result.with_default (currentChoice, nextIndex)

#             Finished if wasPressed ->
#                 units
#                 |> List.find_first \{ cell, army } -> cell == hovering && army == theArmy
#                 |> Result.map_ok \u -> (Selected u.id [], nextIndex)
#                 |> Result.with_default (Finished, nextIndex)

#             Selected id path if wasPressed ->
#                 getUnitById id
#                 |> Result.map_ok \u ->
#                     if u.cell == hovering then
#                         (Finished, nextIndex)
#                     else if isOccupied hovering then
#                         selected
#                         |> Result.map_ok \newUnit -> (Selected newUnit.id newUnit.lastPath, nextIndex)
#                         |> Result.with_default (Finished, nextIndex)
#                     else
#                         (Destination u.id hovering path, nextIndex)
#                 |> Result.with_default (currentChoice, nextIndex)

#             Selected id prevPath ->
#                 destUpdated =
#                     List.last prevPath
#                     |> Result.map_ok \prevDest -> prevDest != hovering
#                     |> Result.with_default Bool.true
#                 unitMoved =
#                     List.first prevPath
#                     |> Result.try \last ->
#                         getUnitById id
#                         |> Result.map_ok \u -> u.cell != last
#                     |> Result.with_default Bool.true
#                 if unitMoved || destUpdated then
#                     (
#                         updatePlayerPlannedPath id hovering prevPath getUnitById isOccupied,
#                         nextIndex,
#                     )
#                 else
#                     (Selected id prevPath, nextIndex)

#             otherwise -> (otherwise, nextIndex)

#     (resultChoice, newIndex)

# updatePlayerPlannedPath = \id, hovering, prevPath, getUnitById, isOccupied ->
#     getUnitById id
#     |> Result.map_ok \u ->
#         withoutMe = \cell ->
#             if u.cell == cell then
#                 Bool.false
#             else
#                 isOccupied cell
#         if withoutMe hovering then
#             prevPath
#         else
#             Hex.findGraph u.cell hovering withoutMe
#             |> Result.map_ok \p ->
#                 when u.readiness is
#                     Moving ->
#                         Unit.reroutePath p u.lastPath u.cell

#                     _ -> p
#             |> Result.with_default prevPath
#     |> Result.map_ok \newPath -> Selected id newPath
#     |> Result.with_default Finished

# getCombatOrders = \unitsAndModifiers ->
#     justUnits = List.map unitsAndModifiers .0
#     List.keepOks unitsAndModifiers \(u, modifier) ->
#         Unit.combatOrder u modifier justUnits

# randList = \{ length } ->
#     List.range { start: At 0, end: At length }
#     |> List.walk (Task.ok []) \last, _ ->
#         last
#         |> Task.await \accum ->
#             W4.randBetween { start: 1, before: 100 }
#             |> Task.map \mod -> List.append accum mod

# runCombat = \units, modifiers ->
#     indexer = makeUnitIdIndexer units
#     # First get all of the eligible attackers
#     # and their single target
#     combatOrders =
#         List.map2 units modifiers \u, m -> (u, m)
#         |> getCombatOrders
#     # Now walk over the list of (attacker, defender, damage) triples
#     # and accumulate an updated list of units
#     List.walk combatOrders units \accum, (source, target, damage) ->
#         # Update `readinesss` of attacking unit
#         updatedAccum =
#             List.find_firstIndex accum \u -> u.id == source.id
#             |> Result.map_ok \attackerIdx ->
#                 List.update accum attackerIdx \u ->
#                     { u & readiness: Cooldown (Num.round u.cooldownRate) }
#             |> Result.with_default accum
#         ## Update `health` of defender
#         indexer target.id
#         |> Result.map_ok \victimIndex ->
#             List.update updatedAccum victimIndex \u ->
#                 #           ^^--- update the new version
#                 { u & health: Unit.takeHit u damage }
#         |> Result.with_default updatedAccum

# ## runCombat should get targets, update health
# expect
#     testUnits = [
#         {
#             id: 0,
#             cooldownRate: 100.0,
#             readiness: Ready,
#             army: Union,
#             cell: doubled 2 4,
#             health: Living (Health.make 200),
#             attackDamage: 5u32,
#         },
#         {
#             id: 1,
#             cooldownRate: 50.0,
#             readiness: Ready,
#             army: Confederates,
#             cell: doubled 3 3,
#             health: Living (Health.make 100),
#             attackDamage: 10u32,
#         },
#     ]

#     actual =
#         runCombat testUnits [100, 100]
#         |> List.map \{ health } ->
#             when health is
#                 Living h -> Health.health h
#                 _ -> 0

#     expectedLessThan = List.map testUnits \u ->
#         when u.health is
#             Living h -> Health.health h
#             _ -> 0

#     List.all
#         (List.map2 actual expectedLessThan (\a, e -> a <= e))
#         \r -> r == Bool.true

# ## runCombat should not over/underflow
# expect
#     testUnits = [
#         {
#             id: 0,
#             cooldownRate: 100.0,
#             readiness: Ready,
#             army: Union,
#             cell: doubled 2 4,
#             health: Living (Health.make 9),
#             attackDamage: 10u32,
#         },
#         {
#             id: 1,
#             cooldownRate: 100.0,
#             readiness: Ready,
#             army: Confederates,
#             cell: doubled 3 3,
#             health: Living (Health.make 100),
#             attackDamage: 10u32,
#         },
#     ]
#     actual = runCombat testUnits [100, 100] |> List.map Unit.isAlive
#     expected = [Bool.false, Bool.true]
#     actual == expected
# # updateBackground : Model -> Model
# # updateBackground = \gameState ->
# #     {
# #         gameState &
# #         background:
# #             when gameState.screenState is
# #                 TitleScreen _ -> getArt gameState
# #                 InGame _ -> getArt gameState
# #                 GameOver { winner } ->
# #                     when winner is
# #                         Union -> Assets.dawn
# #                         Confederates -> Assets.flame
# #     }

# # getArt : Model -> Sprite
# # getArt = \model ->
# #     if model.frameCount % 301 != 0 then
# #         model.background
# #     else
# #         totalBackgrounds = List.len model.backgrounds
# #         (Num.toU32 model.frameCount) % (Num.toU32 totalBackgrounds)
# #         |> \index -> List.get model.backgrounds (Num.toU64 index)
#
# getPadOwner : List Unit, LaunchPad -> [Owned [Union, Confederates], Unowned]
# getPadOwner = |units, pad|
#     byArmy = List.walk pad [] |accum, cell|
#         List.find_first units |u| u.cell == cell
#         |> Result.map_ok |u| List.append accum u.army
#         |> Result.with_default accum

#     (union, confederates) = List.walk byArmy (0, 0) |accum, army|
#         when army is
#             Union -> (accum.0 + 1, accum.1)
#             Confederates -> (accum.0, accum.1 + 1)

#     if union == confederates then
#         Unowned
#     else if union > 0 and confederates == 0 then
#         Owned Union
#     else if confederates > 0 and union == 0 then
#         Owned Confederates
#     else
#         Unowned

getFirstMove = |forArmy, units|
    List.find_first units |{ army }| army == forArmy
    |> Result.map_ok |{ id }| Selected id []
    |> Result.with_default Finished

newGame : U64, Army, Army -> GameState
newGame = |startFrame, player1Army, player2Army|
    launchIn = 60 * 11

    units = Unit.initial (|_| Bool.true) # List.dropLast initialUnits 0 # [] #initialUnits
    unionMove = getFirstMove Union units
    confederateMove = getFirstMove Confederates units

    {
        startFrame,
        units,
        launchIn,
        unitIndex: {
            union: 1u64,
            confederate: 1u64,
        },
        moves: (unionMove, confederateMove),
        armies: (player1Army, player2Army),
        map: {
            obstacles: [
                doubled 2 4,
                doubled 4 6,
                doubled 8 6,
                doubled 6 4,
                doubled 5 5,
                doubled 7 5,
                doubled 6 6,
                doubled 6 8,
                doubled 5 7,
                doubled 7 7,
                doubled 10 4,
                doubled 2 10,
                doubled 3 11,
            ],
            launchPads: [
                # [
                #     doubled 3 9,
                #     doubled 2 10,
                #     doubled 3 11,
                # ],
                [
                    doubled 5 3,
                    doubled 7 3,
                ],
                [
                    doubled 5 9,
                    doubled 6 10,
                    doubled 7 9,
                ],
            ],
        },
        launchTimer: launchIn,
        hovering: {
            union: doubled 4 0,
            confederate: doubled 8 4,
        },
    }


# armyColor = |army|
#     when army is
#         Union -> Color2
#         Confederates -> Color3

# armyName = |army|
#     when army is
#         Union -> "Union"
#         Confederates -> "Confederacy"

# armyPalette = |army|
#     when army is
#         Union -> unionPalette
#         Confederates -> confederatePalette

# playerArmy = |player|
#     when player is
#         Player1 | Player3 -> Union
#         Player2 | Player4 -> Confederates


# init : Task Model []
# init =
#     W4.setPalette! palette
#     Task.ok baseState
# # getPadOwner : List Unit, LaunchPad -> [Owned [Union, Confederates], Unowned]
# getPadOwner = \units, pad ->
#     byArmy = List.walk pad [] \accum, cell ->
#         List.find_first units \u -> u.cell == cell
#         |> Result.map_ok \u -> List.append accum u.army
#         |> Result.with_default accum

#     (union, confederates) = List.walk byArmy (0, 0) \accum, army ->
#         when army is
#             Union -> (accum.0 + 1, accum.1)
#             Confederates -> (accum.0, accum.1 + 1)

#     if union == confederates then
#         Unowned
#     else if union > 0 && confederates == 0 then
#         Owned Union
#     else if confederates > 0 && union == 0 then
#         Owned Confederates
#     else
#         Unowned

# countPadsByOwner = \owners ->
#     List.walk owners (0, 0) \accum, owner ->
#         when owner is
#             Owned Union -> (accum.0 + 1, accum.1)
#             Owned Confederates -> (accum.0, accum.1 + 1)
#             Unowned -> accum

# getLaunchStatus = \owners ->
#     (union, confederates) = countPadsByOwner owners
#     if union > confederates then
#         InControl Union
#     else if confederates > union then
#         InControl Confederates
#     else
#         StaleMate

# cellObstacle : List Doubled -> (Doubled -> Bool)
# cellObstacle = \occupied -> \cell -> List.contains occupied cell

# isCellOccupied : List Unit, List Doubled -> (Doubled -> Bool)
# isCellOccupied = \units, obstacles ->
#     List.map units (\u -> u.cell)
#     |> List.concat obstacles
#     |> cellObstacle

 #             |> Result.with_default model.background


# drawUnit!: Unit.Unit, Hex.Point -> {}
drawUnit! = \unit ->
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

    outlinePoint = Hex.addPoint point { x: (Hex.hexSize / -2), y: 16 }
    (readyColors, width) =
        when unit.readiness is
            Cooldown timer ->
                t =
                    unit.cooldownRate
                    |> Num.sub (Num.to_f32 timer)
                    |> Num.div unit.cooldownRate
                w = Hex.lerp 0 64 t |> Num.round
                (
                    { fill: Red, border: Black },
                    w,
                )

            Ready -> ({ fill: Blue, border: Black }, Hex.horizontalSpace |> Num.round)
            Moving _ -> ({ fill: RGBA 0 0 0 0, border: Black }, Hex.horizontalSpace |> Num.round)
    Draw.rectangle! {
        color: Black,
        rect: {
            width: 68,
            height: 68,
            x: drawTo.x - (68 / 2),
            y: drawTo.y - (68 / 2),
        }
    }
    Draw.rectangle! {
        color: White,
        rect: {
            width: 64,
            height: 64,
            x: drawTo.x - (64 / 2),
            y: drawTo.y - (64 / 2),
        }
    }
    _ = when unit.type is
        Cavalry  -> Draw.circle! {
            center: { x: drawTo.x, y: drawTo.y - 12 },
            radius: 16,
            color: Black,
        }
        Artillery -> Draw.circle! {
            center: { x: drawTo.x, y: drawTo.y - 12 },
            radius: 16,
            color: Maroon,
        }
        Infantry -> Draw.rectangle! {
            color: Red,
            rect: {
                width: 24,
                height: 24,
                x: drawTo.x - 12,
                y: drawTo.y - 16,
            }
        }
    # Draw.rectangle! {
    #     color: readyColors.border,
    #     rect: {
    #         x: outlinePoint.x + 1 |> Num.to_f32,
    #         y: outlinePoint.y + 1 |> Num.to_f32,
    #         width: width - 2 |> Num.to_f32,
    #         height: 1,
    #     }
    # }
    Draw.rectangle_gradient_h! {
        left: readyColors.fill,
        right: Red,
        rect: {x: outlinePoint.x,
        y: outlinePoint.y,
        width: Hex.horizontalSpace / 2,
        height: 8 }
    }
    renderHexOutline! unit.cell Hex.hexSize (RGBA 0 0 0 0)
