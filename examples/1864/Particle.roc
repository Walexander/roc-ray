module [Entity, System, CompDeathTime, CompRender, CompFade, CompExplode, CompGraphic, CompVelocity, CompPosition, ECS, ComponentData, Components,
    add, spawn, make, update, get_by_component, move_to]
import rr.RocRay exposing [Color ]
import Hex
import rand.Random
import PointyHex
import HexTile
import Matrix4 exposing [Matrix4]

Entity : {
  id: I32,
}
CompDeathTime: {
  lifetime: I32,
  dead_frame: I32,
}
PathFinder : Hex.Doubled, Hex.Doubled -> List Hex.Doubled
CompFade : {
      rRate: U8,
    rMin: U8,
    gRate: U8,
    gMin: U8,
    bRate: U8,
    bMin: U8,
    aRate: U8,
    aMin: U8,
}

CompExplode : {
  num_particles: I32
}

CompGraphic : {
  color: Color,
  rotation: F32,
  radius: F32,
}

CompHealth: {
  max_health: U16,
  current_health: U16
}

CompPosition : {
  x: F32,
  y: F32,
}

# CompAttack: {
#   attack_damaage: U16,
# }
# CompRotation : {
#   radians: F32,
# }
CompTransform : {
  transform: Matrix4
}
CompVelocity : {
  dx: F32,
  dy: F32,
}
CompMoveRequest : {
  destination: Hex.Doubled
}
CompMoveSegment: {
  start: Hex.Point,
  end: Hex.Point,
  rate: F32,
  progress: F32,
}
CompPathRequest : {
  start: Hex.Doubled,
  goal: Hex.Doubled,
}
CompOccupies : {
  cell: Hex.Doubled,
  army: [ Union, Confederates ]
}
CompPath : {
  path: List Hex.Doubled,
  goal: Hex.Doubled
}

AttackOrders : {
  id: I32,
  target: CompOccupies,
  target_id: I32,
}
CompRender : [
  Texture {
    texture: RocRay.Texture,
    source: RocRay.Rectangle,
    origin: RocRay.Vector2,
    scale: RocRay.Vector2,
    tint: RocRay.Color,
    flip: [FlipX, FlipY, FlipBoth, None],
  }
]
Components : [
  Position,
  Moveable,
  Transform,
  Graphics,
  Killable,
  MoveRequest,
  Occupies,
  PathRequest,
  MoveSegment,
  Renderable,
  Path,
]

ComponentData : [
  Position CompPosition,
  Moveable CompVelocity,
  Transform CompTransform,
  Graphics CompGraphic,
  Killable CompDeathTime,
  MoveRequest CompMoveRequest,
  PathRequest CompPathRequest,
  Occupies CompOccupies,
  MoveSegment CompMoveSegment,
  Renderable CompRender,
  Path CompPath,
]

System: ECS -> ECS
ECS: {
  entities: List Entity,
  next_size: I32,
  current_size: U32,
  rand: Random.State,
  max: I32,

  fadable: Dict I32 CompFade,
  killable: Dict I32 CompDeathTime,
  explodable: Dict I32 CompExplode,
  positionable: Dict I32 CompPosition,
  moveable: Dict I32 CompVelocity,
  scalable: Dict I32 CompGraphic,
  transformable: Dict I32 CompTransform,
  health: Dict I32 CompHealth,
  occupants: Dict I32 CompOccupies,
  emissions: List { lifetime: I32, position: CompPosition, explosion: CompExplode },
  removals: List Entity,
  attack_orders: List AttackOrders,
  paths: Dict I32 CompPath,
  renderables: Dict I32 CompRender,
  path_requests: Dict I32 CompPathRequest,
  move_requests: Dict I32 CompMoveRequest,
  move_segments: Dict I32 CompMoveSegment,
  render_requests: List CompRender,
}


make : Random.State -> ECS
make = |rand| {
  max: 256,
  entities: [],
  next_size: 0,
  current_size: 0,
  rand,
  fadable: Dict.empty {},
  killable: Dict.empty {},
  moveable: Dict.empty {},
  positionable: Dict.empty {},
  transformable: Dict.empty {},
  explodable: Dict.empty {},
  scalable: Dict.empty {},
  health: Dict.empty {},
  occupants: Dict.empty {},
  path_requests: Dict.empty {},
  move_requests: Dict.empty {},
  move_segments: Dict.empty {},
  paths: Dict.empty {},
  renderables: Dict.empty {},
  emissions: [],
  removals: [],
  attack_orders: [],
  render_requests: [],
}

add_entity : ECS -> (ECS, I32)
add_entity = |ecs|
  id = ecs.next_size + 1

  ({
    ecs&
    next_size: id,
    entities: List.append ecs.entities { id },
    current_size: ecs.current_size + 1,
  }, id)


# spawn : ECS, { pos: { x: F32, y: F32 }, max_lifetime: I32, num_particles: U32 } -> ECS
spawn = |ecs, { position ?? { x: 0, y: 0 }, num_particles ?? 0, max_lifetime ?? 120 }|
  rand_v = { Random.chain  <-
    life: Random.bounded_u32(Num.to_f32 max_lifetime |> Num.div 2 |> Num.round, max_lifetime),
    dx: Random.bounded_i32 -100_000 100_000,
    dy: Random.bounded_i32 -100_000 100_000,
    r: Random.bounded_u8(192, 255),
    g: Random.bounded_u8(64, 128),
    b: Random.bounded_u8(64, 128),
    a: Random.bounded_u8(128, 255),
  } |> Random.map |{life, dx, dy, r, g, b, a}| {
      life,
      dx: dx |> Num.to_f32 |> Num.div 100_00_000,
      dy: dy |> Num.to_f32 |> Num.div 100_00_000,
      color: RGBA(r, g, b, a)
  }

  next_v = Random.step ecs.rand rand_v
  life_frames = next_v.value.life |> Num.to_i32
  life_scale = 0.1 * (Num.to_f32 life_frames)
  (ecs1, id) = add_entity ecs

  { ecs1&
    rand: next_v.state,
    killable: Dict.insert ecs1.killable id { lifetime: life_frames, dead_frame: life_frames },
    explodable: if num_particles <= 0 then Dict.remove ecs1.explodable id else Dict.insert ecs1.explodable id { num_particles },
    scalable: Dict.insert ecs1.scalable id {
        rotation: 0,
        radius: life_scale |> Num.to_f32,
        color: next_v.value.color,
    },
    transformable: Dict.insert ecs1.transformable id { transform: Matrix4.identity },
    positionable: Dict.insert ecs1.positionable id (position),
    moveable: Dict.insert ecs1.moveable id ( { dx: next_v.value.dx, dy: next_v.value.dy }),
  }

add : ECS, List ComponentData -> (I32, ECS)
add = |ecs, components|
  ( ecs_, id ) = add_entity ecs
  List.walk components ecs_ |accum, component|
    when component is
      Position position -> { accum& positionable: Dict.insert accum.positionable id position }
      Moveable velocity -> {accum& moveable: Dict.insert accum.moveable id velocity }
      Transform xform -> { accum& transformable: Dict.insert accum.transformable id xform}
      Killable life -> { accum& killable: Dict.insert accum.killable id life }
      Graphics graphics -> { accum& scalable: Dict.insert accum.scalable id graphics }
      MoveRequest request -> {accum & move_requests: Dict.insert accum.move_requests id request}
      PathRequest request -> {accum & path_requests: Dict.insert accum.path_requests id request}
      Occupies occupancy -> {accum & occupants: Dict.insert accum.occupants id occupancy}
      MoveSegment segment -> {accum & move_segments: Dict.insert accum.move_segments id segment}
      Path path -> {accum & paths: Dict.insert accum.paths id path}
      Renderable render -> {accum & renderables: Dict.insert accum.renderables id render }
  |> |ecs1| (id, ecs1)
      # Drawable data -> { accum& drawable: Dict.insert accum.drawable id data }
      # Pathable path -> {accum& pathable: Dict.insert accum.pathable id path }

get_by_component : ECS, List Components -> Dict I32 (List ComponentData)
get_by_component = |ecs, components|
   List.walk_with_index components (Set.empty {}) |ids, component, index|
        c_ids = when component is
            Transform -> Dict.keys ecs.transformable
            Moveable -> Dict.keys ecs.moveable
            Position -> Dict.keys ecs.positionable
            Graphics -> Dict.keys ecs.scalable
            Killable -> Dict.keys ecs.scalable
            MoveRequest -> Dict.keys ecs.move_requests
            MoveSegment -> Dict.keys ecs.move_segments
            Occupies -> Dict.keys ecs.occupants
            PathRequest -> Dict.keys ecs.path_requests
            Path -> Dict.keys ecs.paths
            Renderable -> Dict.keys ecs.renderables

        if index == 0 then Set.from_list c_ids
        else Set.intersection ids Set.from_list(c_ids)
  |> Set.walk (Dict.empty {}) |accum, id|
      List.walk components accum |accum2, comp|
        result = when comp is
          Position -> Dict.get(ecs.positionable, id) |> Result.map_ok Position
          Transform -> Dict.get(ecs.transformable, id) |> Result.map_ok Transform
          Moveable -> Dict.get(ecs.moveable, id) |> Result.map_ok Moveable
          Graphics -> Dict.get(ecs.scalable, id) |> Result.map_ok Graphics
          Killable -> Dict.get(ecs.killable, id) |> Result.map_ok Killable
          MoveRequest -> Dict.get(ecs.move_requests, id) |> Result.map_ok MoveRequest
          Occupies -> Dict.get(ecs.occupants, id) |> Result.map_ok Occupies
          PathRequest -> Dict.get(ecs.path_requests, id) |> Result.map_ok PathRequest
          MoveSegment -> Dict.get(ecs.move_segments, id) |> Result.map_ok MoveSegment
          Renderable -> Dict.get(ecs.renderables, id) |> Result.map_ok Renderable
          Path -> Dict.get(ecs.paths, id) |> Result.map_ok Path
        data = Result.map_ok result List.single |> Result.with_default []
        existing = Dict.get accum2 id |> Result.with_default []
        new_data = List.concat existing data
        Dict.insert accum2 id new_data

remove_entity : ECS, I32 -> ECS
remove_entity = |ecs, id| {
    ecs&
    entities: List.keep_if ecs.entities |{id: id_}| id_ != id,
    current_size: ecs.current_size - 1,
    fadable: Dict.remove ecs.fadable id,
    killable: Dict.remove ecs.killable id,
    explodable: Dict.remove ecs.explodable id,
    moveable: Dict.remove ecs.moveable id,
    positionable: Dict.remove ecs.positionable id,
    scalable: Dict.remove ecs.scalable id,
    transformable: Dict.remove ecs.transformable id,
    paths: Dict.remove ecs.paths id,
    occupants: Dict.remove ecs.occupants id,
    move_segments: Dict.remove ecs.move_segments id,
    path_requests: Dict.remove ecs.path_requests id,
    move_requests: Dict.remove ecs.move_requests id,
}

## TODO: when moving, this attempt to find a path from both our "start" and "end" cells
## Translate a movement request into a path finding one by looking up the current cell.
## this will always use the cell of the current position as the starting point which
## can sometimes cause us to proceed to the next cell only to promptly turn back around.
## this should probably make some attempt to identify if we are
## already heading in the direction of our new goal or if we need to turn around.

move_request_system : ECS -> ECS
move_request_system = |ecs|
  dict = get_by_component ecs [MoveRequest, Position]
  size = Dict.len dict
  _ = if size > 0 then
     dbg "processing ${Inspect.to_str size} move requests"
  else
      ""
  Dict.walk dict ecs |accum, id, comp|
      when comp is
        [MoveRequest { destination }, Position point] ->
          from_cell = PointyHex.pixel_to_hex point
          {
            accum&
            move_requests: Dict.remove accum.move_requests id,
            path_requests:
              Dict.insert accum.path_requests id { start: from_cell, goal: destination }
          }
        _ -> accum

## path_request_system(ecs, path_finder)
## handle path finding
## smoothly transition current movemennt to match the new path
path_request_system : ECS, PathFinder -> ECS
path_request_system = |ecs, path_finder|
  dict = get_by_component ecs [PathRequest, Occupies]
  Dict.walk dict ecs |accum, id, components|
      when components is
        [PathRequest { start, goal }, Occupies _] ->
            curr_path = Dict.get accum.paths id |> Result.map_ok .path |> Result.with_default []
            # path find to our new goal
            new_path = path_finder(start, goal)
            if List.len new_path <= 0 then
              accum
            else
              # get the *next* cell (second element) for
              # the previous path
              prev = List.get(curr_path, 1) ?? start
              # and the next one
              next = List.get(new_path, 1)  ?? start
              curr_segment = Dict.get accum.move_segments id
              (path0, new_segment) =
                # are we currently moving?
                when curr_segment is
                  ## no. kick off the new move_segment
                  Err _ -> (new_path, {
                      rate: 0.7,
                      progress: 0,
                      start: PointyHex.hex_to_pixel start,
                      end: PointyHex.hex_to_pixel next
                    })
                  ## yes we are moving so we need to cleanly handle reversing directions
                  ## when necessary
                  Ok segment ->
                    ## where did you come from?
                    start_cell = PointyHex.pixel_to_hex segment.start
                    ## where did you go?
                    end_cell = PointyHex.pixel_to_hex segment.end
                    ## is the next cell for our new path the one we are currently leaving?
                    if start_cell == start && next != end_cell then
                      ## yes --
                      ## so prepend the previous end cell to our new path
                      (List.prepend new_path end_cell,
                      ## and reverse direction, position and progress
                      { start: segment.end, end: segment.start, progress: 1 - segment.progress, rate: segment.rate})

                    ## the next cell in our new path is the same as our previous one
                    ## lucky us
                    else if next == end_cell then
                      ## so we can re-use the existing segment
                      (new_path, segment)
                    else
                      ## we have new plans -- re-use the existing segment so we continue heading to our previous destination
                      ## and prepend that to the path list
                      (List.prepend new_path prev, segment)
              dbg "moving from ${Inspect.to_str start} --> ${Inspect.to_str goal} in ${List.len path0 |> Inspect.to_str} steps"
              {accum &
                path_requests: Dict.remove accum.path_requests id,
                move_segments: Dict.insert accum.move_segments id new_segment,
                paths: if List.len path0 > 0 then Dict.insert accum.paths id { goal, path: path0 } else accum.paths,

              }
        _ -> accum

move_to : ECS, I32, Hex.Doubled -> ECS
move_to = |ecs, id, to| {ecs &
    move_requests: Dict.insert ecs.move_requests id {destination: to}
}

move_segment_system : ECS, _, _ -> ECS
move_segment_system = |ecs, dt_, get_cost|
  dt = Num.to_f64 dt_ |> Num.div 1000 |> Num.to_f32
  get_by_component ecs [Path, MoveSegment, Occupies]
  |> Dict.walk ecs |accum, id, component|
    when component is
      [Path { goal, path }, MoveSegment { start, end, progress, rate }, Occupies { cell, army }] ->
        position = Hex.pointLerp(start, end, progress_)
        cost = get_cost cell
        progress_ = progress + dt * (rate / cost)
        pos_cell = PointyHex.pixel_to_hex position
        if progress_ >= 1 then
            when path is
                [_, _] | [_] | [] ->
                    { accum &
                        move_segments: Dict.remove accum.move_segments id,
                        paths: Dict.remove accum.paths id,
                        positionable: Dict.insert accum.positionable id position,
                    }
                [_, to, next, ..] ->
                    {accum&
                        paths: Dict.insert accum.paths id { goal, path: (List.drop_first path 1) },
                        move_segments: Dict.insert accum.move_segments id {
                            start: PointyHex.hex_to_pixel to,
                            end: PointyHex.hex_to_pixel next,
                            rate,
                            progress: 0,
                        },
                    }
        else
            {accum&
                positionable: Dict.insert accum.positionable id position,
                occupants: Dict.insert accum.occupants id { cell: pos_cell, army },
                move_segments: Dict.insert accum.move_segments id {
                  start, end, progress: progress_, rate,
                },
            }
      _ ->
        dbg "W.T.F? ${Inspect.to_str id}"
        accum

movement_system : ECS, U64 -> ECS
movement_system = |ecs, dt|
    dt_ = Num.to_f32 dt
    # helper = movement_helper dt_
    { ecs&
      positionable: get_by_component ecs [Position, Moveable]
        |> Dict.walk ecs.positionable |accum, id, component|
          when component is
            [Position { x, y }, Moveable { dx, dy }] ->
              Dict.insert accum id { x: x + dx * dt_, y: y + dy * dt_ }
            _ -> accum
    }
transform_system : ECS -> ECS
transform_system = |ecs|
  components = get_by_component ecs [Position, Graphics, Transform]
  transforms : Dict I32 CompTransform
  transforms = Dict.walk components (Dict.empty {}) |updates, id, component|
      when component is
        [Position pos, Graphics {rotation, radius},  Transform _] ->
          scale = Num.max(24, 5 * radius) |> Num.min 48
          scale_rotate = Matrix4.multiply(
            Matrix4.rotate rotation,
            Matrix4.scale { x: scale, y: scale }
          )
          transform = Matrix4.translate pos
            |> Matrix4.multiply scale_rotate
          Dict.insert updates id { transform }
        _ -> updates
  { ecs & transformable: transforms }

# attack_system : ECS -> ECS
# attack_system = |ecs|
#   attack_orders = Dict.to_list ecs.occupants
#       |> List.keep_oks |(id, occupant)|
#         List.keep_oks PointyHex.neighbors(occupant.cell) |cell|
#           Dict.get ecs.occupants_by_cell cell
#           |> Result.try |neighbor_id|
#             Dict.get ecs.occupants neighbor_id
#             |> Result.try |neighbor| if neighbor.army != occupant.army then Ok({ target: neighbor, target_id: neighbor_id }) else Err KeyNotFound
#         |> List.first
#         |> Result.map_ok |{ target, target_id }| { id, target, target_id }
#   { ecs & attack_orders }


lifetime_system : ECS -> ECS
lifetime_system = |ecs|{ecs &
    killable: Dict.map ecs.killable |_, {lifetime, dead_frame}|
      { dead_frame: if dead_frame > 0 then dead_frame - 1 else 0, lifetime }

}
kill_system : ECS -> ECS
kill_system = |ecs|
  removals = Dict.to_list ecs.killable |> List.keep_oks |(id, { dead_frame })|
    if dead_frame > 0 then Err Alive
    else Ok { id }
  {ecs & removals}


# scaling_system : ECS -> ECS
# scaling_system = |ecs|
#   { ecs &
#       scalable: map2 ecs.killable ecs.scalable |killable, scalable|
#         { scalable &
#           radius: (killable.dead_frame |> Num.to_f32) / (killable.lifetime  |> Num.to_f32),
#         }

#   }
explode_system : ECS -> ECS
explode_system = |ecs|
  emissions = List.keep_oks(ecs.removals, |{id}|
    Result.map2(Dict.get ecs.positionable id, Dict.get ecs.explodable id, |position, explosion| { position, explosion, lifetime: 0 })
    |> Result.map2 (Dict.get ecs.killable id) |{position, explosion}, { lifetime }|
      List.map(List.range { start: At 0, end: Before explosion.num_particles}, |_|
        {
          position: position,
          lifetime: (lifetime |> Num.to_f32 |> Num.div 8 |> Num.round),
          explosion: { num_particles: 0 }
        }
      )
  ) |> List.join
  { ecs& emissions }

emitter_system : ECS -> ECS
emitter_system = |ecs|
  List.walk ecs.emissions ecs |world, { position, lifetime, explosion }|
    spawn world { position, num_particles: explosion.num_particles, lifetime }
  |> |world| { world & emissions: [] }

# map2 : Dict k a, Dict k b, (a, b -> c) -> Dict k c
# map2 = |dict1, dict2, f|
#   Dict.to_list dict1
#       |> List.keep_oks |(k, a)|
#         Dict.get(dict2, k) |> Result.map_ok |b| (k, f(a, b))
#       |> Dict.from_list

# routing_system : ECS, (Hex.Doubled -> Bool) -> ECS
routing_system = |ecs, is_occupied|
    get_by_component ecs [Path, Occupies]
    |> Dict.walk ecs |accum, id, c|
            when c is
                [Path { path, goal }, Occupies { cell }] ->
                    when path is
                        [_, dest, ..] if dest != cell && is_occupied dest ->
                          dbg "Re-routing entity[${Inspect.to_str id}] ${Inspect.to_str cell} --> ${Inspect.to_str dest} to ${Inspect.to_str goal}"
                          move_to accum id goal
                          |> |ecs_| {ecs_ & }
                        _ -> accum
                _ -> accum



removal_system = |ecs|
  List.walk ecs.removals ecs |accum, removal|
      remove_entity accum removal.id
  |> |ecs1| { ecs1 & removals: [] }


update = |model, dt, is_blocked, cost_fn|
  occupants = Dict.values model.ecs.occupants
  is_occupied = |test_cell|
      List.walk_until occupants Bool.false |_, {cell}|
        if test_cell == cell || is_blocked cell then
          Break Bool.true
        else
          Continue Bool.false

  path_finder = HexTile.make_path_finder model.map is_occupied

  {
    model &
    ecs: movement_system model.ecs dt
      |> kill_system
      |> explode_system
      |> emitter_system
      |> removal_system
      |> lifetime_system
      |> transform_system
      |> move_request_system
      |> path_request_system path_finder
      |> move_segment_system dt cost_fn
      |> routing_system is_occupied
  }
