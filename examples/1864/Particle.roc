module [Entity, System, CompDeathTime, CompFade, CompExplode, CompGraphic, CompVelocity, CompPosition, ECS, spawn, make, update, get_by_component]
import rr.RocRay exposing [Color ]
import Hex
import PointyHex
import rand.Random
import Matrix4 exposing [Matrix4]

Entity : {
  id: I32,
}
CompDeathTime: {
  lifetime: I32,
  dead_frame: I32,
}
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

CompAttack: {
  attack_damaage: U16,
}

CompPosition : {
  x: F32,
  y: F32,
}

CompRotation : {
  radians: F32,
}
CompTransform : {
  transform: Matrix4
}

CompVelocity : {
  dx: F32,
  dy: F32,
}

CompOccupies : {
  cell: Hex.Doubled,
  army: [ Union, Confederates ]
}
System: ECS -> ECS
AttackOrders : {
  id: I32,
  target: CompOccupies,
  target_id: I32,
}
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
  # occupants_by_cell: Dict Hex.Doubled I32,

  emissions: List { lifetime: I32, position: CompPosition, explosion: CompExplode },
  removals: List Entity,
  attack_orders: List AttackOrders,
}



gen_entities = |count|
  List.repeat { id: 0 } count
  |> gen_entities_helper 0

gen_entities_helper = |entities, index|
  List.get entities index
    |> Result.map_ok |entity|
      List.set entities index { entity& id: Num.to_i32 index }
      |> gen_entities_helper (index + 1)
    |> Result.with_default entities

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
  # occupants_by_cell: Dict.empty {},
  emissions: [],
  removals: [],
  attack_orders: [],
}

add_entity : ECS -> (ECS, I32)
add_entity = |ecs|
  id = ecs.next_size + 1

  ({
    ecs&
    next_size: id,
    entities: List.append ecs.entities { id },
    current_size: ecs.current_size + 1,
    # fadable: Dict.insert ecs.fadable id { rRate: 0, rMin: 0,  gRate: 0, gMin: 0, bRate: 0, bMin: 0, aRate: 0, aMin: 0 },
    # explodable: Dict.insert ecs.explodable id { num_particles: 0 },
    # positionable: Dict.insert ecs.positionable id { x: 0, y: 0 },
    # moveable: Dict.insert ecs.moveable id { dx: 0.25, dy: 0.15 },
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
      # dx: (Num.to_f32 life) / 64, #Num.to_f32 dx |> Num.mul (Num.to_f32 max_lifetime) |> Num.div (max_lifetime|> Num.to_f32 |> Num.mul 5_000_000),
      dx: dx |> Num.to_f32 |> Num.div 100_00_000,
      dy: dy |> Num.to_f32 |> Num.div 100_00_000,
      # dy: Num.to_f32 dy |> Num.div 100_000_000,  #|> Num.mul Num.mul (Num.to_f32 max_lifetime) |> Num.div (max_lifetime|> Num.to_f32 |> Num.mul 100_000_000),
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


Components : [
  Position,
  Moveable,
  Transform,
  Graphics,
  Killable,
]
ComponentData : [
  Position CompPosition,
  Moveable CompVelocity,
  Transform CompTransform,
  Graphics CompGraphic,
  Killable CompDeathTime,
]

  # get_by_component : ECS, List Components -> Dict I32 (List [Moveable CompVelocity, Position CompPosition, Transform CompTransform])
get_by_component : ECS, List Components -> Dict I32 (List ComponentData)
get_by_component = |ecs, components|
   List.walk_with_index components (Set.empty {}) | ids, component, index|
        c_ids = when component is
            Transform -> Set.from_list(Dict.keys ecs.transformable)
            Moveable -> Set.from_list(Dict.keys ecs.moveable)
            Position -> Set.from_list(Dict.keys ecs.positionable)
            Graphics -> Set.from_list(Dict.keys ecs.scalable)
            Killable -> Set.from_list(Dict.keys ecs.scalable)

        if index == 0 then c_ids
        else Set.union ids c_ids
  |> Set.walk (Dict.empty {}) |accum, id|
      List.walk components accum |accum2, comp|
        result = when comp is
          Position -> Dict.get(ecs.positionable, id) |> Result.map_ok Position
          Transform -> Dict.get(ecs.transformable, id) |> Result.map_ok Transform
          Moveable -> Dict.get(ecs.moveable, id) |> Result.map_ok Moveable
          Graphics -> Dict.get(ecs.scalable, id) |> Result.map_ok Graphics
          Killable -> Dict.get(ecs.killable, id) |> Result.map_ok Killable
        data = Result.map_ok result List.single |> Result.with_default []
        existing = Dict.get accum2 id |> Result.with_default []
        new_data = List.concat existing data
        Dict.insert accum2 id new_data


movement_system : ECS, U64 -> _
movement_system = |ecs, dt|
    dt_ = Num.to_f32 dt
    helper = movement_helper dt_
    { ecs&
      # positionable:
      #   Dict.walk ecs.moveable ecs.positionable |pos, key, value| helper pos key value
    }
movement_helper : F32 -> (Dict I32 CompPosition, I32, CompVelocity -> Dict I32 CompPosition)
movement_helper = |dt_|
  |positions, id, { dx, dy }|
    Dict.get positions id
      |> Result.map_ok | {x, y}|
          data = { x: x + dx * dt_, y: y + dy * dt_}
          Dict.insert positions id data
      |> Result.with_default positions

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
  {ecs & removals }


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

map2 : Dict k a, Dict k b, (a, b -> c) -> Dict k c
map2 = |dict1, dict2, f|
  Dict.to_list dict1
      |> List.keep_oks |(k, a)|
        Dict.get(dict2, k) |> Result.map_ok |b| (k, f(a, b))
      |> Dict.from_list


removal_system = |ecs|
  List.walk ecs.removals ecs |accum, removal|
      remove_entity accum removal.id
  |> |ecs1| { ecs1 & removals: [] }

update = |model, dt|
  {
    model &
    ecs: movement_system model.ecs dt
      |> kill_system
      # |> scaling_system
      |> explode_system
      |> emitter_system
      |> removal_system
      |> lifetime_system
      |> transform_system
      # |> |ecs|
      #   if ecs.current_size == 0 then
      #     spawn(ecs, { position: { x: 48, y: -92 }, num_particles:  1, max_lifetime: 180 })
      #     # |> spawn { position: { x: 16, y: 64 }, num_particles:  16, max_lifetime: 30 }
      #   else ecs
  }
