module [Entity, CompDeathTime, CompFade, CompExplode, CompGraphic, CompVelocity, CompPosition, ECS, spawn, make, update]
import rr.RocRay exposing [Color, rgba]
import rand.Random
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
  radius: F32,
}

CompPosition : {
  x: F32,
  y: F32,
}
CompVelocity : {
  dx: F32,
  dy: F32,
}
CompAcceleration : {
  ddx: F32,
  ddy: F32
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
  explodable: Dict.empty {},
  scalable: Dict.empty {},
}

add_entity : ECS -> (ECS, I32)
add_entity = |ecs|
  id = ecs.next_size + 1

  ({
    ecs&
    next_size: id,
    entities: List.append ecs.entities { id },
    current_size: ecs.current_size + 1,
    fadable: Dict.insert ecs.fadable id { rRate: 0, rMin: 0,  gRate: 0, gMin: 0, bRate: 0, bMin: 0, aRate: 0, aMin: 0 },
    explodable: Dict.insert ecs.explodable id { num_particles: 0 },
    positionable: Dict.insert ecs.positionable id { x: 0, y: 0 },
    moveable: Dict.insert ecs.moveable id { dx: 0.25, dy: 0.15 },
  }, id)

spawn : ECS, { x: F32, y: F32 }, I32 -> ECS
spawn = |ecs, position, num_particles|
  rand_v = { Random.chain  <-
    life: Random.bounded_u32(20, 20 * 3),
    dx: Random.bounded_i32 -50_000 90_000,
    dy: Random.bounded_i32 -50_000 90_000,
    r: Random.bounded_u8(0, 255),
    g: Random.bounded_u8(0, 255),
    b: Random.bounded_u8(0, 255),
    a: Random.bounded_u8(128, 255),
  } |> Random.map |{life, dx, dy, r, g, b, a}| {
      life,
      dx: Num.to_f32 dx |> Num.div 1_000_000,
      dy: Num.to_f32 dy |> Num.div 1_000_000,
      color: RGBA(r, g, b, a)
  }
  next_v = Random.step ecs.rand rand_v
  life_frames = next_v.value.life |> Num.to_i32
  life_scale = 10 / (Num.to_f32 life_frames)
  (ecs1, id) = add_entity ecs

  { ecs1&
    rand: next_v.state,
    killable: Dict.insert ecs1.killable id { lifetime: life_frames, dead_frame: life_frames },
    explodable: if num_particles <= 0 then Dict.remove ecs1.explodable id else Dict.insert ecs1.explodable id { num_particles },
    scalable: Dict.insert ecs.scalable id {
        radius: life_scale |> Num.to_f32,
        color: next_v.value.color,
    },
    positionable: Dict.insert ecs.positionable id position,
    moveable: Dict.insert ecs.moveable id { dx: next_v.value.dx, dy: next_v.value.dy },
  }

movement_system : ECS, U64 -> _
movement_system = |ecs, dt|
    dt_ = Num.to_f32 dt
    helper = movement_helper dt_
    { ecs&
      positionable:
        Dict.walk ecs.moveable ecs.positionable |pos, key, value| helper pos key value
    }
movement_helper : F32 -> (Dict I32 CompPosition, I32, CompVelocity -> Dict I32 CompPosition)
movement_helper = |dt_|
  |positions, id, { dx, dy }|
    Dict.get positions id
      |> Result.map_ok |{x, y}| Dict.insert positions id { x: x + dx * dt_, y: y + dy * dt_}
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

}

lifetime_system : ECS -> ECS
lifetime_system = |ecs|

    updated_killable = Dict.map ecs.killable |_, {lifetime, dead_frame}|
      { dead_frame: if dead_frame > 0 then dead_frame - 1 else 0, lifetime }

    Dict.walk updated_killable { ecs & killable: updated_killable } |ecs_, id, { lifetime, dead_frame }|
      if dead_frame <= 0 then
        Dict.get ecs_.explodable id
          |> Result.try | { num_particles } |
            Dict.get(ecs_.positionable, id)
            |> Result.map_ok(|pos|
                List.walk(List.range({start: At 1, end: At num_particles}), ecs_, |accum, _|
                  spawn(accum, { x: pos.x, y: pos.y }, 0)
                )
              )
          |> Result.with_default ecs_
          |> remove_entity id
      else { ecs_&
        scalable: Dict.update ecs_.scalable id |value|
          when value is
            Ok { color } -> Ok { radius: (dead_frame |> Num.to_f32) / (lifetime|> Num.to_f32), color }
            Err Missing -> Err Missing
      }


update = |model, dt|
  {
    model &
    ecs: movement_system model.ecs dt |> lifetime_system |> |ecs|
      if ecs.current_size == 0 then spawn ecs { x: 12, y: 15 } 16 #|> spawn { x: 5, y: 25 } 0
      else ecs
  }
