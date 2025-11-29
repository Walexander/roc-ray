module [process_trauma]
import Model
import Hex exposing [lerp]

process_trauma = |world, move|
  launch_state = world.launch_state
  countdown_pct = Num.max(0, 1 - (Num.to_f32 world.countdown / Num.to_f32 world.countdown_start))
  timer_trauma  = when launch_state is
      InControl _ if world.countdown > 0 -> countdown_pct
      _ -> Num.max(world.trauma - 0.01, 0)
  player_trauma = when move is
    AddTrauma -> lerp world.trauma 1 0.5
    _ -> 0

  { world &
      trauma: Num.max(
      (Hex.lerp world.trauma timer_trauma 0.5),
      player_trauma
    )
  }
