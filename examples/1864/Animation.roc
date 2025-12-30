module [Animation, process, make, percent]
Animation : {
  start_time: F32,
  duration: F32,
  timer: F32,
  finished: Bool,
}

make : { duration: F32, start_time: U64 } -> Animation
make = |{ duration, start_time }|
  { start_time: Num.to_f32 start_time |> Num.div 1000,
    duration,
    finished: Bool.false,
    timer: 0, }

process : Animation, U64 -> Animation
process = |animation, dt|
  { animation & timer: animation.timer + (Num.to_f32 dt|>Num.div 1000), finished: animation.timer >= animation.duration }
  # if (Num.to_f32 new_timer) >= frame_time then
  #   { animation & timer: animation.timer - (Num.round frame_time), frame_index: (animation.frame_index + 1) % animation.frame_count }
  # else
  #   { animation & timer: new_timer, finished: Bool.true }

percent : Animation -> F32
percent = |animation|
  Num.to_f32 animation.timer |> Num.div animation.duration
