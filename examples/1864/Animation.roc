module [Animation, process, make]
Animation : {
  frame_index: I64,
  timer: I64,

  frame_count: I64,
  start_time: U64,
  fps: F32
}

make = |{ frame_count, start_time, fps }|
  { frame_index: 0, timer: 0, frame_count, start_time, fps }

process : Animation, U64 -> Animation
process = |animation, dt|
  deltaTime = dt |> Num.to_i64
  new_timer = animation.timer + (deltaTime |> Num.to_i64)
  frame_time = 1_000 / (animation.fps)

  if (Num.to_f32 new_timer) >= frame_time then
    { animation & timer: animation.timer - (Num.round frame_time), frame_index: (animation.frame_index + 1) % animation.frame_count }
  else
    { animation & timer: new_timer }
