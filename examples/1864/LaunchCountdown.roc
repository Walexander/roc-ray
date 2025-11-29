module [update]

update = |world, dt|
    count = when world.launch_state is
        InControl _ -> Num.max(dt, world.countdown) - dt
        _ -> world.countdown
    { world & countdown: Num.max 0 count}

