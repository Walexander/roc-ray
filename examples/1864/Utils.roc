module [ frameCountToSeconds ]

frameCountToSeconds = \x -> Num.to_frac x |> Num.div 60
