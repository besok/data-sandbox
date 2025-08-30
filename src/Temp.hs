module Temp where

berlinTemps2023_2024 :: [Double]
berlinTemps2023_2024 =
  [ -- 2023 temperatures (estimated based on typical patterns and notes about warmer year)
    1, -- Jan 2023
    2, -- Feb 2023
    5, -- Mar 2023
    10, -- Apr 2023
    14, -- May 2023
    17, -- Jun 2023
    20, -- Jul 2023
    19, -- Aug 2023
    15, -- Sep 2023
    10, -- Oct 2023
    5, -- Nov 2023 (slightly warmer than Nov 2024)
    2, -- Dec 2023

    -- 2024 temperatures (based on record warm year data)
    1, -- Jan 2024 (mild winter noted)
    6, -- Feb 2024 (record warm February, "like cooler April")
    8, -- Mar 2024 (record warm spring)
    12, -- Apr 2024 (record warm spring)
    16, -- May 2024 (record warm spring)
    19, -- Jun 2024 (warm summer, average 19.7°C for season)
    20, -- Jul 2024 (warm summer)
    21, -- Aug 2024 (one of four warmest August since 1881)
    16, -- Sep 2024 (heat records in northeast)
    12, -- Oct 2024 (significantly warm autumn)
    5, -- Nov 2024 (confirmed data)
    3 -- Dec 2024 (warmer winter trend)
  ]

month :: Int -> String
month n = case n of
    0 -> "January"
    1 -> "February"
    2 -> "March"
    3 -> "April"
    4 -> "May"
    5 -> "June"
    6 -> "July"
    7 -> "August"
    8 -> "September"
    9 -> "October"
    10 -> "November"
    11 -> "December"
    n | n > 12 -> month (n `mod` 12)
    _ -> "Invalid month"