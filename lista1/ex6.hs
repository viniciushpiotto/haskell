square6 :: Int -> Double
square6 0 = sqrt 6
square6 x = sqrt (6 + square6 (x - 1))