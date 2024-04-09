howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | isEqual x y && isEqual y z = 3
    | isEqual x y || isEqual y z || isEqual x z = 2
    | otherwise = 0

isEqual :: Int -> Int -> Bool
isEqual x y = x == y 