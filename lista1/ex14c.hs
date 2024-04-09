sales :: Int -> Int
sales 1 = 0
sales 2 = 12
sales 3 = 0
sales 4 = 12
sales 5 = 20
sales 6 = 0
sales 7 = 1

zerosInPeriod :: [Int]
zerosInPeriod = daysWithZero 1 7

daysWithZero :: Int -> Int -> [Int]
daysWithZero x y
    | x == y = isZeroDay x
    | otherwise = isZeroDay x ++ (daysWithZero (x + 1) y)

isZeroDay :: Int -> [Int]
isZeroDay x
    | sales x == 0 = [x]
    | otherwise = []