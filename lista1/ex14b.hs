sales :: Int -> Int
sales 1 = 23
sales 2 = 12
sales 3 = 2
sales 4 = 12
sales 5 = 20
sales 6 = 1
sales 7 = 1

noZeroInPeriod :: Bool
noZeroInPeriod = findZero (makeList 1 7)

findZero :: [Int] -> Bool
findZero [] = True
findZero (x:xs)
    | x == 0 = False
    | otherwise = findZero xs

makeList :: Int -> Int -> [Int]
makeList x y
    | x == y = [sales x]
makeList x y = [sales x] ++ makeList (x + 1) y
