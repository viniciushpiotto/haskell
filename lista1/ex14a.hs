sales :: Int -> Int
sales 1 = 23
sales 2 = 12
sales 3 = 2
sales 4 = 12
sales 5 = 20
sales 6 = 0
sales 7 = 1

howManyLess :: Int -> Int -> Int -> Int
howManyLess value x y = lessValueDays (makeList x y) value

lessValueDays :: [Int] -> Int -> Int
lessValueDays [] _ = 0
lessValueDays (x:xs) n
    | x < n = 1 + lessValueDays xs n
    | otherwise = lessValueDays xs n

makeList :: Int -> Int -> [Int]
makeList x y
    | x == y = [sales x]
makeList x y = [sales x] ++ makeList (x + 1) y