howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x y z = length' (listWithMultiples (makeList y z) x)

makeList :: Int -> Int -> [Int]
makeList x y = [x .. y]

length' :: [a] -> Int
length' [] = 0 
length' (_:xs) = 1 + length' xs

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

listWithMultiples :: [Int] -> Int -> [Int]
listWithMultiples [] _ = []
listWithMultiples (x:xs) n = isMultiple [x] n ++ listWithMultiples xs n

isMultiple :: [Int] -> Int -> [Int]
isMultiple (x:_) n
    | x `mod` n == 0 = [x]
    | otherwise = []