anyDigit :: Int -> Int -> Int
anyDigit x y = getNumberWithIndice (invertList (transformIntToList y)) x

transformIntToList :: Int -> [Int]
transformIntToList 0 = []
transformIntToList x = transformIntToList (x `div` 10) ++ [x `mod` 10]

invertList :: [a] -> [a]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

getNumberWithIndice :: [Int] -> Int -> Int
getNumberWithIndice [] _ = (-1)
getNumberWithIndice x i
    | i == ((length' x) - 1) = head' x
    | otherwise = getNumberWithIndice (tail' x) i

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs