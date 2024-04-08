invertInt :: Int -> Int
invertInt x = transformListToInt (invertList (transformIntToList x))

transformIntToList :: Int -> [Int]
transformIntToList 0 = []
transformIntToList x = transformIntToList (x `div` 10) ++ [x `mod` 10]

invertList :: [Int] -> [Int]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

length' :: [a] -> Int
length' [] = 0 
length' (_:xs) = 1 + length' xs

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

transformListToInt :: [Int] -> Int
transformListToInt [] = 0
transformListToInt x = (head' x) * (10 ^ ((length' x) - 1)) + transformListToInt (tail' x)