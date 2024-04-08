lastDigit :: Int -> Int
lastDigit x = head' (invertList (transformIntToList x)) 

transformIntToList :: Int -> [Int]
transformIntToList 0 = []
transformIntToList x = transformIntToList (x `div` 10) ++ [x `mod` 10]

invertList :: [Int] -> [Int]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

head' :: [Int] -> Int
head' (x:_) = x