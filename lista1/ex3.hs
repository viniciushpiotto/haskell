soma:: Int -> Int -> Int
soma x y =  x + y

multWithSoma :: Int -> Int -> Int
multWithSoma _ 0 = 0
multWithSoma 0 _ = 0
multWithSoma x 1 = x
multWithSoma x y = soma x 0 + multWithSoma x (y - 1) 