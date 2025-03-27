potencia :: Int -> Int -> Int
potencia x y
    | y == 0 = 1
    | otherwise = x * potencia x (y - 1)