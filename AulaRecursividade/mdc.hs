mdc :: Int -> Int -> Int
mdc x y
    | y == 0 = x
    | otherwise = mdc y (mod x y)