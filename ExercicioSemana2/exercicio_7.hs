intercalacao :: [Int] -> [Int] -> [Int]
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys)
    | x <= y    = x : intercalacao xs (y:ys)
    | otherwise = y : intercalacao (x:xs) ys
