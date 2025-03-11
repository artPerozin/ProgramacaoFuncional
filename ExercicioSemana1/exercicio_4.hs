sumParNumbers :: Int -> Int
sumParNumbers x
    | x <= 0 = 0
    | x `mod` 2 == 0 = x + sumParNumbers (x - 2)
    | otherwise = sumParNumbers (x - 1)
