sumPot2m :: Int -> Int -> Int
sumPot2m m n
    | n >= 0 = m * (2^n) + sumPot2m m (n - 1)
    | otherwise = 0