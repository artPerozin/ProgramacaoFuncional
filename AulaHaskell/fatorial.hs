fatorialDe :: Int -> Int
fatorialDe x 
    | x == 0 = 1
    | otherwise = x * fatorialDe (x - 1)