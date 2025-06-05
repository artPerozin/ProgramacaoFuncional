fatiar :: Int -> Int -> [a] -> [a]
fatiar n m xs = fatiarAux n m 0 xs
    where
        fatiarAux _ _ _ [] = []
        fatiarAux n m i (x:xs)
            | i < n     = fatiarAux n m (i+1) xs
            | i > m     = []
            | otherwise = x : fatiarAux n m (i+1) xs
