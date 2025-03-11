isPrimo :: Int -> String
isPrimo x
    | x <= 1    = "Número Inválido"
    | x == 2    = "Primo"
    | otherwise = if any (\y -> x `mod` y == 0) [2..(floor (sqrt (fromIntegral x)))] then "Nao Primo" else "Primo"
