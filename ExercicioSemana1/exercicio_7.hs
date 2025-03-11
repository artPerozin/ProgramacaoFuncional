seriePi :: Int -> Double
seriePi n 
    | n <= 0    = 0 
    | even n    = seriePi (n - 1)
    | otherwise = somaSerie n 1 1 0

somaSerie :: Int -> Int -> Double -> Double -> Double
somaSerie n i sinal soma
    | termo < 4 / fromIntegral n = soma
    | otherwise = somaSerie n (i + 2) (-sinal) (soma + sinal * termo)
  where
    termo = 4 / fromIntegral i