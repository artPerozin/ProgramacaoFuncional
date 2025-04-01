seriePI :: Int -> Double
seriePI n 
  | n <= 0 = 0 
  | otherwise = somaSerie n 1 1 0

somaSerie :: Int -> Int -> Double -> Double -> Double
somaSerie n i sinal soma
  | 4 / fromIntegral i <= 4 / fromIntegral n = soma
  | otherwise = somaSerie n (i + 2) (-sinal) (soma + sinal * (4 / fromIntegral i))
