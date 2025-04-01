isValidTriangle :: Int -> Int -> Int -> Bool
isValidTriangle a b c = a + b > c && a + c > b && b + c > a

triangleClass :: Int -> Int -> Int -> String
triangleClass a b c
    | a == b && b == c = "Equilatero"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Escaleno"

triangleClassWithValidation :: Int -> Int -> Int -> String
triangleClassWithValidation a b c
    | a + b < c || a + c < b || b + c < a = "Triangulo Invalido"
    | a == b && b == c = "Equilatero"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Escaleno"

sumParNumbers :: Int -> Int
sumParNumbers x
    | x <= 0 = 0
    | x `mod` 2 == 0 = x + sumParNumbers (x - 2)
    | otherwise = sumParNumbers (x - 1)

sumPot2m :: Int -> Int -> Int
sumPot2m m n
    | n >= 0 = m * (2^n) + sumPot2m m (n - 1)
    | otherwise = 0

seriePI :: Int -> Double
seriePI n 
  | n <= 0 = 0 
  | otherwise = somaSerie n 1 1 0

somaSerie :: Int -> Int -> Double -> Double -> Double
somaSerie n i sinal soma
  | 4 / fromIntegral i <= 4 / fromIntegral n = soma
  | otherwise = somaSerie n (i + 2) (-sinal) (soma + sinal * (4 / fromIntegral i))